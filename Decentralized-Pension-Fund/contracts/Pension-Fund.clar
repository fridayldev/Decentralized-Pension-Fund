;; Decentralized Pension Fund Smart Contract
;; Community-managed retirement savings with transparent governance

;; Contract constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1001))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1002))
(define-constant ERR_INVALID_AMOUNT (err u1003))
(define-constant ERR_NOT_ELIGIBLE (err u1004))
(define-constant ERR_ALREADY_WITHDRAWN (err u1005))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u1006))
(define-constant ERR_VOTING_ENDED (err u1007))
(define-constant ERR_ALREADY_VOTED (err u1008))
(define-constant ERR_INVALID_PROPOSAL (err u1009))
(define-constant MIN_CONTRIBUTION u1000000) ;; 1 STX minimum
(define-constant RETIREMENT_AGE_BLOCKS u1051200) ;; ~2 years in blocks
(define-constant VOTING_PERIOD u4320) ;; ~30 days in blocks

;; Data variables
(define-data-var next-proposal-id uint u1)
(define-data-var total-fund-balance uint u0)
(define-data-var fund-yield-rate uint u500) ;; 5% annual yield (basis points)

;; Data maps
(define-map Contributors 
  principal 
  {
    balance: uint,
    contribution-start-block: uint,
    total-contributed: uint,
    last-yield-claim: uint,
    is-active: bool
  }
)

(define-map Proposals
  uint
  {
    proposer: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    proposal-type: (string-ascii 20),
    amount: uint,
    recipient: principal,
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool
  }
)

(define-map ProposalVotes
  {proposal-id: uint, voter: principal}
  {vote: bool, voting-power: uint}
)

(define-map Governance
  principal
  {
    voting-power: uint,
    last-activity-block: uint
  }
)

;; Private functions
(define-private (calculate-yield (balance uint) (blocks-passed uint))
  (let ((annual-blocks u52560)) ;; ~1 year in blocks
    (/ (* (* balance (var-get fund-yield-rate)) blocks-passed) (* annual-blocks u10000))
  )
)

(define-private (update-voting-power (contributor principal) (amount uint))
  (let ((current-power (default-to {voting-power: u0, last-activity-block: u0} 
                                  (map-get? Governance contributor))))
    (map-set Governance contributor 
      {
        voting-power: (+ (get voting-power current-power) amount),
        last-activity-block: block-height
      }
    )
  )
)

(define-private (is-proposal-active (proposal-id uint))
  (match (map-get? Proposals proposal-id)
    proposal (and 
               (>= block-height (get start-block proposal))
               (<= block-height (get end-block proposal))
               (not (get executed proposal)))
    false
  )
)

;; Public functions

;; Contribute to pension fund
(define-public (contribute (amount uint))
  (let ((contributor tx-sender)
        (current-data (default-to 
          {balance: u0, contribution-start-block: u0, total-contributed: u0, last-yield-claim: u0, is-active: false}
          (map-get? Contributors contributor))))
    
    (asserts! (>= amount MIN_CONTRIBUTION) ERR_INVALID_AMOUNT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount contributor (as-contract tx-sender)))
    
    ;; Update contributor data
    (map-set Contributors contributor
      {
        balance: (+ (get balance current-data) amount),
        contribution-start-block: (if (get is-active current-data) 
                                    (get contribution-start-block current-data) 
                                    block-height),
        total-contributed: (+ (get total-contributed current-data) amount),
        last-yield-claim: block-height,
        is-active: true
      }
    )
    
    ;; Update voting power
    (update-voting-power contributor amount)
    
    ;; Update total fund balance
    (var-set total-fund-balance (+ (var-get total-fund-balance) amount))
    
    (ok true)
  )
)

;; Claim yield rewards
(define-public (claim-yield)
  (let ((contributor tx-sender)
        (contributor-data (unwrap! (map-get? Contributors contributor) ERR_NOT_ELIGIBLE)))
    
    (asserts! (get is-active contributor-data) ERR_NOT_ELIGIBLE)
    
    (let ((blocks-since-claim (- block-height (get last-yield-claim contributor-data)))
          (yield-amount (calculate-yield (get balance contributor-data) blocks-since-claim)))
      
      (asserts! (> yield-amount u0) ERR_INVALID_AMOUNT)
      (asserts! (>= (stx-get-balance (as-contract tx-sender)) yield-amount) ERR_INSUFFICIENT_BALANCE)
      
      ;; Transfer yield to contributor
      (try! (as-contract (stx-transfer? yield-amount tx-sender contributor)))
      
      ;; Update last yield claim
      (map-set Contributors contributor
        (merge contributor-data {last-yield-claim: block-height})
      )
      
      (ok yield-amount)
    )
  )
)

;; Withdraw pension (only after retirement age)
(define-public (withdraw-pension)
  (let ((contributor tx-sender)
        (contributor-data (unwrap! (map-get? Contributors contributor) ERR_NOT_ELIGIBLE)))
    
    (asserts! (get is-active contributor-data) ERR_NOT_ELIGIBLE)
    (asserts! (>= (- block-height (get contribution-start-block contributor-data)) RETIREMENT_AGE_BLOCKS) 
              ERR_NOT_ELIGIBLE)
    
    (let ((withdrawal-amount (get balance contributor-data)))
      (asserts! (> withdrawal-amount u0) ERR_INSUFFICIENT_BALANCE)
      (asserts! (>= (stx-get-balance (as-contract tx-sender)) withdrawal-amount) ERR_INSUFFICIENT_BALANCE)
      
      ;; Transfer pension to contributor
      (try! (as-contract (stx-transfer? withdrawal-amount tx-sender contributor)))
      
      ;; Mark contributor as inactive and reset balance
      (map-set Contributors contributor
        (merge contributor-data {balance: u0, is-active: false})
      )
      
      ;; Update total fund balance
      (var-set total-fund-balance (- (var-get total-fund-balance) withdrawal-amount))
      
      (ok withdrawal-amount)
    )
  )
)

;; Create governance proposal
(define-public (create-proposal (title (string-utf8 100)) 
                               (description (string-utf8 500))
                               (proposal-type (string-ascii 20))
                               (amount uint)
                               (recipient principal))
  (let ((proposer tx-sender)
        (proposal-id (var-get next-proposal-id))
        (governance-data (unwrap! (map-get? Governance proposer) ERR_UNAUTHORIZED)))
    
    (asserts! (>= (get voting-power governance-data) u1000000) ERR_UNAUTHORIZED) ;; Min 1 STX voting power
    
    (map-set Proposals proposal-id
      {
        proposer: proposer,
        title: title,
        description: description,
        proposal-type: proposal-type,
        amount: amount,
        recipient: recipient,
        votes-for: u0,
        votes-against: u0,
        start-block: block-height,
        end-block: (+ block-height VOTING_PERIOD),
        executed: false
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let ((voter tx-sender)
        (governance-data (unwrap! (map-get? Governance voter) ERR_UNAUTHORIZED))
        (proposal (unwrap! (map-get? Proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
    
    (asserts! (is-proposal-active proposal-id) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? ProposalVotes {proposal-id: proposal-id, voter: voter})) ERR_ALREADY_VOTED)
    
    (let ((voting-power (get voting-power governance-data)))
      ;; Record vote
      (map-set ProposalVotes {proposal-id: proposal-id, voter: voter}
        {vote: support, voting-power: voting-power}
      )
      
      ;; Update proposal vote counts
      (map-set Proposals proposal-id
        (if support
          (merge proposal {votes-for: (+ (get votes-for proposal) voting-power)})
          (merge proposal {votes-against: (+ (get votes-against proposal) voting-power)})
        )
      )
      
      (ok true)
    )
  )
)

;; Execute proposal (if voting passed)
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (map-get? Proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
    
    (asserts! (> block-height (get end-block proposal)) ERR_VOTING_ENDED)
    (asserts! (not (get executed proposal)) ERR_ALREADY_WITHDRAWN)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_UNAUTHORIZED)
    
    ;; Execute based on proposal type
    (if (is-eq (get proposal-type proposal) "fund-transfer")
      (begin
        (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
        (var-set total-fund-balance (- (var-get total-fund-balance) (get amount proposal)))
      )
      ;; Add other proposal types as needed
      true
    )
    
    ;; Mark as executed
    (map-set Proposals proposal-id (merge proposal {executed: true}))
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-contributor-info (contributor principal))
  (map-get? Contributors contributor)
)

(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? Proposals proposal-id)
)

(define-read-only (get-governance-info (participant principal))
  (map-get? Governance participant)
)

(define-read-only (get-total-fund-balance)
  (var-get total-fund-balance)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (calculate-potential-yield (contributor principal))
  (match (map-get? Contributors contributor)
    data (let ((blocks-since-claim (- block-height (get last-yield-claim data))))
           (calculate-yield (get balance data) blocks-since-claim))
    u0
  )
)

(define-read-only (is-eligible-for-withdrawal (contributor principal))
  (match (map-get? Contributors contributor)
    data (and 
           (get is-active data)
           (>= (- block-height (get contribution-start-block data)) RETIREMENT_AGE_BLOCKS))
    false
  )
)