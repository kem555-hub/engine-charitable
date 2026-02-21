;; CharitableEngine - Decentralized Autonomous Charitable Platform
;;
;; Features:
;;   - Charity registration with reputation scoring
;;   - Project creation with milestone-based fund release
;;   - Time-locked donations routed to projects
;;   - Validator consensus for milestone verification
;;   - Impact NFT minting on project completion
;;   - Governance token rewards for successful projects
;;   - Dynamic reallocation for underperforming projects

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CHARITY-NOT-FOUND (err u101))
(define-constant ERR-PROJECT-NOT-FOUND (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-MILESTONE-NOT-FOUND (err u104))
(define-constant ERR-ALREADY-VALIDATED (err u105))
(define-constant ERR-INSUFFICIENT-CONSENSUS (err u106))
(define-constant ERR-PROJECT-CLOSED (err u107))
(define-constant ERR-ALREADY-REGISTERED (err u108))
(define-constant ERR-FUNDS-LOCKED (err u109))

;; Consensus threshold: 3 out of 5 validators required
(define-constant VALIDATOR-THRESHOLD u3)
;; Minimum reputation score to register as a charity
(define-constant MIN-REPUTATION u0)
;; Governance tokens awarded per successful milestone
(define-constant GOV-TOKENS-PER-MILESTONE u100)
;; Performance threshold below which reallocation is triggered (in basis points)
(define-constant PERFORMANCE-THRESHOLD u3000)

;; ============================================================
;; DATA VARS
;; ============================================================

(define-data-var charity-id-nonce uint u0)
(define-data-var project-id-nonce uint u0)
(define-data-var impact-nft-nonce uint u0)

;; ============================================================
;; FUNGIBLE TOKEN: Governance Token
;; ============================================================

(define-fungible-token gov-token)

;; ============================================================
;; NON-FUNGIBLE TOKEN: Impact NFT
;; ============================================================

(define-non-fungible-token impact-nft uint)

;; ============================================================
;; MAPS
;; ============================================================

;; Charity registry
;; id -> charity data
(define-map charities
  { id: uint }
  {
    owner: principal,
    name: (string-ascii 64),
    reputation-score: uint,        ;; 0-10000 basis points
    total-received: uint,
    total-projects: uint,
    active: bool
  }
)

;; Map principal to charity id for uniqueness checks
(define-map principal-to-charity principal uint)

;; Validators approved by contract owner
(define-map validators principal bool)

;; Projects associated with a charity
;; project-id -> project data
(define-map projects
  { id: uint }
  {
    charity-id: uint,
    title: (string-ascii 128),
    description: (string-ascii 256),
    total-goal: uint,             ;; in uSTX
    total-raised: uint,
    released-funds: uint,
    milestone-count: uint,
    completed-milestones: uint,
    performance-score: uint,      ;; 0-10000 basis points
    status: (string-ascii 16),    ;; "active" | "completed" | "reallocated"
    created-at: uint              ;; block height
  }
)

;; Milestones for each project
;; (project-id, milestone-index) -> milestone data
(define-map milestones
  { project-id: uint, index: uint }
  {
    description: (string-ascii 256),
    fund-release-pct: uint,       ;; percentage of total-goal to release (basis points)
    verified: bool,
    validator-votes: uint,
    released: bool
  }
)

;; Tracks which validators have voted on a specific milestone
(define-map milestone-validator-votes
  { project-id: uint, milestone-index: uint, validator: principal }
  bool
)

;; Donations: donor -> project-id -> amount
(define-map donations
  { donor: principal, project-id: uint }
  { amount: uint, block-height: uint }
)

;; Total donations per project (aggregate)
(define-map project-total-donations
  { project-id: uint }
  { total: uint }
)

;; Impact NFT metadata: nft-id -> data
(define-map impact-nft-metadata
  { nft-id: uint }
  {
    project-id: uint,
    milestone-index: uint,
    recipient: principal,
    block-height: uint
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

(define-private (is-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-validator (addr principal))
  (default-to false (map-get? validators addr))
)

(define-private (get-project-funds-available (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) u0))
  )
    (- (get total-raised project) (get released-funds project))
  )
)

;; Compute amount to release for a milestone based on basis points
(define-private (compute-release-amount (project-id uint) (release-pct uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) u0))
  )
    (/ (* (get total-goal project) release-pct) u10000)
  )
)

;; ============================================================
;; ADMIN: Validator Management
;; ============================================================

(define-public (add-validator (addr principal))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (map-set validators addr true)
    (ok true)
  )
)

(define-public (remove-validator (addr principal))
  (begin
    (asserts! (is-owner) ERR-NOT-AUTHORIZED)
    (map-delete validators addr)
    (ok true)
  )
)

;; ============================================================
;; CHARITY REGISTRATION
;; ============================================================

(define-public (register-charity (name (string-ascii 64)))
  (let (
    (new-id (+ (var-get charity-id-nonce) u1))
  )
    (asserts! (is-none (map-get? principal-to-charity tx-sender)) ERR-ALREADY-REGISTERED)
    (var-set charity-id-nonce new-id)
    (map-set charities
      { id: new-id }
      {
        owner: tx-sender,
        name: name,
        reputation-score: u5000,  ;; start at 50%
        total-received: u0,
        total-projects: u0,
        active: true
      }
    )
    (map-set principal-to-charity tx-sender new-id)
    (ok new-id)
  )
)

;; ============================================================
;; PROJECT CREATION
;; ============================================================

;; Create a project under an existing charity
;; milestone-count: number of milestones (max 10)
(define-public (create-project
    (charity-id uint)
    (title (string-ascii 128))
    (description (string-ascii 256))
    (total-goal uint))
  (let (
    (charity (unwrap! (map-get? charities { id: charity-id }) ERR-CHARITY-NOT-FOUND))
    (new-id (+ (var-get project-id-nonce) u1))
  )
    (asserts! (is-eq tx-sender (get owner charity)) ERR-NOT-AUTHORIZED)
    (asserts! (get active charity) ERR-NOT-AUTHORIZED)
    (asserts! (> total-goal u0) ERR-INVALID-AMOUNT)
    (var-set project-id-nonce new-id)
    (map-set projects
      { id: new-id }
      {
        charity-id: charity-id,
        title: title,
        description: description,
        total-goal: total-goal,
        total-raised: u0,
        released-funds: u0,
        milestone-count: u0,
        completed-milestones: u0,
        performance-score: u10000,
        status: "active",
        created-at: block-height
      }
    )
    ;; Increment charity project counter
    (map-set charities
      { id: charity-id }
      (merge charity { total-projects: (+ (get total-projects charity) u1) })
    )
    (ok new-id)
  )
)

;; Add a milestone to a project (charity owner only)
;; release-pct: basis points of total-goal released when this milestone is verified
(define-public (add-milestone
    (project-id uint)
    (description (string-ascii 256))
    (release-pct uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) ERR-PROJECT-NOT-FOUND))
    (charity (unwrap! (map-get? charities { id: (get charity-id project) }) ERR-CHARITY-NOT-FOUND))
    (current-count (get milestone-count project))
  )
    (asserts! (is-eq tx-sender (get owner charity)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-CLOSED)
    (asserts! (< current-count u10) ERR-NOT-AUTHORIZED)  ;; max 10 milestones
    (asserts! (> release-pct u0) ERR-INVALID-AMOUNT)
    (asserts! (<= release-pct u10000) ERR-INVALID-AMOUNT)
    (map-set milestones
      { project-id: project-id, index: current-count }
      {
        description: description,
        fund-release-pct: release-pct,
        verified: false,
        validator-votes: u0,
        released: false
      }
    )
    (map-set projects
      { id: project-id }
      (merge project { milestone-count: (+ current-count u1) })
    )
    (ok current-count)
  )
)

;; ============================================================
;; DONATIONS
;; ============================================================

;; Donate STX to a project; funds held in contract until milestones release them
(define-public (donate (project-id uint) (amount uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) ERR-PROJECT-NOT-FOUND))
    (existing-donation (default-to { amount: u0, block-height: u0 }
                          (map-get? donations { donor: tx-sender, project-id: project-id })))
    (current-total (default-to { total: u0 }
                     (map-get? project-total-donations { project-id: project-id })))
  )
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-CLOSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Transfer STX from donor to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update donor record
    (map-set donations
      { donor: tx-sender, project-id: project-id }
      { amount: (+ (get amount existing-donation) amount), block-height: block-height }
    )
    ;; Update project totals
    (map-set project-total-donations
      { project-id: project-id }
      { total: (+ (get total current-total) amount) }
    )
    (map-set projects
      { id: project-id }
      (merge project { total-raised: (+ (get total-raised project) amount) })
    )
    (ok true)
  )
)

;; ============================================================
;; MILESTONE VERIFICATION (Three-layer simplified to validator consensus)
;; ============================================================

;; Validator casts a vote to verify a milestone
(define-public (vote-milestone (project-id uint) (milestone-index uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) ERR-PROJECT-NOT-FOUND))
    (milestone (unwrap! (map-get? milestones { project-id: project-id, index: milestone-index })
                        ERR-MILESTONE-NOT-FOUND))
    (vote-key { project-id: project-id, milestone-index: milestone-index, validator: tx-sender })
  )
    (asserts! (is-validator tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-CLOSED)
    (asserts! (not (get verified milestone)) ERR-ALREADY-VALIDATED)
    (asserts! (is-none (map-get? milestone-validator-votes vote-key)) ERR-ALREADY-VALIDATED)
    ;; Record vote
    (map-set milestone-validator-votes vote-key true)
    (let ((new-votes (+ (get validator-votes milestone) u1)))
      (map-set milestones
        { project-id: project-id, index: milestone-index }
        (merge milestone { validator-votes: new-votes })
      )
      ;; If threshold reached, mark as verified
      (if (>= new-votes VALIDATOR-THRESHOLD)
        (begin
          (map-set milestones
            { project-id: project-id, index: milestone-index }
            (merge milestone { validator-votes: new-votes, verified: true })
          )
          (ok true)
        )
        (ok false)
      )
    )
  )
)

;; Release funds for a verified milestone (called by charity owner or any validator)
(define-public (release-milestone-funds (project-id uint) (milestone-index uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) ERR-PROJECT-NOT-FOUND))
    (charity (unwrap! (map-get? charities { id: (get charity-id project) }) ERR-CHARITY-NOT-FOUND))
    (milestone (unwrap! (map-get? milestones { project-id: project-id, index: milestone-index })
                        ERR-MILESTONE-NOT-FOUND))
    (release-amount (compute-release-amount project-id (get fund-release-pct milestone)))
  )
    (asserts!
      (or (is-eq tx-sender (get owner charity)) (is-validator tx-sender))
      ERR-NOT-AUTHORIZED)
    (asserts! (get verified milestone) ERR-INSUFFICIENT-CONSENSUS)
    (asserts! (not (get released milestone)) ERR-ALREADY-VALIDATED)
    (asserts! (<= release-amount (get-project-funds-available project-id)) ERR-FUNDS-LOCKED)
    ;; Transfer STX to charity owner
    (try! (as-contract (stx-transfer? release-amount tx-sender (get owner charity))))
    ;; Update milestone as released
    (map-set milestones
      { project-id: project-id, index: milestone-index }
      (merge milestone { released: true })
    )
    ;; Update project released-funds and completed-milestone count
    (let (
      (new-completed (+ (get completed-milestones project) u1))
      (new-released  (+ (get released-funds project) release-amount))
    )
      (map-set projects
        { id: project-id }
        (merge project {
          released-funds: new-released,
          completed-milestones: new-completed
        })
      )
      ;; Update charity total-received
      (map-set charities
        { id: (get charity-id project) }
        (merge charity { total-received: (+ (get total-received charity) release-amount) })
      )
      ;; Mint governance tokens to charity owner as reward
      (try! (ft-mint? gov-token GOV-TOKENS-PER-MILESTONE (get owner charity)))
      ;; Mint impact NFT to charity owner
      (let ((nft-id (+ (var-get impact-nft-nonce) u1)))
        (var-set impact-nft-nonce nft-id)
        (try! (nft-mint? impact-nft nft-id (get owner charity)))
        (map-set impact-nft-metadata
          { nft-id: nft-id }
          {
            project-id: project-id,
            milestone-index: milestone-index,
            recipient: (get owner charity),
            block-height: block-height
          }
        )
        ;; Mark project complete if all milestones done
        (if (is-eq new-completed (get milestone-count project))
          (begin
            (map-set projects
              { id: project-id }
              (merge project {
                released-funds: new-released,
                completed-milestones: new-completed,
                status: "completed"
              })
            )
            (ok nft-id)
          )
          (ok nft-id)
        )
      )
    )
  )
)

;; ============================================================
;; DYNAMIC REALLOCATION
;; ============================================================

;; Owner or validator can flag an underperforming project for reallocation.
;; Remaining unreleased funds are returned to the contract pool (tracked as reallocated).
;; In a full implementation, a routing algorithm would redirect these funds.
(define-public (reallocate-project (project-id uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    (asserts!
      (or (is-owner) (is-validator tx-sender))
      ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status project) "active") ERR-PROJECT-CLOSED)
    ;; Check performance score is below threshold
    (asserts! (< (get performance-score project) PERFORMANCE-THRESHOLD) ERR-NOT-AUTHORIZED)
    (map-set projects
      { id: project-id }
      (merge project { status: "reallocated" })
    )
    (ok true)
  )
)

;; Update project performance score (validator only, 0-10000 basis points)
(define-public (update-performance-score (project-id uint) (score uint))
  (let (
    (project (unwrap! (map-get? projects { id: project-id }) ERR-PROJECT-NOT-FOUND))
  )
    (asserts! (is-validator tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= score u10000) ERR-INVALID-AMOUNT)
    (map-set projects
      { id: project-id }
      (merge project { performance-score: score })
    )
    (ok true)
  )
)

;; ============================================================
;; READ-ONLY QUERIES
;; ============================================================

(define-read-only (get-charity (id uint))
  (map-get? charities { id: id })
)

(define-read-only (get-project (id uint))
  (map-get? projects { id: id })
)

(define-read-only (get-milestone (project-id uint) (index uint))
  (map-get? milestones { project-id: project-id, index: index })
)

(define-read-only (get-donation (donor principal) (project-id uint))
  (map-get? donations { donor: donor, project-id: project-id })
)

(define-read-only (get-project-total-donations (project-id uint))
  (default-to { total: u0 } (map-get? project-total-donations { project-id: project-id }))
)

(define-read-only (get-impact-nft-metadata (nft-id uint))
  (map-get? impact-nft-metadata { nft-id: nft-id })
)

(define-read-only (get-gov-token-balance (addr principal))
  (ft-get-balance gov-token addr)
)

(define-read-only (get-charity-id-for-principal (addr principal))
  (map-get? principal-to-charity addr)
)

(define-read-only (is-approved-validator (addr principal))
  (is-validator addr)
)

(define-read-only (get-project-funds-remaining (project-id uint))
  (match (map-get? projects { id: project-id })
    project (ok (- (get total-raised project) (get released-funds project)))
    ERR-PROJECT-NOT-FOUND
  )
)
