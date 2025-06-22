;; ===========================================
;; DECENTRALIZED SCIENTIFIC PUBLICATION PLATFORM
;; ===========================================

;; ===========================================
;; Contract 1: Scientific Papers & Reviews
;; File: contracts/scientific-papers.clar
;; ===========================================

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-PAPER-NOT-FOUND (err u1002))
(define-constant ERR-REVIEW-NOT-FOUND (err u1003))
(define-constant ERR-ALREADY-REVIEWED (err u1004))
(define-constant ERR-INVALID-SCORE (err u1005))
(define-constant ERR-PAPER-ALREADY-EXISTS (err u1006))
(define-constant ERR-INSUFFICIENT-REVIEWS (err u1007))
(define-constant ERR-INVALID-STATUS (err u1008))

;; Data Variables
(define-data-var next-paper-id uint u1)
(define-data-var next-review-id uint u1)
(define-data-var min-reviews-for-publication uint u3)
(define-data-var plagiarism-threshold uint u80) ;; 80% similarity threshold

;; Data Maps
(define-map papers
  uint
  {
    author: principal,
    title: (string-ascii 256),
    abstract: (string-ascii 1024),
    content-hash: (buff 32),
    citation-count: uint,
    publication-date: (optional uint),
    status: (string-ascii 20), ;; "submitted", "under-review", "published", "rejected"
    field: (string-ascii 50),
    plagiarism-score: uint,
    funding-required: uint,
    funding-raised: uint
  }
)

(define-map reviews
  uint
  {
    paper-id: uint,
    reviewer: principal,
    score: uint, ;; 1-10 score
    comments: (string-ascii 1024),
    review-date: uint,
    reward-claimed: bool
  }
)

(define-map paper-reviews uint (list 10 uint)) ;; Maps paper-id to list of review-ids
(define-map reviewer-papers principal (list 50 uint)) ;; Maps reviewer to papers they've reviewed
(define-map author-papers principal (list 100 uint)) ;; Maps author to their papers
(define-map citations { citing-paper: uint, cited-paper: uint } bool)
(define-map authorized-reviewers principal bool)

;; Authorization Functions
(define-public (authorize-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-reviewers reviewer true))
  )
)

(define-public (revoke-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-delete authorized-reviewers reviewer))
  )
)

;; Paper Submission
(define-public (submit-paper
  (title (string-ascii 256))
  (abstract (string-ascii 1024))
  (content-hash (buff 32))
  (field (string-ascii 50))
  (funding-required uint))
  (let
    (
      (paper-id (var-get next-paper-id))
      (current-height stacks-block-height)
    )
    (map-set papers paper-id {
      author: tx-sender,
      title: title,
      abstract: abstract,
      content-hash: content-hash,
      citation-count: u0,
      publication-date: none,
      status: "submitted",
      field: field,
      plagiarism-score: u0,
      funding-required: funding-required,
      funding-raised: u0
    })
    (map-set author-papers tx-sender
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? author-papers tx-sender)) paper-id)
        u100)))
    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  )
)

;; Plagiarism Detection (simplified - in production would integrate with external service)
(define-public (report-plagiarism (paper-id uint) (similarity-score uint))
  (begin
    (asserts! (default-to false (map-get? authorized-reviewers tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (<= similarity-score u100) ERR-INVALID-SCORE)
    (match (map-get? papers paper-id)
      paper (begin
        (map-set papers paper-id (merge paper { plagiarism-score: similarity-score }))
        (if (>= similarity-score (var-get plagiarism-threshold))
          (map-set papers paper-id (merge paper { status: "rejected" }))
          true)
        (ok true))
      ERR-PAPER-NOT-FOUND)
  )
)

;; Peer Review Submission
(define-public (submit-review
  (paper-id uint)
  (score uint)
  (comments (string-ascii 1024)))
  (let
    (
      (review-id (var-get next-review-id))
      (current-height stacks-block-height)
    )
    (asserts! (default-to false (map-get? authorized-reviewers tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= score u1) (<= score u10)) ERR-INVALID-SCORE)
    (asserts! (is-some (map-get? papers paper-id)) ERR-PAPER-NOT-FOUND)

    ;; Check if reviewer has already reviewed this paper
    (asserts! (is-none (index-of (default-to (list) (map-get? reviewer-papers tx-sender)) paper-id)) ERR-ALREADY-REVIEWED)

    ;; Create review
    (map-set reviews review-id {
      paper-id: paper-id,
      reviewer: tx-sender,
      score: score,
      comments: comments,
      review-date: current-height,
      reward-claimed: false
    })

    ;; Update paper reviews list
    (map-set paper-reviews paper-id
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? paper-reviews paper-id)) review-id)
        u10)))

    ;; Update reviewer papers list
    (map-set reviewer-papers tx-sender
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? reviewer-papers tx-sender)) paper-id)
        u50)))

    (var-set next-review-id (+ review-id u1))

    ;; Update paper status to under-review
    (match (map-get? papers paper-id)
      paper (map-set papers paper-id (merge paper { status: "under-review" }))
      false)

    (ok review-id)
  )
)

;; Publication Decision
(define-public (publish-paper (paper-id uint))
  (match (map-get? papers paper-id)
    paper
    (let
      (
        (reviews-list (default-to (list) (map-get? paper-reviews paper-id)))
        (review-count (len reviews-list))
        (current-height stacks-block-height)
      )
      (asserts! (is-eq tx-sender (get author paper)) ERR-NOT-AUTHORIZED)
      (asserts! (>= review-count (var-get min-reviews-for-publication)) ERR-INSUFFICIENT-REVIEWS)

      ;; Check average score (simplified - would implement proper scoring algorithm)
      (map-set papers paper-id (merge paper {
        status: "published",
        publication-date: (some current-height)
      }))
      (ok true)
    )
    ERR-PAPER-NOT-FOUND
  )
)

;; Citation Management
(define-public (add-citation (citing-paper-id uint) (cited-paper-id uint))
  (begin
    (asserts! (is-some (map-get? papers citing-paper-id)) ERR-PAPER-NOT-FOUND)
    (asserts! (is-some (map-get? papers cited-paper-id)) ERR-PAPER-NOT-FOUND)

    (map-set citations { citing-paper: citing-paper-id, cited-paper: cited-paper-id } true)

    ;; Increment citation count
    (match (map-get? papers cited-paper-id)
      paper (map-set papers cited-paper-id
        (merge paper { citation-count: (+ (get citation-count paper) u1) }))
      false)

    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-paper (paper-id uint))
  (map-get? papers paper-id)
)

(define-read-only (get-review (review-id uint))
  (map-get? reviews review-id)
)

(define-read-only (get-paper-reviews (paper-id uint))
  (map-get? paper-reviews paper-id)
)

(define-read-only (get-author-papers (author principal))
  (map-get? author-papers author)
)

(define-read-only (is-authorized-reviewer (reviewer principal))
  (default-to false (map-get? authorized-reviewers reviewer))
)

(define-read-only (get-citation-count (paper-id uint))
  (match (map-get? papers paper-id)
    paper (some (get citation-count paper))
    none)
)

;; ===========================================
;; Contract 2: Research Tokens & Funding
;; File: contracts/research-token.clar
;; ===========================================

;; Import the scientific papers contract (assuming it's deployed)
;; (use-trait scientific-papers-trait .scientific-papers)

;; Constants
(define-constant TOKEN-NAME "Research Token")
(define-constant TOKEN-SYMBOL "RSRCH")
(define-constant TOKEN-DECIMALS u6)
(define-constant INITIAL-SUPPLY u1000000000000) ;; 1M tokens with 6 decimals

(define-constant ERR-NOT-TOKEN-OWNER (err u2001))
(define-constant ERR-INSUFFICIENT-BALANCE (err u2002))
(define-constant ERR-FUNDING-GOAL-NOT-MET (err u2003))
(define-constant ERR-ALREADY-FUNDED (err u2004))
(define-constant ERR-INVALID-AMOUNT (err u2005))

;; Token standard implementation
(define-fungible-token research-token INITIAL-SUPPLY)

;; Data Variables
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var review-reward-base uint u1000000) ;; 1 token (with 6 decimals)
(define-data-var citation-reward uint u500000) ;; 0.5 token per citation
(define-data-var publication-reward uint u5000000) ;; 5 tokens for publication

;; Funding tracking
(define-map paper-funding uint { goal: uint, raised: uint, contributors: (list 50 principal) })
(define-map contributor-amounts { paper-id: uint, contributor: principal } uint)

;; Initialize contract with initial token distribution
(begin
  (ft-mint? research-token INITIAL-SUPPLY tx-sender)
)

;; SIP-010 Standard Functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
    (ft-transfer? research-token amount sender recipient)
  )
)

(define-read-only (get-name)
  (ok TOKEN-NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance research-token who))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply research-token))
)

(define-read-only (get-token-uri)
  (ok (var-get token-uri))
)

;; Reward Distribution Functions (Simplified - rewards are distributed directly)
(define-public (distribute-review-reward (reviewer principal) (score uint))
  (let
    (
      (reward-amount (* (var-get review-reward-base) score))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-TOKEN-OWNER)
    (asserts! (and (>= score u1) (<= score u10)) ERR-INVALID-AMOUNT)

    ;; Mint reward tokens
    (try! (ft-mint? research-token reward-amount reviewer))
    (ok reward-amount)
  )
)

(define-public (distribute-citation-reward (author principal) (citation-count uint))
  (let
    (
      (reward-amount (* (var-get citation-reward) citation-count))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-TOKEN-OWNER)

    ;; Mint citation rewards
    (try! (ft-mint? research-token reward-amount author))
    (ok reward-amount)
  )
)

;; Open Access Funding Model (Simplified)
(define-public (fund-research (paper-id uint) (amount uint) (funding-goal uint))
  (let
    (
      (funding-info (default-to { goal: funding-goal, raised: u0, contributors: (list) }
                     (map-get? paper-funding paper-id)))
      (current-raised (get raised funding-info))
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    ;; Transfer tokens from contributor to contract
    (try! (ft-transfer? research-token amount tx-sender (as-contract tx-sender)))

    ;; Update funding tracking
    (map-set paper-funding paper-id {
      goal: funding-goal,
      raised: (+ current-raised amount),
      contributors: (unwrap-panic (as-max-len?
        (append (get contributors funding-info) tx-sender) u50))
    })

    (map-set contributor-amounts { paper-id: paper-id, contributor: tx-sender } amount)

    (ok amount)
  )
)

(define-public (distribute-funding (paper-id uint) (author principal))
  (let
    (
      (funding-info (unwrap! (map-get? paper-funding paper-id) ERR-FUNDING-GOAL-NOT-MET))
      (total-raised (get raised funding-info))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-TOKEN-OWNER)

    ;; Transfer funds to author (90%) and keep 10% as platform fee
    (let
      (
        (author-amount (/ (* total-raised u90) u100))
        (platform-fee (/ (* total-raised u10) u100))
      )
      (try! (as-contract (ft-transfer? research-token author-amount tx-sender author)))
      (ok true)
    )
  )
)

;; Governance and Administration
(define-public (set-review-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-TOKEN-OWNER)
    (var-set review-reward-base new-reward)
    (ok true)
  )
)

(define-public (set-citation-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-TOKEN-OWNER)
    (var-set citation-reward new-reward)
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-funding-info (paper-id uint))
  (map-get? paper-funding paper-id)
)

(define-read-only (get-contribution (paper-id uint) (contributor principal))
  (map-get? contributor-amounts { paper-id: paper-id, contributor: contributor })
)

(define-read-only (get-reward-rates)
  {
    review-base: (var-get review-reward-base),
    citation: (var-get citation-reward),
    publication: (var-get publication-reward)
  }
)
