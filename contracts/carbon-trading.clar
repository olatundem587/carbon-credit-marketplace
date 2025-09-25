;; Carbon Credit Trading Platform
;; Marketplace for trading verified carbon credits with automated offset calculations

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u6001))
(define-constant ERR_CREDIT_NOT_FOUND (err u6002))
(define-constant ERR_INSUFFICIENT_CREDITS (err u6003))
(define-constant ERR_INVALID_PRICE (err u6004))
(define-constant ERR_TRADE_FAILED (err u6005))
(define-constant ERR_ALREADY_VERIFIED (err u6006))
(define-constant ERR_EXPIRED_CREDIT (err u6007))
(define-constant ERR_INVALID_PARAMETERS (err u6008))

;; Credit types
(define-constant CREDIT_TYPE_RENEWABLE u1)
(define-constant CREDIT_TYPE_REFORESTATION u2)
(define-constant CREDIT_TYPE_INDUSTRIAL u3)
(define-constant CREDIT_TYPE_DIRECT_CAPTURE u4)

;; Data Variables
(define-data-var credit-counter uint u0)
(define-data-var total-credits-traded uint u0)
(define-data-var total-co2-offset uint u0)

;; Carbon credit registry
(define-map carbon-credits
  { credit-id: uint }
  {
    issuer: principal,
    owner: principal,
    credit-type: uint,
    co2-amount: uint,
    price-per-ton: uint,
    verification-status: bool,
    issue-date: uint,
    expiry-date: uint,
    project-location: (string-ascii 64),
    metadata-uri: (string-ascii 256)
  }
)

;; Credit ownership tracking
(define-map user-credits
  { user: principal }
  { credit-ids: (list 100 uint), total-credits: uint }
)

;; Trading orders
(define-map trade-orders
  { order-id: uint }
  {
    seller: principal,
    credit-id: uint,
    price: uint,
    quantity: uint,
    active: bool,
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-credit (credit-id uint))
  (map-get? carbon-credits { credit-id: credit-id })
)

(define-read-only (get-user-credits (user principal))
  (default-to { credit-ids: (list), total-credits: u0 }
    (map-get? user-credits { user: user })
  )
)

;; Public functions
(define-public (issue-carbon-credit
  (credit-type uint)
  (co2-amount uint)
  (price-per-ton uint)
  (expiry-date uint)
  (project-location (string-ascii 64))
  (metadata-uri (string-ascii 256)))
  (let ((credit-id (+ (var-get credit-counter) u1)))
    (asserts! (>= credit-type CREDIT_TYPE_RENEWABLE) ERR_INVALID_PARAMETERS)
    (asserts! (<= credit-type CREDIT_TYPE_DIRECT_CAPTURE) ERR_INVALID_PARAMETERS)
    (asserts! (> co2-amount u0) ERR_INVALID_PARAMETERS)
    (asserts! (> price-per-ton u0) ERR_INVALID_PRICE)
    
    (map-set carbon-credits
      { credit-id: credit-id }
      {
        issuer: tx-sender,
        owner: tx-sender,
        credit-type: credit-type,
        co2-amount: co2-amount,
        price-per-ton: price-per-ton,
        verification-status: false,
        issue-date: burn-block-height,
        expiry-date: expiry-date,
        project-location: project-location,
        metadata-uri: metadata-uri
      }
    )
    
    (var-set credit-counter credit-id)
    (ok credit-id)
  )
)

(define-public (trade-carbon-credits (credit-id uint) (quantity uint))
  (let (
    (credit-data (unwrap! (get-credit credit-id) ERR_CREDIT_NOT_FOUND))
    (seller (get owner credit-data))
  )
    (asserts! (get verification-status credit-data) ERR_INVALID_PARAMETERS)
    (asserts! (>= (get co2-amount credit-data) quantity) ERR_INSUFFICIENT_CREDITS)
    (asserts! (< burn-block-height (get expiry-date credit-data)) ERR_EXPIRED_CREDIT)
    
    ;; Transfer credit ownership
    (map-set carbon-credits
      { credit-id: credit-id }
      (merge credit-data { owner: tx-sender })
    )
    
    ;; Update statistics
    (var-set total-credits-traded (+ (var-get total-credits-traded) quantity))
    (var-set total-co2-offset (+ (var-get total-co2-offset) quantity))
    
    (ok credit-id)
  )
)

(define-public (verify-carbon-credit (credit-id uint))
  (let ((credit-data (unwrap! (get-credit credit-id) ERR_CREDIT_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (get verification-status credit-data)) ERR_ALREADY_VERIFIED)
    
    (map-set carbon-credits
      { credit-id: credit-id }
      (merge credit-data { verification-status: true })
    )
    (ok credit-id)
  )
)

(define-public (retire-carbon-credits (credit-id uint) (quantity uint))
  (let ((credit-data (unwrap! (get-credit credit-id) ERR_CREDIT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get owner credit-data)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get co2-amount credit-data) quantity) ERR_INSUFFICIENT_CREDITS)
    
    (var-set total-co2-offset (+ (var-get total-co2-offset) quantity))
    (ok quantity)
  )
)
