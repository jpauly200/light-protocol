;; MetaProtocol - Dynamic Risk-Adjusted Lending Protocol
;; A simplified implementation of core lending pool mechanics

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-insufficient-collateral (err u102))
(define-constant err-loan-not-found (err u103))
(define-constant err-already-liquidated (err u104))
(define-constant err-not-liquidatable (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-pool-not-found (err u107))

;; Collateral ratio: 150% (represented as 15000 for precision)
(define-constant min-collateral-ratio u15000)
(define-constant precision u10000)

;; Data Variables
(define-data-var base-interest-rate uint u500) ;; 5% base rate (500 basis points)
(define-data-var total-liquidity uint u0)
(define-data-var total-borrowed uint u0)
(define-data-var pool-utilization uint u0)
(define-data-var loan-counter uint u0)

;; Data Maps
(define-map lending-pools 
    { pool-id: uint } 
    { 
        liquidity: uint,
        borrowed: uint,
        interest-rate: uint,
        active: bool
    }
)

(define-map user-deposits 
    { user: principal, pool-id: uint } 
    { 
        amount: uint,
        deposit-block: uint
    }
)

(define-map loans 
    { loan-id: uint } 
    { 
        borrower: principal,
        principal-amount: uint,
        collateral-amount: uint,
        interest-rate: uint,
        start-block: uint,
        pool-id: uint,
        active: bool
    }
)

(define-map user-collateral 
    { user: principal } 
    { amount: uint }
)

;; Read-only functions
(define-read-only (get-pool-info (pool-id uint))
    (map-get? lending-pools { pool-id: pool-id })
)

(define-read-only (get-user-deposit (user principal) (pool-id uint))
    (map-get? user-deposits { user: user, pool-id: pool-id })
)

(define-read-only (get-loan-info (loan-id uint))
    (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-user-collateral (user principal))
    (default-to { amount: u0 } (map-get? user-collateral { user: user }))
)

(define-read-only (calculate-utilization (liquidity uint) (borrowed uint))
    (if (is-eq liquidity u0)
        u0
        (/ (* borrowed precision) liquidity)
    )
)

(define-read-only (calculate-dynamic-rate (utilization uint))
    (let
        (
            (base-rate (var-get base-interest-rate))
            (utilization-multiplier (/ utilization u100))
        )
        (+ base-rate utilization-multiplier)
    )
)

(define-read-only (get-collateral-ratio (collateral uint) (loan-amount uint))
    (if (is-eq loan-amount u0)
        u0
        (/ (* collateral precision) loan-amount)
    )
)

;; Public functions

;; Initialize a lending pool
(define-public (create-pool (pool-id uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set lending-pools 
            { pool-id: pool-id }
            { 
                liquidity: u0,
                borrowed: u0,
                interest-rate: (var-get base-interest-rate),
                active: true
            }
        ))
    )
)

;; Deposit liquidity into a pool
(define-public (deposit-liquidity (pool-id uint) (amount uint))
    (let
        (
            (pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) err-pool-not-found))
            (current-deposit (default-to { amount: u0, deposit-block: u0 } 
                (map-get? user-deposits { user: tx-sender, pool-id: pool-id })))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (get active pool) err-pool-not-found)
        
        ;; Update pool liquidity
        (map-set lending-pools 
            { pool-id: pool-id }
            (merge pool { liquidity: (+ (get liquidity pool) amount) })
        )
        
        ;; Update user deposit
        (map-set user-deposits 
            { user: tx-sender, pool-id: pool-id }
            { 
                amount: (+ (get amount current-deposit) amount),
                deposit-block: block-height
            }
        )
        
        ;; Update total liquidity
        (var-set total-liquidity (+ (var-get total-liquidity) amount))
        
        (ok true)
    )
)

;; Withdraw liquidity from a pool
(define-public (withdraw-liquidity (pool-id uint) (amount uint))
    (let
        (
            (pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) err-pool-not-found))
            (user-dep (unwrap! (map-get? user-deposits { user: tx-sender, pool-id: pool-id }) 
                err-insufficient-balance))
        )
        (asserts! (>= (get amount user-dep) amount) err-insufficient-balance)
        (asserts! (>= (get liquidity pool) amount) err-insufficient-balance)
        
        ;; Update pool liquidity
        (map-set lending-pools 
            { pool-id: pool-id }
            (merge pool { liquidity: (- (get liquidity pool) amount) })
        )
        
        ;; Update user deposit
        (map-set user-deposits 
            { user: tx-sender, pool-id: pool-id }
            { 
                amount: (- (get amount user-dep) amount),
                deposit-block: (get deposit-block user-dep)
            }
        )
        
        ;; Update total liquidity
        (var-set total-liquidity (- (var-get total-liquidity) amount))
        
        (ok true)
    )
)

;; Deposit collateral
(define-public (deposit-collateral (amount uint))
    (let
        (
            (current-collateral (get amount (get-user-collateral tx-sender)))
        )
        (asserts! (> amount u0) err-invalid-amount)
        
        (map-set user-collateral 
            { user: tx-sender }
            { amount: (+ current-collateral amount) }
        )
        
        (ok true)
    )
)

;; Borrow from a pool
(define-public (borrow (pool-id uint) (amount uint))
    (let
        (
            (pool (unwrap! (map-get? lending-pools { pool-id: pool-id }) err-pool-not-found))
            (user-coll (get amount (get-user-collateral tx-sender)))
            (required-collateral (/ (* amount min-collateral-ratio) precision))
            (new-loan-id (+ (var-get loan-counter) u1))
            (utilization (calculate-utilization (get liquidity pool) (+ (get borrowed pool) amount)))
            (dynamic-rate (calculate-dynamic-rate utilization))
        )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (get active pool) err-pool-not-found)
        (asserts! (>= (get liquidity pool) amount) err-insufficient-balance)
        (asserts! (>= user-coll required-collateral) err-insufficient-collateral)
        
        ;; Create loan record
        (map-set loans 
            { loan-id: new-loan-id }
            {
                borrower: tx-sender,
                principal-amount: amount,
                collateral-amount: required-collateral,
                interest-rate: dynamic-rate,
                start-block: block-height,
                pool-id: pool-id,
                active: true
            }
        )
        
        ;; Update pool
        (map-set lending-pools 
            { pool-id: pool-id }
            (merge pool { 
                liquidity: (- (get liquidity pool) amount),
                borrowed: (+ (get borrowed pool) amount),
                interest-rate: dynamic-rate
            })
        )
        
        ;; Lock collateral
        (map-set user-collateral 
            { user: tx-sender }
            { amount: (- user-coll required-collateral) }
        )
        
        ;; Update counters
        (var-set loan-counter new-loan-id)
        (var-set total-borrowed (+ (var-get total-borrowed) amount))
        
        (ok new-loan-id)
    )
)

;; Repay loan
(define-public (repay-loan (loan-id uint) (amount uint))
    (let
        (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-loan-not-found))
            (pool (unwrap! (map-get? lending-pools { pool-id: (get pool-id loan) }) err-pool-not-found))
        )
        (asserts! (is-eq tx-sender (get borrower loan)) err-owner-only)
        (asserts! (get active loan) err-already-liquidated)
        (asserts! (>= amount (get principal-amount loan)) err-invalid-amount)
        
        ;; Update loan status
        (map-set loans 
            { loan-id: loan-id }
            (merge loan { active: false })
        )
        
        ;; Update pool
        (map-set lending-pools 
            { pool-id: (get pool-id loan) }
            (merge pool { 
                liquidity: (+ (get liquidity pool) amount),
                borrowed: (- (get borrowed pool) (get principal-amount loan))
            })
        )
        
        ;; Return collateral
        (let
            (
                (user-coll (get amount (get-user-collateral tx-sender)))
            )
            (map-set user-collateral 
                { user: tx-sender }
                { amount: (+ user-coll (get collateral-amount loan)) }
            )
        )
        
        ;; Update total borrowed
        (var-set total-borrowed (- (var-get total-borrowed) (get principal-amount loan)))
        
        (ok true)
    )
)

;; Liquidate undercollateralized loan
(define-public (liquidate-loan (loan-id uint))
    (let
        (
            (loan (unwrap! (map-get? loans { loan-id: loan-id }) err-loan-not-found))
            (collateral-ratio (get-collateral-ratio 
                (get collateral-amount loan) 
                (get principal-amount loan)))
        )
        (asserts! (get active loan) err-already-liquidated)
        (asserts! (< collateral-ratio min-collateral-ratio) err-not-liquidatable)
        
        ;; Mark loan as liquidated
        (map-set loans 
            { loan-id: loan-id }
            (merge loan { active: false })
        )
        
        (ok true)
    )
)

;; Admin: Update base interest rate
(define-public (update-base-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set base-interest-rate new-rate)
        (ok true)
    )
)
