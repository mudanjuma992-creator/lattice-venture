;; LatticeVenture - Decentralized Identity and Skill Verification Platform
;; Dual-token system with Verification Tokens (VT) and Reputation Crystals (RC)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-already-verified (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-already-validator (err u106))
(define-constant err-not-validator (err u107))
(define-constant err-challenge-expired (err u108))

;; Minimum stake required to become a validator
(define-constant min-validator-stake u1000)

;; Challenge duration in blocks
(define-constant challenge-duration u144) ;; ~24 hours

;; Data Variables
(define-data-var total-vt-supply uint u0)
(define-data-var total-rc-supply uint u0)
(define-data-var validator-count uint u0)
(define-data-var challenge-nonce uint u0)

;; Data Maps

;; VT Token balances (Verification Tokens)
(define-map vt-balances principal uint)

;; RC Token balances (Reputation Crystals)
(define-map rc-balances principal uint)

;; Validator information
(define-map validators principal {
    staked-amount: uint,
    reputation-score: uint,
    verified-count: uint,
    slashed-count: uint,
    active: bool
})

;; Skill verification challenges
(define-map skill-challenges uint {
    user: principal,
    skill-id: (string-ascii 64),
    skill-level: uint,
    challenge-hash: (buff 32),
    created-at: uint,
    verifications-needed: uint,
    verifications-received: uint,
    verified: bool
})

;; Validator verifications for challenges
(define-map challenge-verifications {challenge-id: uint, validator: principal} {
    approved: bool,
    timestamp: uint
})

;; User skills (privacy-preserving, stores only hashes)
(define-map user-skills {user: principal, skill-hash: (buff 32)} {
    level: uint,
    verified: bool,
    timestamp: uint,
    rc-earned: uint
})

;; Skill lattice structure (defines skill dependencies)
(define-map skill-lattice (string-ascii 64) {
    required-skills: (list 5 (string-ascii 64)),
    min-level: uint,
    rc-reward: uint
})

;; Read-only functions

(define-read-only (get-vt-balance (account principal))
    (default-to u0 (map-get? vt-balances account))
)

(define-read-only (get-rc-balance (account principal))
    (default-to u0 (map-get? rc-balances account))
)

(define-read-only (get-total-vt-supply)
    (var-get total-vt-supply)
)

(define-read-only (get-total-rc-supply)
    (var-get total-rc-supply)
)

(define-read-only (get-validator-info (validator principal))
    (map-get? validators validator)
)

(define-read-only (is-validator (account principal))
    (match (map-get? validators account)
        validator-info (get active validator-info)
        false
    )
)

(define-read-only (get-challenge (challenge-id uint))
    (map-get? skill-challenges challenge-id)
)

(define-read-only (get-user-skill (user principal) (skill-hash (buff 32)))
    (map-get? user-skills {user: user, skill-hash: skill-hash})
)

(define-read-only (get-validator-count)
    (var-get validator-count)
)

;; Private functions

(define-private (mint-vt (recipient principal) (amount uint))
    (begin
        (map-set vt-balances recipient 
            (+ (get-vt-balance recipient) amount))
        (var-set total-vt-supply (+ (var-get total-vt-supply) amount))
        (ok true)
    )
)

(define-private (mint-rc (recipient principal) (amount uint))
    (begin
        (map-set rc-balances recipient 
            (+ (get-rc-balance recipient) amount))
        (var-set total-rc-supply (+ (var-get total-rc-supply) amount))
        (ok true)
    )
)

(define-private (burn-vt (account principal) (amount uint))
    (let ((balance (get-vt-balance account)))
        (if (>= balance amount)
            (begin
                (map-set vt-balances account (- balance amount))
                (var-set total-vt-supply (- (var-get total-vt-supply) amount))
                (ok true)
            )
            err-invalid-amount
        )
    )
)

;; Public functions

;; Initialize VT tokens for users (owner only, simulates token distribution)
(define-public (initialize-vt (recipient principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (mint-vt recipient amount)
    )
)

;; Register as a validator by staking VT tokens
(define-public (register-validator (stake-amount uint))
    (let ((current-balance (get-vt-balance tx-sender)))
        (asserts! (>= stake-amount min-validator-stake) err-insufficient-stake)
        (asserts! (>= current-balance stake-amount) err-invalid-amount)
        (asserts! (is-none (map-get? validators tx-sender)) err-already-validator)
        
        (try! (burn-vt tx-sender stake-amount))
        
        (map-set validators tx-sender {
            staked-amount: stake-amount,
            reputation-score: u100,
            verified-count: u0,
            slashed-count: u0,
            active: true
        })
        
        (var-set validator-count (+ (var-get validator-count) u1))
        (ok true)
    )
)

;; Submit a skill verification challenge
(define-public (submit-skill-challenge (skill-id (string-ascii 64)) (skill-level uint) (challenge-hash (buff 32)))
    (let ((challenge-id (var-get challenge-nonce)))
        (map-set skill-challenges challenge-id {
            user: tx-sender,
            skill-id: skill-id,
            skill-level: skill-level,
            challenge-hash: challenge-hash,
            created-at: block-height,
            verifications-needed: u3,
            verifications-received: u0,
            verified: false
        })
        
        (var-set challenge-nonce (+ challenge-id u1))
        (ok challenge-id)
    )
)

;; Validator verifies a skill challenge
(define-public (verify-challenge (challenge-id uint) (approved bool))
    (let (
        (validator-info (unwrap! (map-get? validators tx-sender) err-not-validator))
        (challenge (unwrap! (map-get? skill-challenges challenge-id) err-not-found))
    )
        (asserts! (get active validator-info) err-unauthorized)
        (asserts! (not (get verified challenge)) err-already-verified)
        (asserts! (<= (- block-height (get created-at challenge)) challenge-duration) err-challenge-expired)
        
        ;; Record verification
        (map-set challenge-verifications 
            {challenge-id: challenge-id, validator: tx-sender}
            {approved: approved, timestamp: block-height}
        )
        
        ;; Update challenge
        (let ((new-verification-count (+ (get verifications-received challenge) u1)))
            (map-set skill-challenges challenge-id 
                (merge challenge {verifications-received: new-verification-count})
            )
            
            ;; Update validator stats
            (map-set validators tx-sender
                (merge validator-info {
                    verified-count: (+ (get verified-count validator-info) u1),
                    reputation-score: (+ (get reputation-score validator-info) u10)
                })
            )
            
            ;; Check if challenge is fully verified
            (if (>= new-verification-count (get verifications-needed challenge))
                (finalize-skill-verification challenge-id)
                (ok true)
            )
        )
    )
)

;; Finalize skill verification and mint RC tokens
(define-private (finalize-skill-verification (challenge-id uint))
    (let (
        (challenge (unwrap! (map-get? skill-challenges challenge-id) err-not-found))
        (rc-reward u50)
    )
        ;; Mark challenge as verified
        (map-set skill-challenges challenge-id 
            (merge challenge {verified: true})
        )
        
        ;; Store verified skill (privacy-preserving hash)
        (map-set user-skills 
            {user: (get user challenge), skill-hash: (get challenge-hash challenge)}
            {
                level: (get skill-level challenge),
                verified: true,
                timestamp: block-height,
                rc-earned: rc-reward
            }
        )
        
        ;; Mint RC tokens as reward
        (mint-rc (get user challenge) rc-reward)
    )
)

;; Transfer VT tokens
(define-public (transfer-vt (amount uint) (recipient principal))
    (let ((sender-balance (get-vt-balance tx-sender)))
        (asserts! (>= sender-balance amount) err-invalid-amount)
        (try! (burn-vt tx-sender amount))
        (mint-vt recipient amount)
    )
)

;; Transfer RC tokens
(define-public (transfer-rc (amount uint) (recipient principal))
    (let ((sender-balance (get-rc-balance tx-sender)))
        (asserts! (>= sender-balance amount) err-invalid-amount)
        (map-set rc-balances tx-sender (- sender-balance amount))
        (map-set rc-balances recipient (+ (get-rc-balance recipient) amount))
        (ok true)
    )
)

;; Define skill lattice structure (owner only)
(define-public (define-skill (skill-id (string-ascii 64)) (required-skills (list 5 (string-ascii 64))) (min-level uint) (rc-reward uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set skill-lattice skill-id {
            required-skills: required-skills,
            min-level: min-level,
            rc-reward: rc-reward
        })
        (ok true)
    )
)

;; Deactivate a validator (owner only, for slashing)
(define-public (slash-validator (validator principal) (slash-amount uint))
    (let ((validator-info (unwrap! (map-get? validators validator) err-not-validator)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set validators validator
            (merge validator-info {
                staked-amount: (if (> (get staked-amount validator-info) slash-amount)
                    (- (get staked-amount validator-info) slash-amount)
                    u0
                ),
                slashed-count: (+ (get slashed-count validator-info) u1),
                active: (> (get staked-amount validator-info) slash-amount)
            })
        )
        (ok true)
    )
)