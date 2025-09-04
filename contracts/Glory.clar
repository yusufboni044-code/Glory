;; rfp-brief-winning.clar
;; Ultra-brief unique Clarity contract for RFP (Google Clarity Web3)

(clarity-version 2)

(define-data-var rfp-id uint u0)
(define-map rfps ((id uint))
  ((owner principal) (commit-end uint) (reveal-end uint) (winner (optional principal))))
(define-map commits ((id uint) (vendor principal)) ((h (buff 32))))
(define-map reveals ((id uint) (vendor principal)) ((proposal (string-utf8 64))))

;; Create an RFP
(define-public (create-rfp (commit-end uint) (reveal-end uint))
  (let ((id (+ (var-get rfp-id) u1)))
    (asserts! (> commit-end block-height) (err u100))
    (asserts! (> reveal-end commit-end) (err u101))
    (map-set rfps { id: id } { owner: tx-sender, commit-end: commit-end, reveal-end: reveal-end, winner: none })
    (var-set rfp-id id)
    (ok id)))

;; Commit hash = sha256(proposal||salt)
(define-public (commit (id uint) (h (buff 32)))
  (begin
    (asserts! (<= block-height (get commit-end (unwrap! (map-get? rfps { id: id }) (err u102)))) (err u103))
    (map-set commits { id: id, vendor: tx-sender } { h: h })
    (ok true)))

;; Reveal proposal
(define-public (reveal (id uint) (proposal (string-utf8 64)) (salt (buff 32)))
  (let ((c (map-get? commits { id: id, vendor: tx-sender })))
    (match c
      somec (asserts! (is-eq (get h somec) (sha256 (concat (utf8-to-bytes proposal) salt))) (err u104))
             (map-set reveals { id: id, vendor: tx-sender } { proposal: proposal })
             (ok true))
      none (err u105))))

;; Finalize winner (after reveal)
(define-public (finalize (id uint) (winner principal))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (begin
              (asserts! (is-eq (get owner some) tx-sender) (err u106))
              (asserts! (> block-height (get reveal-end some)) (err u107))
              (map-set rfps { id: id } (merge some { winner: (some winner) }))
              (ok winner))
      none (err u108)))))
