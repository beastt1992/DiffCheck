;;;============================================================
;;; DiffCheck.lsp v13 (Thick Red Box Update)
;;; Replaced revision clouds with clean, thick red rectangles.
;;; Commands:  DC / DCC / DCT
;;; Layers:    DIFF_CLOUD (Red, applied only to New region)
;;;============================================================

(setq *dc:tol* 2.0)     ; 座標與特徵容差
(setq *dc:pad* 50.0)    ; 紅框往外擴張的邊距
(setq *dc:thick* 10.0)  ; 紅框的線條粗細 (可依圖面比例調整)
(setq *dc:merge* 150.0) ; 鄰近框框自動合併的距離

;;; ============ UTILITY ============
(defun dc:rnd (v / u) (setq u *dc:tol*) (* u (fix (+ (/ v u) (if (>= v 0) 0.5 -0.5)))))
(defun dc:pr (p) (list (dc:rnd (car p)) (dc:rnd (cadr p))))
(defun dc:padd (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun dc:psub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun dc:f (v) (rtos (dc:rnd v) 2 1))
(defun dc:pf (p) (strcat (dc:f (car p)) "," (dc:f (cadr p))))

;;; ============ SIGNATURES ============
(defun dc:sig-line (e o / p1 p2 tmp)
  (setq p1 (dc:pr (dc:psub (cdr (assoc 10 e)) o)) p2 (dc:pr (dc:psub (cdr (assoc 11 e)) o)))
  (if (or (< (car p2) (car p1)) (and (= (car p2) (car p1)) (< (cadr p2) (cadr p1))))
    (progn (setq tmp p1 p1 p2 p2 tmp)))
  (strcat "L," (dc:pf p1) "," (dc:pf p2)))
(defun dc:sig-circle (e o / c r)
  (strcat "C," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," (dc:f (dc:rnd (cdr (assoc 40 e))))))
(defun dc:sig-arc (e o / c r a1 a2)
  (strcat "A," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," (dc:f (dc:rnd (cdr (assoc 40 e)))) "," (dc:f (dc:rnd (cdr (assoc 50 e)))) "," (dc:f (dc:rnd (cdr (assoc 51 e))))))
(defun dc:sig-lwpoly (e o / pts bul s i p b cl np)
  (setq pts '() bul '())
  (foreach g e
    (if (= (car g) 10) (setq pts (cons (dc:pr (dc:psub (cdr g) o)) pts)))
    (if (= (car g) 42) (setq bul (cons (dc:rnd (cdr g)) bul))))
  (setq pts (reverse pts) bul (reverse bul))
  (while (< (length bul) (length pts)) (setq bul (append bul '(0.0))))
  (setq cl (if (and (assoc 70 e) (= (logand (cdr (assoc 70 e)) 1) 1)) "1" "0"))
  (setq s (strcat "P," cl) np (length pts) i 0)
  (while (< i np) (setq s (strcat s "," (dc:pf (nth i pts)) "," (dc:f (nth i bul))) i (1+ i)))
  s)
(defun dc:sig-text (e o)
  (strcat "T," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," (dc:f (dc:rnd (if (assoc 40 e) (cdr (assoc 40 e)) 0.0))) "," (if (assoc 1 e) (cdr (assoc 1 e)) "")))
(defun dc:sig-insert (e o)
  (strcat "I," (if (assoc 2 e) (cdr (assoc 2 e)) "?") "," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," (dc:f (dc:rnd (if (assoc 41 e) (cdr (assoc 41 e)) 1.0))) "," (dc:f (dc:rnd (if (assoc 42 e) (cdr (assoc 42 e)) 1.0))) "," (dc:f (dc:rnd (if (assoc 50 e) (cdr (assoc 50 e)) 0.0)))))
(defun dc:sig-dim (e o)
  (strcat "D," (itoa (if (assoc 70 e) (logand (cdr (assoc 70 e)) 7) 0)) "," (dc:f (if (assoc 42 e) (dc:rnd (cdr (assoc 42 e))) 0.0)) "," (if (assoc 1 e) (cdr (assoc 1 e)) "<>") "," (dc:pf (if (assoc 11 e) (dc:pr (dc:psub (cdr (assoc 11 e)) o)) '(0.0 0.0)))))

(defun dc:mksig (en o / e tp)
  (setq e (entget en) tp (cdr (assoc 0 e)))
  (cond ((= tp "LINE") (dc:sig-line e o)) ((= tp "CIRCLE") (dc:sig-circle e o)) ((= tp "ARC") (dc:sig-arc e o)) ((= tp "LWPOLYLINE") (dc:sig-lwpoly e o)) ((= tp "TEXT") (dc:sig-text e o)) ((= tp "MTEXT") (dc:sig-text e o)) ((= tp "INSERT") (dc:sig-insert e o)) ((= tp "DIMENSION") (dc:sig-dim e o)) (T nil)))

;;; ============ BOUNDING BOX & MERGE ============
(defun dc:get-bbox (en / obj mn mx)
  (vl-load-com) (setq obj (vlax-ename->vla-object en))
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx)) nil))

(defun dc:box-expand (b1 b2)
  (list (list (min (caar b1) (caar b2)) (min (cadar b1) (cadar b2))) (list (max (caadr b1) (caadr b2)) (max (cadadr b1) (cadadr b2)))))

(defun dc:box-intersect-p (b1 b2 pad)
  (not (or (< (caadr b1) (- (caar b2) pad)) (> (caar b1) (+ (caadr b2) pad)) (< (cadadr b1) (- (cadar b2) pad)) (> (cadar b1) (+ (cadadr b2) pad)))))

(defun dc:merge-boxes (boxes pad / changed merged i j b1 b2 merged-this-pass)
  (setq changed T)
  (while changed
    (setq changed nil merged '() i 0)
    (while (< i (length boxes))
      (setq b1 (nth i boxes) merged-this-pass nil j 0)
      (while (< j (length merged))
        (setq b2 (nth j merged))
        (if (dc:box-intersect-p b1 b2 pad)
          (progn (setq merged (subst (dc:box-expand b1 b2) b2 merged) merged-this-pass T changed T j (length merged))))
        (setq j (1+ j)))
      (if (not merged-this-pass) (setq merged (cons b1 merged)))
      (setq i (1+ i)))
    (setq boxes merged))
  merged)

;;; ============ OFFSET ============
(defun dc:get-center (en / box)
  (if (setq box (dc:get-bbox en))
    (list (/ (+ (caar box) (caadr box)) 2.0) (/ (+ (cadar box) (cadadr box)) 2.0))
    nil))

(defun dc:offset (lA lB / ptsA ptsB i vm bc bv dx dy rdx rdy k f)
  (setq ptsA '() ptsB '() i 0)
  (foreach en lA (if (and (< i 200) (setq c (dc:get-center en))) (setq ptsA (cons c ptsA) i (1+ i))))
  (setq i 0)
  (foreach en lB (if (and (< i 200) (setq c (dc:get-center en))) (setq ptsB (cons c ptsB) i (1+ i))))
  
  (setq vm '() bc 0 bv '(0.0 0.0))
  (foreach a ptsA
    (foreach b ptsB
      (setq rdx (- (car b) (car a)) rdy (- (cadr b) (cadr a)))
      (setq dx (dc:rnd rdx) dy (dc:rnd rdy))
      (setq k (strcat (rtos dx 2 1) "," (rtos dy 2 1)))
      (setq f (assoc k vm))
      (if f
        (setq vm (subst (list k (1+ (cadr f)) (caddr f) (cadddr f)) f vm))
        (setq vm (cons (list k 1 rdx rdy) vm)))))
  
  (foreach v vm (if (> (cadr v) bc) (setq bc (cadr v) bv (list (caddr v) (cadddr v)))))
  (princ (strcat "\n  Auto-align votes: " (itoa bc)))
  bv
)

;;; ============ THICK RED BOX ============
(defun dc:draw-box (x1 y1 x2 y2 layer thick)
  ;; 畫一個固定寬度(粗細)的矩形聚合線
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPolyline")
      '(90 . 4)              ; 4個頂點
      '(70 . 1)              ; 封閉線段
      (cons 43 thick)        ; 全局寬度 (粗細)
      (list 10 x1 y1)
      (list 10 x2 y1)
      (list 10 x2 y2)
      (list 10 x1 y2)
    )
  )
)

;;; ============ MAIN DIFF ============
(defun dc:diff (lA lB ofs / dictB s f cnt-m cnt-a cnt-r diff-boxes box final-boxes)
  (princ "\n  Building Hash Dictionary...")
  (setq dictB '() cnt-m 0 cnt-a 0 cnt-r 0 diff-boxes '())
  
  (foreach en lB
    (if (setq s (dc:mksig en ofs))
      (setq f (assoc s dictB)
            dictB (if f (subst (cons s (cons en (cdr f))) f dictB) (cons (list s en) dictB)))))

  (princ "\n  Matching...")
  (foreach en lA
    (if (setq s (dc:mksig en '(0.0 0.0)))
      (progn
        (setq f (assoc s dictB))
        (if (and f (cdr f))
          (progn
            (setq cnt-m (1+ cnt-m))
            (setq dictB (subst (cons s (cddr f)) f dictB)))
          (progn
            (if (setq box (dc:get-bbox en))
              (setq diff-boxes (cons (list (dc:padd (car box) ofs) (dc:padd (cadr box) ofs)) diff-boxes)))
            (setq cnt-r (1+ cnt-r)))))))

  (foreach f dictB
    (foreach en (cdr f)
      (if (setq box (dc:get-bbox en))
        (setq diff-boxes (cons box diff-boxes)))
      (setq cnt-a (1+ cnt-a))))

  ;; 畫粗紅框
  (if (> (length diff-boxes) 0)
    (progn
      (setq final-boxes (dc:merge-boxes diff-boxes *dc:merge*))
      (foreach box final-boxes
        (dc:draw-box
          (- (caar box) *dc:pad*) (- (cadar box) *dc:pad*)
          (+ (caadr box) *dc:pad*) (+ (cadadr box) *dc:pad*)
          "DIFF_CLOUD" *dc:thick*
        )
      )
    )
  )

  (princ "\n  ── Results ──")
  (princ (strcat "\n  Matched (Unchanged): " (itoa cnt-m)))
  (princ (strcat "\n  Changes detected:    " (itoa (+ cnt-a cnt-r))))
  (princ "\n  All differences marked on Region B (DIFF_CLOUD layer).")
)

;;; ============ COMMANDS ============
(defun dc:sel (msg / ss i r)
  (princ (strcat "\n" msg))
  (if (setq ss (ssget))
    (progn
      (princ (strcat "\n  " (itoa (sslength ss)) " objects selected"))
      (setq i 0 r '()) (repeat (sslength ss) (setq r (cons (ssname ss i) r) i (1+ i)))
      (reverse r))
    nil))

(defun c:DiffCheck ( / lA lB ofs t0)
  (vl-load-com) (setq t0 (getvar "MILLISECS"))
  (setq lA (dc:sel "Select Region A (old):")) (if (null lA) (exit))
  (setq lB (dc:sel "Select Region B (new):")) (if (null lB) (exit))
  (princ "\n  Calculating SyncBlock-style exact offset...")
  (setq ofs (dc:offset lA lB))
  (if (not (tblsearch "LAYER" "DIFF_CLOUD"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 "DIFF_CLOUD") (cons 62 1) '(70 . 0))))
  (command "_.LAYER" "_T" "DIFF_CLOUD" "_ON" "DIFF_CLOUD" "_Unlock" "DIFF_CLOUD" "")
  (dc:diff lA lB ofs)
  (princ (strcat "\n  Time: " (rtos (/ (- (getvar "MILLISECS") t0) 1000.0) 2 2) "s"))
  (princ)
)
(defun c:DC () (c:DiffCheck))
(defun c:DCC () (setq ss (ssget "_X" '((8 . "DIFF_CLOUD")))) (if ss (command "_.ERASE" ss "")) (princ "\n  DIFF_CLOUD Cleared.") (princ))
(defun c:DCT ()
  (princ (strcat "\n  Tol=" (rtos *dc:tol* 2 1) " Merge=" (rtos *dc:merge* 2 0) " Pad=" (rtos *dc:pad* 2 0) " Thick=" (rtos *dc:thick* 2 1)))
  (setq v (getreal "\n  New 'Tolerance' limit (0=cancel): ")) (if (and v (> v 0)) (setq *dc:tol* v))
  (setq v (getreal "\n  New 'Merge Distance' limit (0=cancel): ")) (if (and v (> v 0)) (setq *dc:merge* v))
  (setq v (getreal "\n  New 'Line Thickness' (0=cancel): ")) (if (and v (> v 0)) (setq *dc:thick* v))
  (princ)
)
(princ "\nDiffCheck v13 (Thick Red Box) loaded. Commands: DC, DCC, DCT")
(princ)