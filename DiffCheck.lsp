;;;============================================================
;;; DiffCheck.lsp v14 (O(N log N) Speed Core)
;;; Fixed AutoLISP dictionary loop lag. Replaced with Two-Pointer Sort.
;;; Thick Red Box applied for visual clarity.
;;; Commands:  DC / DCC / DCT
;;; Layers:    DIFF_CLOUD (Red thick box)
;;;============================================================

(setq *dc:tol* 2.0)     ; 座標與特徵容差
(setq *dc:pad* 50.0)    ; 紅框往外擴張的邊距
(setq *dc:thick* 10.0)  ; 紅框的線條粗細
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
(defun dc:sig-circle (e o)
  (strcat "C," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," (dc:f (dc:rnd (cdr (assoc 40 e))))))
(defun dc:sig-arc (e o)
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

;;; ============ BOUNDING BOX & MERGE (List optimized) ============
(defun dc:get-bbox (en / obj mn mx)
  (vl-load-com) (setq obj (vlax-ename->vla-object en))
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx)) nil))

(defun dc:box-expand (b1 b2)
  (list (list (min (caar b1) (caar b2)) (min (cadar b1) (cadar b2))) (list (max (caadr b1) (caadr b2)) (max (cadadr b1) (cadadr b2)))))

(defun dc:box-intersect-p (b1 b2 pad)
  (not (or (< (caadr b1) (- (caar b2) pad)) (> (caar b1) (+ (caadr b2) pad)) (< (cadadr b1) (- (cadar b2) pad)) (> (cadar b1) (+ (cadadr b2) pad)))))

(defun dc:merge-boxes (boxes pad / changed merged b1 b2 merged-this-pass lst temp)
  (setq changed T)
  (while changed
    (setq changed nil merged '() lst boxes)
    (while lst
      (setq b1 (car lst) lst (cdr lst) merged-this-pass nil temp '())
      (while merged
        (setq b2 (car merged) merged (cdr merged))
        (if (dc:box-intersect-p b1 b2 pad)
          (progn (setq b1 (dc:box-expand b1 b2) merged-this-pass T changed T))
          (setq temp (cons b2 temp))))
      (setq merged (cons b1 temp)))
    (setq boxes merged))
  merged)

;;; ============ FAST OFFSET ============
(defun dc:get-fast-pt (en / e tp)
  ;; 避開超慢的 COM 呼叫，直接抓取物件的第一個節點來投票
  (setq e (entget en) tp (cdr (assoc 0 e)))
  (if (member tp '("LINE" "CIRCLE" "ARC" "INSERT" "TEXT" "MTEXT" "LWPOLYLINE" "DIMENSION")) (cdr (assoc 10 e)) nil))

(defun dc:offset (lA lB / ptsA ptsB i vm bc bv dx dy rdx rdy k f)
  (setq ptsA '() ptsB '() i 0)
  (foreach en lA (if (and (< i 30) (setq c (dc:get-fast-pt en))) (setq ptsA (cons c ptsA) i (1+ i))))
  (setq i 0)
  (foreach en lB (if (and (< i 30) (setq c (dc:get-fast-pt en))) (setq ptsB (cons c ptsB) i (1+ i))))
  
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
  (entmake
    (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer) '(100 . "AcDbPolyline")
          '(90 . 4) '(70 . 1) (cons 43 thick)
          (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2))))

;;; ============ MAIN DIFF (O(N log N) Two-Pointer Sort) ============
(defun dc:diff (lA lB ofs / pairsA pairsB lstA lstB pA pB sA sB box diff-boxes final-boxes cnt-m cnt-a cnt-r)
  (princ "\n  Generating Signatures & Sorting (Ultra Fast)...")
  (setq pairsA '() pairsB '() diff-boxes '() cnt-m 0 cnt-a 0 cnt-r 0)

  (foreach en lA (if (setq s (dc:mksig en '(0.0 0.0))) (setq pairsA (cons (cons s en) pairsA))))
  (setq pairsA (vl-sort pairsA '(lambda (a b) (< (car a) (car b)))))

  (foreach en lB (if (setq s (dc:mksig en ofs)) (setq pairsB (cons (cons s en) pairsB))))
  (setq pairsB (vl-sort pairsB '(lambda (a b) (< (car a) (car b)))))

  (princ "\n  Matching...")
  (setq lstA pairsA lstB pairsB)
  (while (and lstA lstB)
    (setq pA (car lstA) pB (car lstB) sA (car pA) sB (car pB))
    (cond
      ((= sA sB) ; Match
       (setq cnt-m (1+ cnt-m) lstA (cdr lstA) lstB (cdr lstB)))
      ((< sA sB) ; Removed in New
       (if (setq box (dc:get-bbox (cdr pA)))
         (setq diff-boxes (cons (list (dc:padd (car box) ofs) (dc:padd (cadr box) ofs)) diff-boxes)))
       (setq cnt-r (1+ cnt-r) lstA (cdr lstA)))
      (T         ; Added in New
       (if (setq box (dc:get-bbox (cdr pB)))
         (setq diff-boxes (cons box diff-boxes)))
       (setq cnt-a (1+ cnt-a) lstB (cdr lstB)))
    )
  )
  
  ;; 處理剩餘的物件
  (while lstA
    (if (setq box (dc:get-bbox (cdr (car lstA))))
      (setq diff-boxes (cons (list (dc:padd (car box) ofs) (dc:padd (cadr box) ofs)) diff-boxes)))
    (setq cnt-r (1+ cnt-r) lstA (cdr lstA)))
  (while lstB
    (if (setq box (dc:get-bbox (cdr (car lstB))))
      (setq diff-boxes (cons box diff-boxes)))
    (setq cnt-a (1+ cnt-a) lstB (cdr lstB)))

  (if (> (length diff-boxes) 0)
    (progn
      (setq final-boxes (dc:merge-boxes diff-boxes *dc:merge*))
      (foreach box final-boxes
        (dc:draw-box
          (- (caar box) *dc:pad*) (- (cadar box) *dc:pad*)
          (+ (caadr box) *dc:pad*) (+ (cadadr box) *dc:pad*)
          "DIFF_CLOUD" *dc:thick*))))

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
  (princ "\n  Calculating Exact Offset...")
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
(princ "\nDiffCheck v14 (O(N log N) Speed Core) loaded. Commands: DC, DCC, DCT")
(princ)