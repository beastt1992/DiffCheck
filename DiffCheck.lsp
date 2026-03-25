;;;============================================================
;;; DiffCheck.lsp v21.1 (Command Alias Fix + Attribute Check)
;;; O(N log N) Fast Core + Spatial Anchor + Localized Box Merging.
;;; Draws optimized Revision Clouds for nearby changes.
;;; Changed aliases to DFC/DFCC/DFCT to avoid ADCENTER conflict.
;;; Added Block Attribute deep comparison (Tags & Values).
;;; Commands:  DFC / DFCC / DFCT
;;; Layers:    DIFF_CLOUD (Red Revision Cloud)
;;;============================================================

(setq *dc:tol* 2.0)       ; Coordinate and feature tolerance
(setq *dc:pad* 20.0)      ; Padding around the bounding box
(setq *dc:arc* 30.0)      ; Arc length for the revision cloud
(setq *dc:merge* 50.0)    ; Safe distance for merging nearby boxes
(setq *dc:maxbox* 0.4)    ; Giant box filter threshold (ignores > 40%)

;;; ============ UTILITY ============
(defun dc:rnd (v / u) (setq u *dc:tol*) (* u (fix (+ (/ v u) (if (>= v 0) 0.5 -0.5)))))
(defun dc:pr (p) (list (dc:rnd (car p)) (dc:rnd (cadr p))))
(defun dc:padd (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun dc:psub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun dc:f (v) (rtos (dc:rnd v) 2 1))
(defun dc:pf (p) (strcat (dc:f (car p)) "," (dc:f (cadr p))))

(defun dc:take (lst n / r i)
  (setq r '() i 0)
  (while (and lst (< i n)) (setq r (cons (car lst) r) lst (cdr lst) i (1+ i)))
  (reverse r))

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

;;; --- MODIFIED: Added Block Attribute Extraction ---
(defun dc:sig-insert (e o / nm atts ent nxt tg vl)
  (setq nm (if (assoc 2 e) (cdr (assoc 2 e)) "?"))
  
  ;; Ignore anonymous block name variations (*U, *X) caused by copy-pasting
  (if (wcmatch nm "`**") (setq nm "ANON"))
  
  ;; Extract attributes if they exist (assoc 66 = 1)
  (setq atts "")
  (if (= (cdr (assoc 66 e)) 1) 
    (progn
      (setq ent (cdr (assoc -1 e)))
      (while (= (cdr (assoc 0 (setq nxt (entget (setq ent (entnext ent)))))) "ATTRIB")
        (setq tg (cdr (assoc 2 nxt)))
        (setq vl (cdr (assoc 1 nxt)))
        (setq atts (strcat atts "|" tg "=" vl))
      )
    )
  )
  
  (strcat "I," nm "," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," 
          (dc:f (dc:rnd (if (assoc 41 e) (cdr (assoc 41 e)) 1.0))) "," 
          (dc:f (dc:rnd (if (assoc 42 e) (cdr (assoc 42 e)) 1.0))) "," 
          (dc:f (dc:rnd (if (assoc 50 e) (cdr (assoc 50 e)) 0.0))) 
          atts) ; Attach attribute string to the end of the signature
)
;;; --------------------------------------------------

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

;;; ============ FAST REVISION CLOUD ============
(defun dc:revcloud (x1 y1 x2 y2 layer arclen / ed px py qx qy side segs i t1 cx cy nv vlist)
  (setq vlist '())
  (foreach ed (list (list x1 y1 x2 y1) (list x2 y1 x2 y2) (list x2 y2 x1 y2) (list x1 y2 x1 y1))
    (setq px (nth 0 ed) py (nth 1 ed) qx (nth 2 ed) qy (nth 3 ed) side (sqrt (+ (* (- qx px) (- qx px)) (* (- qy py) (- qy py)))))
    (if (< side 0.01) (setq segs 1) (setq segs (max 1 (fix (+ (/ side arclen) 0.5)))))
    (setq i 0)
    (repeat segs
      (setq t1 (/ (float i) (float segs)) cx (+ px (* t1 (- qx px))) cy (+ py (* t1 (- qy py))))
      (setq vlist (cons (cons 42 0.35) (cons (list 10 cx cy) vlist)))
      (setq i (1+ i))))
  (setq vlist (reverse vlist))
  (setq nv (/ (length vlist) 2))
  (if (> nv 2) (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer) '(100 . "AcDbPolyline") (cons 90 nv) '(70 . 1)) vlist))))

;;; ============ SPATIAL OFFSET CALCULATION ============
(defun dc:get-fast-pt (en / e tp)
  (setq e (entget en) tp (cdr (assoc 0 e)))
  (if (member tp '("LINE" "CIRCLE" "INSERT" "TEXT")) (cdr (assoc 10 e)) nil))

(defun dc:offset (lA lB / ptsA ptsB vm bc bv dx dy rdx rdy k f pt1 pt2)
  (setq ptsA '() ptsB '())
  (foreach en lA (if (setq c (dc:get-fast-pt en)) (setq ptsA (cons c ptsA))))
  (foreach en lB (if (setq c (dc:get-fast-pt en)) (setq ptsB (cons c ptsB))))
  
  ;; Sort points bottom-left to ensure reliable anchor matching
  (setq ptsA (vl-sort ptsA '(lambda (a b) (if (equal (car a) (car b) 100.0) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  (setq ptsB (vl-sort ptsB '(lambda (a b) (if (equal (car a) (car b) 100.0) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  
  (setq ptsA (dc:take ptsA 50) ptsB (dc:take ptsB 50))
  
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
  
  (if (< bc 3)
    (progn
      (princ "\n  [Warning] Auto-align failed! High variance or inconsistent selection.")
      (setq pt1 (getpoint "\n  👉 Click a common base point in [Region A - Old] (e.g., a column center): "))
      (if pt1 (setq pt2 (getpoint pt1 "\n  👉 Click the same base point in [Region B - New]: ")))
      (if (and pt1 pt2)
        (progn (setq bv (list (- (car pt2) (car pt1)) (- (cadr pt2) (cadr pt1))))
               (princ "\n  [Success] Manual alignment applied.")))))
  bv
)

;;; ============ MAIN DIFF ============
(defun dc:diff (lA lB ofs / pairsA pairsB lstA lstB pA pB sA sB cnt-m cnt-a cnt-r 
                            minX minY maxX maxY totalW totalH ignored-cnt valid-boxes final-boxes)
  (princ "\n  Generating Signatures & Sorting (Ultra Fast)...")
  (setq pairsA '() pairsB '() cnt-m 0 cnt-a 0 cnt-r 0 ignored-cnt 0 valid-boxes '())

  ;; Giant Box Filter Base
  (setq minX 1e99 minY 1e99 maxX -1e99 maxY -1e99)
  (foreach en lB
    (if (setq box (dc:get-bbox en))
      (setq minX (min minX (caar box)) minY (min minY (cadar box))
            maxX (max maxX (caadr box)) maxY (max maxY (cadadr box)))))
  (setq totalW (* (- maxX minX) *dc:maxbox*) totalH (* (- maxY minY) *dc:maxbox*))

  (foreach en lA (if (setq s (dc:mksig en '(0.0 0.0))) (setq pairsA (cons (cons s en) pairsA))))
  (setq pairsA (vl-sort pairsA '(lambda (a b) (< (car a) (car b)))))

  (foreach en lB (if (setq s (dc:mksig en ofs)) (setq pairsB (cons (cons s en) pairsB))))
  (setq pairsB (vl-sort pairsB '(lambda (a b) (< (car a) (car b)))))

  (defun dc:collect-diff-box (en is-added / box bw bh)
    (if (setq box (dc:get-bbox en))
      (progn
        (if (not is-added) (setq box (list (dc:padd (car box) ofs) (dc:padd (cadr box) ofs))))
        (setq bw (- (caadr box) (caar box)) bh (- (cadadr box) (cadar box)))
        (if (or (> bw totalW) (> bh totalH))
          (setq ignored-cnt (1+ ignored-cnt)) 
          (setq valid-boxes (cons box valid-boxes))))))

  (princ "\n  Matching & Grouping...")
  (setq lstA pairsA lstB pairsB)
  (while (and lstA lstB)
    (setq pA (car lstA) pB (car lstB) sA (car pA) sB (car pB))
    (cond
      ((= sA sB) (setq cnt-m (1+ cnt-m) lstA (cdr lstA) lstB (cdr lstB)))
      ((< sA sB) (dc:collect-diff-box (cdr pA) nil) (setq cnt-r (1+ cnt-r) lstA (cdr lstA)))
      (T         (dc:collect-diff-box (cdr pB) T) (setq cnt-a (1+ cnt-a) lstB (cdr lstB)))
    )
  )
  (while lstA (dc:collect-diff-box (cdr (car lstA)) nil) (setq cnt-r (1+ cnt-r) lstA (cdr lstA)))
  (while lstB (dc:collect-diff-box (cdr (car lstB)) T) (setq cnt-a (1+ cnt-a) lstB (cdr lstB)))

  (if valid-boxes
    (progn
      (setq final-boxes (dc:merge-boxes valid-boxes *dc:merge*))
      (foreach box final-boxes
        (dc:revcloud (- (caar box) *dc:pad*) (- (cadar box) *dc:pad*)
                     (+ (caadr box) *dc:pad*) (+ (cadadr box) *dc:pad*)
                     "DIFF_CLOUD" *dc:arc*))))

  (princ "\n  ── Results ──")
  (princ (strcat "\n  Matched (Unchanged): " (itoa cnt-m)))
  (princ (strcat "\n  Changes detected:    " (itoa (+ cnt-a cnt-r))))
  (if (> ignored-cnt 0) (princ (strcat "\n  [Filtered " (itoa ignored-cnt) " giant background elements]")))
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
  (princ "\n  Calculating Spatial Anchor Offset...")
  (setq ofs (dc:offset lA lB))
  (if (not (tblsearch "LAYER" "DIFF_CLOUD"))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 "DIFF_CLOUD") (cons 62 1) '(70 . 0))))
  (command "_.LAYER" "_T" "DIFF_CLOUD" "_ON" "DIFF_CLOUD" "_Unlock" "DIFF_CLOUD" "")
  (dc:diff lA lB ofs)
  (princ (strcat "\n  Time: " (rtos (/ (- (getvar "MILLISECS") t0) 1000.0) 2 2) "s"))
  (princ)
)

;; Changed aliases to avoid ADCENTER conflict
(defun c:DFC () (c:DiffCheck))

(defun c:DFCC () 
  (setq ss (ssget "_X" '((8 . "DIFF_CLOUD")))) 
  (if ss (command "_.ERASE" ss "")) 
  (princ "\n  DIFF_CLOUD Cleared.") 
  (princ)
)

(defun c:DFCT ()
  (princ (strcat "\n  Tol=" (rtos *dc:tol* 2 1) " Merge=" (rtos *dc:merge* 2 0) " Pad=" (rtos *dc:pad* 2 0) " Arc=" (rtos *dc:arc* 2 1)))
  (setq v (getreal "\n  New 'Merge Distance' limit (0=cancel): ")) (if (and v (> v 0)) (setq *dc:merge* v))
  (setq v (getreal "\n  New 'Box Padding' (0=cancel): ")) (if (and v (> v 0)) (setq *dc:pad* v))
  (setq v (getreal "\n  New 'Arc Length' (0=cancel): ")) (if (and v (> v 0)) (setq *dc:arc* v))
  (princ)
)
(princ "\nDiffCheck v21.1 loaded. Commands: DFC, DFCC, DFCT")
(princ)
