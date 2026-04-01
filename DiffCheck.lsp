;;;============================================================
;;; DiffCheck.lsp v31 (No-Click Auto Align Edition)
;;; O(N log N) Fast Core + Spatial Anchor + Localized Box Merging.
;;; 1. Removed manual alignment prompts for a 100% seamless workflow.
;;; 2. Added BBox bottom-left fallback for zero-vote situations.
;;; 3. Uses CMDACTIVE to safely draw native Smart Revision Clouds.
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

;;; ============ BOUNDING BOX UTILITY ============
(defun dc:get-bbox (en / obj mn mx)
  (vl-load-com) (setq obj (vlax-ename->vla-object en))
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
    (list (vlax-safearray->list mn) (vlax-safearray->list mx)) nil))

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

(defun dc:sig-insert (e o / nm atts ent nxt tg vl)
  (setq nm (if (assoc 2 e) (cdr (assoc 2 e)) "?"))
  (if (wcmatch nm "`**") (setq nm "ANON"))
  
  (setq atts "")
  (if (= (cdr (assoc 66 e)) 1)
    (progn
      (setq ent (cdr (assoc -1 e)))
      (while (= (cdr (assoc 0 (setq nxt (entget (setq ent (entnext ent)))))) "ATTRIB")
        (setq tg (cdr (assoc 2 nxt)) vl (cdr (assoc 1 nxt)))
        (setq atts (strcat atts "|" tg "=" vl))
      )
    )
  )
  (strcat "I," nm "," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) 
          "," (dc:f (dc:rnd (if (assoc 41 e) (cdr (assoc 41 e)) 1.0))) 
          "," (dc:f (dc:rnd (if (assoc 42 e) (cdr (assoc 42 e)) 1.0))) 
          "," (dc:f (dc:rnd (if (assoc 50 e) (cdr (assoc 50 e)) 0.0))) 
          atts))

(defun dc:sig-dim (e o)
  (strcat "D," (itoa (if (assoc 70 e) (logand (cdr (assoc 70 e)) 7) 0)) "," (dc:f (if (assoc 42 e) (dc:rnd (cdr (assoc 42 e))) 0.0)) "," (if (assoc 1 e) (cdr (assoc 1 e)) "<>") "," (dc:pf (if (assoc 11 e) (dc:pr (dc:psub (cdr (assoc 11 e)) o)) '(0.0 0.0)))))

(defun dc:sig-hatch (e o / nm box cx cy w h)
  (setq nm (if (assoc 2 e) (cdr (assoc 2 e)) "SOLID"))
  (if (setq box (dc:get-bbox (cdr (assoc -1 e))))
    (progn
      (setq cx (/ (+ (caar box) (caadr box)) 2.0)
            cy (/ (+ (cadar box) (cadadr box)) 2.0)
            w  (- (caadr box) (caar box))
            h  (- (cadadr box) (cadar box)))
      (strcat "H," nm "," (dc:pf (dc:pr (dc:psub (list cx cy) o))) "," (dc:f (dc:rnd w)) "," (dc:f (dc:rnd h))))
    (strcat "H," nm "," (if (assoc 10 e) (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "0,0"))))

(defun dc:sig-solid (e o)
  (strcat "S," (dc:pf (dc:pr (dc:psub (cdr (assoc 10 e)) o))) "," (dc:pf (dc:pr (dc:psub (cdr (assoc 11 e)) o))) "," (dc:pf (dc:pr (dc:psub (cdr (assoc 12 e)) o)))))

(defun dc:mksig (en o / e tp)
  (setq e (entget en) tp (cdr (assoc 0 e)))
  (cond ((= tp "LINE") (dc:sig-line e o))
        ((= tp "CIRCLE") (dc:sig-circle e o))
        ((= tp "ARC") (dc:sig-arc e o))
        ((= tp "LWPOLYLINE") (dc:sig-lwpoly e o))
        ((= tp "TEXT") (dc:sig-text e o))
        ((= tp "MTEXT") (dc:sig-text e o))
        ((= tp "INSERT") (dc:sig-insert e o))
        ((= tp "DIMENSION") (dc:sig-dim e o))
        ((= tp "HATCH") (dc:sig-hatch e o))
        ((= tp "SOLID") (dc:sig-solid e o))
        (T nil)))

;;; ============ BOX MERGE ============
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

;;; ============ 100% AUTO SPATIAL OFFSET ============
(defun dc:get-fast-pt (en / e tp)
  (setq e (entget en) tp (cdr (assoc 0 e)))
  (if (member tp '("LINE" "CIRCLE" "INSERT" "TEXT")) (cdr (assoc 10 e)) nil))

(defun dc:offset (lA lB / ptsA ptsB vm bc bv dx dy rdx rdy k f minXa minYa maxXa maxYa minXb minYb maxXb maxYb box)
  (setq ptsA '() ptsB '())
  (foreach en lA (if (setq c (dc:get-fast-pt en)) (setq ptsA (cons c ptsA))))
  (foreach en lB (if (setq c (dc:get-fast-pt en)) (setq ptsB (cons c ptsB))))
  
  (setq ptsA (vl-sort ptsA '(lambda (a b) (if (equal (car a) (car b) 100.0) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  (setq ptsB (vl-sort ptsB '(lambda (a b) (if (equal (car a) (car b) 100.0) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  
  (setq ptsA (dc:take ptsA 50) ptsB (dc:take ptsB 50))
  
  (setq vm '() bc 0 bv nil)
  (foreach a ptsA
    (foreach b ptsB
      (setq rdx (- (car b) (car a)) rdy (- (cadr b) (cadr a)))
      (setq dx (dc:rnd rdx) dy (dc:rnd rdy))
      (setq k (strcat (rtos dx 2 1) "," (rtos dy 2 1)))
      (setq f (assoc k vm))
      (if f (setq vm (subst (list k (1+ (cadr f)) (caddr f) (cadddr f)) f vm)) (setq vm (cons (list k 1 rdx rdy) vm)))))
  
  (foreach v vm (if (> (cadr v) bc) (setq bc (cadr v) bv (list (caddr v) (cadddr v)))))
  
  ;; ★ 取消所有手動輸入！全自動判斷位移量
  (if (> bc 0)
    (princ (strcat "\n  Auto-align votes: " (itoa bc) " (Anchor matched)"))
    (progn
      (princ "\n  [Warning] No identical anchor found. Using Bounding Box auto-align...")
      ;; 如果連1票都沒有，自動計算兩個選取區的「左下角」座標差值作為位移量
      (setq minXa 1e99 minYa 1e99 maxXa -1e99 maxYa -1e99)
      (foreach en lA (if (setq box (dc:get-bbox en)) (setq minXa (min minXa (caar box)) minYa (min minYa (cadar box)) maxXa (max maxXa (caadr box)) maxYa (max maxYa (cadadr box)))))
      (setq minXb 1e99 minYb 1e99 maxXb -1e99 maxYb -1e99)
      (foreach en lB (if (setq box (dc:get-bbox en)) (setq minXb (min minXb (caar box)) minYb (min minYb (cadar box)) maxXb (max maxXb (caadr box)) maxYb (max maxYb (cadadr box)))))
      
      (if (and (< minXa 1e90) (< minXb 1e90))
         (setq bv (list (- minXb minXa) (- minYb minYa)))
         (setq bv '(0.0 0.0)) ; 如果選到完全沒邊界的幽靈物件，位移歸零
      )
    )
  )
  bv
)

;;; ============ MAIN DIFF ============
(defun dc:diff (lA lB ofs / pairsA pairsB lstA lstB pA pB sA sB cnt-m cnt-a cnt-r 
                            minX minY maxX maxY totalW totalH ignored-cnt valid-boxes final-boxes
                            old_cmd old_os old_cl p1 p2 bw bh safe_arc rev_ent)
  (princ "\n  Generating Signatures & Sorting (Ultra Fast)...")
  (setq pairsA '() pairsB '() cnt-m 0 cnt-a 0 cnt-r 0 ignored-cnt 0 valid-boxes '())

  ;; 1. Analyze and filter
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

  ;; 2. BULLETPROOF CMDACTIVE DRAWING
  (if valid-boxes
    (progn
      (setq final-boxes (dc:merge-boxes valid-boxes *dc:merge*))
      (command "_.UNDO" "_Begin")
      
      (setq old_cmd (getvar "CMDECHO") old_os (getvar "OSMODE") old_cl (getvar "CLAYER"))
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)
      (setvar "CLAYER" "DIFF_CLOUD")
      
      (foreach box final-boxes
        (setq p1 (list (- (caar box) *dc:pad*) (- (cadar box) *dc:pad*)))
        (setq p2 (list (+ (caadr box) *dc:pad*) (+ (cadadr box) *dc:pad*)))
        (setq bw (abs (- (car p2) (car p1))))
        (setq bh (abs (- (cadr p2) (cadr p1))))
        
        ;; Adaptive arc length
        (setq safe_arc (min *dc:arc* (/ (min bw bh) 3.0)))
        (if (< safe_arc 0.5) (setq safe_arc 0.5))
        
        ;; Draw a clean rectangle
        (command "_.RECTANG" "_NON" p1 "_NON" p2)
        (setq rev_ent (entlast))
        
        ;; Set arc length
        (command "_.REVCLOUD" "_A" safe_arc safe_arc)
        
        ;; Dynamic CMDACTIVE dispatch
        (if (> (getvar "CMDACTIVE") 0)
            (command "_O" rev_ent "_No")            
            (command "_.REVCLOUD" "_O" rev_ent "_No") 
        )
        (while (> (getvar "CMDACTIVE") 0) (command "")) 
      )
      
      (setvar "CMDECHO" old_cmd)
      (setvar "OSMODE" old_os)
      (setvar "CLAYER" old_cl)
      (command "_.UNDO" "_End")
    )
  )

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
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord") (cons 2 "DIFF_CLOUD") (cons 62 10) '(70 . 0))))
    
  (command "_.LAYER" "_T" "DIFF_CLOUD" "_ON" "DIFF_CLOUD" "_Unlock" "DIFF_CLOUD" "")
  (dc:diff lA lB ofs)
  (princ (strcat "\n  Time: " (rtos (/ (- (getvar "MILLISECS") t0) 1000.0) 2 2) "s"))
  (princ)
)

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
(princ "\nDiffCheck v31 loaded. Commands: DFC, DFCC, DFCT")
(princ)