;;; AI風格對齊面板 for AutoCAD
;;; 模擬 Illustrator 對齊功能，並加入主要行內註解

;; 取得物件包圍盒（Bounding Box），回傳最小、最大座標清單
(defun getBBox (ent)
  (vla-getBoundingBox (vlax-ename->vla-object ent) 'minpt 'maxpt)
  (list (vlax-safearray->list minpt) (vlax-safearray->list maxpt))
)

;; 取得物件的幾何中心點（包圍盒對角線交點）
(defun getMidPt (ent)
  (setq box (getBBox ent))
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car box) (cadr box))
)

;; 取得物件左、右、上、下邊緣對齊點
(defun getSidePt (ent mode)
  (setq box (getBBox ent))
  (cond
    ((= mode "L") (list (car (car box)) (cadr (getMidPt ent)) 0.0)) ; 左側邊緣
    ((= mode "R") (list (car (cadr box)) (cadr (getMidPt ent)) 0.0)) ; 右側邊緣
    ((= mode "T") (list (car (getMidPt ent)) (cadr (cadr box)) 0.0)) ; 上側邊緣
    ((= mode "B") (list (car (getMidPt ent)) (cadr (car box)) 0.0)) ; 下側邊緣
  )
)

;; 執行所有選取物件的對齊
(defun alignObjs (ss mode basePt)
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)) ; 取得第 i 個物件
    ;; 依據對齊方式決定物件的對齊參考點
    (setq curPt
      (cond
        ((member mode '("L" "R")) (getSidePt ent mode))
        ((member mode '("T" "B")) (getSidePt ent mode))
        ((= mode "C") (getMidPt ent))
        ((= mode "M") (getMidPt ent))
      )
    )
    ;; 根據對齊模式移動物件
    (cond
      ((member mode '("T" "B"))
        (command "_.MOVE" ent "" curPt (list (car curPt) (cadr basePt) 0.0))
      ) ; 上下僅調整Y
      ((member mode '("L" "R"))
        (command "_.MOVE" ent "" curPt (list (car basePt) (cadr curPt) 0.0))
      ) ; 左右僅調整X
      ((= mode "C")
        (command "_.MOVE" ent "" curPt (list (car basePt) (cadr curPt) 0.0))
      ) ; 垂直置中（調整X）
      ((= mode "M")
        (command "_.MOVE" ent "" curPt (list (car curPt) (cadr basePt) 0.0))
      ) ; 水平置中（調整Y）
    )
    (setq i (1+ i))
  )
)

;; 指令主流程
(defun c:AIALIGN (/ ss opt basePt)
  (vl-load-com)
  (prompt "\n請選取要對齊的物件...")
  (setq ss (ssget)) ; 讓使用者框選物件
  (if ss
    (progn
      (initget "Top Bottom Left Right Center Middle")
      (setq opt (getkword "\n請選擇對齊方式 [Top/Bottom/Left/Right/Center/Middle]: "))
      ;; 選項轉換成內部符號
      (cond
        ((= opt "Top") (setq opt "T"))
        ((= opt "Bottom") (setq opt "B"))
        ((= opt "Left") (setq opt "L"))
        ((= opt "Right") (setq opt "R"))
        ((= opt "Center") (setq opt "C"))
        ((= opt "Middle") (setq opt "M"))
      )
      (if (member opt '("T" "B" "L" "R" "C" "M"))
        (progn
          (setq basePt (getpoint "\n請指定對齊基準點: "))
          (if basePt (alignObjs ss opt basePt))
        )
      )
    )
    (prompt "\n未選取任何物件。")
  )
  (princ)
)

(princ "\n指令 AIALIGN 載入完成，可模擬 AI 對齊功能。\n")
