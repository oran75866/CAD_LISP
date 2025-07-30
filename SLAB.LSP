;;; Rhino 風格 SLAB 指令 - 在 AutoCAD 中建立具有厚度的立體板件
;;; 使用者可選取閉合多段線並指定基準高度與厚度

(defun c:SLAB (/ ss baseZ thick i ent copyEnt)
  (vl-load-com)
  (prompt "\n請選取閉合多段線以建立 SLAB...")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if ss
    (progn
      (setq baseZ (getreal "\n請輸入底部高度 <0>: "))
      (if (null baseZ) (setq baseZ 0.0))
      (setq thick (getreal "\n請輸入板厚: "))
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        ;; 將物件複製到指定高度
        (command "_.COPY" ent "" '(0 0 0) (list 0 0 baseZ))
        (setq copyEnt (entlast))
        ;; 轉換為 REGION 才能拉伸為實體
        (command "_.REGION" copyEnt "")
        (setq copyEnt (entlast))
        ;; 執行拉伸成指定厚度
        (command "_.EXTRUDE" copyEnt "" thick)
        (setq i (1+ i))
      )
    )
    (prompt "\n未選取任何物件。")
  )
  (princ)
)

(princ "\n指令 SLAB 載入完成，可建立類似 Rhino 的板件。\n")
