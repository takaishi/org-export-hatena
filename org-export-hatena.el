(defvar org-export-hatena-notation-section "^\\*\\ \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-subsection "^\\*\\* \\[.*\\]+ \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-subsubsection "^\\*\\*\\* \\[.*\\]+ \\([^\t\n\r\f]*\\)$")
(defvar org-export-hatena-notation-quote '("[ ]*#\\+BEGIN_QUOTE" ">>" "[ ]*#\\+END_QUOTE" "<<"))
(defvar org-export-hatena-notation-super-pre '("[ ]*#\\+BEGIN_EXAMPLE" ">||" "[ ]*#\\+END_EXAMPLE" "||<"))
(defvar org-export-hatena-notation-src '("[ ]*#\\+BEGIN_SRC" ">||" "[ ]*#\\+END_SRC" "||<"))

(defun org-export-hatena-section ()
  (let ((section org-export-hatena-notation-section)
	(subsection org-export-hatena-notation-subsection)
	(subsubsection org-export-hatena-notation-subsubsection))
    (goto-char (point-min))
    (while (re-search-forward subsection nil t)
      (replace-match "*t* \\1"))))

(defun org-export-hatena-begin-to-end-org-ul-list ()
  "org ドキュメント中の ul リストを全てはてな記法に変換します．
org ドキュメント中の ul リストは，次の制約を満たしている必要があります．

  * 次のように，リストの1要素が複数行にまたがないこと(これははてな記法と同様の制約です):
        - XXXX
          YYYY

  * リストを1段深くするときは，スペース(またはタブ)2つ分のインデントをとること:
        - 1段目
          - 2段目
            - 3段目
          - 2段目
"
  ;; ここからサブルーチン群
  (defun org-export-hatena-org-ul-list-in-line ()
    "カーソル行の ul リストを返します．"
    (save-excursion
      (let ((bound (progn
                     (end-of-line nil)
                     (point))))
        (beginning-of-line nil)
        (if (re-search-forward "^\\([ \t\f]*-.+\\)$" bound t)
            (match-string 1)
          nil))))

  (defun org-export-hatena-prev-org-ul-list ()
    "現在のカーソル位置の上2行以内にあるリストを返します．
上2行以内にリストがなければ nil を返します．"
    (save-excursion
      (if (< (forward-line -1) 0) nil
        (let ((prev-org-ul-list (org-export-hatena-org-ul-list-in-line)))
          (if prev-org-ul-list prev-org-ul-list
            (if (< (forward-line -1) 0) nil
              (org-export-hatena-org-ul-list-in-line)))))))

  (defun org-export-hatena-org-ul-list-indentation (org-ul-list-line)
    "`org-list-line' が org-mode の ul リストであれば，
その行頭のインデント数を返します．
そうでなければ nil を返します．"
    (if (and org-ul-list-line
             (string-match "^\\([ \t\f]*\\)-" org-ul-list-line))
        (length (match-string 1 org-ul-list-line))
      nil))

  (defun org-export-hatena-prev-org-ul-list-indentation ()
    "現在のカーソル位置の上2行以内にあるリストの行頭インデント数を返します．
上2行以内にリストがなければ nil を返します．"
    (org-export-hatena-org-ul-list-indentation
     (org-export-hatena-prev-org-ul-list)))

  (defun org-export-hatena-org-ul-list-in-line-indentation ()
    "現在のカーソル行にあるリストの行頭インデント数を返します．
現在の行がリストでなければ nil を返します．"
    (org-export-hatena-org-ul-list-indentation
     (org-export-hatena-org-ul-list-in-line)))


  ;; ここからメインの処理
  (save-excursion
    (goto-char (point-min))

    (lexical-let ((prev-org-indent nil)
                  (cur-org-indent nil)
                  (prev-hatena-indent 0))
      (while (not (eq (point) (point-max)))
        (setq prev-org-indent (org-export-hatena-prev-org-ul-list-indentation)
              cur-org-indent (org-export-hatena-org-ul-list-in-line-indentation))
        (if cur-org-indent
            (progn
              (setq prev-hatena-indent
                    (if prev-org-indent
                        (+ prev-hatena-indent
                           (/ (print (- cur-org-indent prev-org-indent))  2))
                      1))
              ;; ここで `prev-hatena-indent' に，現在行をどれだけインデントすべきかが
              ;; 格納されています．

              (beginning-of-line nil)
              (re-search-forward "^\\([ \t\f]*\\)-\\(.*\\)$" nil t)

              (replace-match
               (concat
                (match-string 1)
                (make-string prev-hatena-indent ?-)
                (match-string 2)))))

        (forward-line 1)))

    ;; このままだと，ラインの各要素の行頭のスペースがとれていません
    (goto-char (point-min))
    (while (re-search-forward "^[ \t\f]+-" nil t)
      (replace-match "-"))))

(defun org-export-hatena-begin-to-end (notation)
  (goto-char (point-min))
  (while (re-search-forward (nth 0 notation) nil t)
    (replace-match (nth 1 notation)))
  (goto-char (point-min))
  (while (re-search-forward (nth 2 notation) nil t)
    (replace-match (nth 3 notation))))


(defun org-export-hatena (beg end)
  (interactive "r")
  
  (let ((diary (buffer-substring beg end))
	(quote org-export-hatena-notation-quote)
	(s-pre org-export-hatena-notation-super-pre)
	(src org-export-hatena-notation-src))
    (with-temp-buffer
      (pop-to-buffer (current-buffer))
      (insert diary)
      (org-export-hatena-begin-to-end quote)
      (org-export-hatena-begin-to-end s-pre)
      (org-export-hatena-begin-to-end src)
      (org-export-hatena-section)
      (org-export-hatena-begin-to-end-org-ul-list)
      (setq diary (buffer-substring (point-min) (point-max))))
    (simple-hatena simple-hatena-default-id)
    (simple-hatena-mode)
    (goto-char (point-min))
    (newline)
    (insert diary)))


(provide 'org-export-hatena)
