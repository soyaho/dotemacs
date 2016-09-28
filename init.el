;; PATHを通す

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cask
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cask)
(cask-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defalut setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OP messageを表示しない
(setq inhibit-startup-message t)

;; scratch messageを表示しない
(setq initial-scratch-message "")

;; scratch defalut-modeをtextに
(setq initial-major-mode 'text-mode)

;; .#ファイルを作らないようにする
(setq create-lockfiles nil)

;; backup file残さない
(setq make-backup-files nil)

;; Color
;;______________________________________________________________________
(set-foreground-color                                  "#CCCCCC") ; 文字色
(set-background-color                                  "#000000") ; 背景色
(set-cursor-color                                      "#FF0000") ; カーソル色
(set-face-background 'region                           "#3333FF") ; リージョン
(set-face-foreground 'mode-line                        "#333333") ; モードライン文字
(set-face-background 'mode-line                        "#CCCCCC") ; モードライン背景
(set-face-foreground 'mode-line-inactive               "#CCCCCC") ; モードライン文字(非アクティブ)
(set-face-background 'mode-line-inactive               "#333333") ; モードライン背景(非アクティブ)
(set-face-foreground 'font-lock-comment-delimiter-face "#880000") ; コメントデリミタ
(set-face-foreground 'font-lock-comment-face           "#888888") ; コメント
(set-face-foreground 'font-lock-string-face            "#7FFF7F") ; 文字列
(set-face-foreground 'font-lock-function-name-face     "#BF7FFF") ; 関数名
(set-face-foreground 'font-lock-keyword-face           "#FF7F7F") ; キーワード
(set-face-foreground 'font-lock-constant-face          "#FFBF7F") ; 定数(this, selfなども)
(set-face-foreground 'font-lock-variable-name-face     "#7F7FFF") ; 変数
(set-face-foreground 'font-lock-type-face              "#FFFF7F") ; クラス
(set-face-foreground 'fringe                           "#666666") ; fringe(折り返し記号なでが出る部分)
(set-face-background 'fringe                           "#000000") ; fringe背景

;;
;; Window settings
;;______________________________________________________________________
(if window-system
    (progn
      ;; key binding
      (setq ns-command-modifier (quote meta))
      (setq ns-alternate-modifier (quote super))

      (tool-bar-mode -1)                  ; ツールバー非表示
      (set-scroll-bar-mode nil)           ; スクロールバー非表示
;      (set-frame-parameter nil 'alpha 85) ; 透明度
      (setq-default line-spacing 0.15)             ; 行間

      ;; フォントセットを作る
      (let* ((fontset-name "myfonts") ; フォントセットの名前
             (size 14) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
             (asciifont "Monaco") ; ASCIIフォント
             (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
             (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
             (fontspec (font-spec :family asciifont))
             (jp-fontspec (font-spec :family jpfont))
             (fsn (create-fontset-from-ascii-font font nil fontset-name)))
        (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
        (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
        (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
        (set-fontset-font fsn '(#x0080 . #x024F) fontspec)    ; 分音符付きラテン
        (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)    ; ギリシャ文字
        )

      ;; デフォルトのフレームパラメータでフォントセットを指定
      (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))

      ;; フォントサイズの比を設定
      (dolist (elt '(("^-apple-hiragino.*"               . 1.0);1.2
                     (".*osaka-bold.*"                   . 1.0);1.2
                     (".*osaka-medium.*"                 . 1.0);1.2
                     (".*courier-bold-.*-mac-roman"      . 1.0)
                     (".*monaco cy-bold-.*-mac-cyrillic" . 1.0);0.9
                     (".*monaco-bold-.*-mac-roman"       . 1.0);0.9
                     ))
        (add-to-list 'face-font-rescale-alist elt))

      ;; デフォルトフェイスにフォントセットを設定
      ;; # これは起動時に default-frame-alist に従ったフレームが
      ;; # 作成されない現象への対処
      (set-face-font 'default "fontset-myfonts")

      (setq ns-pop-up-frames nil)

      ;; powerline
      (require 'powerline)
      (set-face-attribute 'mode-line nil
                          :foreground "#fff"
                          :background "#003300"
                          :box nil)
      (set-face-attribute 'powerline-active1 nil
                          :foreground "#fff"
                          :background "#006600"
                          :inherit 'mode-line)
      (set-face-attribute 'powerline-active2 nil
                          :foreground "#000"
                          :background "#000000"
                          :inherit 'mode-line)
      (powerline-default-theme)
))

;; encoding
;;______________________________________________________________________
(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

;; key binding
(keyboard-translate ?\C-h ?\C-?) ;; C-h にバックスペース
(global-unset-key "\C-z") ;; globalを無効化
(global-unset-key "\C-j") ;; globalを無効化
(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "M-?")     'comment-or-uncomment-region)
(global-set-key (kbd "M-c")     'kill-ring-save)
(global-set-key (kbd "M-v")     'yank)

;; 改行オートインデント
(setq indent-line-function 'indent-relative-maybe)
(global-set-key "\C-m" 'newline-and-indent); Returnキーで改行＋オートインデント

;; sym-link開く時聞かれない
(setq vc-follow-symlinks t)

;; C-x p で C-x o の逆の動作を実現する
(define-key ctl-x-map "p"
  '(lambda (arg) (interactive "p") (other-window (- arg))))

;; session
(recentf-mode 1)
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items 1000)

;; ミニバッファ履歴リストの最大長：tなら無限
(setq history-length t)

;; highlight
;;______________________________________________________________________
;; highlight current line
;;(require 'highlight-current-line)
;;(highlight-current-line-on t)
;;(set-face-background 'highlight-current-line-face "#004132")
;;(set-face-background 'highlight-current-line-face "#004132")

;; highlight paren
(show-paren-mode 1)

;; highlight reagion
(setq transient-mark-mode t)

;; yank した文字列を強調表示
(when (or window-system (eq emacs-major-version '21))
  (defadvice yank (after ys:highlight-string activate)
    (let ((ol (make-overlay (mark t) (point))))
      (overlay-put ol 'face 'highlight)
      (sit-for 0.5)
      (delete-overlay ol)))
  (defadvice yank-pop (after ys:highlight-string activate)
    (when (eq last-command 'yank)
      (let ((ol (make-overlay (mark t) (point))))
        (overlay-put ol 'face 'highlight)
        (sit-for 0.5)
        (delete-overlay ol)))))

;;
;; miscellaneous
;;______________________________________________________________________
;; Bell
(setq ring-bell-function 'ignore)

;; Scroll
(setq scroll-step 1)

;; Column number
(column-number-mode t)

;; line number
(global-linum-mode t)
(setq linum-format "%3d  ")
(custom-set-faces
 ;; '(linum ((t (:inherit (shadow default) :background "Gray23")))))
  '(linum ((t (:inherit (shadow default))))))

;; Fill column
(setq default-fill-column 72)

;; Truncate
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; Indent
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(c-set-offset 'case-label '+)

;; Narrowing
(put 'narrow-to-region 'disabled nil)

;; 矩形
(cua-mode t)
(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化

;; メニューバーにファイルパスを表示する
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)

;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)

;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;
(helm-mode 1)
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates (lambda ()
                  (let ((cmds))
                    (mapatoms
                     (lambda (elt) (when (commandp elt) (push elt cmds))))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "A simple helm source for Emacs commands.")

(defvar helm-source-emacs-commands-history
  (helm-build-sync-source "Emacs commands history"
    :candidates (lambda ()
                  (let ((cmds))
                    (dolist (elem extended-command-history)
                      (push (intern elem) cmds))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "Emacs commands history")

(custom-set-variables
 '(helm-mini-default-sources '(helm-source-buffers-list
                               helm-source-recentf
                               helm-source-files-in-current-dir
                               helm-source-emacs-commands-history
                               helm-source-emacs-commands
                               )))

(define-key global-map (kbd "C-o") 'helm-mini)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;
(global-company-mode) ; 全バッファで有効にする 
(setq company-idle-delay 0.1) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)
(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )))

(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))

(define-key company-active-map [tab] 'company-complete-common2)
(define-key company-active-map [backtab] 'company-select-previous) ; おまけ

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-save-buffers-enhanced-interval 5) ; 指定のアイドル秒で保存
(auto-save-buffers-enhanced t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "M-u") 'undo-tree-visualize)

;; undo-limit
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anzu
;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickrun
;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-i") 'quickrun)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-scroll
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smooth-scroll)
(smooth-scroll-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'smooth-scrolling)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iedit
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(global-set-key (kbd "M-n") 'iedit-mode)
(define-key iedit-mode-keymap (kbd "C-m") 'iedit-toggle-selection)
(define-key iedit-mode-keymap (kbd "M-p") 'iedit-expand-up-a-line)
(define-key iedit-mode-keymap (kbd "M-n") 'iedit-expand-down-a-line)
(define-key iedit-mode-keymap (kbd "M-h") 'iedit-restrict-function)
(define-key iedit-mode-keymap (kbd "M-i") 'iedit-restrict-current-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-whitespace-mode 1)
;;; 強調したい要素を指定
(setq whitespace-style '(space-mark tab-mark face spaces tabs trailing))
;;; whitespace-space を全角スペースと定義
(setq whitespace-space-regexp "\\(\u3000+\\)")
;;; 全角スペース，タブに使用する記号
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?□] [?_ ?_])
        (tab-mark     ?\t    [?^ ?\t] [?\\ ?\t])))
;;; 各要素の face 設定
(set-face-attribute 'whitespace-space nil
                    :foreground "green"
                    :background 'unspecified)
(set-face-attribute 'whitespace-tab nil
                    :foreground "purple"
                    :background 'unspecified
                    :underline t)
(set-face-attribute 'whitespace-trailing nil
                    :foreground "purple"
                    :background 'unspecified
                    :underline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nxml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'nxml-mode-hook
          (lambda ()
            ;; 更新タイムスタンプの自動挿入
            (setq time-stamp-line-limit 10000)
            (if (not (memq 'time-stamp write-file-hooks))
                (setq write-file-hooks
                      (cons 'time-stamp write-file-hooks)))
            (setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y %Z")
            (setq time-stamp-start "Last modified:[ \t]")
            (setq time-stamp-end "$")
            ;;
            (setq auto-fill-mode -1)
            (setq nxml-slash-auto-complete-flag t)      ; スラッシュの入力で終了タグを自動補完
            (setq nxml-child-indent 2)                  ; タグのインデント幅
            (setq nxml-attribute-indent 4)              ; 属性のインデント幅
            (setq nxml-bind-meta-tab-to-complete-flag t) 
            (setq nxml-slash-auto-complete-flag t)      ; </の入力で閉じタグを補完する
            (setq nxml-sexp-element-flag t)             ; C-M-kで下位を含む要素全体をkillする
            (setq nxml-char-ref-display-glyph-flag nil) ; グリフは非表示
            (setq tab-width 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(setq auto-mode-alist (cons '("\\.\\(js\\|gs\\)$" . js2-mode) auto-mode-alist))

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)
      (save-excursion
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2))))
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 2 indentation))))
      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-js2-mode-hook ()
  (require 'js)
  (setq js-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2
        js2-mirror-mode nil)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map (kbd "C-m") 'newline-and-indent)
  (define-key js2-mode-map (kbd "M-;") 'js2-mode-toggle-element)
  (define-key js2-mode-map (kbd "C-M-;") 'js2-mode-toggle-hide-functions)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode)
    )
  )
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; --------------------------------------------------
;; ruby-mode
;; http://shibayu36.hatenablog.com/entry/2013/03/18/192651
;; --------------------------------------------------
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)) ;; shebangがrubyの場合、ruby-modeを開く

;; ruby-modeのインデントを改良する
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; --------------------------------------------------
;; ruby-end
;; endや括弧などを自動挿入する
;; http://blog.livedoor.jp/ooboofo3/archives/53748087.html
;; --------------------------------------------------
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))

;; --------------------------------------------------
;; ruby-block
;; endにカーソルを合わせると、そのendに対応する行をハイライトする
;; --------------------------------------------------
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
