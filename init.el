(package-initialize)
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
  
;; HTTP
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defalut setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding
(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

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
;; scratchは残したい
(add-hook 'emacs-startup-hook
      '(lambda ()
         (with-current-buffer "*scratch*"
           (auto-save-mode)
           (set (make-local-variable 'auto-save-timeout) 10)
           (set (make-local-variable 'auto-save-interval) 20))))

;; Color
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

;; Window settings
(if window-system
    (progn
      (setq ns-command-modifier (quote meta))
      (setq ns-alternate-modifier (quote super))

      (tool-bar-mode -1)                  ; ツールバー非表示
      (set-scroll-bar-mode nil)           ; スクロールバー非表示
      (set-frame-parameter nil 'alpha 85) ; 透明度
      (setq-default line-spacing 0.2)    ; 行間

      ;; フォントセットを作る
      (let* (
             (fontset-name "myfonts") ; フォントセットの名前
             (size 13) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
             (asciifont "Monaco") ; ASCIIフォント
             (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
             (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
             (fontspec (font-spec :family asciifont))
             (jp-fontspec (font-spec :family jpfont))
             (fsn (create-fontset-from-ascii-font font nil fontset-name))
             )
        (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
        (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
        (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec)
        (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
        (set-fontset-font fsn '(#x0080 . #x024F) fontspec)    ; 分音符付きラテン
        (set-fontset-font fsn '(#x0370 . #x03FF) fontspec)    ; ギリシャ文字
        )

      ;; デフォルトのフレームパラメータでフォントセットを指定
      (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))

      ;; デフォルトフェイスにフォントセットを設定
      ;; # これは起動時に default-frame-alist に従ったフレームが
      ;; # 作成されない現象への対処
      (set-face-font 'default "fontset-myfonts")

      (setq ns-pop-up-frames nil)

      ;;;;;;;;;;;;;;;;;;;;;;;;
      ;; powerline
      ;;;;;;;;;;;;;;;;;;;;;;;;
      (require 'powerline)
      (set-face-attribute 'mode-line nil
                          :foreground "#fff"
                          :background "#001100"
                          :box nil)
      (set-face-attribute 'powerline-active1 nil
                          :foreground "#fff"
                          :background "#003300"
                          :inherit 'mode-line)
      (set-face-attribute 'powerline-active2 nil
                          :foreground "#fff"
                          :background "#001100"
                          :inherit 'mode-line)
      (powerline-default-theme)      
      )
  )

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

(when (functionp 'mac-auto-ascii-mode)  ;; ミニバッファに入力時、自動的に英語モード
  (mac-auto-ascii-mode 1)
)

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

;; whitespace
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

;;
;; miscellaneous
;;______________________________________________________________________
;; Bell
(setq ring-bell-function 'ignore)

;; Scroll
(setq scroll-step 1)

;; Column number
(column-number-mode t)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(helm-mini-default-sources
   (quote
    (helm-source-buffers-list helm-source-recentf helm-source-files-in-current-dir helm-source-emacs-commands-history helm-source-emacs-commands)))
 '(package-selected-packages
   (quote
    (iedit smooth-scrolling smooth-scroll quickrun company auto-save-buffers-enhanced powerline anzu undo-tree package-build shut-up epl git commander f dash s))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'helm)
(require 'helm-config)

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



(define-key global-map (kbd "C-o") 'helm-mini)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)

(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(global-set-key (kbd "M-u") 'undo-tree-visualize)

;; undo-limit
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

;;;;;;;;;;;;;;;;;;;;;;
;; anzu
;;;;;;;;;;;;;;;;;;;;;;
(global-anzu-mode +1)


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
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 5) ; 指定のアイドル秒で保存
(auto-save-buffers-enhanced t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickrun
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'quickrun)
(global-set-key (kbd "M-i") 'quickrun)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-scroll
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smooth-scroll)
(smooth-scroll-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth-scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
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
