;; ------------------------------------------------------------------------
; Custom Theme
;; ------------------------------------------------------------------------
(load-theme 'tango-dark)

;; 背景色セット
(set-background-color "#222")

;; コメントの色変更
(set-face-foreground 'font-lock-comment-face "#888")
(set-face-foreground 'font-lock-comment-delimiter-face "#888")
(set-face-foreground 'font-lock-doc-face "#888")

;; モードラインカスタマイズ
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)
(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "#333"
    :inverse-video nil
    :box '(:line-width 2 :color "#333" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "#222"
    :inverse-video nil
    :box '(:line-width 2 :color "#222" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;; ------------------------------------------------------------------------
; Load Path
;; ------------------------------------------------------------------------
;; package
(require 'package)
;; melpa読み込み
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; ------------------------------------------------------------------------
; Customize
;; ------------------------------------------------------------------------
;; ------------------------ [Frame Setting] -------------------------
;; オープニングメッセージを表示しない
(setq inhibit-startup-message t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; メニューバーを消す
(menu-bar-mode -1)

;; 列数を表示する
(line-number-mode t)
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)
(setq linum-format "%4d  ")

; 対応する括弧を光らせる
;;(show-paren-mode t)
;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#500")

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ブランチ表示
(let ((cell (or (memq 'mode-line-position mode-line-format)
      (memq 'mode-line-buffer-identification mode-line-format)))
      (newcdr '(:eval (my/update-git-branch-mode-line))))
  (unless (member newcdr mode-line-format)
    (setcdr cell (cons newcdr (cdr cell)))))

(defun my/update-git-branch-mode-line ()
  (let* ((branch (replace-regexp-in-string
                  "[\r\n]+\\'" ""
                  (shell-command-to-string "git symbolic-ref -q HEAD")))
         (mode-line-str (if (string-match "^refs/heads/" branch)
                            (format "[%s]" (substring branch 11))
                          "[Not Repo]")))
    (propertize mode-line-str
                'face '((:foreground "#04C82F" :weight bold)))))

;; ------------------------ [Keybind Setting] -------------------------
;; バックスペースに設定
(global-set-key "\C-h" 'delete-backward-char)

;; 行数で移動
(global-set-key "\M-z" 'goto-line)

;; 画面スクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))

;; 行揃え
(global-set-key (kbd "C-c C-f") 'align-regexp)

;; 画面スクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))

;; 行数で移動
(global-set-key "\M-z" 'goto-line)

;; ------------------------ [Emacs Edit Setting] -------------------------
;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; バックアップファイルを作成させない
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq next-line-add-newlines nil)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck helm-flycheck multiple-cursors elscreen elscreen-buffer-group elscreen-separate-buffer-list emmet-mode helm-ag helm-descbinds helm-elscreen helm-emmet redo+ helm php-mode web-mode)))
 '(tab-width 4))

;; スペースは全角のみを可視化
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; ミニバッファの大文字小文字区別無効化
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ------------------------------------------------------------------------
; Emacs Package Setting
;; ------------------------------------------------------------------------
;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; web-mode
(require 'web-mode)
;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.view"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl?$"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts$"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$"       . web-mode))

;; インデント関係
(setq web-mode-markup-indent-offset 2) ;; html

;; php-mode
(require 'php-mode)
(set-face-foreground 'php-variable-name "#fcaf3e")
(set-face-foreground 'php-string "#B5BD68")

;; helm load
(require 'helm-config)
(require 'helm-ag)
(require 'helm-descbinds);; helm keybind setting
(helm-descbinds-mode)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c C-h") 'helm-mini)
(global-set-key (kbd "C-c b") 'helm-descbinds)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c s") 'helm-ag)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)

;; Emacs終了コマンドの変更(exit -> helm-M-x)
(global-set-key (kbd "C-x C-c") 'helm-M-x)
(defalias 'exit 'save-buffers-kill-emacs)

;; [helm] 最近使ったファイルをメニューに表示
(recentf-mode t)

;; [helm] 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; [helm] 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; [helm] helm-flycheck
(add-hook 'php-mode-hook 'flycheck-mode)
(require 'helm-flycheck)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "\C-c C-n") 'helm-flycheck))

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'php-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4)))
(global-set-key (kbd "C-c i") 'emmet-expand-line)

;; redo+
(require 'redo+)
(global-set-key (kbd "M-/") 'redo)
(setq undo-no-redo t)

;; elscreen
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; プレフィクスキーはC-z
(setq elscreen-prefix-key "\C-q")
(load "elscreen" "ElScreen" t)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; タブ毎にバッファーを持たせる
(elscreen-separate-buffer-list-mode t)

;; multiple-cursors package
(require 'multiple-cursors)
(global-unset-key "\C-t")

;; smartrep package
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(smartrep-define-key global-map "C-t"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))
