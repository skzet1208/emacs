;; ------------------------------------------------------------------------
; Emacs Custom Theme
;; ------------------------------------------------------------------------
(load-theme 'tango-dark)

;; 背景色セット
(set-background-color "black")
;(set-foreground-color "#55ff55")

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.9))

;; C-q 開放
(global-unset-key "\C-q")

;; Emacs終了コマンドの変更
(global-set-key (kbd "C-x C-c") 'helm-M-x)
(defalias 'exit 'save-buffers-kill-emacs) ;; 終了をM-x exitに変更

;; 分割画面サイズ変更
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
(global-set-key (kbd "\C-q r") 'window-resizer)

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

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; ------------------------------------------------------------------------
; Emacs Load Path
;; ------------------------------------------------------------------------

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

;; パッケージのインストールを自動化
;; http://www.emacswiki.org/emacs/auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))
(custom-set-faces)

;; ------------------------------------------------------------------------
; Emacs Package List Load
;; ------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ------------------------------------------------------------------------
; Emacs Tool Setting
;; ------------------------------------------------------------------------

;; ------------------------ [Emacs Frame Setting] -------------------------
;; フレームの右枠削除
(fringe-mode (cons 0 nil))

;; メニューバーを消す
(menu-bar-mode -1)

;; スクロールバーを非表示
(scroll-bar-mode 0)

;; バックアップファイルを作成させない
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; オープニングメッセージを表示しない
(setq inhibit-startup-message t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; 最近使ったファイルをメニューに表示
(recentf-mode t)

;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; ツールバーを消す
(tool-bar-mode -1)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)
(setq linum-format "%4d ")
(set-face-attribute 'linum nil
                    :foreground "#FFF"
                    :background "#222"
                    :height 0.9)

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; カーソル行をハイライトする
(global-hl-line-mode t)
(custom-set-faces '(hl-line ((t (:background "#222")))))

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; スペース、タブなどを可視化する
(global-whitespace-mode 1)

;; C-kで行全体を削除する
(setq kill-whole-line t)

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)
(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
        (count-lines (point-max) (point-min))))))

;; --------------------- [Emacs Environment Setting] ----------------------
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

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; ファイル内のタブをスペースに変更する
(add-hook 'before-save-hook '(lambda () (interactive) (untabify (point-min) (point-max))))

;; 画面スクロール
(global-set-key "\M-n" (lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda () (interactive) (scroll-down 1)))

;; スペースなどの視覚化
(require 'whitespace)
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

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)
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

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#500")

;; バックスペースに設定
(global-set-key "\C-h" 'delete-backward-char)

;; 行数で移動
(global-set-key "\M-z" 'goto-line)

;; 動的略語展開のキー
(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; 現在行を上下へ移動
(defun move-line (arg)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines arg))
    (when (> arg 0)
      (forward-line arg))
    (move-to-column col)))
(global-set-key (kbd "M-N") (lambda () (interactive) (move-line  1)))
(global-set-key (kbd "M-P") (lambda () (interactive) (move-line -1)))

;; 対応する括弧を挿入
;; (electric-pair-mode t)
;; (defun electric-pair ()
;;   "Insert character pair without sournding spaces"
;;   (interactive)
;;   (let (parens-require-spaces)
;;     (insert-pair)))

;; 行揃え
(global-set-key (kbd "C-c f") 'align-regexp)

;; ------------------------------------------------------------------------
; Emacs Package Setting
;; ------------------------------------------------------------------------

;; auto complete load
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; auto complete key bind
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;; helm load
(require 'helm-config)
(require 'helm-ag)
(require 'helm-descbinds);; helm keybind setting
(helm-descbinds-mode)
(global-set-key (kbd "C-;") 'helm-mini)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c b") 'helm-descbinds)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c s") 'helm-ag)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)

;; redo+
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo)
(setq undo-no-redo t)

;; undo-hist
(when (require 'undohist nil t)
  (undohist-initialize))
;; undo-tree
(when (require 'undo-tree nil t)
   (global-undo-tree-mode))

;; php-mode
(require 'php-mode)
(load-library "php-mode")

;; web-mode
(require 'web-mode)
;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.view"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;; インデント関係
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   4)
  (setq web-mode-css-offset    4)
  (setq web-mode-script-offset 4)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(add-hook 'web-mode-hook 'web-mode-hook)

;; gtags package
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'php-mode-hook 'helm-gtags-mode)
(eval-after-load "helm-gtags"
 '(progn
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "<f5>") 'helm-gtags-pop-stack)))

(add-hook 'php-mode-hook
         '(lambda()
            (gtags-mode 1)
            (gtags-make-complete-list)
            ))

;; multiple-cursors package
(require 'multiple-cursors)
(require 'smartrep)

;; smartrep package
(declare-function smartrep-define-key "smartrep")

; multiple-cursors keybind setting
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

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

;; 矩形編集
(cua-mode t)
(setq cua-enable-cua-keys nil) ; デフォルトキーバインドを無効化

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Flycheck
;;(require 'flycheck-p)
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;(add-hook 'php-mode-hook 'flycheck-mode)
;; Flycheck エコーエリア表示
;;(nnrequire 'flycheck-pos-tip)
;;(eval-after-load 'flycheck
;;  '(custom-set-variables
;;   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; emmet-mode package
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'php-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4)))
(global-set-key (kbd "C-c i") 'emmet-expand-line)
