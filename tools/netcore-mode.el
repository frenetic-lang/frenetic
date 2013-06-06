;; command to comment/uncomment text
(defun netcore-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start "(*") (comment-end "*)")
        )
    (comment-dwim arg)))

(setq netcore-keywords
  '(
    ("," . 'font-lock-builtin)
    ("(" . 'font-lock-builtin)
    (")" . 'font-lock-builtin)
    ("{" . 'font-lock-builtin)
    ("}" . 'font-lock-builtin)
    ("!" . 'font-lock-builtin)
    ("*" . 'font-lock-builtin)
    ("=" . 'font-lock-builtin)
    ("->" . 'font-lock-builtin)
    ("&&" . 'font-lock-builtin)
    ("||" . 'font-lock-builtin)
    (";" . 'font-lock-builtin)
    ("|" . 'font-lock-builtin)
    ("+" . 'font-lock-builtin)
    ("let" . 'font-lock-keyword-face)
    ("inPort" . 'font-lock-keyword-face)
    ("in" . 'font-lock-keyword-face)
    ("publicIP" . 'font-lock-keyword-face)
    ("all" . 'font-lock-keyword-face)
    ("fwd" . 'font-lock-keyword-face)
    ("<none>" . 'font-lock-keyword-face)
    ("filter" . 'font-lock-keyword-face)
    ("switch" . 'font-lock-keyword-face)
    ("vlan" . 'font-lock-keyword-face)
    ("srcMAC" . 'font-lock-keyword-face)
    ("dstMAC" . 'font-lock-keyword-face)
    ("srcIP" . 'font-lock-keyword-face)
    ("dstIP" . 'font-lock-keyword-face)
    ("tcpSrcPort" . 'font-lock-keyword-face)
    ("tcpDstPort" . 'font-lock-keyword-face)
    ("frameType" . 'font-lock-keyword-face)
    ("arp" . 'font-lock-keyword-face)
    ("ip" . 'font-lock-keyword-face)
    ("begin" . 'font-lock-keyword-face)
    ("end" . 'font-lock-keyword-face)
    ("if" . 'font-lock-keyword-face)
    ("then" . 'font-lock-keyword-face)
    ("else" . 'font-lock-keyword-face)
    ("pass" . 'font-lock-keyword-face)
    ("drop" . 'font-lock-keyword-face)
    ("monitorPolicy" . 'font-lock-keyword-face)
    ("monitorTable" . 'font-lock-keyword-face)
    ("monitorLoad" . 'font-lock-keyword-face)
    ("monitorPackets" . 'font-lock-keyword-face)
    ("\\([0-9][0-9]:\\)\\{5\\}[0-9][0-9]" . 'font-lock-constant-face)
    ("\\([0-9]\\{1,3\\}\\.\\)\\{1,3\\}[0-9]\\{1,3\\}" .
     'font-lock-constant-face)
    ("\\b[0-9]+\\b" . 'font-lock-constant-face)
    ("\\([A-Z]\\|[a-z]\\|_\\)\\([A-Z]\\|[a-z]\\|_\\|[0-9]\\)*" . 'font-lock-variable-name-face)
    )
)

(defvar netcore-syntax-table nil "Syntax table for `netcore-mode'.")
(setq netcore-syntax-table
  (let ((synTable (make-syntax-table)))

    (modify-syntax-entry ?\( ". 1" synTable)
    (modify-syntax-entry ?\) ". 4" synTable)
    (modify-syntax-entry ?* ". 23" synTable)

    synTable))

(define-derived-mode netcore-mode fundamental-mode
  "syntax mode for NetCore"
  :syntax-table netcore-syntax-table
  (setq font-lock-defaults '(netcore-keywords))
  (setq mode-name "netcore")
  
  (define-key netcore-mode-map [remap comment-dwim] 'netcore-comment-dwim)
)

;; (define-generic-mode
;;   'netcore-mode
;;   '("//")
;;   '("in" "let" "inPort" "publicIP" "all" "fwd" "<none>" 
;;     "filter" "switch" "vlan" "srcMAC" "dstMAC" "srcIP" "dstIP" "tcpSrcPort"
;;     "tcpDstPort" "frameType" "arp" "ip" "inPort" "begin" "end" "if" "then"
;;     "else" "pass" "drop" "monitorPolicy" "monitorTable" "monitorLoad"
;;     "monitorPackets")
;;   '(
;;     ("," . 'font-lock-builtin)
;;     ("(" . 'font-lock-builtin)
;;     (")" . 'font-lock-builtin)
;;     ("{" . 'font-lock-builtin)
;;     ("}" . 'font-lock-builtin)
;;     ("!" . 'font-lock-builtin)
;;     ("*" . 'font-lock-builtin)
;;     ("=" . 'font-lock-builtin)
;;     ("->" . 'font-lock-builtin)
;;     ("&&" . 'font-lock-builtin)
;;     ("||" . 'font-lock-builtin)
;;     (";" . 'font-lock-builtin)
;;     ("|" . 'font-lock-builtin)
;;     ("+" . 'font-lock-builtin)
;;     ("\\([0-9][0-9]:\\)\\{5\\}[0-9][0-9]" . 'font-lock-constant-face)
;;     ("\\([0-9]\\{1,3\\}\\.\\)\\{1,3\\}[0-9]\\{1,3\\}" .
;;      'font-lock-constant-face)
   
;;     ("\\b[0-9]+\\b" . 'font-lock-constant-face)
;;     ("\\([A-Z]\\|[a-z]\\|_\\)\\([A-Z]\\|[a-z]\\|_\\|[0-9]\\)*" . 'font-lock-variable-name-face)
;;     )
;;   '("\\.nc$")
;;   "A mode for NetCore files"
;;   )