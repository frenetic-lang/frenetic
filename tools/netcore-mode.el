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
;;     ("(" . 'font-lock-builtin) **/
;;     (")" . 'font-lock-builtin) **/
;;     ("{" . 'font-lock-builtin) **/
;;     ("}" . 'font-lock-builtin) **/
    ("!" . 'font-lock-builtin)
    ("*" . 'font-lock-builtin)
    ("=" . 'font-lock-builtin)
    ("->" . 'font-lock-builtin)
    ("&&" . 'font-lock-builtin)
    ("||" . 'font-lock-builtin)
    (";" . 'font-lock-builtin)
    ("|" . 'font-lock-builtin)
    ("+" . 'font-lock-builtin)
    (" let" . 'font-lock-keyword-face)
    ("let " . 'font-lock-keyword-face)
    ("inPort " . 'font-lock-function-name-face)
    (" in " . 'font-lock-keyword-face)
    (" publicIP" . 'font-lock-function-name-face)
    ("publicIP " . 'font-lock-function-name-face)
    ("all " . 'font-lock-constant-face)
    (" all" . 'font-lock-constant-face)
    ("fwd" . 'font-lock-function-name-face)
    ("<none>" . 'font-lock-constant-face)
    ("filter " . 'font-lock-function-name-face)
    ("switch " . 'font-lock-function-name-face)
    ("vlan " . 'font-lock-function-name-face)
    ("dlDst" . 'font-lock-function-name-face)
    ("dlSrc" . 'font-lock-function-name-face)
    ("dlTyp" . 'font-lock-function-name-face)
    ("srcIP " . 'font-lock-function-name-face)
    ("dstIP " . 'font-lock-function-name-face)
    ("tcpSrcPort" . 'font-lock-function-name-face)
    ("tcpDstPort" . 'font-lock-function-name-face)
    ("frameType" . 'font-lock-function-name-face)
    (" arp" . 'font-lock-constant-face)
    ("arp " . 'font-lock-constant-face)
    ("ip " . 'font-lock-constant-face)
    (" ip" . 'font-lock-constant-face)
    ("begin" . 'font-lock-keyword-face)
    ("end" . 'font-lock-keyword-face)
    ("if" . 'font-lock-keyword-face)
    ("then" . 'font-lock-keyword-face)
    ("else" . 'font-lock-keyword-face)
    ("pass" . 'font-lock-constant-face)
    ("drop" . 'font-lock-constant-face)
    ("monitorPolicy" . 'font-lock-function-name-face)
    ("monitorTable" . 'font-lock-function-name-face)
    ("monitorLoad" . 'font-lock-function-name-face)
    ("monitorPackets" . 'font-lock-function-name-face)
    ("\\([a-fA-F0-9][a-fA-F0-9]:\\)\\{5\\}[a-fA-F0-9][a-fA-F0-9]" . 'font-lock-constant-face)
    ("\\([0-9]\\{1,3\\}\\.\\)\\{1,3\\}[0-9]\\{1,3\\}" .
     'font-lock-constant-face)
    ("\\b[0-9]+\\b" . 'font-lock-constant-face)
    ("\\([A-Z]\\|[a-z]\\|_\\)\\([A-Z]\\|[a-z]\\|_\\|[0-9]\\)*" . 'font-lock-variable-name-face)
    )
)

(defvar netcore-syntax-table nil "Syntax table for `netcore-mode'.")
(setq netcore-syntax-table
  (let ((synTable (make-syntax-table)))

    (modify-syntax-entry ?\( "(. 1" synTable)
    (modify-syntax-entry ?\) "). 4" synTable)
    (modify-syntax-entry ?* ". 23" synTable)

    synTable))

(define-derived-mode netcore-mode fundamental-mode
  "syntax mode for NetCore"
  :syntax-table netcore-syntax-table
  (setq font-lock-defaults '(netcore-keywords))
  (setq mode-name "netcore")
  
  (define-key netcore-mode-map [remap comment-dwim] 'netcore-comment-dwim)
)