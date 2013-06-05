(define-generic-mode
  'netcore-mode
  '("//")
  '("in" "let" "inPort" "publicIP" "all" "fwd" "<none>" 
    "filter" "switch" "vlan" "srcMAC" "dstMAC" "srcIP" "dstIP" "tcpSrcPort"
    "tcpDstPort" "frameType" "arp" "ip" "inPort" "begin" "end" "if" "then"
    "else" "pass" "drop" "monitorPolicy" "monitorTable" "monitorLoad"
    "monitorPackets")
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
    ("\\([0-9][0-9]:\\)\\{5\\}[0-9][0-9]" . 'font-lock-constant-face)
    ("\\([0-9]\\{1,3\\}\\.\\)\\{1,3\\}[0-9]\\{1,3\\}" .
     'font-lock-constant-face)
   
    ("\\b[0-9]+\\b" . 'font-lock-constant-face)
    ("\\([A-Z]\\|[a-z]\\|_\\)\\([A-Z]\\|[a-z]\\|_\\|[0-9]\\)*" . 'font-lock-variable-name-face)
    )
  '("\\.nc$")
  nil
  "A mode for NetCore files")