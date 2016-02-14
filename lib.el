
(fset 'reset-to-main-screen
      [?\C-s ?R ?E ?S ?E ?T ?\S-  ?T ?O return ?\C-n ?\C-e ?\C-x ?\C-e ?\C-u ?\C- ])

(defun my-bind-keys ()
  (local-set-key (kbd "<f5>") 'reset-to-main-screen))

(add-hook 'clojure-mode-hook 'my-bind-keys)
