;; -*- mode: emacs-lisp; lexical-binding: t -*-

(setq my-on-winnt (eq system-type 'windows-nt))
(setq my-on-wsl (condition-case nil
                    (string-match-p "Microsoft" (shell-command-to-string "uname -a"))
                  (error nil)))  ;; no uname command, not on WSL
;; Windows doesn't support the variable version of the font
(setq my-default-font (if my-on-winnt "Source Code Pro" "Source Code Variable"))
(when my-on-winnt
  (setq dedicated-terminal-command-template "C:/msys64/msys2_shell.cmd -mingw64 -where '%s'"))
(when my-on-wsl
  (setq dedicated-terminal-command-template "/usr/bin/lxterminal --working-directory='%s'"))
