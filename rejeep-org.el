(setq org-default-notes-file "~/Dropbox/notes.org")
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c v") (lambda ()
                                (interactive)
                                (find-file org-default-notes-file)))

(setq org-capture-templates
      '(("n" "Note" checkitem (file+headline org-default-notes-file "Notes"))

        ("p" "Private")
        ("ph" "High" checkitem (file+olp org-default-notes-file "Private" "High"))
        ("pm" "Med" checkitem (file+olp org-default-notes-file "Private" "Med"))
        ("pl" "low" checkitem (file+olp org-default-notes-file "Private" "Low"))

        ("w" "Work")
        ("wh" "High" checkitem (file+olp org-default-notes-file "Work" "High"))
        ("wm" "Med" checkitem (file+olp org-default-notes-file "Work" "Med"))
        ("wl" "low" checkitem (file+olp org-default-notes-file "Work" "Low"))

        ("e" "Ecukes" checkitem (file+headline org-default-notes-file "Ecukes"))
        ("c" "Cask" checkitem (file+headline org-default-notes-file "Cask"))
        ("f" "F" checkitem (file+headline org-default-notes-file "F"))))

(provide 'rejeep-org)
