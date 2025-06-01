;; This is a file with configuration related to my workplace

;; Nice to have for triggering stuff that are unique for this job PC
(setq computer-name "work-pc")

;; Org mode modifications

(defface org-in-review
    '((((class color) (min-colors 88) (background light))
       :background "darkseagreen2")
      (((class color) (min-colors 88) (background dark))
       :foreground "yellow3"))
    "Face for TODO-tasks tagged with IN-PROGRESS"
    :group 'org-faces)

  ;; Yanky way of writing text return functions for
  ;; `org-capture-templates' to have a cleaner alist
  (defun start-day-template () nil
         "\
* [%<%H:%M>] Started [/] %(make-string 44 ?-)

Checklist
- [ ] Set location%?
- [ ] Report time
- [ ] [[https://hms365.sharepoint.com/sites/HMSstart/SitePages/All-news.aspx][News]]
- [ ] Mail/Meetings
- [ ] Jenkins
- [ ] [[https://hms-networks.atlassian.net/issues/?filter=10331][Jira]]
- [-] [[https://review.hms.se/r/dashboard/self][Gerrit]]")

  (defun journal-quit-report ()
    "Check if clock is running, clock out if it is and then print the
clock report from the agenda files and quit"
    (if (org-clock-is-active)
        (org-clock-out))
    (let* ((org-clock-clocktable-default-properties
            '(:scope agenda :maxlevel 4 :block today :properties ("PROJECT") :fileskip0 t)))
      (org-clock-report)))

  (defun general-log-template ()
    "Prompt for a log message and a project to report time on.
If a task is currently clocked, add it at the top of the capture.
If the region is active then paste it into the capture and,
finally, put the point just under the PROPERTY drawer"
    (let ((include-clock-link ""))
      (if (org-clock-is-active)
          (setq include-clock-link "Clocked in on: %K\n"))
      (format
       "\
* [%%<%%H:%%M>] %%^{Log message: |Log}
%%^{PROJECT}p%s
%%?
%%i"
       include-clock-link)))

(customize-set-variable
 'org-capture-templates '(("t" "Todo")
                          ("tt" "New"
                           entry
                           (file+headline org-default-todo-file "Tasks")
                           "* TODO %?\n  %i\n  %a")
                          ("te" "Edit"
                           plain
                           (file+function org-default-todo-file org-goto)
                           "%?\n  %i"
                           :empty-lines 1
                           :unnarrowed t)

                          ("n" "Notes"
                           plain
                           (file+function org-default-notes-file org-goto)
                           "%?")

                          ("c" "Clocking"
                           plain
                           (clock)
                           "%?"
                           :unnarrowed t)

                          ("j" "Journal")

                          ("js" "Start day"
                           entry
                           (file+olp+datetree org-default-journal-file)
                           (function start-day-template)
                           :clock-in t
                           :unnarrowed t
                           :after-finalize (lambda ()
                                             (interactive )
                                             (org-agenda nil "i")))

                          ("jq" "Quit day"
                           entry
                           (file+olp+datetree org-default-journal-file)
                           "* [%<%H:%M>] Quit\n%(journal-quit-report)"
                           :immediate-finish t)

                          ("jp" "Pause"
                           entry
                           (file+olp+datetree org-default-journal-file)
                           "* [%<%H:%M>] Paused\nGoing for %?"
                           :clock-in t
                           :after-finalize (lambda ()
                                             (interactive )
                                             (org-agenda nil "i")))

                          ("jl" "General Log"
                           entry
                           (file+olp+datetree org-default-journal-file)
                           (function general-log-template))

                          ("jm" "Meeting"
                           entry
                           (file+olp+datetree org-default-journal-file)
                           "* [%<%H:%M>][%^{Minutes}m] %?meeting\n%^{TOPIC}p%^{PROJECT}p")

                          ("b" "Book"
                           entry
                           (file+olp+datetree org-default-books-file)
                           (function book-template)))
 "Set up the org capture templates to fit work")

(customize-set-variable
 'org-agenda-custom-commands '(("i" "Agenda and Jira"
                                ((tags-todo "{A[[:digit:]]+_[[:digit:]]+}+TODO={TODO\\|SELECTED-FOR-DEVELOPMENT}"
                                            ((org-agenda-overriding-header "Jiras assigned to me")))
                                 (todo "BLOCKED"
                                       ((org-agenda-overriding-header "Blocked Jiras")))
                                 (todo "IN-PROGRESS"
                                       ((org-agenda-overriding-header "Started Jiras")))
                                 (todo "IN-REVIEW"
                                       ((org-agenda-overriding-header "Jiras on Gerrit")))
                                 (agenda "")
                                 (tags-todo "meeting"
                                            ((org-agenda-overriding-header "Meetings to plan")))
                                 (tags-todo "bolt5g"
                                            ((org-agenda-overriding-header "Stealth backlog Bolt 5G")))
                                 (tags-todo "bolt2"
                                            ((org-agenda-overriding-header "Stealth backlog Bolt II")))
                                 (tags-todo "-{bolt.*}"
                                            ((org-agenda-overriding-header "Everything else"))))))
 "Set up an agenda view that fits work")

(use-package org-jira
  :ensure t
  :after org
  :defines
  org-default-jira-files
  :custom
  (org-agenda-files (nconc org-agenda-files (list org-default-jira-files)))
  (org-jira-working-dir org-default-jira-files)
  (jiralib-url "https://hms-networks.atlassian.net")
  :init
  (defcustom org-default-jira-files (concat org-directory "/jira-api")
    "File to note down information about specific jira issues"
    :type '(file :must-match t)
    :group 'org-capture)
  :config
  (defconst org-jira-progress-issue-flow
    '(("TODO" . "Selected for Development")
      ("Selected for Development" . "In Progress")
      ("In Progress" . "In Review")
      ("In Review" . "Done"))))

(use-package gerrit-getter
  :if (locate-library "gerrit-getter.el")
  ;; Don't forget to make the symbolic link
  :load-path "lisp/gerrit-el"
  :ensure nil)

(use-package copilot
  ;; Repo link https://github.com/zerolfx/copilot.el
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist"
                                                             "*.el"))
  :demand t
  :ensure nil
  :functions
  copilot--overlay-visible
  copilot-accept-completion
  :config
  (general-define-key "TAB" (general-predicate-dispatch 'indent-for-tab-command
                              (copilot--overlay-visible) 'copilot-accept-completion))
  :hook
  (c-ts-mode   . copilot-mode)
  (c++-ts-mode . copilot-mode)
  (bash-ts-mode . copilot-mode)
  (python-ts-mode . copilot-mode))
