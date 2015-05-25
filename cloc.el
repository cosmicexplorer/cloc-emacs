;;; cloc.el --- count lines of code over emacs buffers

;;; Copyright 2015 Danny McClanahan

;;; Author: Danny McClanahan <danieldmcclanahan@gmail.com>
;;; Version: 2015.04.26
;;; Package-Requires: ((cl-lib "0.5"))
;;; Package-Version: 0.0.0
;;; Keywords: cloc, count, source, code, lines
;;; URL: https://github.com/cosmicexplorer/cloc-emacs

;;; This file is not part of GNU Emacs.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This is a small attempt at cloc integration for Emacs. The functionality is
;;; exposed through two functions: cloc, an interactive function which performs
;;; a search through all buffers whose filepaths match the given regex (or the
;;; current buffer, as desired), and counts lines of code in them. It also
;;; exposes cloc-get-results-as-plists, a non-interactive function which does
;;; the same thing, but parses and organizes it all into a list of plists for
;;; easier analysis.

;;; cloc will search over all buffers, including those which do not visit files,
;;; and tramp buffers, but if the buffer is not visiting a file (and therefore
;;; does not have a pathname), cloc will only be able to match the regex to the
;;; buffer's buffer-name.

;;; Example searches include: "\.cpp$", for all C++ sources files, or "/foo/",
;;; where "/foo/" is the name of a project directory; cloc will then count all
;;; code over all open buffers visiting files within a directory named foo.

;;; Usage:

;;; M-x cloc

;;; Code:

(require 'cl-lib)

(defgroup cloc nil
  "An interface to 'cloc'."
  :group 'processes
  :prefix "cloc")

(defcustom cloc-use-3rd-gen nil
  "Whether or not to use cloc's third-generation language output option."
  :group 'cloc)

(defun cloc-format-command (be-quiet bufs-to-cloc)
  "Format the \"cloc\" command according to BE-QUIET and the defcustom
CLOC-USE-3RD-GEN, and run the command on the list of strings held in
BUFFERS-TO-CLOC. Return the command output as a string."
  ;; call-process is cleaner because it doesn't require quotes, but it's more
  ;; annoying to output it to a string, which is what we want. oh well.
  (shell-command-to-string
   (apply
    #'concat
    (cons
     (let ((base (if be-quiet "cloc --quiet --csv "
                   "cloc ")))
       (if cloc-use-3rd-gen
           (concat base "--3 ")
         base))
     (mapcar (lambda (str) (concat "\"" str "\" "))
             bufs-to-cloc)))))

(defun cloc-get-extension (filename)
  "Return the extension of FILENAME (.h, .c, .mp3, etc), if exists, else return
nil."
  ;; this doesn't work on filenames with extensions that have newlines in them
  ;; because . doesn't match newlines; however, if such a file extension were to
  ;; exist and be standardized, we would have far worse problems than whether an
  ;; emacs package supports it
  (let ((match (string-match "\\..+$" filename)))
    (if match (match-string 0 filename) nil)))

(defun cloc-get-buffers-with-regex (regex-str be-quiet)
  "Loop through all open buffers for buffers visiting files whose paths match
REGEX. If the file is not visiting a buffer (or is over a tramp connection), but
its (buffer-name) matches REGEX, the file is written out to a temporary area. A
plist is returned, with :files set to a list of the files which correspond to
open buffers matching REGEX, and :tmp-files set to a list of the files which
have been created in the temporary area (and which should be destroyed by the
caller of this function). An additional property :is-many is always set to t on
the returned list so that a caller can determine whether a list was produced by
this function.

BE-QUIET determines whether to return cloc's output as '--quiet --csv', or
verbose as usual."
  (cl-loop for buf in (buffer-list)
           with tramp-regex-str = "^/ssh:"
           with ret-list = nil
           with tmp-list = nil
           do (let ((buf-path (buffer-file-name buf)))
                ;; if this is a normal file buffer
                (if (and buf-path
                         (string-match-p regex-str buf-path)
                         (not (string-match-p tramp-regex-str buf-path)))
                    (add-to-list 'ret-list buf-path)
                  ;; if this matches and is tramp buf
                  (when (or (and buf-path (string-match-p regex-str buf-path))
                            ;; if this does not visit a file but matches regex
                            (and (not buf-path)
                                 (string-match-p regex-str (buffer-name buf))))
                    (let ((extension (cloc-get-extension (buffer-name buf))))
                      ;; if extension is nil, then file is
                      ;; probs not code, so forget about it
                      (when extension
                        (let ((tmp-file (make-temp-file "cloc" nil extension)))
                          (with-current-buffer buf
                            (write-region nil nil tmp-file))
                          (add-to-list 'ret-list tmp-file)
                          (add-to-list 'tmp-list tmp-file)))))))
           finally (return
                    (list :files ret-list :tmp-files tmp-list :is-many t))))

(defun cloc-get-output (prefix-given be-quiet &optional regex)
  "This is a helper function to get cloc output for a given set of buffers or
the current buffer (if PREFIX-GIVEN is non-nil), as desired. BE-QUIET says
whether to output in CSV format, and REGEX is the optional regex to search
through file paths with. If used programmatically, be aware that it will query
for a regex if one is not provided by argument."
  (if (executable-find "cloc")
      ;; if prefix given, cloc current buffer; don't ask for regex
      (let* ((regex-str
              (or regex prefix-given
                  (read-from-minibuffer "file path regex: ")))
             (buffers-to-cloc
              ;; if blank string given, then assume the current file
              ;; name was what was intended.
              (if (or prefix-given
                      (string= regex-str ""))
                  (list (buffer-file-name))
                (cloc-get-buffers-with-regex regex-str be-quiet)))
             ;; return list so we can tell the difference between an
             ;; invalid regexp versus a real result, even though the
             ;; list always has only one element
             (result-into-list
              ;; check if list is result of cloc-get-buffers-with-regex
              (let ((cloc-bufs-list
                     (if (not (plist-get buffers-to-cloc :is-many))
                         buffers-to-cloc
                       (plist-get buffers-to-cloc :files))))
                (if cloc-bufs-list
                    (cloc-format-command be-quiet cloc-bufs-list)
                  "No filenames were found matching regex."))))
        ;; cleanup!
        (apply #'call-process
               (append (list "rm" nil nil nil)
                       (plist-get buffers-to-cloc :tmp-files)))
        result-into-list)
    "cloc not installed. Download it at http://cloc.sourceforge.net/ or through
your distribution's package manager."))

(defun cloc-get-first-n-of-list (n the-list)
  "Get first N elements of THE-LIST as another list. 1 <= n <= (length
the-list)."
  (cl-loop for item in the-list
           for x from 1 upto n
           collect item))

(defun cloc-get-lines-of-str-as-list (str)
  "Get and return lines of STR (without ending newline) into a list."
  ;; will add a newline to the end of str if not already there, but removes it
  ;; at bottom
  ;; 10 is newline
  (let ((is-final-char-newline (char-equal (aref str (1- (length str))) 10)))
    (unless is-final-char-newline
      (setq str (concat str "\n")))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((line-list nil))
        (setq line-list
              (cons
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               nil))
        (setq line-list
              (append line-list
                      (cl-loop while (= 0 (forward-line 1))
                               collect (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
        (if is-final-char-newline
            line-list
          (cloc-get-first-n-of-list (1- (length line-list)) line-list))))))

(defun cloc-get-line-as-plist (line)
  "This is a helper function to convert a CSV-formatted LINE of cloc output into
a plist representing a cloc analysis."
  (let ((out-plist nil))
    (cl-loop for str-pos from 0 upto (1- (length line))
             with prev-str-pos = 0
             with cur-prop = :files
             while (or cloc-use-3rd-gen
                       (not (eq cur-prop :scale)))
             do (progn
                  (when (char-equal (aref line str-pos) 44) ;  44 is comma
                    (cond ((eq cur-prop :files)
                           (setq out-plist
                                 (plist-put out-plist :files
                                            (string-to-number
                                             (substring line prev-str-pos
                                                        str-pos))))
                           (setq cur-prop :language))
                          ((eq cur-prop :language)
                           (setq out-plist
                                 (plist-put out-plist :language
                                            (substring line prev-str-pos
                                                       str-pos)))
                           (setq cur-prop :blank))
                          ((eq cur-prop :blank)
                           (setq out-plist
                                 (plist-put out-plist :blank
                                            (string-to-number
                                             (substring line prev-str-pos
                                                        str-pos))))
                           (setq cur-prop :comment))
                          ((eq cur-prop :comment)
                           (setq out-plist
                                 (plist-put out-plist :comment
                                            (string-to-number
                                             (substring line prev-str-pos
                                                        str-pos))))
                           (setq cur-prop :code))
                          ((eq cur-prop :code)
                           (setq out-plist
                                 (plist-put out-plist :code
                                            (string-to-number
                                             (substring line prev-str-pos
                                                        str-pos))))
                           (setq cur-prop :scale))
                          ((eq cur-prop :scale)
                           (setq out-plist
                                 (plist-put out-plist :scale
                                            (string-to-number
                                             (substring line prev-str-pos
                                                        str-pos))))
                           (setq cur-prop :3rd-gen-equiv)))
                    (setq prev-str-pos (1+ str-pos))))
             finally (cond ((eq cur-prop :3rd-gen-equiv)
                            (setq out-plist
                                  (plist-put out-plist :3rd-gen-equiv
                                             (string-to-number
                                              (substring line prev-str-pos
                                                         str-pos)))))
                           ((eq cur-prop :code)
                            (setq out-plist
                                  (plist-put out-plist :code
                                             (string-to-number
                                              (substring line prev-str-pos
                                                         str-pos)))))))
    out-plist))

;;;###autoload
(defun cloc-get-results-as-plists (prefix-given regex)
  "Get output of cloc results as a list of plists. Each plist contains as a
property the number of files analyzed, the blank lines, the code lines, comment
lines, etc. for a given language in the range of files tested. If PREFIX-GIVEN
is set to true, this runs on the current buffer. If not, and REGEX is given,
it will search file-visiting buffers for file paths matching the regex. If the
regex is nil, it will prompt for a regex; putting in a blank there will default
to the current buffer."
  (cl-remove-if #'not      ; remove nils which sometimes appear for some reason
                (mapcar
                 #'cloc-get-line-as-plist
                 ;; first two lines are blank line and csv header, so discard
                 (nthcdr 2 (cloc-get-lines-of-str-as-list
                            (cloc-get-output prefix-given t regex))))))

;;;###autoload
(defun cloc (prefix-given)
  "Run the executable \"cloc\" over file-visiting buffers with pathname
specified by a regex. If PREFIX-GIVEN is true or a blank regex is given, the
current buffer is \"cloc'd\". cloc's entire summary output is given in the
messages buffer."
  (interactive "P")
  (message (cloc-get-output prefix-given nil)))

(provide 'cloc)

;;; cloc.el ends here
