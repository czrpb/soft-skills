#lang racket

(require racket/cmdline)
(require racket/string)

(define (process-line line)
  (let ([space8-pattern #px"^        "]
        [space4-pattern #px"^    "]
        [space4-text-colon-pattern #px"^    ([^:]+): "]
        [heading-pattern #px"^## "])
    (let ([new-line
           (cond
             [(regexp-match heading-pattern line)
              (string-append "---\n\n" line)]
             [(regexp-match space8-pattern line)
              (regexp-replace space8-pattern line "  - ")]
             [(regexp-match space4-text-colon-pattern line)
              (regexp-replace space4-text-colon-pattern line "- **\\1**: ")]
             [(regexp-match space4-pattern line)
              (regexp-replace space4-pattern line "- ")]
             [else line])])
      (displayln new-line))))

(define (main file-path)
  (with-input-from-file file-path
    (lambda ()
      (for ([line (in-lines)])
        (process-line line)))))

(command-line
 #:program "process-file"
 #:args (file-path)
 (main file-path))
