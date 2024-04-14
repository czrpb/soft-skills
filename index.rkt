#lang racket

(require racket/system)
(require racket/function)

; Curried function to check if a file has a specific suffix
(define string-suffix?/curry (curry string-suffix?))

; Partial function to check if a file is a PDF
(define pdf-file? (string-suffix?/curry ".pdf"))

; Function to get the directory path from command line arguments or use the current directory
(define (get-directory-path)
  (if (vector-empty? (current-command-line-arguments))
      (current-directory)
      (vector-ref (current-command-line-arguments) 0)))

; Function to get the list of PDF files in the directory
(define (get-pdf-files directory)
  (filter pdf-file? (directory-list directory)))

; Function to read the existing README.md file, if it exists
(define (read-existing-readme directory)
  (let ((readme-path (build-path directory "README.md")))
    (if (file-exists? readme-path)
        (file->string readme-path)
        "")))

; Partial function to create a regular expression for a PDF file section
(define (pdf-section-regexp/partial pdf-file)
  (regexp (string-append "## " pdf-file)))

; Function to create a markdown section for a PDF file
(define (create-markdown-section pdf-file)
  (printf "Enter a description for ~a: " pdf-file)
  (let ((description (read-line)))
    (string-append "## " pdf-file "\n\n"
                   description "\n\n"
                   "[" pdf-file "](" pdf-file ")\n\n")))

; Function to write the updated README.md file
(define (write-readme directory content)
  (let ((readme-path (build-path directory "README.md")))
    (with-output-to-file readme-path
      (lambda ()
        (display content))
      #:exists 'replace)))

; Main function
(define (main)
  (let ((directory (get-directory-path))
        (pdf-files (get-pdf-files (get-directory-path)))
        (existing-readme (read-existing-readme (get-directory-path))))
    (let ((new-sections
           (map create-markdown-section
                (filter (lambda (pdf-file)
                          (not (regexp-match (pdf-section-regexp/partial pdf-file) existing-readme)))
                        pdf-files))))
      (write-readme directory (string-append existing-readme (string-join new-sections "\n"))))))

; Run the main function
(main)
