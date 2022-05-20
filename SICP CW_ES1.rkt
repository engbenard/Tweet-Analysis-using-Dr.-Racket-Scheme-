#lang racket
;;;Load tool for analysing the JSON format,
;;;load the necessary dependencies
(require data-science-master)
(require plot)
(require math)
(require json)
;(require json-parsing)
;(require pict)

;;; Required dependency: csv-reading.
;;; Install with `raco pkg install csv-reading`
(require csv-reading)

;READ DATA
;;;Data is stored on local drive in json format
;You can download the data from https://drive.google.com/file/d/1DnALRODNzKvlkJQ8jE4xCwMmfHAqI19u/view?usp=sharing
(define tweet-sentiments(with-input-from-file "D:/MSc Computer Science/SICP/CW/End-of_Sem Project/uganda_tweets/jan_feb_Ugandatweets2022.json" ; invalid JSON
    (λ () (read-json))))
;tweet-sentiments

;Making csv reader
(define make-tweet-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\|)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

;If reader-spec is given, and is not the null list,
;then a “one-shot” reader constructor is constructed with that spec and used
(define next-row
  (make-csv-reader
   (open-input-file "D:/MSc Computer Science/uganda_tweets.csv")
   '((separator-chars            #\|)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

;reading csv using the with-input-from-file
;convert csv to list
(define tweets (with-input-from-file "D:/MSc Computer Science/uganda_tweets.csv"
	 (λ () (csv->list (current-input-port)))))
;tweets

;(csv->list (make-csv-reader (open-input-string tweets)))

;wrappers to read json data directly
;simple wrapper functions for both reading and writing JSON
(define (read-json-wrapper tweet-sentiments)
  (call-with-input-file tweet-sentiments read-json))

; A jsexpr is defined to represent the JSON strings
;(e.g., lists or hash tables) that can be represented as JSON strings
(define (write-json-wrapper jsexpr tweet-sentiments)
  (call-with-output-file tweet-sentiments
    (λ (x) (write-json jsexpr x))
    #:exists 'replace)) 

(define tweet-filter
  (let ([tmp (map (λ (x) (list (list-ref x 4)))tweets)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp))) 
;tweet-filter

;flatten data
(define save-data (flatten tweet-filter)) 
;save-data

;append saved data by appending the string of data
(define store-data (apply string-append save-data))

;pre-process
(define preprocess-text
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase store-data)) #:websafe? #t)))

;Procedures words to call and preprocess the data
;operations that are called inthe preprocess-text above
;are executed dealing with spaces, punctuation and url removal
(define words (document->tokens preprocess-text #:sort? #t))

(define sentiment (list->sentiment words #:lexicon 'nrc))

;sentiment

(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)) 

(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))
