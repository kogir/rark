#lang racket/base

;; NS Logging

(require srfi/13
         racket/match
         racket/file
         racket/date
         (for-syntax racket/base))

(provide ns-log-enabled?
         ns-log-current-directory
         ns-log-enable
         ns-log-disable
         ns-log)

;; Variables for internal state tracking
(define internal-logger (make-logger))
(define logger-thread #f)
(define log-ports #f)
(define log-directory #f)

;; [#t|#f] when logging is [enabled|disabled]
(define (ns-log-enabled?)
  (if (and logger-thread log-ports log-directory)
      #t
      #f))

;; Returns the current log directory when logging is enabled
;; #f when logging is disabled
(define (ns-log-current-directory)
  (if (ns-log-enabled?)
      log-directory
      #f))

;; Enables logging to the specified directory
;; Returns #t if successful, and #f if an error is encountered
;; Will not make the directory if it does not exist
;; Write access is (somewhat obviously) required
;; TODO: Make thread safe?
(define (ns-log-enable log-dir)
  (cond [(not (string? log-dir)) #f]
        [(eq? log-dir log-directory) #t]
        [(not (directory-exists? log-dir)) #f]
        [#t (ns-log-disable)
            (set! log-directory log-dir)
            (set! log-ports (make-hasheq))
            (set! logger-thread (make-logger-thread log-directory
                                                    internal-logger))
            #t]))

;; Disables logging
(define (ns-log-disable)
  (when logger-thread
    (kill-thread logger-thread)
    (set! logger-thread #f))
  (when log-ports
    (hash-map log-ports
              (lambda (source port) (close-log-port port)))
    (set! log-ports #f))
  (when log-directory
    (set! log-directory #f)))

;; Code that sends entries to the logger

(define (ns-log source format-string . args)
  (when (log-level? internal-logger 'debug)
    (log-message internal-logger
                 'debug
                 (format "~a ~a"
                         (current-inexact-milliseconds)
                         (if (null? args)                             
                             format-string
                             (apply format `(,format-string ,@args))))
                 source)))

;; These functions let us tag the end of log file names
;; They're stolen in spirit from srv.arc and arc.arc

(define (tomorrow-seconds)
  (let* ([now-seconds (current-seconds)]
         [overshoot-seconds (+ now-seconds (* 24 60 60))]
         [overshoot-tomorrow-date (seconds->date overshoot-seconds)]
         [tomorrow-seconds (- overshoot-seconds
                              (date-second overshoot-tomorrow-date)
                              (* 60 (date-minute overshoot-tomorrow-date))
                              (* 60 60 (date-hour overshoot-tomorrow-date)))])
    tomorrow-seconds))

(define (tomorrow-milliseconds)
  (* 1000 (tomorrow-seconds)))

(define (date-string)
  (let* ([today (current-date)]
         [y (number->string (date-year today))]
         [m (number->string (date-month today))]
         [d (number->string (date-day today))])
    (string-append y "-" (string-pad m 2 #\0) "-" (string-pad d 2 #\0))))

(define (logfile-path log-dir source)
  (build-path log-dir (string-append (symbol->string source) "-" (date-string))))

;; Log Ports

(struct log-port (port expire)
  #:transparent)

(define (acquire-port source)
  (let ([port (hash-ref! log-ports source (lambda () (create-log-port source)))])
    (when (log-port-expired? port)
      (close-log-port port)
      (set! port (create-log-port source))
      (hash-set! log-ports source port))
    port))

(define (log-port-expired? port)
  (< (log-port-expire port) (current-milliseconds)))

(define (close-log-port port)
  (close-output-port (log-port-port port)))

(define (create-log-port source)
  (log-port
   (open-output-file (logfile-path log-directory source) #:mode 'text #:exists 'append)
   (tomorrow-milliseconds)))

(define (write-log-port port line)
  (let ([port (log-port-port port)])
    (fprintf port "~a\n" line)
    (flush-output port)))

;; Log Writer

(define (make-logger-thread log-dir logger)
  (define (read-forever receiver)
    (match (sync receiver)
      [(vector _ message source)
       (when (and (string? message) (symbol? source))
         (let ([port (acquire-port source)])
           (write-log-port port message)))])
    (read-forever receiver))
  (thread (lambda () (read-forever (make-log-receiver internal-logger 'debug)))))
