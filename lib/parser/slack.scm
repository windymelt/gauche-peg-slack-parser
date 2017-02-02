(define-module parser.slack
  (use parser.peg)
  (export
   <slack-parse-error>
   slack-parse
))
(select-module parser.slack)

(define-condition-type <slack-parse-error> <error> #f)

(define %user ($lift list->string ($many ($one-of #[a-zA-Z0-9_-]))))

(define %ampm ($or ($y "AM") ($y "PM")))

(define %hour ($lift list->string ($many digit 1 2)))

(define %minutes %hour)

(define %time ($do
               [h %hour]
               [_ ($c #\:)]
               [m %minutes]
               [_ ($optional space)]
               [ampm ($optional %ampm)]
               ($return (list h m ampm))))

(define %time-bracket ($between ($c #\[) %time ($c #\])))

(define %text ($lift list->string ($many ($none-of #[\n]) 1)))

(define %say ($do
              [time %time-bracket]
              [_ ($many ($one-of #[ ]))]
              [_ newline]
              [text %text]
              [_ newline]
              ($return (list time text))))

(define %says ($do
               [u %user]
               [_ space]
               [says ($alternate %say newline)]
               ($return (list u says))))

(define %conversation ($sep-by %says newline))

(define (slack-parse :optional (input (current-input-port)))
  (cond [(string? input)
         (peg-parse-string %conversation input)]
        [(input-port? input)
         (peg-parse-port %conversation input)]
        [else
         (error <slack-parse-error> "input port or string required, but got" input)]))

(provide "parser/slack")
