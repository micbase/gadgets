(ft-set-jid! "user@gmail.com")
(ft-set-server! "talk.google.com")
(ft-set-password! "passwd")
(ft-set-prompt! "#FreeTalk# ")
(ft-set-sslconn! #t)
(ft-set-port! 5223)

(define index #f)
(define endindex #f)
(add-hook! ft-message-receive-hook
		   (lambda (time from nickname message)
			 (begin
			   (set! index (string-contains message "http://mp3.dict.cn/mp3.php"))
			   (if (integer? index)
				 (begin
				   (set! endindex (string-index message #\newline index))
				   (if (integer? endindex)
					 (system (string-append "mpg123 " (substring message index endindex) " 2>/dev/null"))
					 (system (string-append "mpg123 " (substring message index) " 2>/dev/null" ))
					 )))
			   )))
