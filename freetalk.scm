(ft-set-jid! "user@gmail.com")
(ft-set-server! "talk.google.com")
(ft-set-password! "passwd")
(ft-set-prompt! "#FreeTalk# ")
(ft-set-sslconn! #t)
(ft-set-port! 5223)

(define str (string #\null))
(define cmd (string #\null))
(define pronindex 0)
(define pronindex2 0)
(define flag #f)
(define strhelp "  Welcome to GTalk Robot!#\newline
  Here are the commands that I can handle:\n
  1.Dictionary:      dic word\n
     Example:        dic word\n
  2.Shorten URL:     url url\n
     Example:        url http://www.example.com\n
  3.IP Lookup:       add IP\n
     Example:        add 192.168.1.1\n
  4.Weather:         wea city\n
     Example:        wea \n
  5.Remote Control:  ctl command  (This command may be dangerous!)\n
     Example:        ctl ls /usr\n
  6.Todo List:       tdl all  see the all list\n
                     tdl add  add something to the list\n
                     tdl del {number}  del the specific thing\n")

(add-hook! ft-message-receive-hook
		   (lambda (time from nickname message)

			 (if (< (string-length message) 4 )
			   (ft-send-message from "Unknown command, type help for usage")
			   (begin
				 (set! cmd (substring message 4))
				 (cond ((equal? (substring message 0 4) "dic ");Dictionary

						(begin
						  (let*
							((port (open-input-pipe (string-append "lynx -source -assume_charset=UTF8 \"http://dict.cn/ws.php?utf8=true&q=" cmd "\"")))
							 (strline (read-line port)))
							(set! str (string #\null))
							(while (not (eof-object? strline))
								   (if (string-contains strline "<audio>") (set! flag #t))
								   (if (equal? flag #t) (set! str (string-append str strline "<br>\n")))
								   (if (string-contains strline "</def>") (set! flag #f))
								   (set! strline (read-line port))
								   )
							(close-pipe port))
						  (if (equal? str (string #\null))
							(ft-send-message from "Sorry, the word is not found!")
							(begin
							  (set! pronindex (string-contains str "<pron>"))
							  (set! str (string-replace str "[" (+ 6 pronindex) (+ 6 pronindex)))
							  (set! pronindex2 (string-contains str "</pron>"))
							  (set! str (string-replace str "]" pronindex2 pronindex2))
							  (let* ((srcfileport (open-output-file "/tmp/gtalkaudiotmp")))
								(display str srcfileport)
								(close-output-port srcfileport))
							  (send-message-pipe from "lynx -dump -assume_charset=UTF8 -force_html /tmp/gtalkaudiotmp")
							  ))
						  ))

					   ((equal? (substring message 0 4) "help");Print the help
						(ft-send-message from strhelp))

					   ((equal? (substring message 0 4) "url ");Shorten URL
						(send-message-pipe from (string-append "lynx -source \"http://is.gd/create.php?format=simple&url=" cmd "\"")))

					   ((equal? (substring message 0 4) "add ");IP address lookup
						(send-message-pipe from (string-append "lynx -source -assume_charset=UTF8 \"http://ip2loc.appspot.com/q/?ip=" cmd "&alt=plain\"")))

					   ((equal? (substring message 0 4) "wea ");Weather forcast
						(display (string-append "lynx -dump -assume_charset=GBK -force_html http://api.liqwei.com/weather/?city=" cmd)))

					   ((equal? (substring message 0 4) "ctl ");Remote control
						(send-message-pipe from cmd))


					   (else (ft-send-message from "Unknown command, type help for usage"))
					   )))))
