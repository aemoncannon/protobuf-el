
(defun pbel-write-varint (value)
  (message "writing varint %s" value)
  (let ((bits (logand value 127)))
    (setq value (lsh value -7))
    (while (not (zerop value))
      (insert (logior 128 bits))
      (pbel-next)
      (setq bits (logand value 127))
      (setq value (lsh value -7)))
    (insert bits)
    (pbel-next)
    ))

(defun pbel-write-string (value)
  (pbel-write-varint (string-bytes value))
  (insert value)
  (setq i (+ i (string-bytes value))))

(defun pbel-write-bool (value)
  (if value (insert 1) (insert 0))
  (pbel-next))

(defmacro pbel-skip (wire-type)
  "Skip over unrecognized field.
 TODO(aemon) Can optimize these quite a bit."
  `(cond
    ((= wire-type 0) ;; VARINT
     (pbel-read-varint))
    ((= wire-type 2) ;; LENGTH DELIMITED
     (pbel-read-string))
    (t (error (format "Can't skip unrecognized wire type: %s"
		      wire-type)))))

(defmacro pbel-next ()
  '(setq i (+ i 1)))

(defmacro pbel-read-varint ()
  '(let ((result
	  (let ((b1 (char-after i))
		(b2 0)
		(b3 0))
	    (if (zerop (logand 128 b1))
		b1
	      (progn
		(pbel-next)
		(setq b2 (char-after i))
		(if (zerop (logand 128 b2))
		    (logior (lsh (logand 127 b2) 7)
			    (logand 127 b1))
		  (progn
		    (pbel-next)
		    (setq b3 (char-after i))
		    (if (zerop (logand 128 b3))
			(logior (lsh (logand 127 b3) 14)
				(lsh (logand 127 b2) 7)
				(logand 127 b1))
		      (progn
			(error "fuck! four byte varint!"))
		      ))))))))
     (pbel-next)
     (message "reading varint %s" result)
     result
     ))

(defmacro pbel-read-bool ()
  '(> (pbel-read-varint) 0))

(defmacro pbel-read-string ()
  `(let ((len (pbel-read-varint)))
     (prog1 (decode-coding-region i (+ i len) 'utf-8 t)
       (setq i (+ i len)))))

(defmacro pbel-wire-type (varint-form)
  `(logand 7 ,varint-form))

(defmacro pbel-field-number (varint-form)
  `(lsh ,varint-form -3))


(defun pbel-test()
  (with-temp-buffer
    (insert-file-contents-literally "out")
    (set-buffer-multibyte nil)
    (let ((i 1)
	  (msg))
      (message "%s" (buffer-string))
      (setq msg (pbel-read-ensimemessage))
      (with-temp-buffer
	(let ((i 0))
	  (set-buffer-multibyte nil)
	  (pbel-write-ensimemessage msg)
	  (message "%s" (buffer-string))
	  )))))


