(in-package :cl-user)

(defpackage :wav-parser
  (:use :cl)
  (:export))

(in-package :wav-parser)

;;;; fmt chunk

(defclass fmt (r-iff:leaf)
  ((audio-format :initarg :audio-format
                 :type (unsigned-byte 16)
                 :accessor audio-format)
   (num-channels :initarg :num-channels
                 :type (unsigned-byte 16)
                 :accessor num-channels)
   (sample-rate :initarg :sample-rate
                :type (unsigned-byte 32)
                :accessor sample-rate)
   (bits-per-sample :initarg :bit-per-sample
                    :type (unsigned-byte 16)
                    :accessor bits-per-sample)
   (extra-params :initarg :extra-params
                 :type (vector (unsigned-byte 8))
                 :accessor extra-params)))

(defmethod initialize-instance ((o fmt) &key id stream size)
  (with-slots ((chunk-id r-iff:id) audio-format num-channels sample-rate
               byte-rate block-align bits-per-sample extra-params)
      o
    (setf chunk-id id
          audio-format (nibbles:read-ub16/le stream)
          num-channels (nibbles:read-ub16/le stream)
          sample-rate (nibbles:read-ub32/le stream))
    (nibbles:read-ub32/le stream) ; Discard byte-rate.
    (nibbles:read-ub16/le stream) ; Discard block-align.
    (setf bits-per-sample (nibbles:read-ub16/le stream))
    (when (< 16 size)
      (setf extra-params
              (r-iff:read-vector stream (nibbles:read-ub16/le stream))))
    o))

(defun byte-rate (fmt)
  (/ (* (sample-rate fmt) (num-channels fmt) (bits-per-sample fmt)) 8))

(defun block-align (fmt) (/ (* (num-channels fmt) (bits-per-sample fmt)) 8))

(defmethod r-iff:compute-length ((chunk fmt))
  (+ r-iff:+size-of-header+ 16
     (if (slot-boundp chunk 'extra-params)
         (let ((length (length (extra-params chunk))))
           (if (zerop length)
               0
               (+ 2 length)))
         0)))

(defmethod r-iff:write-chunk ((chunk fmt) stream)
  (with-slots ((data r-iff:data))
      chunk
    (nibbles:write-ub16/le (audio-format chunk) stream)
    (nibbles:write-ub16/le (num-channels chunk) stream)
    (nibbles:write-ub32/le (sample-rate chunk) stream)
    (nibbles:write-ub32/le (byte-rate chunk) stream)
    (nibbles:write-ub16/le (block-align chunk) stream)
    (nibbles:write-ub16/le (bits-per-sample chunk) stream)
    (when (slot-boundp chunk 'extra-params)
      (let ((extra-params-size (length (extra-params chunk))))
        (when (< 0 extra-params-size)
          (nibbles:write-ub16/le extra-params-size stream)
          (write-sequence (extra-params chunk) stream)))))
  chunk)

;;;; cue chunk

(defclass cue (r-iff:leaf) ())

(defclass cue-point ()
  ((id :initarg :id :type (unsigned-byte 32) :accessor id<-cue-point)
   (position :initarg :position
             :type (unsigned-byte 32)
             :accessor position<-cue-point)
   (data-chunk-id :initarg :data-chunk-id
                  :type r-iff:id
                  :accessor data-chunk-id)
   (chunk-start :initarg :chunk-start
                :type (unsigned-byte 32)
                :accessor chunk-start)
   (block-start :initarg :block-start
                :type (unsigned-byte 32)
                :accessor block-start)
   (sample-start :initarg :sample-start
                 :type (unsigned-byte 32)
                 :accessor sample-start)))

(defconstant +size-of-cue-point+ 24)

(defmethod r-iff:write-chunk ((chunk cue-point) stream)
  (nibbles:write-ub32/le (id<-cue-point chunk) stream)
  (nibbles:write-ub32/le (position<-cue-point chunk) stream)
  (write-sequence (babel:octets-to-string (data-chunk-id chunk)) stream)
  (nibbles:write-ub32/le (chunk-start chunk) stream)
  (nibbles:write-ub32/le (block-start chunk) stream)
  (nibbles:write-ub32/le (sample-start chunk) stream)
  chunk)

(defmethod initialize-instance ((chunk cue) &key id stream size)
  (declare (ignore size))
  (with-slots ((chunk-id r-iff:id) (chunk-data r-iff:data))
      chunk
    (setf chunk-id id
          chunk-data
            (loop :repeat (nibbles:read-ub32/le stream)
                  :collect (make-instance 'cue-point
                                          :id (nibbles:read-ub32/le stream)
                                          :position (nibbles:read-ub32/le
                                                      stream)
                                          :data-chunk-id (r-iff::read-id
                                                           stream)
                                          :chunk-start (nibbles:read-ub32/le
                                                         stream)
                                          :block-start (nibbles:read-ub32/le
                                                         stream)
                                          :sample-start (nibbles:read-ub32/le
                                                          stream)))))
  chunk)

(defmethod r-iff:compute-length ((chunk cue))
  (+ r-iff:+size-of-header+ 4
     (* +size-of-cue-point+ (length (r-iff:data<-chunk chunk)))))

(defmethod r-iff:write-chunk ((chunk cue) stream)
  (nibbles:write-ub32/le (length (r-iff:data<-chunk chunk)) stream)
  (dolist (cue-point (r-iff:data<-chunk chunk))
    (r-iff:write-chunk cue-point stream))
  chunk)

;;;; LABL
; https://sites.google.com/site/musicgapi/technical-documents/wav-file-format#labl

(defclass labl (r-iff:leaf)
  ((cue-point-id :initarg :cue-point-id
                 :type (unsigned-byte 32)
                 :accessor cue-point-id)
   (text :initarg :text :type string :accessor text)))

(defmethod print-object ((chunk labl) stream)
  (print-unreadable-object (chunk stream :type t) (prin1 (text chunk) stream)))

(defmethod initialize-instance ((chunk labl) &key id stream size)
  (with-slots ((chunk-id r-iff:id) cue-point-id text)
      chunk
    (setf chunk-id id
          cue-point-id (nibbles:read-ub32/le stream))
    (when (< 4 size)
      (setf text
              (let ((v
                     (make-array (list (- size 4))
                                 :element-type '(unsigned-byte 8))))
                (assert (= (- size 4) (read-sequence v stream)))
                (string-right-trim '(#\nul) (babel:octets-to-string v))))))
  (when (oddp (- size 4))
    (read-byte stream))
  chunk)

(defmethod r-iff:compute-length ((chunk labl))
  (+ r-iff:+size-of-header+ 4
     (if (slot-boundp chunk 'text)
         (1+ ; <--- Nul char length.
          (babel:string-size-in-octets (text chunk)))
         0)))

(defmethod r-iff:write-chunk ((chunk labl) stream)
  (nibbles:write-ub32/le (cue-point-id chunk) stream)
  (when (slot-boundp chunk 'text)
    (write-sequence (babel:string-to-octets (text chunk)) stream)
    (write-byte 0 stream))
  chunk)

;;;; PARSERS

(r-iff:defparser "WAVE" #'r-iff:node)

(r-iff:defparser "fmt " #'r-iff:leaf :default-class fmt)

(r-iff:defparser "cue " #'r-iff:leaf :default-class cue)

(r-iff:defparser "data" #'r-iff:leaf)

(r-iff:defparser "labl" #'r-iff:leaf :default-class labl)

;;;; IMPORT

(defun wav (pathname) (r-iff:riff pathname))
