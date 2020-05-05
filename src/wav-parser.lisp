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
              (string-right-trim '(#\Nul)
                                 (r-iff:read-string stream (- size 4))))))
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

;;;; BROADCAST AUDIO EXTENSION "bext"
; https://tech.ebu.ch/docs/tech/tech3285.pdf

(defclass bext (r-iff:leaf)
  ((description :initarg :description :type string :accessor description)
   (originator :initarg :originator :type string :accessor originator)
   (originator-reference :initarg :originator-reference
                         :type string
                         :accessor originator-reference)
   (origination-date :initarg :origination-date
                     :type string
                     :accessor origination-date)
   (origination-time :initarg :origination-time
                     :type string
                     :accessor origination-time)
   (time-reference-low :initarg :time-reference-low
                       :type (unsigned-byte 32)
                       :accessor time-reference-low)
   (time-reference-high :initarg :time-reference-high
                        :type (unsigned-byte 32)
                        :accessor time-reference-high)
   (version :initarg :version :type (unsigned-byte 16) :accessor version)
   (umids :initarg :umids :type (vector (unsigned-byte 8) 64) :accessor umids)
   (loudness-value :initarg :loudness-value
                   :type (signed-byte 16)
                   :accessor loudness-value)
   (loudness-range :initarg :loudness-range
                   :type (signed-byte 16)
                   :accessor loudness-range)
   (max-peak-true-level :initarg :max-peak-true-level
                        :type (signed-byte 16)
                        :accessor max-peak-true-level)
   (max-momentary-loudness :initarg :max-momentary-loudness
                           :type (signed-byte 16)
                           :accessor max-momentary-loudness)
   (max-short-term-loudness :initarg :max-short-term-loudness
                            :type (signed-byte 16)
                            :accessor max-short-term-loudness)
   (coding-history :initarg :coding-history
                   :type string
                   :accessor coding-history)))

(defmethod initialize-instance ((chunk bext) &key id stream size)
  (with-slots ((chunk-id r-iff:id) description originator originator-reference
               origination-date origination-time time-reference-low
               time-reference-high version umids loudness-value loudness-range
               max-peak-true-level max-momentary-loudness
               max-short-term-loudness coding-history)
      chunk
    (setf chunk-id id
          description
            (string-right-trim '(#\Nul) (r-iff:read-string stream 256))
          originator (string-right-trim '(#\Nul) (r-iff:read-string stream 32))
          originator-reference
            (string-right-trim '(#\Nul) (r-iff:read-string stream 32))
          origination-date (r-iff:read-string stream 10)
          origination-time (r-iff:read-string stream 8)
          time-reference-low (nibbles:read-ub32/le stream)
          time-reference-high (nibbles:read-ub32/le stream)
          version (nibbles:read-ub16/le stream)
          umids (r-iff:read-vector stream 64)
          loudness-value (nibbles:read-sb16/le stream)
          loudness-range (nibbles:read-sb16/le stream)
          max-peak-true-level (nibbles:read-sb16/le stream)
          max-momentary-loudness (nibbles:read-sb16/le stream)
          max-short-term-loudness (nibbles:read-sb16/le stream))
    ;; Discard reserved.
    (loop :repeat 180
          :do (read-byte stream))
    (setf coding-history
            (let ((string
                   (read-string stream
                                (- size 256 32 32 10 8 4 4 2 64 2 2 2 2 2
                                   180))))
              (string-right-trim '(#\Nul #\Newline #\Return) string))))
  chunk)

(defmethod r-iff:compute-length ((chunk bext))
  (+ 256 32 32 4 2 64 2 2 2 2 2 180
     (r-iff:ensure-even (babel:string-size-in-octets (coding-history chunk))) 2 ; <---
                                                                                ; CR/LF
     ))

(defmethod r-iff:write-chunk ((chunk bext) stream)
  (flet ((write-ranged-string (string size)
           (let ((octets (nibbles:make-octet-vector size))
                 (vector (babel:string-to-octets string)))
             (replace octets vector :end2 (min 255 (length vector)))
             (write-sequence octets stream))))
    (write-ranged-string (description chunk) 256)
    (write-ranged-string (originator chunk) 32)
    (write-ranged-string (originator-reference chunk) 32))
  (write-sequence (babel:string-to-octets (origination-date chunk)) stream)
  (write-sequence (babel:string-to-octets (origination-time chunk)) stream)
  (nibbles:write-ub32/le (time-reference-low chunk) stream)
  (nibbles:write-ub32/le (time-reference-high chunk) stream)
  (nibbles:write-ub16/le (version chunk) stream)
  (write-sequence (umids chunk) stream)
  (nibbles:write-sb16/le (loudness-value chunk) stream)
  (nibbles:write-sb16/le (loudness-range chunk) stream)
  (nibbles:write-sb16/le (max-peak-true-level chunk) stream)
  (nibbles:write-sb16/le (max-momentary-loudness chunk) stream)
  (nibbles:write-sb16/le (max-short-term-loudness chunk) stream)
  (dotimes (x 180) (write-byte 0 stream))
  (write-sequence (babel:string-to-octets (coding-history chunk)) stream)
  (write-byte (char-code #\Return) stream)
  (write-byte (char-code #\Newline) stream)
  (when (oddp (babel:string-size-in-octets (coding-history chunk)))
    (write-byte 0 stream))
  chunk)

;;;; PARSERS

(r-iff:defparser "WAVE" #'r-iff:node)

(r-iff:defparser "fmt " #'r-iff:leaf :default-class fmt)

(r-iff:defparser "cue " #'r-iff:leaf :default-class cue)

(r-iff:defparser "data" #'r-iff:leaf)

(r-iff:defparser "labl" #'r-iff:leaf :default-class labl)

(r-iff:defparser "bext" #'r-iff:leaf :default-class bext)

;;;; IMPORT

(defun wav (pathname) (r-iff:riff pathname))
