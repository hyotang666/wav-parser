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
              (let ((v
                     (make-array (list (nibbles:read-ub16/le stream))
                                 :element-type '(unsigned-byte 8))))
                (assert (= size (read-sequence v stream)))
                v)))
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

;;;; PARSERS

(r-iff:defparser "WAVE" #'r-iff:node)

(r-iff:defparser "fmt "
                 (lambda (&rest args)
                   (let ((r-iff:*leaf-class* 'fmt))
                     (apply #'r-iff:leaf args))))

(r-iff:defparser "data" #'r-iff:leaf)

;;;; IMPORT

(defun wav (pathname) (r-iff:riff pathname))
