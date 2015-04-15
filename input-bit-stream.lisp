;;;; Bit Streams for LispWorks
;;;;
;;;; Copyright (c) 2015 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License. You may obtain
;;;; a copy of the License at
;;;;
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(in-package :bit-stream)

(defmethod make-input-bit-stream ((source sequence) &key (start 0) end (pack-order :lsb))
  "Create an input bit stream from a list of bytes."
  (let ((bytes (coerce (subseq source start end) '(vector (unsigned-byte 8)))))
    (make-instance 'input-bit-stream :bytes bytes :pack-order pack-order)))

(defmethod make-input-bit-stream ((source vector) &key (start 0) end (pack-order :lsb))
  "Create an input bit stream from a vector of bytes."
  (let ((bytes (coerce (subseq source start end) '(vector (unsigned-byte 8)))))
    (make-instance 'input-bit-stream :bytes bytes :pack-order pack-order)))

(defmethod make-input-bit-stream ((source string) &key (start 0) end (pack-order :lsb))
  "Create an input bit stream from a string of bytes."
  (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code (subseq source start end))))
    (make-instance 'input-bit-stream :bytes bytes :pack-order pack-order)))

(defmethod make-input-bit-stream ((source stream) &key (start (file-position source)) end (pack-order :lsb))
  "Create an input bit stream from another input stream. Reads the entire source stream."
  (let* ((len (- (file-length source) (file-position source)))
         (seq (make-array len :element-type '(unsigned-byte 8) :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq source :start start :end end))
    (make-instance 'input-bit-stream :bytes seq :position start :pack-order pack-order)))

(defmethod make-input-bit-stream ((source pathname) &key (start 0) end (pack-order :lsb))
  "Create an input bit stream by reading a source file off disk."
  (with-open-file (stream source :element-type '(unsigned-byte 8))
    (make-input-bit-stream stream :start start :end end :pack-order pack-order)))

(defmethod stream-read-bits ((stream input-bit-stream) n)
  "Read bits from an input stream, which may read bytes."
  (assert (open-stream-p stream))
  (with-slots (bytes order position bits buffer)
      stream
    (declare (fixnum bits buffer))
    (do ()
        ((>= bits n)
         (prog1 (if (eq order :msb)
                    (ldb (byte n (- bits n)) buffer)
                  (ldb (byte n 0) buffer))
           (if (eq order :msb)
               (dpb 0 (byte n (- bits n)) buffer)
             (setf buffer (ash buffer (- n))))
           (decf bits n)))
      (if (eq order :msb)
          (setf buffer (dpb (aref bytes position) (byte 8 0) (ash buffer 8)))
        (setf buffer (dpb (aref bytes position) (byte 8 bits) buffer)))
      (incf bits 8)
      (incf position))))

(defmethod stream-read-byte ((stream input-bit-stream))
  "Read a single byte from an input bit stream."
  (stream-read-bits stream 8))

(defmethod stream-flush-buffer ((stream input-bit-stream))
  "Clear the left over bits forcing a new byte on the next read."
  (with-slots (bits buffer)
      stream
    (setf bits 0)
    (setf buffer 0)))

(defmethod stream-file-position ((stream input-bit-stream))
  "Return the current bit position in the stream."
  (with-slots (position bits)
      stream
    (- (* position 8) bits)))

(defmethod (setf stream-file-position) (position-spec (stream input-bit-stream))
  "Change the current position of the stream."
  (with-slots (bytes position bits buffer)
      stream
    (when (< position-spec (* (length bytes) 8))
      (prog1 t
        (multiple-value-bind (byte-pos bits-remaining)
            (truncate position-spec 8)
          (setf position byte-pos
                bits 0
                buffer 0)
          (when (plusp bits-remaining)
            (stream-read-bits stream bits-remaining)))))))
