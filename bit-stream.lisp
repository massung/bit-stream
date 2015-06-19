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

(defpackage :bit-stream
  (:use :cl :lw :stream)
  (:export
   #:make-input-bit-stream
   #:make-output-bit-stream

   ;; reading and writing bits
   #:stream-read-bits
   #:stream-write-bits

   ;; return the current byte vector from an output stream
   #:get-output-bit-stream-bytes

   ;; macro to produce a vector of bytes
   #:with-input-bit-stream
   #:with-output-bit-stream))

(in-package :bit-stream)

(defclass bit-stream ()
  ((bytes    :reader bit-stream-bytes      :initarg :bytes)
   (order    :reader bit-stream-pack-order :initarg :pack-order :initform :lsb)
   (position :reader bit-stream-position   :initarg :position   :initform 0)
   (bits     :reader bit-stream-bits       :initarg :bits       :initform 0)
   (buffer   :reader bit-stream-buffer     :initarg :buffer     :initform 0))
  (:documentation "An LZW bit vector that can be read from and written to in bit packets."))

(defclass input-bit-stream (fundamental-binary-input-stream bit-stream) ())
(defclass output-bit-stream (fundamental-binary-output-stream bit-stream) ())

(defconstant +nibble-swap+ #(#b0000 #b1000 #b0100 #b1100 #b0010 #b1010 #b0110 #b1110
                             #b0001 #b1001 #b0101 #b1101 #b0011 #b1011 #b0111 #b1111)
  "Reversed nibbles in a byte for MSB packing order.")

(defun swap-byte (byte)
  "Invert the bits in a byte so that the most significant bits are now the lowest."
  (let ((lo-nib (elt +nibble-swap+ (ldb (byte 4 0) byte)))
        (hi-nib (elt +nibble-swap+ (ldb (byte 4 4) byte))))
    (dpb lo-nib (byte 4 4) hi-nib)))

(defmethod initialize-instance :after ((stream bit-stream) &key &allow-other-keys)
  "Assert that the packing order of the bits is either LSB or MSB."
  (assert (or (eq (bit-stream-pack-order stream) :lsb)
              (eq (bit-stream-pack-order stream) :msb))))

(defmethod stream-element-type ((stream bit-stream))
  "Required type of bit streams."
  '(unsigned-byte 8))

(defmethod file-length ((stream bit-stream))
  "Return the length of the bit stream in bits."
  (* (length (bit-stream-bytes stream)) 8))

(defmacro with-input-bit-stream ((stream-var source &key (pack-order :lsb)) &body body)
  "Create an input bit stream, popuplate it with the source, run a body of code."
  `(let ((,stream-var (make-input-bit-stream ,source :pack-order ,pack-order)))
     (unwind-protect
         (progn ,@body)
       (close ,stream-var))))

(defmacro with-output-bit-stream ((stream-var &key (pack-order :lsb)) &body body)
  "Create an output bit stream, write to it, and return the resulting bytes."
  `(let ((,stream-var (make-output-bit-stream :pack-order ,pack-order)))
     (unwind-protect
         (progn ,@body)
       (close ,stream-var))
     (bit-stream-bytes ,stream-var)))
