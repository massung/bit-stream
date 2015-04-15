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

(defmethod make-output-bit-stream ()
  "Create a new, LZW, output bit stream that bits can be written to."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (make-instance 'output-bit-stream :bytes bytes)))

(defmethod stream-write-bits ((stream output-bit-stream) n size)
  "Write bits (size n) to the output bit stream."
  (assert (open-stream-p stream))
  (with-slots (bytes position bits buffer)
      stream
    (setf buffer (dpb n (byte size bits) buffer))
    (incf bits size)
    (do ()
        ((< bits 8) n)
      (vector-push-extend (ldb (byte 8 0) buffer) bytes)
      (setf buffer (ash buffer (- 8)))
      (decf bits 8)
      (incf position))))

(defmethod stream-write-byte ((stream output-bit-stream) byte)
  "Write a full byte (8 bits) to the output bit stream."
  (stream-write-bits stream byte 8))

(defmethod stream-flush-buffer ((stream output-bit-stream))
  "Force the left over bits to write out."
  (with-slots (bytes position bits buffer)
      stream
    (when (plusp bits)
      (vector-push-extend buffer bytes)
      (setf buffer 0)
      (setf bits 0)
      (incf position))))

(defmethod stream-force-output ((stream output-bit-stream))
  "Force the left over bits in the buffer to write."
  (stream-flush-buffer stream))

(defmethod close :after ((stream output-bit-stream) &key abort)
  "Closing an LZW output stream will force output and return the bytes."
  (unless abort
    (stream-flush-buffer stream))
  (bit-stream-bytes stream))
