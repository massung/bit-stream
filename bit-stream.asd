(defpackage :bit-stream-asd
  (:use :cl :asdf))

(in-package :bit-stream-asd)

(defsystem :bit-stream
  :name "bit-stream"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Binary Bit Streams for LispWorks."
  :serial t
  :components ((:file "bit-stream")
               (:file "input-bit-stream")
               (:file "output-bit-stream")))
