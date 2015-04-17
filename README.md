# Bit Streams for LispWorks

The `bit-stream` package is a simple implementation of reading and writing bits (and bytes) to and from vectors as if they were input and output streams for [LispWorks](http://www.lispworks.com/).

## Quickstart

Let's start by creating a byte vector from a stream of bits.

	CL-USER > (setq os (make-output-bit-stream))
	#<BIT-STREAM::OUTPUT-BIT-STREAM 200E350F>

Next, let's write a series of 3-bit values to it.

	CL-USER > (loop for i in '(4 1 6 6 2 2 5 4)
	                do (stream-write-bits os i 3)
	                finally (close os))
	T

Let's see what bytes were written...

	CL-USER > (get-output-bit-stream-bytes os)
	#(140 45 149)

Now, let's convert that vector into an `input-bit-stream` and read the 3-bit values back out of it.

	CL-USER > (setq is (make-input-bit-stream *))
	#<BIT-STREAM::INPUT-BIT-STREAM>

	CL-USER > (loop for i below 8 collect (stream-read-bits is 3))
	(4 1 6 6 2 2 5 4)

## Creating Input Bit Streams

The `make-input-bit-stream` function is a generic method and can take a bunch of different sources:

* A list of bytes.
* An array of bytes.
* A string of ASCII characters.
* An input stream of bytes.
* A pathname to a file that will be slurped in.

In addition to handling multiple sources, it also takes an optional *start* and *end* parameters.

	(make-input-bit-stream source &key start end)

*NOTE: When creating an input-bit-stream from another input stream, the start defaults to the current file-position of the source stream.*

## How Bits are Packed

The most important thing to remember is the order that bits are written and read back in the bit streams. By default, they follow the same packing order as [LZW (LSB first) compression](https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Welch#Packing_order).

For example, if you had the following bytes in a file:

	(#x8C #x2D #x99)

That would be equivalent to the following in binary:

	10001100 | 00101101 | 10011001

When read in, each byte is slurped and the lowest-significant bits are read first, until the next byte is slurped in. If the above `bit-input-stream` was read in 3-bit chunks, the following values would be returned:

	(4 1 6 6 2 2 6 4)

Notice how the lowest 3 bits of 8C are 100 (4). The next lowest 3 bits are 001 (1). The next lowest 3 bits requires that a new byte (2D) be slurped and then becomes 110 (6), etc.

The `output-bit-stream` ensures that bits are written in the same manner. If the above list of values were written to a stream 3 bits each, the output vector would be `#(#x8C #x2D #x99)`.

### Specifying the Packing Order

When calling and of the methods that create an input or output bit stream, you can optionally pass a `:pack-order` of `:msb` in order to make the packing order most-significant-bit first.

	CL-USER > (setq is (make-input-bit-stream #(140 45 149) :pack-order :msb))
	#<BIT-STREAM::INPUT-BIT-STREAM 200959CB>

	CL-USER > (loop for i below 8 collect (stream-read-bits is 3))
	(1 6 0 2 3 3 2 5)

Let's just confirm that's correct...

	10001100 | 00101101 | 10011001

The first 3 bits should be 001 (1), followed by 110 (6), followed by 000 (0), 010 (2), ... Looks good!

## Helper Macros

There are two helper macros for working with input and output bit-streams:

	(with-input-bit-stream (stream-var source &key pack-order) &body body)
	(with-output-bit-stream (stream-var &key pack-order) &body body)

These will create the stream, execute the body, and ensure that the stream is closed properly. The return value for the `output-bit-stream` will be the byte vector written.
