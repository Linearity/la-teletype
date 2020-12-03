# la-teletype

Text in games is often animated as if it were printed on a
teletype/teleprinter.  We can represent the essence of this with a signal
function whose output is a time-varying string.

This package provides some basic modes for such a signal function.  Generally
it uses the input signal to determine the speed of typing and when to scroll
the lines of text to make room for a new line.  The output is generally a
list of lines and a vector giving the offset at which to draw the lines.

The most granular modes are exposed, as are more sophisticated combinations.
These include a mode for filling a certain number of lines and a mode
for scrolling through an entire paragraph.