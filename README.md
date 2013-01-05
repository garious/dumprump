# Normalizing GCC tools with objrump and elfrump

objdumprump, the tail end of objdump
--------------------------------

Usage:

     $ objdump -d my.o | objdumprump

And likewise readelf:

     $ readelf -s my.o | readelfrump

The purpose of 'objdumprump' and 'readelfrump' is to normalize the output
for 'objdump' and 'readelf' such that the output of a GCC-compiled
object can be compared to an LLVM-compiled object.

