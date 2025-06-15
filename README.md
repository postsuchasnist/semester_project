# semester_project

Here are implementations of three algorithms from geometry: finding a convex hull, determining whether lines intersect and finding the closest pair of points. Each runs in O(n log n).
There are 6 .hs files, three of them with the corresponding names to the algorithms, Util.hs file, where common functions are stored. generator.hs file, which generates random points. geo.hs file, where the main function is. There are also 3 python files, each with corresponding name to the algorithm, and each displays a result.

# how it works

I recommend running ghc -package containers geo.hs to compile the file and then ./geo.hs convex 5/intersect 3/closest 10 for example. The thing is that I use Data.Set module, which by some reasons I could not properly install, so runghc did not work for me. The script makes an output, which should be stored in a file nd then python scripts read this file and show the result graphically, using matplotlib. Point have float-valued coordinates, so randomizer makes them a bit ugly due to the fact, that they have several digits after a decimal comma. Also, geo.hs writes to the file with utf-16le encoding, which is my case, so the python programs also use utf-16le encoding. If it is standard utf-8, then nothing would be plotted, as it could not be able to read it properly. 
