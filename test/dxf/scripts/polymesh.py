from sdxf import *

d=Drawing()
d.append(PolyLine(
	points=[
		[(1,1,1),(2,1,1),(2,2,1),(1,2,1)],
		[(1,1,1),(2,1,1),(2,2,1),(1,2,1)]
	],
	flag=POLYFACE_MESH,
	color=1
))
print d
