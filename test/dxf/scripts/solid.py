from sdxf import *

d=Drawing()
d.append(Solid(points=[(4,4,0),(5,4,0),(7,8,0),(9,9,0)],color=3))
print d
