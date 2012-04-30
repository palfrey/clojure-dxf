from sdxf import *

d=Drawing()
d.append(Rectangle(point=(2,2,2),width=4,height=3,color=6,solid=Solid(color=2)))
print d
