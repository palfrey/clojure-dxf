from sdxf import *

d=Drawing()
d.append(Insert('test',point=(3,3,3),cols=5,colspacing=2))
print d
