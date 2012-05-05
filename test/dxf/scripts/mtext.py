from sdxf import *

d=Drawing()
d.append(Mtext('Click on Ads' + newline + 'multiple lines with mtext',point=(1,1,1),color=5,rotation=90))
print d
