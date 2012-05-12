from sdxf import *

d=Drawing()
d.views.append(View('Normal'))
d.views.append(ViewByWindow('Window',leftBottom=(1,0),rightTop=(2,1)))
print d
