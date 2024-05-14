import numpy as np
import matplotlib.pyplot as plt
## AK135_DPRK_locations_xyC.txt  newplotres.py  REGRELLOC_DPRK_locations_xyC.txt


# xlo     = -2.0
# xhi     =  3.0
# ylo     = -2.0
# yhi     =  3.0

resfile = 'AK.BRLK_func.txt'
with open( resfile ) as f:
    lines = f.readlines()
    xvallist = []
    yvallist = []
    for line in lines:
        xvallist.append( float( line.split()[ 0] )  )
        yvallist.append( float( line.split()[ 1] )  )

xvalarr1 = np.asarray( xvallist )
yvalarr1 = np.asarray( yvallist )


fig = plt.figure( figsize=(10,10)  )
ax1 = fig.add_subplot(111)
ax1.set_xlabel('x')
ax1.set_ylabel('y')
#plt.xlim( xlo, xhi )
#plt.ylim( ylo, yhi )
plt.scatter(xvalarr1,yvalarr1,c='b')
#plt.scatter(xvalarr2,yvalarr2,c='r')
# plt.subplots_adjust(bottom=0.35)
plt.show( )

