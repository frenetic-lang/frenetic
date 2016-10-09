#!/usr/bin/python
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as pp
import numpy as np

cdict = {'red':  ((0.0, 1.0, 1.0),
                   (0.25, 1.0, 1.0),
                   (0.5, 1.0, 1.0),
                   (0.75, 0.9, 0.9),
                   (1.0, 0.8, 0.8)),

         'green': ((0.0, 1.0, 1.0),
                   (0.25, 0.4, 0.4),
                   (0.5, 0.2, 0.2),
                   (0.75, 0.2, 0.2),
                   (1.0, 0.0, 0.0)),

         'blue':  ((0.0, 1.0, 1.0),
                   (0.25, 0.4, 0.4),
                   (0.5, 0.2, 0.2),
                   (0.75, 0.2, 0.2),
                   (1.0, 0.0, 0.0))
        }
pp.register_cmap(name='custom', data=cdict)

def setupMPPDefaults():
    pp.rcParams['font.size'] = 20
    pp.rcParams['mathtext.default'] = 'regular'
    pp.rcParams['ytick.labelsize'] = 24
    pp.rcParams['xtick.labelsize'] = 20
    pp.rcParams['legend.fontsize'] = 20
    pp.rcParams['lines.markersize'] = 12
    pp.rcParams['axes.titlesize'] = 24
    pp.rcParams['axes.labelsize'] = 24
    pp.rcParams['image.cmap'] = 'custom'


setupMPPDefaults()


column_labels = [str(i+1) for i in range(12)]
row_labels = [str(i+1) for i in range(12)]

dmatrix = [[0 for x in range(12)] for y in range(12)]
with open("abilene.txt") as f:
    line = f.readlines()[20]
    dems = [float(x) * 8/100/100000 for x in line.split()]
    for src in range(1,13):
        for dst in range(1,13):
            if src != dst:
                dmatrix[src-1][dst-1] = dems[(src-1)*12 + (dst-1)]
    data = np.array(dmatrix)

fig = pp.figure(figsize=(7,5))
ax = fig.add_subplot(111)
#fig, ax = pp.subplots()
heatmap = ax.pcolor(data, cmap='Reds')

ax.set_yticks(np.arange(data.shape[0])+0.5, minor=False)
ax.set_xticks(np.arange(data.shape[1])+0.5, minor=False)

ax.set_xticklabels(row_labels, minor=False)
ax.set_yticklabels(column_labels, minor=False)
ax.set_xlabel("source")
ax.set_ylabel("destination")
clb = fig.colorbar(heatmap, ax=ax)
clb.set_label('Mbps')
pp.savefig('heatmap.pdf', bbox_inches='tight')
