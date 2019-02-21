import matplotlib.pyplot as pp

def setupMPPDefaults():
    # pp.rcParams['font.size'] = 22
    pp.rcParams['mathtext.default'] = 'regular'
    # pp.rcParams['ytick.labelsize'] = 22
    # pp.rcParams['xtick.labelsize'] = 22
    # pp.rcParams['legend.fontsize'] = 22
    # pp.rcParams['lines.markersize'] = 12
    # pp.rcParams['lines.linewidth'] = 3
    # pp.rcParams['axes.titlesize'] = 22
    # pp.rcParams['axes.labelsize'] = 22
    pp.rcParams['axes.edgecolor'] = 'grey'
    pp.rcParams['axes.linewidth'] = 3.0
    pp.rcParams['axes.grid'] = True
    pp.rcParams['grid.alpha'] = 0.4
    pp.rcParams['grid.color'] = 'grey'
    pp.rcParams['grid.linestyle'] = '--'
    pp.rcParams['legend.frameon'] = True
    pp.rcParams['legend.framealpha'] = 0.8
    pp.rcParams['legend.numpoints'] = 1
    pp.rcParams['legend.scatterpoints'] = 1
    #  pp.rcParams['text.usetex'] = True
    pp.rcParams['pdf.fonttype'] = 42
    pp.rcParams['ps.fonttype'] = 42


