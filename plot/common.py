#!/usr/bin/python
import sys
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as pp
import numpy as np

def setupMPPDefaults():
    pp.rcParams['font.size'] = 20
    pp.rcParams['mathtext.default'] = 'regular'
    pp.rcParams['ytick.labelsize'] = 24
    pp.rcParams['xtick.labelsize'] = 20
    pp.rcParams['legend.fontsize'] = 20
    pp.rcParams['lines.markersize'] = 16
    pp.rcParams['axes.titlesize'] = 24
    pp.rcParams['axes.labelsize'] = 24

algs = ['ECMP', 'Raecke', 'KSP', 'Multi']

def gen_label (scm):
    if scm == "Raecke":
        return r"""$R\"acke$"""
    else:
        return scm

def getLineMarkersDict():
   return {
           'ECMP'   : 'o',
           'KSP'    : '^',
           'Multi'  : '.',
           'Raecke' : 'x',
           'RW'     : 'v',
           'SPF'    : 'd',
           }

def getLineMEWDict():
   return {
           'ECMP'   : 4,
           'KSP'    : 4,
           'Multi'  : 4,
           'Raecke' : 6,
           'RW'     : 4,
           'SPF'    : 4,
           }

def getLineColorsDict():
    return {
           'ECMP'   : 'coral',
           'KSP'    : 'blue',
           'Multi'  : 'magenta',
           'Raecke' : 'green',
           'RW'     : 'black',
           'SPF'    : 'darkcyan',
           }

def getLineFormatsDict():
    return {
           'ECMP'   : '--',
           'KSP'    : '--',
           'Multi'  : '--',
           'Raecke' : '--',
           'RW'     : '--',
           'SPF'    : '--',
           }

file_columns = {
        "Throughput" : 0,
        "Max Congestion" : 1,
        "Mean Latency" : 2,
        }

data_dir = './'

file_names = {
        'ECMP':'distributedECMP.dat',
        'KSP':'centralizedKSP.dat',
        'Multi':'distributedMulti.dat',
        'Raecke':'centralizedRaecke.dat',
        'RW':'distributedRW.dat',
        'SPF':'distributedSPF.dat',
        }

def parse_data (file_name):
    data = dict()
    with open(file_name) as f:
        for line in f.readlines():
            if line[0] == "#":
                continue
            else:
                tokens = line.split()
                if len(tokens) <= 1:
                    continue
                iternum = int(tokens[0])
                stats = [float(v) for v in tokens[1:]]
                data[iternum] = stats
    return data

def plot_cdf (scm_data, plot_dir, metric, max_iters):
    setupMPPDefaults()
    fmts = getLineFormatsDict()
    mrkrs = getLineMarkersDict()
    mews = getLineMEWDict()
    colors = getLineColorsDict()
    fig = pp.figure(figsize=(7,5))
    ax = fig.add_subplot(111)

    column = file_columns.get(metric)
    scm_metric_data = dict()
    for scm, iter_stats in scm_data.iteritems():
        scm_metric_data[scm] = dict()
        for i,stats in iter_stats.iteritems():
           scm_metric_data[scm][i] = stats[column]
    for scm, metric_list in sorted(scm_metric_data.iteritems()):
        xs =  sorted(metric_list.keys())
        ys = [metric_list[i] for i in xs]
        ax.plot((xs[-1], xs[-1]), (ys[-1], ys[-1]), linestyle=':',
                color=colors[scm])
        ax.errorbar(xs, ys,
                label=gen_label(scm),
                marker=mrkrs[scm],
                markevery=2,
                markerfacecolor=colors[scm],
                linewidth=4,
                linestyle=fmts[scm],
                color=colors[scm],
                mew=mews[scm],
                markeredgecolor=colors[scm])
    ax.set_xlabel("Iterations")
    pp.xlim(1,max_iters)
    ymin, ymax = pp.ylim()
    if ymax >= 1.1 and ymax < 10:
        ymax = 6
    elif ymax >= 0.78 and ymax < 1.1:
        ymax = 1.05
    elif ymax >= 0.73 and ymax < 0.78:
        ymax = 0.8
    else:
        ymax = 0.58
    pp.ylim(ymin, ymax)
    ax.set_ylabel(metric)
    ax.legend(loc=4, borderaxespad=0., fancybox=True)
    #ax.spines['right'].set_visible(False)
    #ax.spines['top'].set_visible(False)
    #ax.yaxis.set_ticks_position('left')
    #ax.xaxis.set_ticks_position('bottom')
    pp.tight_layout()
    pp.savefig(plot_dir + metric + "-line.pdf")


def plot_bar (scm_list, scm_data, plot_dir, metric, n):
    setupMPPDefaults()
    colors = getLineColorsDict()
    fig = pp.figure(figsize=(6,5))
    ax = fig.add_subplot(111)
    bar_width = 0.5
    column = file_columns.get(metric)
    n_groups = len(scm_data)
    metric_val = [scm_data[scm][n][column] for scm in scm_list]
    index = np.arange(n_groups)
    ax.bar(index, metric_val, bar_width, color=[colors[x] for x in scm_list], align='center')
    ax.set_ylabel(metric)
    pp.xticks(index, [gen_label(x) for x in scm_list])
    pp.tight_layout()
    pp.savefig(plot_dir + metric + "-bar.pdf")

