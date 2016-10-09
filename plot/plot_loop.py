#!/usr/bin/python
import sys
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as pp
import numpy as np
from common import *

max_iters = 15

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Usage: " + sys.argv[0] + " path_to_data_dir"
    else:
        data = dict()
        scm_list = ['ECMP', 'RW']
        for scm in scm_list:
            file_name = file_names[scm]
            data[scm] = parse_data(data_dir + sys.argv[1] + "/" + file_name)
        for metric in file_columns.keys():
            plot_cdf (data, data_dir + sys.argv[1] + "/", metric, max_iters)

