#!/usr/bin/env python3
import json
from sys import argv
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt

file = argv[1]
raw_data = Path(file).read_text()
json = json.loads(raw_data)

plt.plot(json["hop_count_cdf"])
plt.show()
