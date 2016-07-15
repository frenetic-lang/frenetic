import sys

input_file = sys.argv[1]
data = []

with open(input_file) as f:
  for line in f:
    if line.startswith("Running"): continue
    vals = line.split(',')
    name = vals[0]
    rules = int(vals[-2])
    time = float(vals[-1])
    data.append({ "name" : name, "rules" : rules, "time" : time })

data.sort(key=lambda x: x["time"], reverse=False)

print("SUMMARY")
fmt = "{:>7}: {:8.3f}"
for i, title in enumerate(["1/4", "median", "3/4", "maximum"]):
  idx = int(len(data)/4.0 * (i+1.0)) - 1
  time = data[idx]["time"]
  print(fmt.format(title, time))

# print(data)
