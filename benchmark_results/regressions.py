import sys
import pandas as pd
import numpy as np

if len(sys.argv)<3:
    sys.exit("Needs at least 2 file to compare")
filenames = sys.argv[1:]

schema = [
  ('Workload', 'category'),
  ('Type', 'category'),
  ('Category', 'category'),
  ('Method', 'category'),
  ('Time', np.float64),
  ('TimeLB', np.float64),
  ('TimeUB', np.float64),
  ('TimeSD', np.float64),
  ('TimeSDLB', np.float64),
  ('TimeSDUB', np.float64)
]
names = [ p[0] for p in schema if p ]
dtype = dict(schema)
def read_csv(fname):
    with open(fname, 'r') as fd:
        _ = fd.readline() # discard the first line
        return pd.read_csv(fd, sep='[/,]', engine='python',
            header=None, names=names, dtype=dtype, index_col=[0,1,2,3])

all_timedata = []
for f in filenames:
    data = read_csv(f).Time
    control_time = data['URI', 'Trie', :, 'reverse'][0]
    ans = data[:, 'ADFA'] # / control_time
    all_timedata.append(ans)

all_time_ratio = {}
is_regression = pd.Series(False, all_timedata[0].index)
is_improvement = pd.Series(False, all_timedata[0].index)

for i in range(1,len(filenames)):
    time_ratio = all_timedata[i] / all_timedata[0]
    all_time_ratio[filenames[i]] = time_ratio
    is_regression |= time_ratio > 1.1
    is_improvement |= time_ratio < 0.9

all_time_ratio = pd.DataFrame(all_time_ratio)

print("Regressions:")
print(all_time_ratio[is_regression])
print("Improvements:")
print(all_time_ratio[is_improvement])

