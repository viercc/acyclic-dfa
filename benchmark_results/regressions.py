import pandas as pd
import numpy as np

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

baseline_time = read_csv('baseline.csv').Time
after_time = read_csv('after.csv').Time

def normalize(df):
    t_base=df['URI', 'Trie', :, 'reverse'][0]
    ans = df[:, 'ADFA'] / t_base
    return ans

baseline_control_time = baseline_time['URI', 'Trie', :, 'reverse'][0]
after_control_time = after_time['URI', 'Trie', :, 'reverse'][0]
control_ratio = after_control_time / baseline_control_time

time_ratio = after_time[:, 'ADFA'] / baseline_time[:, 'ADFA'] / control_ratio

print("Regressions:")
print(time_ratio[time_ratio > 1.1])
print("Improvements:")
print(time_ratio[time_ratio < 0.9])
