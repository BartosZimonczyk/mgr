from pathlib import Path
import pandas as pd
import numpy as np

path = r'~/Repos/mgr/Simulations/MC_powers/n100_N10000_r5/Tables/'  # or unix / linux / mac path

# Get the files from the path provided in the OP
files = Path(path).glob('*.csv')  # .rglob to get subdirectories

dfs = list()
for f in files:
    data = pd.read_csv(f)
    # .stem is method for pathlib objects to get the filename w/o the extension
    data['file'] = f.stem
    dfs.append(data)

df = pd.concat(dfs, ignore_index=True)

