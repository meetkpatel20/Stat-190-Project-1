import pandas as pd
import glob

path = r'/Users/gonzalovaldenebro/Library/CloudStorage/OneDrive-DrakeUniversity/STAT 190/Project 1/Data/Fault_Codes_Time_Series' # use your path
all_files = glob.glob(path + "/*.csv")

list = []

for filename in all_files:
    df = pd.read_csv(filename, index_col=None, header=0)
    list.append(df)

frame = pd.concat(list, axis=0, ignore_index=True)
frame.to_csv('merged_csv.csv', index=False, header=False)

print(frame)
