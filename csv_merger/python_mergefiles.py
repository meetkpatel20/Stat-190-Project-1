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

#-----------------------------------------------------------------------------------------------------------#
#New tweaked code for merging



def is_non_zero_file(fpath):  
    return os.path.isfile(fpath) and os.path.getsize(fpath) > 0

import pandas as pd
import glob
import os
import numpy as np

directoryPath = '/content/drive/MyDrive/STAT 190 data/Gearbox Oil Temperature/' # use your path

datalist = []

files = glob.glob(directoryPath+'*.csv')


for file_name in glob.glob(directoryPath+'*.csv'):
  if is_non_zero_file(file_name):
      temp_df = pd.read_csv(file_name)
      df = pd.DataFrame(temp_df)
      df.columns =['Turbine', 'Error Time', 'Error Date', 'Temp', 'Error Type']
      datalist.append(df)

frame = pd.concat(datalist, axis=0, ignore_index=True)
frame.to_csv('/content/drive/MyDrive/STAT 190 data/All_oil_temp_merged.csv', index=False, header=True)

#print(frame)
