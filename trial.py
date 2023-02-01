import pandas as pd
import glob

path = r'/Users/gonzalovaldenebro/Library/CloudStorage/OneDrive-DrakeUniversity/STAT 190/Project 1/Data/Fault_Codes_Time_Series/merged_csv.csv' 
all_files = glob.glob(path + "/*.csv")

column_names = ['Turbine', 'Time', 'Date', 'error_id', 'status_code', 'error_message', 'error_category'] 

list = []
for filename in all_files:
    df = pd.read_csv(filename, header=None, names=column_names)
    list.append(df)

frame = pd.concat(list, axis=0, ignore_index=True)
frame.to_csv('merged_csv.csv', index=False)



