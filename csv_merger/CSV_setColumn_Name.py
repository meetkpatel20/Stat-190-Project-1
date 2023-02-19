import pandas as pd
import glob

path = r'/Users/gonzalovaldenebro/Library/CloudStorage/OneDrive-DrakeUniversity/STAT 190/Project 1/Data/Non_MergedData/Windspeed' # use your path
all_files = glob.glob(path + "/*.csv")

# define the column names
column_names = ['TurbineName', 'TimeStamp', 'Date', 'MilesPerHour','MetricName']

# loop over the file names
for file_name in all_files:
    # read the CSV file into a data frame
    df = pd.read_csv(file_name, header=None)
      # assign the column names to the data frame
    df.columns = column_names
        # write the data frame back out to a CSV file
    df.to_csv(file_name, index=False)