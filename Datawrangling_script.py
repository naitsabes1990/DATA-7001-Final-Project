###Data Wrangling for Track Data Set###
import pandas as pd
import numpy as np

#Track Data Set (TDS)
TDS = pd.read_csv("C:\\Users\\sorel\\Desktop\\MSc Data Science UQ\\Introduction to Data Science\\Semester Project\\Group Project\\Spotify Non-Academia\\ismpd\\Combined Files\\combined-csv-files.csv", header = 0, low_memory= False)
TDS.head()
#Checking structure
TDS.shape #Get dimensions

#Drop useless cols 
useless_cols = ['analysis_url', 'track_href', 'type', 'uri', 'Unnamed: 0']
for column in useless_cols:
    TDS.drop(column, axis=1, inplace=True)
#Drop Header Rows
TDS = TDS[TDS.valence!='valence']
#Setting Index
TDS.set_index('id')

#Converting Data Types
columns = list(TDS.columns.values) #get new columns

for column in columns:
    if column!='id':
        TDS[str(column)]=TDS[str(column)].astype(float)
    else:
        pass
#Drop NaN rows
TDS = TDS.dropna()

    





    





    
        
        

