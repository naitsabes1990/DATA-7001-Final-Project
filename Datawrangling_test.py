###Data Wrangling for Track Data Set###
import pandas as pd
import numpy as np

#Track Data Set (TDS)
TDS = pd.read_csv("C:\\Users\\sorel\\Desktop\\MSc Data Science UQ\\Introduction to Data Science\\Semester Project\\Group Project\\Spotify Non-Academia\\ismpd\\Combined Files\\combined-csv-files.csv", header = 0, low_memory= False)
TDS.head()
#Checking structure
TDS.shape #Get dimensions

#Drop Header Rows
TDS_1 = TDS[TDS.valence=='valence']
TDS = TDS[TDS.valence!='valence']
