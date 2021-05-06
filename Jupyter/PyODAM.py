import requests
import pandas as pd
import warnings

class Odam:
   def __init__(self, repos, dataset, ssl_verify=None):
       self.repos = repos
       self.dataset = dataset
       if ssl_verify is None:
           self.ssl_verify = False
       else:
           self.ssl_verify = ssl_verify
       warnings.filterwarnings('ignore')

   def getDataFromODAM(self, subset='', query=''):
       headers = {'authorization': "Basic API Key Ommitted", 'accept': "text/csv"}
       urlapi = self.repos+'/getdata/tsv/'+self.dataset
       if subset:
           urlapi = urlapi+'/('+subset+')'
       if query:
           urlapi = urlapi+'/'+query

       ## API Call to retrieve report
       response = requests.get(urlapi, headers=headers, verify=self.ssl_verify)

       ## API Results
       data = response.text
   
       ## Parse data into a DataFrame
       ## see https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html
       labels = data.split('\n')[0].split('\t')
       df = pd.DataFrame([x.split('\t') for x in data.split('\n')], columns=labels)
       df.drop(df.index[0], inplace=True)
       df = df.mask(df.eq('None')).dropna().reset_index()
   
       ## Convert all variables (columns) to numeric when possible
       ## see http://queirozf.com/entries/pandas-dataframe-examples-column-operations
       for l in labels:
           try:
               df[l] = pd.to_numeric(df[l])
           except:
               pass

       # Return Data.frame
       return df

   def getSubsetFromODAM(self, subset='', query=''):
       df1 = self.getDataFromODAM(subset, query)
       df2 = self.getDataFromODAM(subset, 'identifier')
       df3 = self.getDataFromODAM(subset, 'factor')
       df4 = self.getDataFromODAM(subset, 'quantitative')
       df5 = self.getDataFromODAM(subset, 'qualitative')
   
       # Keep only columns that have been converted to numeric (thus removing columns with NA)
       S = subset.split(',')
       numvars = []
       for s in S:
           numvars = numvars + self.intersection(df4[df4.Subset==s]['Attribute'], self.getVarNum(df1))

       list1, list2 = ['data', 'identifier', 'factor', 'quantitative', 'qualitative', 'numvars' ], \
                      [df1, df2, df3, df4, df5, numvars ]
       d = dict( zip( list1, list2 ))
       return(d)

   @staticmethod
   def intersection(lst1, lst2): 
       lst3 = [value for value in lst1 if value in lst2] 
       return lst3 

   @staticmethod
   def getVarNum(dataframe):
       varnum=[]
       for l in dataframe.columns:
           try:
               dataframe[l] = pd.to_numeric(dataframe[l])
               varnum.append(l)
           except:
               pass
       return varnum

   @staticmethod
   def convertDateToStr(DataNum):
       dateStr = [ ( pd.to_datetime('1899-12-30') + pd.to_timedelta(x,'D') ).strftime("%m/%d/%Y") for x in DataNum ]
       return(dateStr)

   @staticmethod
   def convertTimeToStr(TimeNum):
       timeStr = [ ( pd.to_datetime('1899-12-30') + pd.to_timedelta(x,'D') ).strftime("%H:%M") for x in TimeNum ]
       return(timeStr)

