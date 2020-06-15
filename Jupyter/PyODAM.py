# %load http://pmb-bordeaux.fr/scripts/PyODAM.py
import requests
import pandas as pd

def getDataFromODAM(dataset, subset='', query=''):
    # See http://pmb-bordeaux.fr/odamsw/
    headers = {'authorization': "Basic API Key Ommitted", 'accept': "text/csv"}
    urlcomp = 'http://pmb-bordeaux.fr/getdata/tsv/'+dataset
    if subset:
        urlcomp = urlcomp+'/('+subset+')'
    if query:
        urlcomp = urlcomp+'/'+query

    ## API Call to retrieve report
    rcomp = requests.get(urlcomp, headers=headers)

    ## API Results
    data = rcomp.text

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

def intersection(lst1, lst2): 
    lst3 = [value for value in lst1 if value in lst2] 
    return lst3 

def getVarNum(dataframe):
    varnum=[]
    for l in dataframe.columns:
        try:
            dataframe[l] = pd.to_numeric(dataframe[l])
            varnum.append(l)
        except:
            pass
    return varnum

def getSubsetFromODAM(dataset, subset='', query=''):
    df1 = getDataFromODAM(dataset, subset, query)
    df2 = getDataFromODAM(dataset, subset, 'identifier')
    df3 = getDataFromODAM(dataset, subset, 'factor')
    df4 = getDataFromODAM(dataset, subset, 'quantitative')
    df5 = getDataFromODAM(dataset, subset, 'qualitative')

    # Keep only columns that have been converted to numeric (thus removing columns with NA)
    S = subset.split(',')
    numvars = []
    for s in S:
        numvars = numvars + intersection(df4[df4.Subset==s]['Attribute'], getVarNum(df1))

    list1, list2 = ['data', 'identifier', 'factor', 'quantitative', 'qualitative', 'numvars' ], \
                   [df1, df2, df3, df4, df5, numvars ]
    d = dict( zip( list1, list2 ))
    return(d)

def convertDateToStr(DataNum):
    dateStr = [ ( pd.to_datetime('1899-12-30') + pd.to_timedelta(x,'D') ).strftime("%m/%d/%Y") for x in DataNum ]
    return(dateStr)

def convertTimeToStr(TimeNum):
    timeStr = [ ( pd.to_datetime('1899-12-30') + pd.to_timedelta(x,'D') ).strftime("%H:%M") for x in TimeNum ]
    return(timeStr)

