## You have to load http://pmb-bordeaux.fr/scripts/PyODAM.py before

# Get the subset list of a dataset
dataset = 'frim1'
getDataFromODAM(dataset)

# Get all values of a merged data subsets ( both activome & qNMR_metabofor) the specific 'sample' entry equal to 365
subset = 'activome,qNMR_metabo'
df = getSubsetFromODAM(dataset, subset,'sample/365?limit=10')
data = df['data']

# Convert both data and time in MS Excel format into String
data.HarvestDate = convertDateToStr(data.HarvestDate)
data.HarvestHour = convertTimeToStr(data.HarvestHour)
print("Output1:\n--------------\n", data,"\n\n")

# Get the variable list within the 'factor' category of a merged data subset
print("Output2:\n--------------\n", df['factor'],"\n\n")

# Get the variable list within the 'identifier' category of a merged data subset
print("Output3:\n--------------\n", df['identifier'],"\n\n")

# Get the variable list within the 'quantitative' category of a merged data subset
quantitative = df['quantitative']
print("Output4:\n--------------\n", quantitative.loc[ 0:10, ],"\n\n")

# Select the variables from the merged data belongings to the 'activome' data subset
print("Output5:\n--------------\n", data[quantitative[quantitative.Subset=='activome']['Attribute']],"\n\n")

# Select the variables from the merged data belongings to the 'qNMR_metabo' data subset
print("Output6:\n--------------\n", data[quantitative[quantitative.Subset=='qNMR_metabo']['Attribute']],"\n\n")

# Convert a sub-data set to numpy format
numpy_matrix = data[quantitative[quantitative.Subset=='activome']['Attribute']].to_numpy()
print("Output7:\n--------------\n", numpy_matrix,"\n\n")

