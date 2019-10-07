## You have to load http://pmb-bordeaux.fr/scripts/PyODAM.py before

# Get the subset list of a dataset
dataset = 'frim1'
getDataFromODAM(dataset)

# Get all values of a merged data subsets ( both activome & qNMR_metabofor) the specific 'sample' entry equal to 365
subset = 'activome,qNMR_metabo'
df = getDataFromODAM(dataset, subset,'sample/365?limit=10')

# Convert both data and time in MS Excel format into String
df.HarvestDate = convertDateToStr(df.HarvestDate)
df.HarvestHour = convertTimeToStr(df.HarvestHour)
print("Output1:\n--------------\n", df,"\n\n")

# Get the variable list within the 'factor' category of a merged data subset
df2 = getDataFromODAM(dataset, subset, 'factor')
print("Output2:\n--------------\n", df2,"\n\n")

# Get the variable list within the 'identifier' category of a merged data subset
df3 = getDataFromODAM(dataset, subset, 'identifier')
print("Output3:\n--------------\n", df3,"\n\n")

# Get the variable list within the 'quantitative' category of a merged data subset
df4 = getDataFromODAM(dataset, subset, 'quantitative')
print("Output4:\n--------------\n", df4.loc[ 0:10, ],"\n\n")

# Select the variables from the merged data belongings to the 'activome' data subset
print("Output5:\n--------------\n", df[df4[df4.Subset=='activome']['Attribute']],"\n\n")

# Select the variables from the merged data belongings to the 'qNMR_metabo' data subset
print("Output6:\n--------------\n", df[df4[df4.Subset=='qNMR_metabo']['Attribute']],"\n\n")

# Convert a sub-data set to numpy format
numpy_matrix = df[df4[df4.Subset=='activome']['Attribute']].to_numpy()
print("Output7:\n--------------\n", numpy_matrix,"\n\n")

