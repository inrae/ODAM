# %load http://pmb-bordeaux.fr/scripts/PyODAM.py

# See https://towardsdatascience.com/pca-using-python-scikit-learn-e653f8989e60
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

def PCA_compute(X, Y, n=2, scale=True):

    # Standardizing the features
    if scale:
        X = StandardScaler().fit_transform(X)

    # PCA Projection to 2D
    pca = PCA(svd_solver='full', n_components=n)
    principalComponents = pca.fit_transform(X)
    principalDf = pd.DataFrame(data = principalComponents
                             , columns = [x+y for x,y in zip(['PC']*n, [ str(x) for x in range(1, n+1) ])])
    d = dict( zip( ['Scores', 'Factor', 'EV'], [ principalDf, Y, pca.explained_variance_ratio_*100 ] ))
    return(d)

# To change, see with %matplotlib -l
%matplotlib inline

# See https://matplotlib.org/index.html
#     https://www.data-blogger.com/2017/11/15/python-matplotlib-pyplot-a-perfect-combination/
from matplotlib import pyplot as plt
import matplotlib as mpl
import numpy as np

def plotPCA(pca, pc1=1, pc2=2, factorlevels=None, colors=[ 'r', 'b', 'g', 'm', 'y', 'c' ]):
    if (factorlevels is None):
        factorlevels = []
        for f in pca['Factor']:
            if f not in factorlevels:
                factorlevels.append(f)
    factorlevels.sort()
    
    n = len(factorlevels)
    c1=np.array(mpl.colors.to_rgb('red'))
    c2=np.array(mpl.colors.to_rgb('green'))
    if (colors is None):
        colors = [ mpl.colors.to_hex((1-mix/(n-1))*c1 + mix/(n-1)*c2) for mix in range(0,n) ]

    # Visualize 2D Projection
    fig = plt.figure(figsize = (12,12))
    ax = fig.add_subplot(1,1,1) 
    ax.set_xlabel('Principal Component '+str(pc1)+' ('+str(round(pca['EV'][pc1-1],2))+')', fontsize = 15)
    ax.set_ylabel('Principal Component '+str(pc2)+' ('+str(round(pca['EV'][pc2-1],2))+')', fontsize = 15)
    ax.set_title('PCA', fontsize = 20)
    n = max([pc1,pc2])
    cols = np.asarray([x+y for x,y in zip(['PC']*n, [ str(x) for x in range(1, n+2) ])])
    for target, color in zip(factorlevels,colors):
        indicesToKeep = pca['Factor'] == target
        ax.scatter(pca['Scores'].loc[indicesToKeep, cols[pc1-1]]
                 , pca['Scores'].loc[indicesToKeep, cols[pc2-1]]
                 , c = color
                 , s = 50)
    ax.legend(factorlevels)
    ax.grid()


## API Call to retrieve data
dataset = 'frim1'
subset = 'qNMR_metabo'
d = getSubsetFromODAM(dataset, subset)

## Retrieve factors
d['factor']

# Matrix X
X = d['data'][d['numvars']]

# Factor vector
facname = d['factor'].Attribute[1]
Y = d['data'][facname]

# Factor levels
factorlevels = []
for f in Y:
    if f not in factorlevels:
        factorlevels.append(f)
factorlevels

# Compute PCA
res_pca = PCA_compute(X, Y, n=3, scale=True)

# Factor level selection
targets = ['FF.01', 'FF.02', 'FF.04', 'FR.02', 'FR.04']

# Plot PCA Scores
plotPCA(res_pca, 1, 3, targets)

# Explained Variance (%)
res_pca['EV']

