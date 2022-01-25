## CHANGES

### Data explorer v1.4 (Nov 2021-Jan 2022)

* Collection management
    * Adding a collection information section including the list of metadata in addition to the list of datasets

* Possibility to select several data subsets
    * Consideration in graphics (colors, legend)

* Extension of query-string parameters
    * In addition to the type of analysis and the data subset(s), it is possible to specify the factor(s) and the variable(s).

* Possible change of the theme colors
    * parameterization in global.ini

* Customization of the title "ODAM Dataexplorer"
    * by replacing it with a custum label with its corresponding URL 
    * parameterization in global.ini

* Addition / extension of biostatistical analyses
    * Mean comparison of factor levels
        * Added in the "Univariate" section 
        * Case where 'Factor for Grouping' is identical to "Factor for X Axis"
        * Possibility to select levels when the number of factor levels > 3 (Select First Factor Levels)
   
    * Volcanoplot (Anova)
        * Added in a new section "Multi-univariate"
        * Possibility to select the factor levels (Select First Factor Levels)
   
    * t-SNE: "r t-sne plot"
        * Added in the "Mutivariate" section

* Intersection between data subsets
    * Integration of the UpSetR tool
    * Either For all data subsets (UpSetR tool)
    *  Or  according to the selection of data subsets  (Venn diagram if 2<nb<4)

* Management of large data tables in the data explorer
    * When loading, keep only the first variables within the maximum allowed (1000 by default).
    * Thus just put the most significant and/or interesting variables first in the attributes table.
    * Creation of the script to sort the variables in the file a_attributes.tsv according to their significance (see scripts/odam_reorder_vars.R)

* Numerous improvements / corrections
    * Numerical Identifiers & Numerical  factors better managed
    * Addition of the type of correlation in Multivariate / Correlation
    * Addition of polygons encirclement for PCA/ICA/t-SNE

* Upgrade to R 4.0

### API getdata v1.4 (Nov 2021)

* Added sqlite 3.0 to speed up response times on large files as data query system (default q-Text-sql)
    * Added the "build" service to build the database from the tabulated files.

* Adding an authorization mechanism by key, possibly associated with an IP number.
    * Parametrization within a file authkeys.tsv to be added in the dataset directory

* Upgrade to PHP 7.2
