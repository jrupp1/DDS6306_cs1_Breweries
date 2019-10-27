# DDS6306_cs1_Breweries

# Read Me

This repository contains the project work by Jason Rupp and Jon Paugh for project 1 for the class DS 6306 "Doing Data Science" at SMU's Data Science Masters Degree Program.

The file beers.csv and breweries.csv in this repository contain the inital starting data, and can also be found on Kaggle here:
https://www.kaggle.com/nickhould/craft-cans

BeerFullWithGeocodes.csv contains the original beer and brewery data joined, and with geocodes added.

Additional data used used in conjunciton with the craft brewery and beer data:
* AdjustedIncomeByState.csv contains the adjustments to income to account for higher or lower cost in the state, organized by state.
   This data came from here: https://taxfoundation.org/new-state-level-price-data-shows-smaller-state-real-income-differences/
* HippyData.csv - contains the # of communes and cooperatives per state. 
   This data came from here: https://www.ic.org/directory/communes/
   And here: http://www.coopdirectory.org/directory.htm#West_Virginia
   This web page suggested these data points for identifying "Hippie" states: https://www.estately.com/blog/2014/05/is-your-state-a-land-of-hippies-or-is-it-full-of-squares/
* ASM_2016_31AS101_with_ann.csv contains the manufacturing data by state for 2016.
   This data came from here: https://www.census.gov/data/tables/2016/econ/asm/2016-asm.html
   
* Beer style guidelines at this website https://www.bjcp.org/stylecenter.php . They provided a link to an xml doc that has a number of stats about different beer styles. We parse the required data and create a data frame to attempt to impute missing valsCreated a csv called "abv_ibu_ref.csv".

The main file containing the analysis work is _BeerAnalysis.Rmd.

The file _BeerAnalysis.html contains the knitr output in html from running the full R markdown file. Go here to see both the execution results and output as well as the R code for the analysis.


Presentations:
Jon Paugh's youtube video is here: https://www.youtube.com/watch?v=XgoDy0ZKkrc

Jason Rupp's youtube video is here: https://youtu.be/0-QieTEXXOc 

# Codebook
The following explains the variable names used in the project.

Beer: dataframe of beer data csv

Brewery: dataframe of brewery data csv

dfFull: dataframe of joined beer to brewery data, each beer with corresponding brewery

dfFullNoJoin: temp dataframe to check to see if there were any beers that failed to match a brewery

dfBreweryByState: dataframe grouping breweries together by state for emuneration

dfBreweryByStateWithStatePop: Joining brewery by state dataframe with state population data from the 2015 census data in the usmap package

notNaCols: Enumerating the number of rows which aren't NANs in each column of the dataframe

isNaCols: enumerating the number of rows which are NANs in each column of the dataframe

dfFull_vt: dataframe with the number sums for each not na/na row with column names, created for graphing

naPlot1: First of the plots displaying the NAN row values for each column

naPlot2: Second of the plots displaying the NAN rows values with the percentage above 

hp: raw html read from website

beer_cat: html node text that holds category data, parsed with regex to assemble final dataframe 

beer_catz: final data frame with beer category and id based on html doc

beer_subcat: html node text that holds subcategory data, parsed with regex to assemble final dataframe 

beer_subcatz: final data frame with beer category and id based on html doc

beer_stats: extracting the beer characteristic data from the subcategory text. Stats such as IBU, ABV, Color, etc. 

ibu_range: Extracting the ibu range, you can parse low and high values

ibu_lrng: Low range of ibu values

ibu_hrng: high range of ibu values

ibu_rng_df: Subcategories of beer with the range of IBUs for each

abv_range: Extracting the ibu range, you can parse low and high values

abv_lrng: Low range of ibu values

abv_hrng:  High range of ibu values

rng_df: Join ABV data with Subcategory/IBU data

rng_df_t: Total range dataframe, matching subcategories of beer with categories of beer and their typical IBU and ABV ranges, with mean values for both

df_BeerStyles: The unique beer styles in the full data frame, these will have special characters placed in them to match with range dataframe

beerSubcats: Subcategories of beers from the reference range dataframe

dfRefMatchesSubCat_inx: This will return the index of the matches between the subcategory df and the reference df. This index will match the index of the df with the ibu/abv data and we can extract that data later 

t_beer_catz: Copy of the category dataframe to see if there are matches in the full dataframe

dfMatchesCatInx: This will return the index of the matches between the category df and the reference df. This index will match the index of the df with the ibu/abv data and we can extract that data later 

ibu_blnks: Rows from the full dataframe with blank ibu

abv_blnks: Rows from the full dataframe with blank abv

df_to_impt: Dataframe of blank ibu values from the full dataframe, will end up with mean ibus by style from the reference dataframe

inxHolder: list that will grab the numeric index index of the row where the match in the subcategory dataframe occurs, need this for merge, will be added to df_to_impt

ibus_to_replace: This will be the mean ibus from the reference dataframe

df_imputed_ibus: Dataframe of beers from the full dataframe with imputed mean ibus from the reference dataframe

dfFull_imp: Copy of full dataframe to impute with mean

dfFull_inx_to_impt: Indexes of rows with NAN in the ibu column

notNaCols_imp: Enumerating the number of rows which aren't' NANs in each column of the imputed dataframe  

isNaCols_imp: Enumerating the number of rows which are NANs in each column of the imputed dataframe  

dfFull_vt_imp: Dataframe to graph the number of missing values after imputation

naPlot1_imp: First of the plots displaying the NAN row values for each column after imputation 

naPlot2_imp: Second of the plots displaying the NAN row values for each column after imputation with the percentage above  

dfMedianABVAndIBUByState: Dataframe with mean abv and ibu by state for histograms

abvMean: Mean ABV overall 

abvSd: Standard deviation of ABV overall

dfIndianPaleAles: Dataframe of just IPAs for classification

dfOtherAles: Dataframe of beers that aren't' IPAs for classification

dfAllAles: Dataframe of all beers with a classification column labeling IPA or Other

splitPerc: Percentage the data will be split for train/test

iterations: Number of times the model will be run

numks: High number of k that will be used

masterAcc: Hold the accuracies over each iteration of the model

masterSpec: Hold the specificities over each iteration of the model

masterSens: Hold the specificities over each iteration of the model

trainIndices: Indexes of the all ale dataframe that will be used in the training model

train: Training model data from the all ale dataframe 

test: Test model data from all ale dataframe

classifications: This is what the model classified the beer as, this will be added to a table and used to calculate model statistics

CM: Confusion Matrix of model data, values can be used to calculate accuracy, sensitivity and specificity

MeanAcc: Mean accuracy by value of k, will be used to graph 

MeanSens: Mean sensitivity by value of k, will be used to graph 

MeanSpec: Mean specificity by value of k, will be used to graph 

test: Data frame used to hold the decision boundaries for ibu and abv to graph

classif: knn model used to graph with optimal k

prob: the prob attribute from the classif model

dataf: Dataframe used to graph decision boundries in knn model 

dfFullWithGeocodes: Dataframe of geocoded breweries

usa_center: Center point of the USA latitude and longitude

USAMap: Map of the united states

dfStateIncomeData: State income tax dataframe

dfBeerWithIncomes: Joined dataframe from the brewery by state with population dataframe, and the state income tax data above

dfManufacturing: Dataframe of manufacturing data from the United states, opened from a csv called "ASM_2016_31AS101_with_annClean.csv" from reference

dfBeerWithIncomeManuf: Adding manufacturing data to the beer data with income

dfHippyData: Dataframe pulled in from internet source csv "HippyData.csv"

dfBeerWithHipyData: Dataframe of manufacturing, income and hippy data 

dfBeerWithHipyDataNoNVermont: Same dataframe as above without Vermont 

dfGoodPlacesForBrewery: Dataframe of the best states to choose to open a craft brewery
