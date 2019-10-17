library(plyr)
library(tidyverse)
library(hunspell)
#library(RCurl)
library(jsonlite)
library(rvest)
library(naniar)
library(stringi)

#installed.packages()
#
#str = c('pumpking', 'garnit', 'fart', 'oxygen', 'goose neck', 'budwiser')
#
#inx = c(1, 2, 3, 4, 5, 6)
#
#df <- data.frame(x = inx, y = str)
#
#typeof(df)
#
#colchx <- df$y
#
#chxd <- df %>% filter((hunspell_check(y))==FALSE)
#
#chxd
#
#gh <- hunspell_check(colchx)
#
#gh
#
#typeof(gh)
#
#df$y <- as.character(df$y)
#
#typeof(df$y)
#
#df$y
#
#str(df)
#
##Send df and col to spell check return df of hits
#splChxRtnDF <- function(dt, chxCol)
#{
#  colchx <- dt$chxCol
#  
#  chxd <- dt %>% filter((hunspell_check(chxCol))==FALSE)
#  
#  return(chxd)
#}
#
#
#test <- splChxRtnDF(df, df$y)
#
#test
#
#int <- 13
#
#i <- int%%12
#
#i
#
#k <- row_number(df$y)
#
#k
#
#grphData <- df %>% as_tibble() %>% mutate(inxng = ifelse(row_number() <= 12, row_number(), ifelse(row_number#()%%12 == 0, 1, (row_number()%%12)+1)))
#
#typeof(k)
#
#inxColGrphText <- function(df, colToGrph)
#{
#  
#  grphData <- df %>% as_tibble() %>% mutate(xData = row_number(), yData = ifelse(row_number() <= 12, row_number#(), ifelse(row_number()%%12 == 0, 12, row_number()%%12)))
#  
#  grphData %>% ggplot(aes(xData, yData, label = colToGrph)) + geom_label() + xlim(0, 13)
#  
##return(grphData)
#  
#}
#
#inxColGrphText(df, df$y)
#
#
#wordDefinitionSearch <- function(defWord)
#{
#  # Put Current M-W key here, collKey stands for collegiate, also have Spanish API key
#  
#  #defWord <- "test"
#  collKey <- "300d35b9-c2ae-4e29-9b78-07ea5c86e62b"
#
#  rawUrl <- str_c("https://dictionaryapi.com/api/v3/references/collegiate/json/",defWord,"?key=",   collKey)
#  
#  defWordJSON <- fromJSON(rawUrl)
#  
#  returnVal <- ifelse(is.list(defWordJSON), TRUE, FALSE)
#  
#  returnVal
#  
#  return(returnVal)
#    
#}
#
#
# https://raw.githubusercontent.com/meanphil/bjcp-guidelines-2015/master/styleguide.xml

#hp<-read_html("https://raw.githubusercontent.com/meanphil/bjcp-guidelines-2015/master#/styleguide.xml")
#
#hp
#
#typeof(hp)
#
##beer_style_name <- html_nodes(hp,"name")
#
##length(beer_style_name)
#
##typeof(beer_style_name[1])
#
##length(as.character(beer_style_name[1]))
#
##strLen <- nchar(as.character(beer_style_name[1]))
#
##typeof(nchar(as.character(beer_style_name[1])))
#
##as.character(beer_style_name[1:10])
#
##beer_catz <- stri_sub(as.character(beer_style_name), 7, -8)
#
##beer_catz
#
##beer_style_name[1]
#
##beer_style_stats <- html_nodes(hp, "stats")
#
##head(beer_style_stats)
#
#beer_cat <- html_nodes(hp, "category")
#
#beer_cat
#
##as.character(beer_cat[1])
#
#beer_catz <- str_extract(as.character(beer_cat), "(<name>)([\\w\\s]+)(</name>)")
#
#beer_catz <- stri_sub(as.character(beer_catz), 7, -8)
#
#beer_catz
#
#beer_subcat <- html_nodes(hp, "subcategory")
#
#beer_subcat <- as.character(beer_subcat)
#
##beer_subcat
#
##beer_subcat[91]
#
#beer_subcatz <- str_extract(beer_subcat, "(<name>)([|\\w|\\d|\\s|[:punct:]]+)(</name>)")
#
#length(beer_subcatz)
#
#beer_subcatz
#
#beer_subcatz <- stri_sub(as.character(beer_subcatz), 7, -8)
#
#beer_subcatz
#
#beer_stats <- str_extract(beer_subcat, "(<stats>)((\\s|\\S)+)(</stats>)")
#
#beer_stats[1]
#
#ibu_range <- str_extract(beer_stats, "(<ibu flexible=)((\\s+|\\S+))(</ibu>)")
#
#ibu_range
#
##missing_ibu_range <- grep("true", beer_ibu_range)
##
##missing_ibu_range
##
##beer_subcatz_with_ibus <- beer_subcatz[-missing_ibu_range]
##
##beer_subcatz_with_ibus
##
##beer_subcatz_without_ibus <- beer_subcatz[missing_ibu_range]
##
##beer_subcatz_without_ibus <- data.frame(subcategory=beer_subcatz_without_ibus) %>% as#.tibble#()
##
##beer_subcatz_without_ibus
##
##raw_ibus <- ibu_range[-missing_ibu_range]
##
##raw_ibus
#
#ibu_lrng <-  str_extract(ibu_range, "(<low>)([0-9]+)(</low>)")
#
#ibu_lrng <- str_extract(ibu_lrng, "[0-9]+")
#
##ibu_lrng
#
#ibu_rng_df <- data.frame(subcategory=beer_subcatz_with_ibus) %>% as.tibble()
#
#ibu_rng_df <- cbind(ibu_rng_df, ibu_lrng)
#
#ibu_rng_df
#
#tail(ibu_rng_df)
#
#ibu_hrng <-  str_extract(raw_ibus, "(<high>)([0-9]+)(</high>)")
#
#ibu_hrng <- str_extract(ibu_hrng, "[0-9]+")
#
#ibu_hrng
#
#ibu_rng_df <- cbind(ibu_rng_df, ibu_hrng) 
#
#
#ibu_rng_df <- rbind.fill(ibu_rng_df, beer_subcatz_without_ibus) %>% as.tibble()
#
#ibu_rng_df
#
#abv_range <- str_extract(beer_stats, "(<abv flexible=)((\\s+|\\S+))(</abv>)")
#
#abv_range
#
#missing_abv_range <- grep("true", raw_abv)
#
#missing_abv_range
#
#raw_abvs <- abv_range[-missing_abv_range]
#
#raw_abvs
#
#beer_subcatz_with_abvs <- beer_subcatz[-missing_abv_range]
#
#beer_subcatz_with_abvs
#
#beer_subcatz_without_abvs <- beer_subcatz[missing_abv_range]
#
#beer_subcatz_without_abvs <- data.frame(subcategory=beer_subcatz_without_abvs) %>% as.tibble#()
#
#beer_subcatz_without_abvs
#
#abv_lrng <-  str_extract(raw_abvs, "(<low>)(([0-9]+)(\\.)([0-9]+)|([0-9]+))(</low>)")
#
#abv_lrng <- str_extract(abv_lrng, "(([0-9]+)(\\.)([0-9]+)|([0-9]+))")
#
#abv_lrng
#
#abv_rng_df <- data.frame(subcategory=beer_subcatz_with_abvs) %>% as.tibble()
#
#abv_rng_df <- cbind(abv_rng_df, abv_lrng)
#
#abv_rng_df
#
#tail(abv_rng_df)
#
#abv_hrng <-  str_extract(raw_abvs, "(<high>)(([0-9]+)(\\.)([0-9]+)|([0-9]+))(</high>)")
#
#abv_hrng <- str_extract(abv_hrng, "(([0-9]+)(\\.)([0-9]+)|([0-9]+))")
#
#abv_hrng
#
#abv_rng_df <- cbind(abv_rng_df, abv_hrng) 
#
#
#abv_rng_df <- rbind.fill(abv_rng_df, beer_subcatz_without_abvs) %>% as.tibble()
#
#abv_rng_df
#
#rng_df <- full_join(ibu_rng_df, abv_rng_df, by="subcategory")
#
#rng_df
#
##beer_ranges <- str_extract_all(beer_stats, "(<(abv|ibu) flexible=)((\\s+|\\S+))(</(abv|ibu#)>)")
##
##beer_ranges
##
##beer_ranges <- unlist(beer_ranges)
#
#grep("<ibu\\b", unlist(beer_ranges))
#
#ibu_rng <- beer_ranges[grep("<ibu\\b", beer_ranges)]
#
#ibu_rng
#
#
#
#typeof(missing_ibu_range)
#
#beer_ibu_range[-missing_ibu_range]
#
#
hp<-read_html("https://raw.githubusercontent.com/meanphil/bjcp-guidelines-2015/master/styleguide.xml")

hp

beer_cat <- html_nodes(hp, "category")

beer_catz <- str_extract(as.character(beer_cat), "(<name>)([\\w\\s]+)(</name>)")

beer_catz <- stri_sub(as.character(beer_catz), 7, -8)

beer_catz_id <- str_extract(as.character(beer_cat), "(<category id=)([:graph:]+)(>)")

beer_catz_id

beer_catz_id <- str_replace_all(str_extract_all(as.character(beer_catz_id), "(\")([:graph:]+)(\")"), "\"", "")

beer_catz_id

beer_catz <- data.frame(cat_id = beer_catz_id, category = beer_catz)

beer_subcat <- html_nodes(hp, "subcategory")

beer_subcat <- as.character(beer_subcat)

#beer_subcat

beer_subcatz <- str_extract(beer_subcat, "(<name>)([|\\w|\\d|\\s|[:punct:]]+)(</name>)")

beer_subcatz <- stri_sub(as.character(beer_subcatz), 7, -8)

#beer_subcatz

beer_subcatz_id <- str_extract_all(as.character(beer_subcat), "(<subcategory id=)([:graph:]+)(>)")

#beer_subcatz_id <- str_extract_all(beer_subcatz_id, ("\"[:alnum:]\""))

beer_subcatz_id <- str_replace_all(str_extract_all(as.character(beer_subcatz_id), "(\")([:graph:]+)(\")"), "\"", "")

#beer_subcatz_id

beer_stats <- str_extract(beer_subcat, "(<stats>)((\\s|\\S)+)(</stats>)")

ibu_range <- str_extract(beer_stats, "(<ibu flexible=)((\\s+|\\S+))(</ibu>)")

#ibu_range

ibu_lrng <-  str_extract(ibu_range, "(<low>)([0-9]+)(</low>)")

ibu_lrng <- str_extract(ibu_lrng, "[0-9]+")

#ibu_lrng

ibu_hrng <-  str_extract(ibu_range, "(<high>)([0-9]+)(</high>)")

ibu_hrng <- str_extract(ibu_hrng, "[0-9]+")

#ibu_hrng

ibu_rng_df <- data.frame(subcategory_id = beer_subcatz_id, subcategory=beer_subcatz) %>% as_tibble()

ibu_rng_df <- cbind(ibu_rng_df, ibu_lrng, ibu_hrng)

#ibu_rng_df

abv_range <- str_extract(beer_stats, "(<abv flexible=)((\\s+|\\S+))(</abv>)")

#abv_range

abv_lrng <-  str_extract(abv_range, "(<low>)(([0-9]+)(\\.)([0-9]+)|([0-9]+))(</low>)")

abv_lrng <- str_extract(abv_lrng, "(([0-9]+)(\\.)([0-9]+)|([0-9]+))")

#abv_lrng

abv_hrng <-  str_extract(abv_range, "(<high>)(([0-9]+)(\\.)([0-9]+)|([0-9]+))(</high>)")

abv_hrng <- str_extract(abv_hrng, "(([0-9]+)(\\.)([0-9]+)|([0-9]+))")

#abv_hrng

rng_df <- cbind(ibu_rng_df,abv_lrng, abv_hrng)

rng_df

rng_df_t <- mutate(rng_df, category_id = substr(as.character(rng_df$subcategory_id), 1, (nchar(as.character(rng_df$subcategory_id))-1)))

rng_df_t <- left_join(rng_df_t, beer_catz, by = c("category_id" = "cat_id"))

rng_df_t

rng_df_t <- rng_df_t[c(7,8,1,2,3,4,5,6)]

head(rng_df_t)

#rng_df_t <- mutate(rng_df_t, ibu_avgA = ((as.numeric(rng_df_t$ibu_lrng) + as.numeric#rng_df_t$ibu_hrng)/2)))

rng_df_t <- mutate(rng_df_t, ibu_avg = (mapply(function(x, y) ((x+y)/2) , as.numeric(as.character(rng_df_t$ibu_lrng)),as.numeric(as.character(rng_df_t$ibu_hrng)))))

rng_df_t <- mutate(rng_df_t, abv_avg = (mapply(function(x, y) ((x+y)/2) , as.numeric(as.character(rng_df_t$abv_lrng)),as.numeric(as.character(rng_df_t$abv_hrng)))))                                               


#3a<-as.numeric(as.character(rng_df_t$ibu_lrng[1]))
#b<-as.numeric(as.character(rng_df_t$ibu_hrng[1]))

#(as.numeric(as.character(rng_df_t$ibu_lrng[1]) + as.numeric(rng_df_t$ibu_hrng[1])))

#mean(c(a,b))


head(rng_df_t)

rng_df_t

beers <- (read.csv(file.choose()))
typeof(beers)
head(beers)
dim(beers)

breweries <- (read.csv(file.choose()))
head(brewries)
dim(brewries)


dfFull = left_join(beers, breweries, by = c("Brewery_id" =  "Brew_ID"))

names(dfFull)

as.character(unique(dfFull$Style))



df_BeerStyles <- as.character(unique(dfFull$Style))#c("American Pale Lager", "American Pale Ale (APA)", "American IPA", "American Double / Imperial IPA", "Oatmeal Stout", "American Porter", "Saison / Farmhouse Ale", "Belgian IPA", "Cider", "Baltic Porter", "Tripel", "American Barleywine", "Winter Warmer", "American Stout", "Fruit / Vegetable Beer", "English Strong Ale", "American Black Ale", "Belgian Dark Ale", "American Blonde Ale", "American Amber / Red Ale", "Berliner Weissbier", "American Brown Ale", "American Pale Wheat Ale", "Belgian Strong Dark Ale", "KÃ¶lsch", "English Pale Ale", "American Amber / Red Lager", "English Barleywine", "Milk / Sweet Stout", "German Pilsener", "Pumpkin Ale", "Belgian Pale Ale", "American Pilsner", "American Wild Ale", "English Brown Ale", "Altbier", "California Common / Steam Beer", "Gose", "Cream Ale", "Vienna Lager", "Witbier", "American Double / Imperial Stout", "Munich Helles Lager", "Schwarzbier", "MÃ¤rzen / Oktoberfest", "Extra Special / Strong Bitter (ESB)", "Rye Beer", "Euro Dark Lager", "Hefeweizen", "Foreign / Export Stout", "Other", "English India Pale Ale (IPA)", "Czech Pilsener", "American Strong Ale", "Mead", "Euro Pale Lager", "American White IPA", "Dortmunder / Export Lager", "Irish Dry Stout", "Scotch Ale / Wee Heavy", "Munich Dunkel Lager", "Radler", "Bock", "English Dark Mild Ale", "Irish Red Ale", "Rauchbier", "BiÃ¨re de Garde", "Doppelbock", "Dunkelweizen", "Belgian Strong Pale Ale", "Dubbel", "Quadrupel (Quad)", "Russian Imperial Stout", "English Pale Mild Ale", "Maibock / Helles Bock", "Herbed / Spiced Beer", "American Adjunct Lager", "Scottish Ale", "", "Smoked Beer", "Light Lager", "Abbey Single Ale", "Roggenbier", "Kristalweizen", "American Dark Wheat Ale", "English Stout", "Old Ale", "American Double / Imperial Pilsner", "Flanders Red Ale", "Keller Bier / Zwickel Bier", "American India Pale Lager", "Shandy", "Wheat Ale", "American Malt Liquor", "English Bitter", "Chile Beer", "Grisette", "Flanders Oud Bruin", "Braggot", "Low Alcohol Beer")

df_BeerStyles <- data.frame(beerStyle = df_BeerStyles)

df_BeerStyles$beerStyle

df_BeerStyles$beerStyle <- str_replace_all(as.character(df_BeerStyles$beerStyle),"[/]{1}", "")


df_BeerStyles$beerStyle <- str_replace_all(as.character(df_BeerStyles$beerStyle),"[\\s]{1}", "~~")

df_BeerStyles$beerStyle <- str_replace_all(as.character(df_BeerStyles$beerStyle),"~~~~", "~~")

df_BeerStyles$beerStyle <- str_replace_all(as.character(df_BeerStyles$beerStyle), "((~~)(\\(.+))", "")

df_BeerStyles

#tests

beerSubcats <- data.frame(beerSubcat = beer_subcatz)

#beer_subcatz

#fartherTests$subcategory %>% is.na()

beerSubcats$beerSubcat <- str_replace_all(as.character(beerSubcats$beerSubcat),"[\\s]{1}", "~~")

beerSubcats$beerSubcat <- str_replace_all(as.character(beerSubcats$beerSubcat),"~~~~", "~~")

beerSubcats$beerSubcat <- str_replace_all(as.character(beerSubcats$beerSubcat), "((~~)(\\(.+))", "")

#prit<-str_detect(as.character(fartherTests$beer_subcatz),"[\\s]{1}")
#prit


#fartherTests

#passic <- which(as.character(tests$sty)%in%as.character(fartherTests$beer_subcatz))
#tests[passic,]


dfRefMatchesSubCat <- which(as.character(beerSubcats$beerSubcat)%in%as.character(df_BeerStyles$beerStyle))
dfRefMatchesSubCat
#tests[rassic,]

#tests[-rassic,]

#tests[rassic,]

#beer

arrange(as.data.frame(beerSubcats[dfRefMatchesSubCat,]))

rng_df_t[dfRefMatchesSubCat,] %>% arrange(subcategory)

t_beer_catz <- beer_catz

#t_beer_catz

t_beer_catz$category <- str_replace_all(as.character(t_beer_catz$category),"[\\s]{1}", "~~")

t_beer_catz$category <- str_replace_all(as.character(t_beer_catz$category),"~~~~", "~~")

t_beer_catz$category <- str_replace_all(as.character(t_beer_catz$category), "((~~)(\\(.+))", "")

#t_beer_catz

#t_beer_catz$category

#tests$sty

#fartherTests$beer_subcatz

#tests$sty

#massic <-which(as.character(tests$sty)%in%as.character(t_beer_catz$category))
#massic

#tests[massic,]

dfMatchesCatInx <-which(as.character(t_beer_catz$category)%in%as.character(df_BeerStyles$beerStyle))

dfMatchesCatInx

t_beer_catz[dfMatchesCatInx,][,1]

dfRefMatchesCat <- rng_df_t %>% filter(category_id == 14)

#rng_df_t[dfMatchesCat,]

#rng_df_t %>% filter(category_id == 14)

dfRefMatchesSubCat

rng_df_f <- rbind(rng_df_t[dfRefMatchesSubCat,], dfRefMatchesCat)

rng_df_f

write.csv(rng_df_f, file="/Users/Jason/documents/school/casestudy1beerdata/abv_ibu_ref.csv")

write.csv(rng_df_t, file="/Users/Jason/documents/school/casestudy1beerdata/abv_ibu_ref_all.csv")

head(dfFull)



dfFull %>% filter(is.na(Brewery_id))


dim(beers)
dim(brewries)
dim(df)

names(beers)
names(brewries)
names(df)


df$Name.x

df$Name.y

names(dfFull$Name.x)<- c("brew")

names(dfFull$Name.y) <- c("brewery")

head(dfFull)

#str(df)
#
#typeof(df)
#
#df <- do.call(rbind.data.frame, df)
#
#typeof(df)
#
#df <- df%>% as_tibble()
#
#names(df)[1] <- c("brew")

names(dfFull) <- c("brew", "beer_ID", "abv", "ibu", "brewery_id", "style", "ounces", "brewery", "city", "state")

dfFull

gg_miss_var(dfFull)

ibu_miss_val <- dfFull %>% filter(is.na(ibu))

head(ibu_miss_val)

levels(ibu_miss_val$style)

dim(ibu_miss_val)

ibu_miss_val$style

ibu_miss_val %>% ggplot(aes(x = style, y = ..count..)) + geom_bar()

ibu_mVals_byStyleCount <- ibu_miss_val %>% count(style)

ibu_mVals_byStyleCount %>% arrange(desc(n))

dfFull <- 








#nullStyle <- df %>% filter(df$style == "")
#
#nullStyle
#
#dim(nullStyle)
#
#nullStyle$brew












#1.   How many breweries are present in each state?
#  
#  2.   Merge beer data with the breweries data. Print the first 6 observations and the last six #observations to check the merged file.  (RMD only, this does not need to be included in the presentati#on or the deck.)
#
#3.   Address the missing values in each column.
#
#4.   Compute the median alcohol content and international bitterness unit for each state. Plot a bar #chart to compare.
#
#5.   Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
#  
#  6.   Comment on the summary statistics and distribution of the ABV variable.
#
#7.   Is there an apparent relationship between the bitterness of the beer and its alcoholic content? #Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.
#
#8.  Budweiser would also like to investigate the difference with respect to IBU and ABV between IPAs #(India Pale Ales) and other types of Ale (any beer with “Ale” in its name other than IPA).  You decide #to use KNN clustering to investigate this relationship.  Provide statistical evidence one way or the #other. You can of course assume your audience is comfortable with percentages … KNN is very easy to #understand.  
#
#9. Knock their socks off!  Find one other useful inference from the data that you feel Budweiser may #be able to find value in.  You must convince them why it is important and back up your conviction with #appropriate statistical evidence. 

  
  









##beer_ibu_range <- as.data.frame(beer_ibu_range) %>% as.tibble()
#
##beer_ibu_range
#
#str_trim("  Hello world! ")
#
#str_trim("  Hello world! ", method="left")
#str_trim("  Hello world! ", method="right")
#
#
#str_trim(" ..Hello ... world! ", pattern=" \\.")
#
#
#xml_children(beer_style_name)
#
#hp_catgR <- html_nodes(hp, "ibu")
#
#hp_catgR
#
#hp_atgR <- html_nodes(hp, "abv")
#
#hp_atgR
#
##hp_catgR %>% html_nodes("name")
#
#
#hp_priceR <- html_nodes(hp,"price")
#hp_descR <- html_nodes(hp,"description")
#hp_name = stri_sub(hp_nameR,7,-8)
#hp_name
#hp_price = stri_sub(hp_priceR,8,-9)
#hp_price
#hp_desc = stri_sub(hp_descR,14,-15)
#hp_desc
#bfast = data.frame(hp_name,hp_price,hp_desc)
#grep("toast", bfast$hp_desc)
#grepl("toast",bfast$hp_desc)
#
#
#
#
#
#
#
#
#
#
#defWordJSON <- wordDefinitionSearch("test")
#
#defWordJSON
#
##typeof(defWordJSON)
#
##typeof(defWordJSON)=="character"
#
##Not Found
##is.character(defWordJSON)
#
##Found
##is.list(defWordJSON)
#
#defExists <- ifelse(defWordJSON, "Definition Found","Definition Not Found")
#  
#defExists
#  
#  
#
#
##bnb <- left_join(brewries,beers, by = c("Brew_ID" = "Brewery_id"))
#
#brewUrl <- "https://en.wikipedia.org/wiki/List_of_beer_styles"
#
#brew_types <- brewUrl %>% read_html() %>% html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>% #html_table()
#
#head(brew_types[[1]][[1]])
#
#termBeers <- brew_types[[1]][[1]]
#
#length(termBeers)
#
#typeof(termBeers)
#
##Mdf <- left_join(beers, brewries, by.x = "Brewery_ID", by.y = "Brew_ID")
##  
##Mdf  
#
#df <- left_join(beers, brewries, by = c("Brewery_id" = "Brew_ID"))
#
#df

