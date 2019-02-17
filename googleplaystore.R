googleplaystore_original <- read.csv("~/Data Science/googleplaystore_original.csv")
View(googleplaystore_original)
library("dplyr")
library("stringr")
library("ggplot2")
library("caTools")
library(MASS)
library(rpart)
library(rpart.plot)

#Change 1.9 to NA in Category variable
googleplaystore_original%>% distinct(Category)
googleplaystore_original$Category<- as.character(googleplaystore_original$Category)
googleplaystore_original <-transform(googleplaystore_original , Category = ifelse(Category == "1.9", "NA", Category))
#Change NAN to NA in Rating variable and convert to numeric data type
distinct(googleplaystore_original, Rating)
googleplaystore_original$Rating<- as.character(googleplaystore_original$Rating)
googleplaystore_original <-transform(googleplaystore_original , Rating = ifelse(Rating == "NaN", "NA", Rating))
googleplaystore_original<-transform(googleplaystore_original, Rating = as.numeric(Rating))
#Check Reviews variable for values with characters and change them to NA, then chang the variable to a numeric datatype.
filter(googleplaystore_original, grepl("[A-Z]",Reviews))
googleplaystore_original$Reviews<- as.character(googleplaystore_original$Reviews)
googleplaystore_original <-transform(googleplaystore_original , Reviews = ifelse(Reviews == "3.0M", "NA", Reviews))
googleplaystore_original$Reviews<- as.numeric(googleplaystore_original$Reviews)
#Remove values not ending in M or k from the size variable
googleplaystore_original$Size<- as.character(googleplaystore_original$Size)
googleplaystore_original <-transform(googleplaystore_original , Size = ifelse(Size == "Varies with device", "NA", Size))
googleplaystore_original$Size<- as.character(googleplaystore_original$Size)
googleplaystore_original <-transform(googleplaystore_original , Size = ifelse(Size == "1,000+", "NA", Size))
#Remove M from values that have M, and convert to kilobyte by multipling by 1000. 
#Remove K from values that have K, and convert to kilobyte by multipling by 1.
googleplaystore_original <-mutate(googleplaystore_original, SizeKB= case_when(grepl("M",Size) ~  as.numeric(sub("M", 
                                                                                                                      "", googleplaystore_original$Size))*1000,grepl("k",Size) ~  as.numeric(sub("k", "", googleplaystore_original$Size))*1 ))
#Remove the + sign from the Installs variable  
googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)
googleplaystore_original <-transform(googleplaystore_original , Installs = ifelse(grepl("\\+",googleplaystore_original
                                                                                        $Installs), sub("\\+", "", googleplaystore_original$Installs), Installs))
#Change all values with alphabets to NA
googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)
googleplaystore_original <-transform(googleplaystore_original , Installs = ifelse(grepl("[A-Z]", Installs), "NA", 
                                                                                  Installs))
#Downloads are coded as follows and assigned to a new variable AdjustedInstall2: 
#between 0-500 is coded as 1
#between 1000-1,000,000 is coded as 2
#between 5,000,000-1,000,000,000 is coded as 3
googleplaystore_original <- mutate(googleplaystore_original, AdjustedInstall2= case_when(googleplaystore_original$Install=="0"|googleplaystore_original$Install=="1"|googleplaystore_original$Install=="5"|googleplaystore_original$Install=="10"|googleplaystore_original$Install=="50"|googleplaystore_original$Install=="100"|googleplaystore_original$Install=="500" ~ "1",
                                                                                         googleplaystore_original$Install=="1,000"|googleplaystore_original$Install=="5,000"|googleplaystore_original$Install=="10,000"|googleplaystore_original$Install=="50,000"|googleplaystore_original$Install=="100,000"|googleplaystore_original$Install=="500,000"|googleplaystore_original$Install=="1,000,000" ~ "2",
                                                                                         googleplaystore_original$Install=="5,000,000"|googleplaystore_original$Install=="10,000,000"|googleplaystore_original$Install=="50,000,000"|googleplaystore_original$Install=="100,000,000"|googleplaystore_original$Install=="500,000,000"|googleplaystore_original$Install=="1,000,000,000"  ~ "3",
                                                                                        ))
googleplaystore_original$AdjustedInstall2<- ordered(googleplaystore_original$AdjustedInstall2, levels=c("1","2","3"))
#Downloads are coded as follows and assigned to a new variable AdjustedInstall: 
#between 0 is coded as "No Install"
#between 0-500 is coded as "Extremely Small"
#between 1000-10,000 is coded as "Very Small"
#between 50,000-500,000 is coded as "Small"
#between 1,000,000 - 10,000,000 is coded as "Medium"
#between 50,000,000-500,000,000 is coded as "Large"
#between 1,000,000,000 is coded as "Very Large"
googleplaystore_original <- mutate(googleplaystore_original, AdjustedInstall= case_when(googleplaystore_original$Install=="0" ~ "No Install",   
                                                                                         googleplaystore_original$Install=="1"|googleplaystore_original$Install=="5"|googleplaystore_original$Install=="10"|googleplaystore_original$Install=="50"|googleplaystore_original$Install=="100"|googleplaystore_original$Install=="500" ~ "Extremely Small",
                                                                                         googleplaystore_original$Install=="1,000"|googleplaystore_original$Install=="5,000"|googleplaystore_original$Install=="10,000" ~ "Very Small",
                                                                                         googleplaystore_original$Install=="50,000"|googleplaystore_original$Install=="100,000"|googleplaystore_original$Install=="500,000" ~ "Small",
                                                                                         googleplaystore_original$Install=="1,000,000"|googleplaystore_original$Install=="5,000,000"|googleplaystore_original$Install=="10,000,000" ~ "Medium",
                                                                                         googleplaystore_original$Install=="50,000,000"|googleplaystore_original$Install=="100,000,000"|googleplaystore_original$Install=="500,000,000" ~ "Large",
                                                                                         googleplaystore_original$Install=="1,000,000,000" ~ "Very Large"))
googleplaystore_original$AdjustedInstall<- ordered(googleplaystore_original$AdjustedInstall, levels=c("NA","No Install","Extremely Small","Very Small","Small","Medium","Large","Very Large"))
#Remove all "," from the Installs variable
googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)
googleplaystore_original<-transform(googleplaystore_original , Installs = gsub(",","", googleplaystore_original$Installs))
googleplaystore_original$Installs<- ordered(googleplaystore_original$Installs, levels=c("NA","0","1","5","10","50","100","500","1000","5000","10000","50000","100000","500000","1000000","5000000","10000000","50000000","100000000","500000000", "1000000000"))
#Change all NAN or 0 to NA in the Type variable
googleplaystore_original$Type<- as.character(googleplaystore_original$Type)
googleplaystore_original%>% distinct(Type)
googleplaystore_original <-transform(googleplaystore_original , Type = ifelse(Type == "NaN", "NA", Type))
googleplaystore_original <-transform(googleplaystore_original , Type = ifelse(Type == "0", "NA", Type))
#Change all values with alphabets and $ to NA in variable price, then convert it to numeric data type
googleplaystore_original$Price<- as.character(googleplaystore_original$Price)
googleplaystore_original <-transform(googleplaystore_original , Price= ifelse(grepl("[A-Z]", Price), "NA", Price))
googleplaystore_original$Price<- as.character(googleplaystore_original$Price)
googleplaystore_original <-transform(googleplaystore_original , Price= ifelse(grepl("\\$", Price), sub("\\$", "",                                                                                                        googleplaystore_original$Price), Price))
googleplaystore_original$Price<- as.numeric(googleplaystore_original$Price)
#Change blank values to NA for the ContentRating variable
googleplaystore_original%>% distinct(ContentRating)
googleplaystore_original$ContentRating<- as.character(googleplaystore_original$ContentRating)
googleplaystore_original <-transform(googleplaystore_original, ContentRating = ifelse(ContentRating == "", "NA", ContentRating))
#Change value with "11-Feb-18" in the Genres variable to NA                                                                                      
googleplaystore_original%>% distinct(Genres)
googleplaystore_original$Genres<- as.character(googleplaystore_original$Genres)
googleplaystore_original <-transform(googleplaystore_original , Genres = ifelse(Genres == "11-Feb-18", "NA", Genres))
#Change all value in the Lastupdated variable not having the short month format to NA 
googleplaystore_original[which(grepl("[A-Z][a-z]({2})", googleplaystore_original$LastUpdated) ==FALSE), 11]
googleplaystore_original$LastUpdated<- as.character(googleplaystore_original$LastUpdated)
googleplaystore_original <-transform(googleplaystore_original , LastUpdated = ifelse(LastUpdated == "1.0.19", "NA", 
                                                                                     LastUpdated))
#Split LastUpdated variable into Day, Month and Year variables
googleplaystore_original <- data.frame(googleplaystore_original, str_split_fixed(googleplaystore_original$LastUpdated, 
                                                                                 "-", 3))
colnames(googleplaystore_original )[17] <- "Day"
colnames(googleplaystore_original )[18] <- "Month"
colnames(googleplaystore_original )[19] <- "Year"
#Change blank values to NA for variable Month
googleplaystore_original$Month<- as.character(googleplaystore_original$Month)
googleplaystore_original <-transform(googleplaystore_original , Month = ifelse(Month == "", "NA", Month))
#Change blank values to NA for variable Year
googleplaystore_original$Year<- as.character(googleplaystore_original$Year)
googleplaystore_original <-transform(googleplaystore_original , Year = ifelse(Year== "", "NA", Year))
#Change allvalues with NaN or blanks to NA for the AndroidVer variable
distinct(googleplaystore_original, AndroidVer)
googleplaystore_original$AndroidVer <- as.character(googleplaystore_original$AndroidVer )
googleplaystore_original <-transform(googleplaystore_original , AndroidVer = ifelse(AndroidVer == "Varies with device"|
                                                                                      AndroidVer =="NaN"|AndroidVer =="", "NA", AndroidVer))
#AndroidVer values are coded based on the first number of each version, assigned to a new variable MinimumVer: 
googleplaystore_original<-mutate(googleplaystore_original, MinimumVer= case_when(grepl("^1",googleplaystore_original$AndroidVer) ~ "Version 1s",grepl("^2",googleplaystore_original$AndroidVer) ~ "Version 2s",grepl("^3",googleplaystore_original$AndroidVer) ~ "Version 3s",grepl("^4",googleplaystore_original$AndroidVer) ~ "Version 4s",grepl("^5",googleplaystore_original$AndroidVer) ~ "Version 5s",grepl("^6",googleplaystore_original$AndroidVer) ~ "Version 6s",grepl("^7",googleplaystore_original$AndroidVer) ~ "Version 7s",grepl("^8",googleplaystore_original$AndroidVer) ~ "Version 8s"))
googleplaystore_original$MinimumVer<- ordered(googleplaystore_original$MinimumVer, levels=c("NA", "Version 1s","Version 2s","Version 3s","Version 4s","Version 5s","Version 6s","Version 7s","Version 8s"))
#Exclude observations with AdjustedInstall2 = NA
googleplaystore_original <- filter(googleplaystore_original, AdjustedInstall2 !="NA")
#Save in computer path as googleplaystore_original_clean.csv
write.csv(googleplaystore_original, file = "~/Data Science/googleplaystore_original_clean.csv")

#Most apps had downloads of 1000000 and above with an average number of downloads of 15,464,339 (between 10,000,000 and 50,000,000)
ggplot(googleplaystore_original, aes(x=Installs))+geom_bar()
summarise(googleplaystore_original, avg = mean(as.numeric(as.character(Installs)), na.rm = TRUE))

#Distinct number of app categories is 33
#Top 50% of apps by category (top 3 categories are Family, Game and Tools)
#bottom 3 categories with worst presence (Beauty, Parenting, Comics)
distinct(googleplaystore_original,Category)
top_50_category<-arrange(top_n(count(googleplaystore_original, Category), n=17),desc(n))
top_50_category
ggplot(inner_join(googleplaystore_original, top_50_category, by = "Category"), aes(AdjustedInstall, fill=Category)) +geom_bar()+facet_grid(Category ~ .)
arrange(top_n(count(googleplaystore_original, Category), n=-3),desc(n))

#Among apps with extremely small downloads (1-500), Family apps have the largest presence, followed by Tool apps, then Medical apps 
#Among apps with very small downloads (1000-10000), Family apps have the largest presence, followed by Tool apps, then Medical apps 
#Among apps with small downloads (50000-500000), Family apps have the largest presence, followed by Gaming apps, then Tool apps 
#Among apps with medium downloads (1000000-10000000), Family apps have the largest presence, followed by Gaming apps, then Tool apps 
#Among apps with large downloads (50000000-500000000), Gaming apps have the largest presence, followed by Family apps, then Communication apps 
#Among apps with very large downloads (1000000000), Communication apps have the largest presence, followed by Social apps, then Gaming apps 
ggplot(filter(googleplaystore_original,AdjustedInstall =="Extremely Small"), aes(Category, fill= Category)) +geom_bar()
ggplot(filter(googleplaystore_original,AdjustedInstall =="Very Small"), aes(Category, fill= Category)) +geom_bar()
ggplot(filter(googleplaystore_original,AdjustedInstall =="Small"), aes(Category, fill= Category)) +geom_bar()
ggplot(filter(googleplaystore_original,AdjustedInstall =="Medium"), aes(Category, fill= Category)) +geom_bar()
ggplot(filter(googleplaystore_original,AdjustedInstall =="Large"), aes(Category, fill= Category)) +geom_bar()

#Overall, the communication apps have the highest average downloads, followed by Social apps, then Video_Player apps
Category_by_mean_installs <- aggregate(as.numeric(as.character(googleplaystore_original[, 6])), list(googleplaystore_original$Category), mean, na.rm=TRUE)
colnames(Category_by_mean_installs)[1] <- "Category"
colnames(Category_by_mean_installs)[2] <- "MeanInstalls"
Category_by_mean_installs
ggplot(Category_by_mean_installs, aes(x=MeanInstalls, y= Category)) +geom_point()

#The density graph shows that Rating has an outlier with value 19
#Remove outlier
#Density graph now shows that Rating is left skewed with an average rating of 4.193338 
ggplot(googleplaystore_original, aes(Rating)) +geom_density()
googleplaystore_original<-filter(googleplaystore_original,Rating != 19)
ggplot(googleplaystore_original, aes(Rating)) +geom_density()
summarise(googleplaystore_original, avg = mean(Rating, na.rm = TRUE))

#To see how number of Installs are affected by average Ratings
#The scatter plot below shows that the relationship between the number of app downloads and mean rating follows a curved non-linear model. The model shows that apps with small number of downloads from 5000 and below, had an increase in mean rating as the number of downloads reduced, this demonstrates that apps with downloads fewer than 5000 are negatively affected by ratings. On the other hand, the mean ratings of apps with downloads between 5,000 and 1,000,000 increase as the number of downloads increase, this illustrates that apps with this range of downloads are positively influenced by ratings. Also, the mean ratings of apps with downloads 1,000,000 and above decrease as the number of downloads increase, thus displaying that apps with 1,000,000+ downloads are negatively impacted by ratings.
installs_by_mean_rating <- aggregate(googleplaystore_original[, 3], list(googleplaystore_original$Installs), mean, na.rm=TRUE)
colnames(installs_by_mean_rating)[1] <- "Installs"
colnames(installs_by_mean_rating)[2] <- "MeanRating"
ggplot(installs_by_mean_rating, aes(x=MeanRating, y= Installs)) +geom_point() + xlim(4,5)

#The scatter plot shows that there is a  strong positive relationship between average reviews and number of Installs
installs_by_mean_reviews <- aggregate(googleplaystore_original[, 4], list(googleplaystore_original$Installs), mean, na.rm=TRUE)
colnames(installs_by_mean_reviews)[1] <- "Installs"
colnames(installs_by_mean_reviews)[2] <- "MeanReviews"
installs_by_mean_reviews
ggplot(installs_by_mean_reviews, aes(x=MeanReviews, y= Installs)) +geom_point()

#Density graph now shows that Size is right skewed with an average size of 21516.53
#The scatter plot shows that there is a positive relationship between average app size and number of Installs
ggplot(googleplaystore_original, aes(SizeKB)) +geom_density()
summarise(googleplaystore_original, avg = mean(SizeKB, na.rm = TRUE))
installs_by_mean_size <- aggregate(googleplaystore_original[, 13], list(googleplaystore_original$Installs), mean, na.rm=TRUE)
colnames(installs_by_mean_size)[1] <- "Installs"
colnames(installs_by_mean_size)[2] <- "MeanSize"
installs_by_mean_size
ggplot(installs_by_mean_size, aes(x=MeanSize, y= Installs)) +geom_point()

#Density graph now shows that Price is right skewed with an average of 1.027368
#The scatter plot shows that there is a slight negative relationship between average app price and number of Installs
ggplot(googleplaystore_original, aes(Price)) +geom_density()
summarise(googleplaystore_original, avg = mean(Price, na.rm = TRUE))
installs_by_mean_price <- aggregate(googleplaystore_original[, 8], list(googleplaystore_original$Installs), mean, na.rm=TRUE)
colnames(installs_by_mean_price)[1] <- "Installs"
colnames(installs_by_mean_price)[2] <- "MeanPrice"
installs_by_mean_price
ggplot(installs_by_mean_price, aes(x=MeanPrice, y= Installs)) +geom_point()+xlim(0,3.2)


#The bar chart shows that paid Apps had less downloads than free Apps and less Apps where paid for
ggplot(googleplaystore_original, aes(Installs, fill=Type)) +geom_bar()+facet_grid(Type ~ .)
ggplot(googleplaystore_original, aes(AdjustedInstall, fill=Type)) +geom_bar()+facet_grid(Type ~ .)

#The bar chart below shows that apps that require a minimum install versions in the 4s are the most downloaded accross all install levels
#This followed by apps that require version 2s
ggplot(filter(googleplaystore_original, MinimumVer!="NA"), aes( x= AdjustedInstall, fill =MinimumVer)) +geom_bar(position = "dodge")

#The bar chart below shows that apps that require a minimum install version in the 2s were downloaded most between 2011 and 2015
#Downloads of apps requiring version 4s took over in 2016, and grew exponentialy all the way to 2018
ggplot(filter(googleplaystore_original, MinimumVer!="NA"), aes( x= Year, fill =MinimumVer)) +geom_bar(position = "dodge")

#Top performing app categories (99 percentile or more in Reviews, rating and number of Installs) are Gaming apps, Communication apps, Family apps and Social apps
top<-filter(googleplaystore_original, Reviews >=quantile(googleplaystore_original$Reviews, c(.99), na.rm = TRUE)& Reviews >=quantile(googleplaystore_original$Rating, c(.99), na.rm = TRUE)& quantile(as.numeric(as.character(googleplaystore_original$Installs)), c(.99), na.rm = TRUE))
arrange(top_n(count(top, Category), n=3),desc(n))

#Change Installs variable back to category type
googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)

#Split into training and testing set
set.seed(3000)
split = sample.split(googleplaystore_original$Installs, SplitRatio = 0.7)
training = subset(googleplaystore_original, split==TRUE)
testing = subset(googleplaystore_original, split==FALSE)
#Build the Classification tree model
tree = rpart(AdjustedInstall2 ~ Rating+Price+SizeKB+MinimumVer, data =training, method="class", control = rpart.control(minbucket = 100))
#Plot the decision tree model
rpart.plot(tree)
#Predict for the training set using the Classisfication tree model
treepredict = predict(tree,  type="class")
t<-table(training$AdjustedInstall2, treepredict)
#Get prediction accuracy of training set
sum(diag(t))/sum(t)
#Predict for the testing set using the Classisfication tree model
treepredict1 = predict(tree, newdata = testing, type="class")
t1<-table(testing$AdjustedInstall2, treepredict1)
#Get prediction accuracy of testing set and compare with accuracy of training set
sum(diag(t1))/sum(t1)
#Get the AUC for each classification type
treepredictprob1 = predict(tree, newdata = testing, type="prob")
auc(testing$AdjustedInstall2, treepredictprob1[,1])
auc(testing$AdjustedInstall2, treepredictprob1[,2])
auc(testing$AdjustedInstall2, treepredictprob1[,3])
#Plot the AUROC for each classification type
plot(roc(testing$AdjustedInstall2, treepredictprob1[,1]))
plot(roc(testing$AdjustedInstall2, treepredictprob1[,2]))
plot(roc(testing$AdjustedInstall2, treepredictprob1[,3]))

