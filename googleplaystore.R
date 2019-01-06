googleplaystore_original <- read.csv("~/Data Science/googleplaystore_original.csv")
View(googleplaystore_original)
library("dplyr")
library("stringr")
library("ggplot2")

googleplaystore_original%>% distinct(Category)
googleplaystore_original$Category<- as.character(googleplaystore_original$Category)
googleplaystore_original <-transform(googleplaystore_original , Category = ifelse(Category == "1.9", "NA", Category))
distinct(googleplaystore_original, Rating)
googleplaystore_original$Rating<- as.character(googleplaystore_original$Rating)
googleplaystore_original <-transform(googleplaystore_original , Rating = ifelse(Rating == "NaN", "NA", Rating))
googleplaystore_original<-transform(googleplaystore_original, Rating = as.numeric(Rating))
filter(googleplaystore_original, grepl("[A-Z]",Reviews))
googleplaystore_original$Reviews<- as.character(googleplaystore_original$Reviews)
googleplaystore_original <-transform(googleplaystore_original , Reviews = ifelse(Reviews == "3.0M", "NA", Reviews))
googleplaystore_original$Size<- as.character(googleplaystore_original$Size)
googleplaystore_original <-transform(googleplaystore_original , Size = ifelse(Size == "Varies with device", "NA", Size))
googleplaystore_original$Size<- as.character(googleplaystore_original$Size)
googleplaystore_original <-transform(googleplaystore_original , Size = ifelse(Size == "1,000+", "NA", Size))
googleplaystore_original <-mutate(googleplaystore_original, SizeKB= case_when(grepl("M",Size) ~  as.numeric(sub("M", 
                                                                                                                      "", googleplaystore_original$Size))*1000,grepl("k",Size) ~  as.numeric(sub("k", "", googleplaystore_original$Size))*1 ))
googleplaystore_original <- mutate(googleplaystore_original, AdjustedSize= case_when(googleplaystore_original$SizeKB>1 & googleplaystore_original$SizeKB<501 ~ "Extremely Small",
                                                                                      googleplaystore_original$SizeKB>500 & googleplaystore_original$SizeKB<1001 ~ "Very Small",
                                                                                      googleplaystore_original$SizeKB>1000 & googleplaystore_original$SizeKB<5001 ~ "Small",
                                                                                      googleplaystore_original$SizeKB>5000 & googleplaystore_original$SizeKB<10001 ~ "Medium",
                                                                                      googleplaystore_original$SizeKB>10000 & googleplaystore_original$SizeKB<50000 ~ "Large",
                                                                                      googleplaystore_original$SizeKB>50001 & googleplaystore_original$SizeKB<100000 ~ "Very Large")) 

googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)
googleplaystore_original <-transform(googleplaystore_original , Installs = ifelse(grepl("\\+",googleplaystore_original
                                                                                        $Installs), sub("\\+", "", googleplaystore_original$Installs), Installs))
googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)
googleplaystore_original <-transform(googleplaystore_original , Installs = ifelse(grepl("[A-Z]", Installs), "NA", 
                                                                                  Installs))
googleplaystore_original$Installs<- as.character(googleplaystore_original$Installs)
googleplaystore_original<-transform(googleplaystore_original , Installs = gsub(",","", googleplaystore_original$Installs))
googleplaystore_original <- mutate(googleplaystore_original, AdjustedInstall= case_when(googleplaystore_original$Install=="0" ~ "No Install", 
                                                                                        googleplaystore_original$Install=="1"|googleplaystore_original$Install=="5"|googleplaystore_original$Install=="10"|googleplaystore_original$Install=="50"|googleplaystore_original$Install=="100"|googleplaystore_original$Install=="500" ~ "Extremely Small", 
                                                                                        googleplaystore_original$Install=="1,000"|googleplaystore_original$Install=="5,000"|googleplaystore_original$Install=="10,000" ~ "Very Small", 
                                                                                        googleplaystore_original$Install=="50,000"|googleplaystore_original$Install=="100,000"|googleplaystore_original$Install=="500,000" ~ "Small",
                                                                                        googleplaystore_original$Install=="1,000,000"|googleplaystore_original$Install=="5,000,000"|googleplaystore_original$Install=="10,000,000" ~ "medium", 
                                                                                        googleplaystore_original$Install=="50,000,000"|googleplaystore_original$Install=="100,000,000"|googleplaystore_original$Install=="500,000,000" ~ "Large", 
                                                                                        googleplaystore_original$Install=="1,000,000,000" ~ "Very Large"))
googleplaystore_original$Installs<- ordered(googleplaystore_original$Installs, levels=c("NA","0","1","5","10","50","100","500","1000","5000","10000","50000","100000","500000","1000000","5000000","10000000","50000000","100000000","500000000", "1000000000"))
googleplaystore_original$Type<- as.character(googleplaystore_original$Type)
googleplaystore_original%>% distinct(Type)
googleplaystore_original <-transform(googleplaystore_original , Type = ifelse(Type == "NaN", "NA", Type))
googleplaystore_original <-transform(googleplaystore_original , Type = ifelse(Type == "0", "NA", Type))
googleplaystore_original$Price<- as.character(googleplaystore_original$Price)
googleplaystore_original <-transform(googleplaystore_original , Price= ifelse(grepl("[A-Z]", Price), "NA", Price))
googleplaystore_original$Price<- as.character(googleplaystore_original$Price)
googleplaystore_original <-transform(googleplaystore_original , Price= ifelse(grepl("\\$", Price), sub("\\$", "", 
                                                                                                       googleplaystore_original$Price), Price))
googleplaystore_original%>% distinct(ContentRating)
googleplaystore_original$ContentRating<- as.character(googleplaystore_original$ContentRating)
googleplaystore_original <-transform(googleplaystore_original, ContentRating = ifelse(ContentRating == "", "NA", 
                                                                                      ContentRating))
googleplaystore_original%>% distinct(Genres)
googleplaystore_original$Genres<- as.character(googleplaystore_original$Genres)
googleplaystore_original <-transform(googleplaystore_original , Genres = ifelse(Genres == "11-Feb-18", "NA", Genres)

googleplaystore_original[which(grepl("[A-Z][a-z]({2})", googleplaystore_original$LastUpdated) ==FALSE), 11]
googleplaystore_original$LastUpdated<- as.character(googleplaystore_original$LastUpdated)
googleplaystore_original <-transform(googleplaystore_original , LastUpdated = ifelse(LastUpdated == "1.0.19", "NA", 
                                                                                     LastUpdated))
googleplaystore_original <- data.frame(googleplaystore_original, str_split_fixed(googleplaystore_original$LastUpdated, 
                                                                                 "-", 3))
colnames(googleplaystore_original )[16] <- "Day"
colnames(googleplaystore_original )[17] <- "Month"
colnames(googleplaystore_original )[18] <- "Year"
googleplaystore_original$Month<- as.character(googleplaystore_original$Month)
googleplaystore_original <-transform(googleplaystore_original , Month = ifelse(Month == "", "NA", Month))
googleplaystore_original$Year<- as.character(googleplaystore_original$Year)
googleplaystore_original <-transform(googleplaystore_original , Year = ifelse(Year== "", "NA", Year))
distinct(googleplaystore_original, AndroidVer)
googleplaystore_original$AndroidVer <- as.character(googleplaystore_original$AndroidVer )
googleplaystore_original <-transform(googleplaystore_original , AndroidVer = ifelse(AndroidVer == "Varies with device"|
                                                                                      AndroidVer =="NaN"|AndroidVer =="", "NA", AndroidVer))
write.csv(googleplaystore_original, file = "~/Data Science/googleplaystore_original_clean.csv")

#To see how number of Installs are affected by mean Ratings
mean_rate_by_installs <- aggregate(googleplaystore_original[, 3], list(googleplaystore_original$Installs), sum, na.rm=TRUE)
colnames(mean_rate_by_installs)[1] <- "Installs"
colnames(mean_rate_by_installs)[2] <- "MeanRating"