# script to calculate gdd
# Sept 20, 2022
# alinazengziyun@yahoo.com



# group by year??? or just look at one year


# experiment

data1 <- read.csv("output/dailyclim/dailytemp_provenance_EA FAGUSY Gömöry & Paule 2011_2011_2020.csv",header = TRUE)
data3 <- read.csv("output/dailyclim/dailytemp_provenance_EA FRAXEX Rosique-Esplugas 2021_2011_2020.csv",header = TRUE)

# later on I can combine all files in this folder so I can read at the same time
# and then choose maybe just one year and merge with my master file, using label and doy




# combine all the files in the folder
# https://stackoverflow.com/questions/47171374/cursor-sometimes-changes-from-a-vertical-line-to-an-underscore-in-rstudio#:~:text=The%20underscore%20means%20that%20you%20are%20in%20Overwrite,the%20rest%20of%20the%20line%20to%20move%20further.
library(dplyr)
install.packages("readr")
library(readr)
df <- list.files("Output/dailyclim", pattern="*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 


write.csv(df,"Output/dailyclim/all_provenances_dailyclim.csv", row.names = FALSE)

df <- read.csv("Output/dailyclim/all_provenances_dailyclim.csv",header = TRUE)
df$gdd_numbers <- ifelse(df$temp > 0, df$temp, 0) # this step is just to filter out everything that is below 0 celsius degree

# try grouping by studies and year


df <- df %>% dplyr::group_by(label, year, identifier)




# do only 2011 first :))))
df <- subset(df, df$year == "2011")

df <- df %>% dplyr::group_by(label, identifier, year)




df_test <- df%>% dplyr::group_by(label, identifier) %>% 
  summarise(gdd = cumsum(df$gdd_numbers),
            .groups = 'drop')
            
df_test <- df %>% dplyr::group_by(label, identifier, year) %>% 
  mutate(gdd = cumsum(df$gdd_numbers))


df_test2 <- df%>% dplyr::group_by(label, identifier, year) %>% 
  summarise(gdd = cumsum(df$gdd_numbers))   



df_test3 <- gapply(df, FUN = cumsum(df$gdd_numbers) )
            
df$gdd <- cumsum(df$gdd_numbers)


# nothing seem to be working.... maybe i need to list the files and do for i in XXXXXX, but still need to group_by year even if i do that





data2011 <- subset(data, data$year == "2011")

data2011$gdd_numbers <- ifelse(data2011$temp > 0, data2011$temp, 0)
data2011$gdd <- cumsum(data2011$gdd_numbers)

#hmm lemme see if this works with group by year


data <- dplyr::group_by(year)