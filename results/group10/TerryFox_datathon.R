# Terry Fox datathon

library(plyr)
library(dplyr)
library(ggplot2)

df_address <- read.csv("address_names.csv", stringsAsFactors = FALSE)
df_final <- read.csv("final_terry.csv", stringsAsFactors = FALSE)
df_school <- read.csv("NSRD 2014 School sites.csv", stringsAsFactors = FALSE)
df_online_data <- read.csv("NSRD online data.csv", stringsAsFactors = FALSE)
df_paper_data <- read.csv("NSRD paper data.csv", stringsAsFactors = FALSE)
df_fundraisers <- read.csv("TFR 2014 online fundraisers.csv", stringsAsFactors = FALSE)
df_TFR <- read.csv("TFR 2014 participant and option 1 data.csv", stringsAsFactors = FALSE)
df_run_sites <- read.csv("TFR 2014 Run sites.csv", stringsAsFactors = FALSE)

dim(df_address)  #25r; 4c
    # (from Canada Post; streets with Terry Fox in name)

dim(df_final)  #216,614r; 19c
#[1] "X2014.Batch.ID"   "City"             "Country"          "PC"               "PROV"            
# [6] "Special.Note"     "TFF.OFFICE"       "Unnamed..15"      "Unnamed..16"      "Unnamed..17"     
# [11] "Unnamed..18"      "donation"         "site"             "last_name_hashed" "Address1_hashed" 
# [16] "Address2_hashed"  "receipt_num"      "email_hashed"     "PC_REDUCE"  

dim(df_school)  #9,803r; 7c
    #"List of schoolname, site number"

dim(df_online_data)  #5,022r; 7c  
    #"Students who have fundraised using the online system"

dim(df_paper_data)  #9,060r; 7c
    #"Amount raised by students with pledge sheets per school"

dim(df_fundraisers)  #9,960r; 7c
    #"list of those who participated in Terry Fox Run and registered or donated online"

dim(df_TFR)  #27,470r; 7c
    #"list of those who participate in Terry Fox Run 2014"

dim(df_run_sites)  #827r; 5c
    #"list of Terry Fox run - run sites, city not available"

###################################################################################
### NSRD2014
################################
# df_school (to delete: rows 9798-9803)
# df_online_data (to delete: rows 5017-5022)
# df_paper_data (to delete: rows 9055-9060)("Postal.Code" column: all "NA")

table(df_paper_data[1:9797,]$City, exclude = NULL)

###################################################################################
### TFR
################################
# df_run_sites (to delete: rows 822-827, column 1(all "TFR2014"))
# df_fundraisers (to delete: rows 9955-9960, column 1(all "TFR2014"))
# df_TFR (ok)(not useful: column 1(all "TFR2014"))

TFR_cities <- data.frame(unique(df_TFR$City), stringsAsFactors = FALSE)
colnames(TFR_cities) <- c("City")
arranged_TFR_cities <- arrange(TFR_cities, City)
nrow(arranged_TFR_cities)  #2299 rows
#1-500: to "cremona"
#501-1000: "Cremona" to "Kinmount"
#1001-1500: "Kippens" to "Piapot"
#1501-2299: "Pickering" to "Zurich"

TFR_gpdbyCity <- group_by(df_TFR, City)
TFR_summariseAmount <- summarise(TFR_gpdbyCity, CityAmount = sum(Amount))
TFR_sorted_summariseAmount <- arrange(TFR_summariseAmount, desc(CityAmount))
## top 20 cities (not cleaned):
# City CityAmount
# 1         Toronto  147342.87   #or "TORONTO"
# 2                   65251.58   ##################delete############################
# 3          Barrie   53824.68 #ok
# 4         Calgary   49913.69 #ok
# 5        Edmonton   45120.20   #or "EDMONTON"
# 6         Kelowna   43928.21   #or "Kelowma"??? #################################################
# 7   Prince George   40347.70   #or "PRINCE GEORGE"
# 8          London   39685.36 #ok
# 9     Stouffville   31806.45   #or "STOUFFVILLE"
# 10      Vancouver   29459.40   #or "VANCOUVER"
# 11         Surrey   24644.83 #ok
# 12         Sarnia   24330.00 #ok
# 13         Ottawa   23222.55   #or "OTTAWA"
# 14     Huntsville   23179.55 #ok
# 15     Burlington   22288.25   #or "Burliington" or "BURLINGTON" ###############################
# 16  POINTE-CLAIRE   21595.00   #or "Pointe Claire" or "POINTE CLAIRE" ##########################
# 17         Guelph   20965.00 #ok
# 18       Winnipeg   20088.80   #or "WINNIPEG" 
# 19    Mississauga   19591.15   #or "Misissauga" ################################################
# 20    Scarborough   19134.20   #or "SCARBOROUGH"
# 21       Oakville   18772.50 #ok

filter(df_TFR, City == "TORONTO") ## total "Amount" == 20
filter(df_TFR, City == "EDMONTON")  ## total "Amount" == 0
filter(df_TFR, City == "Kelowma")  ## total "Amount" == 0
filter(df_TFR, City == "PRINCE GEORGE") ## total "Amount" == 30
filter(df_TFR, City == "STOUFFVILLE") ## total "Amount" == 20
filter(df_TFR, City == "VANCOUVER") ## total "Amount" == 5
filter(df_TFR, City == "OTTAWA") ## total "Amount" == 990
filter(df_TFR, City == "Burliington" | City == "BURLINGTON") ##REALIZED THERE'S A BURLINGTON IN
           ##ONTARIO AND IN QUEBEC; THUS, NEED TO GROUP BY CITY AND BY PROVINCE!!!
filter(df_TFR, City == "Pointe Claire" | City == "POINTE CLAIRE") ## total "Amount" == 55
filter(df_TFR, City == "WINNIPEG") ## total "Amount" == 350
filter(df_TFR, City == "Misissauga") ## total "Amount" == 400
filter(df_TFR, City == "SCARBOROUGH") ## total "Amount" == 20

table(df_TFR$Province, exclude = NULL)

TFR_Cityclean1 <- apply(df_TFR, 1, function(y) gsub("Kelowma", "Kelowna", y))
TFR_Cityclean2 <- apply(TFR_Cityclean1, 1, function(y) gsub("Burliington", "Burlington", y))
TFR_Cityclean3 <- apply(TFR_Cityclean2, 1, function(y) gsub("POINTE-CLAIRE", "Pointe Claire", y))
TFR_Cityclean4 <- apply(TFR_Cityclean3, 1, function(y) gsub("Misissauga", "Mississauga", y))

TFR_Cityclean5 <- data.frame(apply(TFR_Cityclean4, 2, toupper))

TFR_cities_top20clean <- data.frame(unique(TFR_Cityclean5$City), stringsAsFactors = FALSE)
colnames(TFR_cities_top20clean) <- c("City")
arranged_TFR_cities_top20clean <- arrange(TFR_cities_top20clean, City)
nrow(arranged_TFR_cities_top20clean) #2209 rows

TFR_Cityclean5$Amount <- as.numeric(as.character(TFR_Cityclean5$Amount))
TFR_Cityclean5$City <- as.character(TFR_Cityclean5$City)

TFR_Cityclean5_gpdbyCity <- group_by(TFR_Cityclean5, City)
TFR_Cityclean5_summariseAmount <- summarise(TFR_Cityclean5_gpdbyCity, CityAmount = sum(Amount))
TFR_Cityclean5_sorted_summariseAmount <- arrange(TFR_Cityclean5_summariseAmount, desc(CityAmount))

print.data.frame(TFR_Cityclean5_sorted_summariseAmount[1:21,])
## top 20 cities after cleaning
# City CityAmount
# 1        TORONTO  147362.87
# 2                  65251.58
# 3         BARRIE   53824.68
# 4        CALGARY   49913.69
# 5       EDMONTON   45120.20
# 6        KELOWNA   43928.21
# 7  PRINCE GEORGE   40377.70
# 8         LONDON   39685.36
# 9    STOUFFVILLE   31826.45
# 10     VANCOUVER   29464.40
# 11        SURREY   24644.83
# 12        SARNIA   24330.00
# 13        OTTAWA   24212.55
# 14    HUNTSVILLE   23179.55
# 15    BURLINGTON   22418.25
# 16 POINTE CLAIRE   21650.00
# 17        GUELPH   20965.00
# 18      WINNIPEG   20438.80
# 19   MISSISSAUGA   19991.15
# 20   SCARBOROUGH   19154.20
# 21      OAKVILLE   18772.50


### GROUP BY CITY AND PROVINCE

#check with uncleaned dataset:
TFR_gpdbyCityProv <- group_by(df_TFR, City, Province)
TFR_gpdbyCityProv_summariseAmount <- summarise(TFR_gpdbyCityProv, CPAmount = sum(Amount))
TFR_ungpd <- ungroup(TFR_gpdbyCityProv_summariseAmount)
TFR_sorted_ungpd <- arrange(TFR_ungpd, desc(CPAmount))

print.data.frame(TFR_sorted_ungpd[1:21,])
##uncleaned dataset:
# City Province  CPAmount
# 1        Toronto       ON 146142.87
# 2         Barrie       ON  53824.68
# 3                      NS  52729.53
# 4        Calgary       AB  48747.69
# 5       Edmonton       AB  44980.20
# 6        Kelowna       BC  43908.21
# 7  Prince George       BC  40292.70
# 8         London       ON  39665.36
# 9    Stouffville       ON  31786.45
# 10     Vancouver       BC  29319.40
# 11        Surrey       BC  24643.33
# 12        Sarnia       ON  24220.00
# 13    Huntsville       ON  23179.55
# 14        Ottawa       ON  22912.55
# 15    Burlington       ON  21980.75
# 16 POINTE-CLAIRE       QC  21595.00
# 17        Guelph       ON  20725.00
# 18      Winnipeg       MB  19948.80
# 19   Mississauga       ON  19591.15
# 20   Scarborough       ON  19134.20
# 21      Oakville       ON  18717.50


### use dataset with clean top 20 cities:
TFR_Cityclean5_gpdbyCityProv <- group_by(TFR_Cityclean5, City, Province)
TFR_Cityclean5_gpdbyCityProv_summariseAmount <- summarise(TFR_Cityclean5_gpdbyCityProv, CityProvAmount = sum(Amount))
TFR_Cityclean5_ungpd_summariseAmount <- ungroup(TFR_Cityclean5_gpdbyCityProv_summariseAmount)
TFR_Cityclean5_sorted_ungpd_summariseAmount <- arrange(TFR_Cityclean5_ungpd_summariseAmount, desc(CityProvAmount))

print.data.frame(TFR_Cityclean5_sorted_ungpd_summariseAmount[1:21,])
##with clean top 20 cities dataset:
# City Province CityProvAmount
# 1        TORONTO       ON      146142.87
# 2         BARRIE       ON       53824.68
# 3                      NS       52729.53
# 4        CALGARY       AB       48747.69
# 5       EDMONTON       AB       44980.20
# 6        KELOWNA       BC       43908.21
# 7  PRINCE GEORGE       BC       40292.70
# 8         LONDON       ON       39665.36
# 9    STOUFFVILLE       ON       31786.45
# 10     VANCOUVER       BC       29319.40
# 11        SURREY       BC       24643.33
# 12        SARNIA       ON       24220.00
# 13    HUNTSVILLE       ON       23179.55
# 14        OTTAWA       ON       22912.55
# 15    BURLINGTON       ON       22080.75
# 16 POINTE CLAIRE       QC       21650.00
# 17        GUELPH       ON       20725.00
# 18      WINNIPEG       MB       20298.80
# 19   MISSISSAUGA       ON       19991.15
# 20   SCARBOROUGH       ON       19134.20
# 21      OAKVILLE       ON       18717.50

#CHECK:
filter(TFR_Cityclean5, City == "TORONTO" & Province != "ON") # all postal codes are in ON
filter(TFR_Cityclean5, City == "CALGARY" & Province != "AB") # all postal codes are in AB or BC
filter(TFR_Cityclean5, City == "EDMONTON" & Province != "AB") # all postal codes are in AB
filter(TFR_Cityclean5, City == "KELOWNA" & Province != "BC") # all postal codes are in BC
filter(TFR_Cityclean5, City == "PRINCE GEORGE" & Province != "BC") # all postal codes are in BC
filter(TFR_Cityclean5, City == "LONDON" & Province != "ON") # all postal codes are in ON
filter(TFR_Cityclean5, City == "STOUFFVILLE" & Province != "ON") # all postal codes are in ON
filter(TFR_Cityclean5, City == "VANCOUVER" & Province != "BC") #all postal codes are in BC
filter(TFR_Cityclean5, City == "SURREY" & Province != "BC") #all postal codes are in BC
filter(TFR_Cityclean5, City == "SARNIA" & Province != "ON") #all postal codes are in ON
filter(TFR_Cityclean5, City == "OTTAWA" & Province != "ON") #all postal codes, except 1 NA, are in ON
#-----NOTE: there's a Burlington in NL
filter(TFR_Cityclean5, City == "BURLINGTON" & Province != "ON") #there's a Burlington in NL too
filter(TFR_Cityclean5, City == "GUELPH" & Province != "ON") #all postal codes are in ON
#-----NOTE: there's a record in BC (error?)
filter(TFR_Cityclean5, City == "WINNIPEG" & Province != "MB") #all postal codes, except 1 in BC, are in MB
filter(TFR_Cityclean5, City == "SCARBOROUGH" & Province != "ON") #all postal codes are in ON
filter(TFR_Cityclean5, City == "OAKVILLE" & Province != "ON") #all postal codes are in ON

### so: take the top 10 from the top20cities clean dataset NOT grouped by Province

### DATAVIZ

### use this dataset: TFR_Cityclean5_sorted_summariseAmount
top10 <- TFR_Cityclean5_sorted_summariseAmount[c(1,3:11),]
top10_ordered <- transform(top10, City = reorder(City, order(CityAmount, decreasing = TRUE)))

tfr <- ggplot(top10_ordered, aes(factor(City), CityAmount))
tfr + geom_bar(stat="identity") + labs(x = "City") + labs(y = "Amount (dollars)") +
  ggtitle("TFR2014: Top 10 Cities that Raised \n the Most Through Pledge Sheets") +
  theme(axis.text.x = element_text(angle = 90, size = 20, vjust = 0.5, hjust = 1)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 40)) +
  theme(axis.title.x = element_text(size = 40)) +
  theme(plot.title = element_text(face="bold", size=40))
 

