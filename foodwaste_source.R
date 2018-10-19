# libraries
library(tidyverse)

# Code for creating visualization of california agricultural production
# from 2012 to 2016 with waste figures at different levels of production
# for all categories of crops and animal products.

# read in crop table files
crop_2016 <- read.table(file="calcrop2016.txt", header=TRUE, sep=" ")
crop_2015 <- read.table(file="calcrop2015.txt", header=TRUE, sep=" ")
crop_2014 <- read.table(file="calcrop2014.txt", header=TRUE, sep=" ")
crop_2013 <- read.table(file="calcrop2013.txt", header=TRUE, sep=" ")
crop_2012 <- read.table(file="calcrop2012.txt", header=TRUE, sep=" ")

# read in animal table files
animal_2016 <- read.table(file="calanim2016.txt", header=TRUE, sep=" ")
animal_2015 <- read.table(file="calanim2015.txt", header=TRUE, sep=" ")
animal_2014 <- read.table(file="calanim2014.txt", header=TRUE, sep=" ")
animal_2013 <- read.table(file="calanim2013.txt", header=TRUE, sep=" ")
animal_2012 <- read.table(file="calanim2012.txt", header=TRUE, sep=" ")

# read in file with yearly output and input costs
ag_bal <- read.table(file="cal_rev_cost_2012_2016.txt", header=TRUE, sep=" ")

# clip unnecessary columns from crop tables 
crop_2016 <- crop_2016[1:6]
crop_2015 <- crop_2015[1:6]
crop_2014 <- crop_2014[1:6]
crop_2013 <- crop_2013[1:6]
crop_2012 <- crop_2012[1:6]

# clip unnecessary columns from animal tables 
animal_2012 <- animal_2012[1:5]
animal_2013 <- animal_2013[1:5]
animal_2014 <- animal_2014[1:5]
animal_2015 <- animal_2015[1:5]
animal_2016 <- animal_2016[1:5]

# rename columns by year (unused)
#names(crop_2016)[2:6] <- paste(names(crop_2016[2:6]), "_2016", sep="")

# add year columns
crop_2016$year <- 2016
crop_2015$year <- 2015
crop_2014$year <- 2014
crop_2013$year <- 2013
crop_2012$year <- 2012
animal_2012$year <- 2012
animal_2013$year <- 2013
animal_2014$year <- 2014
animal_2015$year <- 2015
animal_2016$year <- 2016


# merge (used different approach)
#cal_crop <- merge(crop_2016[1:6], crop_2015[1:6],
#                 by="commodity", all=TRUE)

# put yearly crop data in single df
cal_crop <- rbind(crop_2016, crop_2015)
cal_crop <- rbind(cal_crop, crop_2014)
cal_crop <- rbind(cal_crop, crop_2013)
cal_crop <- rbind(cal_crop, crop_2012)

# put yearly animal data in single df
cal_animal <- rbind(animal_2016, animal_2015)
cal_animal <- rbind(cal_animal, animal_2014)
cal_animal <- rbind(cal_animal, animal_2013)
cal_animal <- rbind(cal_animal, animal_2012)

# create a crop production df 
cal_crop_prod <- cal_crop %>% select(commodity, year, production_kton)

# make index of NA values for production
prod_na <- which(is.na(cal_crop_prod[3]))

# find averages for missing production values
miss_prod_avg <- cal_crop_prod[!is.na(cal_crop_prod[3]),] %>%
                   filter(commodity %in% cal_crop_prod[prod_na,1]) %>%
                   group_by(commodity) %>%
                   summarise(avg = mean(production_kton))

# fill missing production values with 90% of average
cal_crop_prod[prod_na,3] <- miss_prod_avg[,2] * .9

# find missing commodities
# create yearly commodity lists
commod_2012 <- cal_crop_prod %>%
                 filter(year==2012) %>%
                 select(commodity)

commod_2013 <- cal_crop_prod %>%
  filter(year==2013) %>%
  select(commodity)

commod_2014 <- cal_crop_prod %>%
  filter(year==2014) %>%
  select(commodity)

commod_2015 <- cal_crop_prod %>%
  filter(year==2015) %>%
  select(commodity)

commod_2016 <- cal_crop_prod %>%
  filter(year==2016) %>%
  select(commodity)

commod_2012 <- commod_2012[[1]]
commod_2013 <- commod_2013[[1]]
commod_2014 <- commod_2014[[1]]
commod_2015 <- commod_2015[[1]]
commod_2016 <- commod_2016[[1]]

# check years to make sure that commodity names match
commod_2012[!commod_2012 %in% commod_2013]
commod_2013[!commod_2013 %in% commod_2012]
# no processed spinach after 2012
commod_2013[!commod_2013 %in% commod_2014]
commod_2013[!commod_2013 %in% commod_2015]
# 2013-2015 contain all the same commodities
commod_2013[!commod_2013 %in% commod_2016]
commod_2016[!commod_2016 %in% commod_2013]
# Processed and Fresh Market Strawberries combined in 2016
# Plumes and Prunes (Dried Plums) combined in 2016

# Create split 2012-2015 to divide combined 2016 commodities
split_com <- cal_crop_prod %>%
               filter(commodity %in% c("Strawberries_FM","Strawberries_Proc",
                                       "Plums", "Plums_Dried")) %>%
               group_by(commodity) %>%
               summarise(total_prod = sum(production_kton))

Straw_FM_split <- split_com[3,2]/(split_com[3,2]+split_com[4,2])
Straw_Proc_split <- split_com[4,2]/(split_com[3,2]+split_com[4,2])
Plums_split <- split_com[1,2]/(split_com[1,2]+split_com[2,2])
Plums_Dried_split <- split_com[2,2]/(split_com[1,2]+split_com[2,2])

# split 2016 data for strawberries and plums into seperate rows
Straw_2016 <- cal_crop_prod[which(cal_crop_prod[1] == "Strawberries_All"),3]
Plum_2016 <- cal_crop_prod[which(cal_crop_prod[1] == "Plums_Prunes"),3]
cal_crop_prod[nrow(cal_crop_prod)+1,] <- c("Strawberries_Proc", 2016, Straw_2016 * Straw_Proc_split)
cal_crop_prod[nrow(cal_crop_prod)+1,] <- c("Plums_Dried", 2016, Plum_2016 * Plums_Dried_split)
cal_crop_prod[which(cal_crop_prod[1] == "Strawberries_All"),] <- c("Strawberries_FM", 2016, Straw_2016 * Straw_FM_split)
cal_crop_prod[which(cal_crop_prod[1] == "Plums_Prunes"),] <- c("Plums", 2016, Plum_2016 * Plums_split)

# cal_crop_prod now has clean data for all 5 years

# create a animal production df 
cal_animal_prod <- cal_animal %>% select(commodity, year, marketings_klbs)
# remove negligible chicken production
cal_animal_prod <- cal_animal_prod %>% filter(commodity != "Chickens")

# convert Keggs to Klbs
egg_wgt <- 0.131
egg_dex <- which(cal_animal_prod[1]=="Eggs")
cal_animal_prod[egg_dex,3] <- cal_animal_prod[egg_dex,3] * egg_wgt

# convert Klbs to Ktons for all animal products
cal_animal_prod[,3] <- cal_animal_prod[,3]/2000
names(cal_animal_prod) <- c(names(cal_animal_prod)[1:2], "marketings_ktons")

# sum production over years of crop data.
cal_total_crop <- cal_crop_prod %>% 
  group_by(commodity) %>%
  summarise(years=n(), production=sum(production_kton))

# load table for crop waste numbers (created by hand)
cal_crop_waste <- read.table(file="calcropwaste.txt", header=TRUE, sep=" ")
cal_crop_waste <- cal_crop_waste[2:6]


# sum production over years of animal data.
cal_total_animal <- cal_animal_prod %>% 
  group_by(commodity) %>%
  summarise(years=n(), production=sum(marketings_ktons))

# create animal waste table by hand, commented out for future runs
#cal_animal_waste <- edit(cal_total_animal)

# save created animal waste table, commented out for future runs
# write.csv(cal_animal_waste, "cal_animal_waste.csv", row.names=FALSE)

# Read in previously created animal waste table
cal_animal_waste <- read.csv(file="cal_animal_waste.csv")

# turn NAs into 0s for crop waste table
index_na2 <- which(is.na(cal_crop_waste[2]))
index_na3 <- which(is.na(cal_crop_waste[3]))
cal_crop_waste[index_na2,2] <- 0.0
cal_crop_waste[index_na3,3] <- 0.0

# Create and save a crop type table by hand, commented out for future runs
# cal_crop_type <- edit(cal_total_crop[1])
# write.csv(cal_crop_type, "cal_crop_type.csv", row.names=FALSE)

# read in crop type file
cal_crop_type <- read.csv(file="cal_crop_type.csv")

# create table with all crop values
cal_crop_full <- merge(cal_total_crop, cal_crop_type, by="commodity", all=TRUE)
cal_crop_full <- merge(cal_crop_full, cal_crop_waste, by="commodity", all=TRUE)

# create table with all animal values
cal_animal_full <- merge(cal_total_animal, cal_animal_waste, by="commodity", all=TRUE)

# remove peppermint and feed, not food products
cal_crop_full <- cal_crop_full %>% filter(commodity != "Peppermint" & commodity != "Hay")

# create crop usage table
cal_crop_use <- 
  data_frame('Commodity'=cal_crop_full$commodity,
             'Type'= cal_crop_full$type,
             'Unharvested'=cal_crop_full$production*cal_crop_full$unharvested,
             'Ugly_and_Blemished'=cal_crop_full$production*cal_crop_full$ugly_blemished,
             'Eaten'=cal_crop_full$production*(1-(cal_crop_full$retail+cal_crop_full$consumer)),
             'Retail_Waste'=cal_crop_full$production*cal_crop_full$retail,
             'Home_Waste'=cal_crop_full$production*cal_crop_full$consumer)

# create animal usage table
cal_animal_use <- 
  data_frame('Commodity'=cal_animal_full$commodity,
             'Eaten'=cal_animal_full$production*(1-(cal_animal_full$Retail+cal_animal_full$Consumer)),
             'Retail_Waste'=cal_animal_full$production*cal_animal_full$Retail,
             'Home_Waste'=cal_animal_full$production*cal_animal_full$Consumer)

# create tables for graphing
cal_crop_graph <- cal_crop_use %>%
  gather(key=Usage, value=Ktons,Eaten,Retail_Waste,Home_Waste)
cal_animal_graph <- cal_animal_use %>%
  gather(key=Usage, value=Ktons,Eaten,Retail_Waste,Home_Waste)

# fix order of usage for proper graph stacking
cal_crop_graph$Usage <- factor(cal_crop_graph$Usage)
cal_crop_graph$Usage <- factor(cal_crop_graph$Usage,
                               levels=c("Home_Waste","Retail_Waste","Eaten"), ordered=TRUE)
cal_animal_graph$Usage <- factor(cal_animal_graph$Usage)
cal_animal_graph$Usage <- factor(cal_animal_graph$Usage,
                               levels=c("Home_Waste","Retail_Waste","Eaten"), ordered=TRUE)

# create reusable themes and legends
theme_crop <-
  theme(axis.text.x = element_text(color="black", size=9, angle=45),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
        plot.title = element_text(face="bold", color="blue", size=16, hjust=0.5),
        axis.title.x = element_text(color="blue3", size=14, face="bold"),
        axis.title.y = element_text(color="coral", size=14, face="bold"))

theme_crop_sm <- 
  theme(axis.text.x = element_text(color="black", size=9, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
        plot.title = element_text(face="bold", color="blue", size=16, hjust=0.5),
        axis.title.x = element_text(color="blue3", size=14, face="bold"),
        axis.title.y = element_text(color="coral", size=14, face="bold"))

 
legend_crop <- scale_fill_manual(name="How Used", 
                                 labels = c("Home Waste", "Retail Waste", "Eaten"), 
                                 values = c("Home_Waste"="orange", 
                                            "Retail_Waste"="yellow", 
                                            "Eaten"="green"))
  
# graph of all crops waste at retail and home level
cal_crop_graph %>% 
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
  geom_col() +
  labs(title="Total California Food Crop Production 2012-2016", x="Crop",
       y="Production in 1000s of tons") +
  theme_crop +
  legend_crop



# graph of all crops waste at retail and home level w/o processed tomatoes and wine grapes
cal_crop_graph %>% 
  filter(Commodity != "Tomatoes_Proc" & Commodity != "Grapes_Wine" ) %>%
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
    geom_bar(stat="identity") +
    labs(title="California Food Crop Production and Waste, 2012-2016\n w/o processed tomatoes and wine grapes",
         x="Crop", y="Production in 1000s of tons") +
    legend_crop + 
    theme_crop


# graph of nut crops waste at retail and home level
cal_crop_graph %>% 
  filter(Type=="nut") %>%
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
    geom_bar(stat="identity") +
    labs(title="Total California Nut Crop Production 2012-2016",
         x="Crop", y="Production in 1000s of tons") +
    legend_crop +
    theme_crop_sm

# graph of grain, sugar and oil crops waste at retail and home level
cal_crop_graph %>% 
  filter(Type=="grain" | Type=="sugar" | Type=="oil") %>%
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
  labs(title="Total California Grain, Sugar and Oil Crop Production 2012-2016",
       x="Crop", y="Production in 1000s of tons") +
  legend_crop +
  theme_crop_sm

# graph of fruit crop waste at retail and home level w/o wine grapes
cal_crop_graph %>% 
  filter(Type=="fruit" & Commodity != "Grapes_Wine") %>%
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
  labs(title="Total California Fruit Crop Production 2012-2016\n without wine grapes",
       x="Crop", y="Production in 1000s of tons") +
  legend_crop + 
  theme_crop

# graph of vegetable crop waste at retail and home level w/o wine grapes
cal_crop_graph %>% 
  filter(Type=="vegetable" & Commodity != "Tomatoes_Proc") %>%
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
  labs(title="Total California Vegetable Crop Production 2012-2016\n without processed tomatoes",
       x="Crop", y="Production in 1000s of tons") +
  legend_crop + 
  theme_crop


# graph of animal production waste at retail and home level
cal_animal_graph %>% 
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
  labs(title="Total California Animal Production 2012-2016",
       x="Animal Product", y="Production in 1000s of tons") +
  legend_crop +
  theme_crop_sm

# graph of animal production waste at retail and home level w/o Diary
cal_animal_graph %>% 
  filter(Commodity != "Dairy") %>%
  ggplot(aes(x=Commodity,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
  labs(title="Total California Animal Production 2012-2016\n without Dairy Production",
       x="Animal Product", y="Production in 1000s of tons") +
  legend_crop +
  theme_crop_sm

# compare crops by type
cal_crop_type_graph <- cal_crop_use %>%
  group_by(Type) %>%
  summarise(Eaten=sum(Eaten), Retail_Waste=sum(Retail_Waste), Home_Waste=sum(Home_Waste)) %>%
  gather(key=Usage, value=Ktons,Eaten,Retail_Waste,Home_Waste) 
  
cal_crop_type_graph$Usage <- factor(cal_crop_type_graph$Usage)
cal_crop_type_graph$Usage <- factor(cal_crop_type_graph$Usage,
                               levels=c("Home_Waste","Retail_Waste","Eaten"), ordered=TRUE)

cal_crop_type_graph %>%
  ggplot(aes(x=Type,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
    labs(title="California Crop Production by Type of Crop 2012-2016",
         x="Crop Type", y="Production in 1000s of tons") +
    legend_crop +
    theme_crop_sm
  
# Create combined table to compare animal and crop production and waste.
cal_crop_total_use <- cal_crop_use %>%
  summarise(Type="Crop", Eaten=sum(Eaten), Retail_Waste=sum(Retail_Waste), Home_Waste=sum(Home_Waste))
cal_animal_total_use <- cal_animal_use %>%
  summarise(Type="Animal",Eaten=sum(Eaten), Retail_Waste=sum(Retail_Waste), Home_Waste=sum(Home_Waste))  
cal_total_use <- rbind(cal_crop_total_use,cal_animal_total_use)
cal_total_graph <- cal_total_use %>%
  gather(key=Usage, value=Ktons,Eaten,Retail_Waste,Home_Waste)
cal_total_graph$Usage <- factor(cal_total_graph$Usage)
cal_total_graph$Usage <- factor(cal_total_graph$Usage,
                                    levels=c("Home_Waste","Retail_Waste","Eaten"), ordered=TRUE)


# Compare animal products to crops
cal_total_graph %>% 
  ggplot(aes(x=Type,y=Ktons,fill=Usage)) +
  geom_bar(stat="identity") +
  labs(title="California Crop and Animal Production 2012-2016",
       x="Product Type", y="Production in 1000s of tons") +
  legend_crop +
  theme_crop_sm +
  geom_text(aes(label=paste(round(Ktons/1000, 1),"M Tons"),
                y=c(140000,70000,161000,85000, 220000, 105000))) +
  geom_text(aes(label=paste("Total Eaten:",paste(round((Ktons[1]+Ktons[2])/1000, 1),"M Tons"),sep='\n'),
                y=150000, x="Animal"), color="green", size=6) +
  geom_text(aes(label=paste("Total Home and","Retail Waste:",paste(round(sum(Ktons[3:6])/1000, 1),"M Tons"), sep='\n'),
                y=200000, x="Animal"), color="goldenrod1", size=6)




# look at comparable costs to produce animal vs. plant based products
# reduce table to total production and seperable inputs for crops and livestock
cost_comp <-  ag_bal %>%
  filter(Item %in% c("Crop_Value_Total","Livestock_Value_Total","Feed_purchased",
                     "Livestock_poultry_purchased","Seed_purchased","Fertilizers_lime",
                     "Pesticides","Petroleum_fuel","Electricity"))

# flip table so years are observations, convert $1000 values to numerics
cost_comp <- t(cost_comp)
colnames(cost_comp) <- cost_comp[1,]
cost_df <- data_frame(year=rownames(cost_comp)[2:6],cost_comp[2:6,1],cost_comp[2:6,2],
                      cost_comp[2:6,3],cost_comp[2:6,4],cost_comp[2:6,5],cost_comp[2:6,6],
                      cost_comp[2:6,7],cost_comp[2:6,8],cost_comp[2:6,9])
colnames(cost_df)[2:10] <- colnames(cost_comp)
for (i in 2:10) {
  cost_df[i] <- as.numeric(cost_df[[i]])
}
# Sum costs for crops and livestock
cost_df <- cost_df %>%
  mutate(Crop_Cost_Total=Seed_purchased+Fertilizers_lime+Pesticides+Petroleum_fuel,
         Livestock_Cost_Total=Feed_purchased+Livestock_poultry_purchased+Electricity)

# create reduced table with ratio of cost to total value
cost_red <- cost_df %>%
  select(1:3,11:12)
cost_red <- cost_red %>%
  mutate(Crop_Cost_Ratio=Crop_Cost_Total/Crop_Value_Total, 
         Livestock_Cost_Ratio=Livestock_Cost_Total/Livestock_Value_Total)

cost_red$year <- as.integer(str_sub(cost_red$year,2,5))

# show year by year graph of cost of crop production
cost_red %>% 
  ggplot(aes(x=year,y=Crop_Value_Total/1000000)) +
  geom_col(fill ="blue", color="black") +
  geom_col(data = cost_red, aes(x=year, y=Crop_Cost_Total/1000000), fill="goldenrod", color="black") +
  labs(title="California Crop Value and Cost of inputs 2012-2016\n without labor and capital equipment costs",
       x="Year", y="Value in Billions of Dollars") +
  coord_flip() +
  theme_crop_sm + 
  geom_text(aes(label=paste(round(Crop_Value_Total/1000000, 1),"B"),
                y=Crop_Value_Total/1000000+1.5)) +
  geom_text(aes(label=paste(round(Crop_Cost_Total/1000000, 1),"B"),
                y=Crop_Cost_Total/1000000-1.3))+
  geom_text(aes(label="Cost of Inputs", x=2016.7,y=4.5), size= 6, color="goldenrod") +
  geom_text(aes(label="Total Value", x=2016.7,y=30), size= 6, color="blue")

# show year by year graph of cost of livestock production
cost_red %>% 
  ggplot(aes(x=year,y=Livestock_Value_Total/1000000)) +
  geom_col(fill ="blue", color="black") +
  geom_col(aes(x=year, y=Livestock_Cost_Total/1000000), fill="goldenrod", color="black") +
  labs(title="California Animal Product Value and Cost of inputs 2012-2016\n without labor and capital equipment costs",
       x="Year", y="Value in Billions of Dollars") +
  coord_flip() +
  theme_crop_sm + 
  geom_text(aes(label=paste(round(Livestock_Value_Total/1000000, 1),"B"),
                y=Livestock_Value_Total/1000000+.6)) +
  geom_text(aes(label=paste(round(Livestock_Cost_Total/1000000, 1),"B"),
                y=Livestock_Cost_Total/1000000-.6))+
  geom_text(aes(label="Cost of Inputs", x=2016.7,y=2.5), size= 6, color="goldenrod") +
  geom_text(aes(label="Total Value", x=2016.7,y=13), size= 6, color="blue")


# create 2015 produce production table
wonky_2015 <- data_frame(commodity=rep(c("Fruit", "Citrus", "Vegetable"),2))
wonky_2015$region <- c(rep("US",3), rep("California",3))
wonky_2015$marketable <- c(14078,3794,21762, sum(crop_2015[c(25:34,36,37,39,41,42,44:46,49,50),5]), 
                        3138, sum(crop_2015[1:22,5]))
wonky_2015 <- wonky_2015 %>%
  mutate(unharvest = marketable*0.1, discard = marketable*0.2)

# create produce table for visualization
wonky_graph <- wonky_2015 %>%
  gather(key=use, value=Ktons,marketable,unharvest,discard) 
wonky_graph$use <- factor(wonky_graph$use)
wonky_graph$use <- factor(wonky_graph$use,
                                levels=c("discard","unharvest","marketable"), ordered=TRUE)

# create legend and theme for wonkey
legend_wonky <- scale_fill_manual(name="Produce Destination\nDue to Appearance ", 
                                 labels = c("Harvested but\nnot Marketable", "Not Harvested", "Marketable"), 
                                 values = c("discard"="chartreuse3", 
                                            "unharvest"="darkseagreen1", 
                                            "marketable"="blue"))

theme_wonky <-
  theme(axis.text.x = element_text(face="bold", color="maroon", size=10, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=10, angle=0),
        plot.title = element_text(face="bold", color="blue", size=16, hjust=0.5),
        axis.title.x = element_text(color="coral", size=14, face="bold"),
        axis.title.y = element_text(color="coral", size=14, face="bold"))

# Graph California Ugly produce
wonky_graph %>% 
  filter(region=="California") %>%
  ggplot(aes(x=commodity,y=Ktons,fill=use)) +
  geom_bar(stat="identity") +
  labs(title="California Produce Discarded or Unharvested\ndue to Blemishes, Ugliness or Size, 2015",
       x="Produce Type", y="Production in 1000s of tons") +
  legend_wonky +
  theme_wonky +
  geom_text(aes(label=paste(round(Ktons, 0),"KTons"),
                y=c(6200,2900,10000,6900,3350,11000,8200,3900,13000)), size=3.5) 

# Graph US Ugly produce
wonky_graph %>% 
  filter(region=="US") %>%
  ggplot(aes(x=commodity,y=Ktons,fill=use)) +
  geom_bar(stat="identity") +
  labs(title="US Produce Discarded or Unharvested\ndue to Blemishes, Ugliness or Size, 2015",
       x="Produce Type", y="Production in 1000s of tons") +
  legend_wonky +
  theme_wonky +
  geom_text(aes(label=paste(round(Ktons, 0),"KTons"),
                y=c(13500,3400,21000,15000,4200,23000,17600,5500,27000)), size=3.5) 

# Total all types of produce
wonky_graph %>%  
  group_by(region, use) %>%
  summarize(Ktons=sum(Ktons)) %>%
  ggplot(aes(x=region,y=Ktons,fill=use)) +
  geom_bar(stat="identity") +
  labs(title="Total Produce Discarded or Unharvested\ndue to Blemishes, Ugliness or Size, 2015",
       x="Total Produce", y="Production in 1000s of tons") +
  legend_wonky +
  theme_wonky +
  geom_text(aes(label=paste(round(Ktons, 0),"KTons"),
                y=c(24000,21000,18000,50000,42000,38000)), size=3.5) 




