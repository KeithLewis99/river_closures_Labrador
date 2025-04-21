# Set up a project to produce river_closure plots for Inland Compliance and Resource Management - Labrador: 2025-04-22)


# This is a template for setting up a new project in RStudio with GitHub
# - see the below link for directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - Version Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
#if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("data_derived"))dir.create("data_derived")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
#if(!dir.exists("ms"))dir.create("ms") # manuscript
#if(!dir.exists("report"))dir.create("report") #for rmd report
#if(!dir.exists("refs"))dir.create("refs") #for rmd report


# Created 2025-04-17

## 
# libraries ----
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readxl)
library(lubridate)
library(stringr)


# import ----
# import files in catch and convert to proper format
#create a pattern and bind directory to pattern
## excel ----
temp = list.files(path = "data-working/", pattern="*.xlsx", full.names = T)

serial <- stringr::str_extract(temp, "(?<=_)[0-9]{8,}(?=_)")
river <- stringr::str_match(temp, "_([^_]+)_")
river <- river[,2]
river[1] <- "CharBrook2"
rivers <- c("CharBrook2 River", "CharBrook1 River", "Eagle River1", "Hunt River1", "Hunt River2",         
"Hunt River - BoatDock",  "Hunt River - TroutPool", "StLewis River1", "StLewis River2", "Eagle River2",   
"Flowers River", "Sandhill River")
river.number <- c(NA, NA, 10, 2, 2, 2, 2, 179, 179, 10, 1, 11)
SFA <- c(NA, NA, 2, 1, 1, 1, 1, 2, 2, 2, 1, 2)

# import files as a list
ls_cell = (lapply(temp, read_excel))
# str(ls_cell)
# str(ls_cell,1)
# str(ls_cell[1])

# shows that variables differ by station
lapply(ls_cell, head, 1)
lapply(ls_cell[-c(7:10, 23, 32:34)], head, 1)
lapply(ls_cell[c(7:9, 23, 32:34)], head, 1)
lapply(ls_cell[c(10)], head, 1)


col_names1 <- c("id", "Date",  "Temp.C") #
ls_cell <- lapply(ls_cell, function(df){
   colnames(df) <- c("id", "Date",  "Temp.C") #
   return(df)
})


ls_cell <- Map(function(df, serial) {
   df$id_serial <- serial
   return(df)
}, ls_cell, serial)


ls_cell <- Map(function(df, river.number) {
   df$river.number <- river.number
   return(df)
}, ls_cell, river.number)

ls_cell <- Map(function(df, SFA) {
   df$SFA <- SFA
   return(df)
}, ls_cell, SFA)


ls_cell <- Map(function(df, rivers) {
   df$river <- rivers
   return(df)
}, ls_cell, rivers)

df_temp1 <- bind_rows(ls_cell)
# str(df_temp1)


# csv ---- 
## do skip 2 in the future so that you don't have to change the column names in the csv and then add in the column names
shin1 <- read.csv("data-working/Shinneys_River_Lower_26Sept2024_21340906.csv",  skip = 1, fileEncoding="latin1")
# str(shin1)
shin1 <- shin1[,1:3]
names(shin1)[names(shin1) == "Date.Time"] <- "Date"
shin1$Date <- format(strptime(shin1$Date, format = "%m/%d/%Y %H:%M"), "%m/%d/%y %H:%M")
# str(shin1)
########## the problem is with the above line

shin1$Date <- as.POSIXct(shin1$Date, format = "%m/%d/%y %H:%M")
shin1$id_serial <- "21340906"
shin1$river <- "Shinneys River Lower"
shin1$river.number <- 15
shin1$SFA <- 2



shin2 <- read.csv("data-working/shinneys_upper_sept_27_2024_21400367.csv",  skip = 1, fileEncoding="latin1")
# str(shin2)
shin2 <- shin2[, c(1, 2, 4)] 
names(shin2)[names(shin2) == "Date.Time"] <- "Date"

shin2$Date <- format(strptime(shin2$Date, format = "%m/%d/%y %I:%M:%S %p"), "%m/%d/%y %H:%M:%S")
shin2$Date <- as.POSIXct(shin2$Date, format = "%m/%d/%y %H:%M:%S")
shin2$id_serial <- "21400367"
shin2$river <- "Shinneys River Upper"
shin2$river.number <- 15
shin2$SFA <- 2


# combine ----
df_temp2 <- bind_rows(shin1, shin2)
# str(df_temp2)

# str(df_temp1)
df_lab <- bind_rows(df_temp1, df_temp2[, c(1, 2, 3, 4, 6, 7, 5)])
df_lab$Year <- as.integer(format(df_lab$Date, "%Y"))
#str(df_lab)
# 
rivers_csv <- c("Shinneys River Lower", "Shinneys River Upper")
rivers <- c(rivers, rivers_csv)



# seasons ----
## set dates ----
# take water, select columns, filer by year, and filter by date
season24 <- df_lab |> 
   select(Year, SFA, river.number, river, id_serial, Date, Temp.C) |> 
   distinct() |> 
   filter(Year == 2024) |> 
   filter(date(Date) >= as_date("2024-06-15") & date(Date) <= as_date("2024-09-15"))
#str(season24)


season23 <- df_lab |> 
   select(Year, SFA, river.number, river, id_serial, Date, Temp.C) |> 
   distinct() |> 
   filter(Year == 2023) |> 
   filter(date(Date) >= as_date("2023-06-15") & date(Date) <= as_date("2023-09-15"))


# status ----
## 20 ----
season20 <- bind_rows(season24, season23) |>
   group_by(Year, SFA, river.number, river, Date) |>
   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |>
   ungroup() |>
   mutate(above20 = Temp.C >= 20) |>
   group_by(Year, SFA, river.number, river) |>
   arrange(river.number, Date) |>
   mutate(period = NA,
          period = replace(period, above20 == lag(above20), 0),
          period = replace(period, above20 != lag(above20), 1),
          period = replace(period, is.na(period), 1),
          period = cumsum(period)) |>
   mutate(hoursdiff = difftime(lead(Date), Date, units = "hours"),
          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |>
    ungroup() |>
   group_by(Year, SFA, river.number, river, period, above20) |>
   mutate(hours = cumsum(hoursdiff)) |>
   ungroup()


closures20 <- season20 |>
   filter(above20 == TRUE) |>
   filter(hours >= 72) |>
   # group_by(Station, Serial, SFA, River.Number, River.Name, period) |>
   group_by(Year, SFA, river.number, river, period) |>
   summarise(days.closed = (max(hours)-72)/24,
             start = min(Date),
             stop = max(Date),
             Avg.Temp = mean(Temp.C),
             SD.Temp = sd(Temp.C)) |>
   ungroup()

## 19 ----
season19 <- bind_rows(season24, season23) |>
   group_by(Year, SFA, river.number, river, Date) |>
   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |>
   ungroup() |>
   mutate(above19 = Temp.C >= 19) |>
   group_by(Year, SFA, river.number, river) |>
   arrange(river.number, Date) |>
   mutate(period = NA,
          period = replace(period, above19 == lag(above19), 0),
          period = replace(period, above19 != lag(above19), 1),
          period = replace(period, is.na(period), 1),
          period = cumsum(period)) |>
   mutate(hoursdiff = difftime(lead(Date), Date, units = "hours"),
          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |>
   ungroup() |>
   group_by(Year, SFA, river.number, river, period, above19) |>
   mutate(hours = cumsum(hoursdiff)) |>
   ungroup()

closures19 <- season19 |>
   filter(above19 == TRUE) |>
   filter(hours >= 72) |>
   group_by(Year, SFA, river.number, river, period) |>
   summarise(days.closed = (max(hours)-72)/24,
             start = min(Date),
             stop = max(Date),
             Avg.Temp = mean(Temp.C),
             SD.Temp = sd(Temp.C)) |>
   ungroup()


## 18 ----
season18 <- bind_rows(season24, season23) |>
   group_by(Year, SFA, river.number, river, Date) |>
   summarise(Temp.C = mean(Temp.C, na.rm = TRUE)) |>
   ungroup() |>
   mutate(above18 = Temp.C >= 18) |>
   group_by(Year, SFA, river.number, river) |>
   arrange(river.number, Date) |>
   mutate(period = NA,
          period = replace(period, above18 == lag(above18), 0),
          period = replace(period, above18 != lag(above18), 1),
          period = replace(period, is.na(period), 1),
          period = cumsum(period)) |>
   mutate(hoursdiff = difftime(lead(Date), Date, units = "hours"),
          hoursdiff = as.numeric(str_remove(hoursdiff, "hours"))) |>
   ungroup() |>
   group_by(Year, SFA, river.number, river, period, above18) |>
   mutate(hours = cumsum(hoursdiff)) |>
   ungroup()

closures18 <- season18 |>
   filter(above18 == TRUE) |>
   filter(hours >= 72) |>
   group_by(Year, SFA, river.number, river, period) |>
   summarise(days.closed = (max(hours)-72)/24,
             start = min(Date),
             stop = max(Date),
             Avg.Temp = mean(Temp.C),
             SD.Temp = sd(Temp.C)) |>
   ungroup()




# df ----
df20 <- season20 |>
   left_join(closures20 |> mutate(status = 'closed'),
             join_by(Year, river, SFA, river.number, period)) |>
   mutate(status = replace(status, Date < start, "monitored")) |>
   mutate(status = replace(status, is.na(status), "open")) # |>
   # left_join(real.closures |> mutate(Year = 2024), join_by(Year, SFA, River.Number), relationship = "many-to-many")


df19 <- season19 |>
   left_join(closures19 |> mutate(status = 'closed'),
             join_by(Year, river, SFA, river.number, period)) |>
   mutate(status = replace(status, Date < start, "monitored")) |>
   mutate(status = replace(status, is.na(status), "open")) # |>
# left_join(real.closures |> mutate(Year = 2024), join_by(Year, SFA, River.Number), relationship = "many-to-many")

df18 <- season18 |>
   left_join(closures18 |> mutate(status = 'closed'),
             join_by(Year, river, SFA, river.number, period)) |>
   mutate(status = replace(status, Date < start, "monitored")) |>
   mutate(status = replace(status, is.na(status), "open")) # |>
# left_join(real.closures |> mutate(Year = 2024), join_by(Year, SFA, River.Number), relationship = "many-to-many")

# manipulations ----
# bind status for 18 & 19 to 20
df20 <- cbind(df20, status19 = df19$status)
df20 <- cbind(df20, status18 = df18$status)




# vertical lines ----
year <- 2023
temp <-  df20 |>
   select(Year, SFA, river.number, river, Date, Temp.C, status, status19, status18) |>
   filter(Year == year)
## set all rivers for 2023

temp1 <- temp[1:(length(rivers)), ]
temp1[1:(length(rivers)), 1:length(temp1)] <- NA
#str(temp1)

# this is for when the monitoring "should" start
source("closure_FUN.R")
v20_2023 <-  vert_lines_min(df20, rivers, 2023, status, "monitored")
v20_2023
 
v19_2023 <-  vert_lines_min(df20, rivers, 2023, status19, "monitored")
v19_2023
 
v18_2023 <-  vert_lines_min(df20, rivers, 2023, status18, "monitored")
v18_2023

vc19_2023 <-  vert_lines_max(df20, rivers, 2023, status19, "closed")
#vc19_2023

vc18_2023 <- vert_lines_max(df20, rivers, 2023, status18, "closed")

## 2024 ----
year <- 2024
temp <-  df20 |>
   select(Year, SFA, river.number, river, Date, Temp.C, status, status19, status18) |>
   filter(Year == year)
## set all rivers for 2023

temp1 <- temp[1:(length(rivers)), ]
temp1[1:(length(rivers)), 1:length(temp1)] <- NA

v20_2024 <-  vert_lines_min(df20, rivers, 2024, status, "monitored")
v20_2024

v19_2024 <-  vert_lines_min(df20, rivers, 2024, status19, "monitored")
v19_2024

v18_2024 <-  vert_lines_min(df20, rivers, 2024, status18, "monitored")
v18_2024 


vc19_2024 <-  vert_lines_max(df20, rivers, 2024, status19, "closed")
vc19_2024

vc18_2024 <- vert_lines_max(df20, rivers, 2024, status18, "closed")
vc18_2024

## join vert lines ----
df_stat_2024 <- left_join(v19_2024, v18_2024, by = c("Year", "SFA", "river.number", "river")) |>  
   select("Year", "SFA", "river.number", "river", mon19_24 = "Date.x", mon18_24 = "Date.y")

df_stat_2024 <- left_join(df_stat_2024, vc19_2024, by = c("Year", "SFA", "river.number", "river")) |>  
   select("Year", "SFA", "river.number", "river", "mon19_24", "mon18_24", close19_24 = "Date")

df_stat_2024 <- left_join(df_stat_2024, vc18_2024, by = c("Year", "SFA", "river.number", "river")) |>  
   select("Year", "SFA", "river.number", "river", "mon19_24", "mon18_24", "close19_24", close18_24 = "Date")




df_stat_2023 <- left_join(v19_2023, v18_2023, by = c("Year", "SFA", "river.number", "river")) |>  
   select("Year", "SFA", "river.number", "river", mon19_23 = "Date.x", mon18_23 = "Date.y")

df_stat_2023 <- left_join(df_stat_2023, vc19_2023, by = c("Year", "SFA", "river.number", "river")) |>  
   select("Year", "SFA", "river.number", "river", "mon19_23", "mon18_23", close19_23 = "Date")

df_stat_2023 <- left_join(df_stat_2023, vc18_2023, by = c("Year", "SFA", "river.number", "river")) |>  
   select("Year", "SFA", "river.number", "river", "mon19_23", "mon18_23", "close19_23", close18_23 = "Date")

df_stat_all <- full_join(df_stat_2024, df_stat_2023, by = c("SFA", "river.number", "river"))  |> arrange(river.number, river)


# river order ----
rivers_df <- df20 |>
   select(river) |> 
   distinct() 
rivers <- as.character(rivers_df[,1])
#str(rivers_df)
# str(rivers)

# remove ----
df20 <- df20 |>
   filter(!(river == "CharBrook2 River" & Year == 2023)) |>
   filter(!(river == "CharBrook1 River" & Year == 2024)) 
#str(df20)

# # END ----