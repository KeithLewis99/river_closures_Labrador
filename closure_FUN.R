

#' river_status
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples

river_status <- function(df){
   for(i in seq_along(river_name)){
      #browser()
      df <-
         bind_rows(
            temp |> filter(River.Name == river_name[i] & status == "monitored") |> slice_min(Time),
            temp |> filter(River.Name == river_name[i] & status19 == "monitored") |> slice_min(Time),
            temp |> filter(River.Name == river_name[i] & status18 == "monitored") |> slice_min(Time)
         ) |>
         mutate(Date = date(Time))
      
      if(nrow(df) == 2){
         df <- df |>
            add_row(df, .before = 1)
         df[1, 5:9] <- NA
      }
      
      if(nrow(df) == 1){
         df <- df |>
            add_row(df, .before = 1) |>
            add_row(df, .before = 1)
         df[1:2, 5:9] <- NA
      }
      if(nrow(df) == 0){
         df <- df |>
            add_row(df, .before = 1) 
         df[1:3, 5:9] <- NA
      }
      if(i ==1) {
         tab1[c(i, i+1, i+2),] <- df
      } else {
         tab1[c((i*2)+(i-2), (i*2)+(i-1), (i*2)+(i)),] <- df
      }
   }
}



#' vert_lines_min
#'
#' @param df 
#' @param vec 
#' @param year 
#' @param status_var 
#' @param status_val 
#'
#' @return
#' @export
#'
#' @examples
vert_lines_min <- function(df, vec, year, status_var, status_val){
   #browser()
   vbl <- enquo(status_var)
   for(i in seq_along(vec)){
      #if(nrow(temp_i) >0){
      temp_i <- df |>
         select(Year, SFA, river.number, river, Date,  Temp.C, status, status19, status18) |>
         filter(Year == year & river == vec[i] & !!vbl == {{status_val}}) |>
         slice_min(Date) 
      if(nrow(temp_i) == 0){
         temp_i <- df |>
            select(Year, SFA, river.number, river, Date,  Temp.C, status, status19, status18) |>
            filter(Year == year & river == vec[i]) |>
            slice_min(Date)
         temp_i[1, 5:9] <- NA
      }
      temp1[i,] <- temp_i
   } 
   ind <- apply(temp1, 1, function(x) all(is.na(x)))
   temp1 <- temp1[!ind,]
   return(temp1)
}


#' vert_lines_max
#'
#' @param df 
#' @param vec 
#' @param year 
#' @param status_var 
#' @param status_val 
#'
#' @return
#' @export
#'
#' @examples
vert_lines_max <- function(df, vec, year, status_var, status_val){
   #browser()
   vbl <- enquo(status_var)
   for(i in seq_along(vec)){
      #if(nrow(temp_i) >0){
      temp_i <- df |>
         select(Year, SFA, river.number, river, Date, Temp.C, status, status19, status18) |>
         filter(Year == year & river == vec[i] & !!vbl == {{status_val}}) |>
         slice_max(Date) 
      if(nrow(temp_i) == 0){
         temp_i <- df |>
            select(Year, SFA, river.number, river, Date,  Temp.C, status, status19, status18) |>
            filter(Year == year & river == vec[i]) |>
            slice_min(Date)
         temp_i[1, 5:9] <- NA
      }
      temp1[i,] <- temp_i
   }
   ind <- apply(temp1, 1, function(x) all(is.na(x)))
   temp1 <- temp1[!ind,]
   return(temp1)
}



#' minDate_temp
#'
#' @param df 
#' @param vec 
#' @param year 
#' @param temp
#'
#' @return
#' @export
#'
#' @examples
minDate_temp <- function(df, vec, year, temp){
   #browser()
   for(i in seq_along(vec)){
      #if(nrow(temp_i) >0){
      temp_i <- df |>
         select(Year, SFA, River.Number, River.Name, Time,  Temp.C) |>
         filter(Year == year & River.Name == vec[i] & Temp.C > temp) |>
         slice_min(Time) 
      if(nrow(temp_i) == 0){
         temp_i[1, 1:6] <- NA
      }
      if(nrow(temp_i) > 1){
         temp_i <- temp_i[1,]
      }
      temp1[i,] <- temp_i
   } 
   ind <- apply(temp1, 1, function(x) all(is.na(x)))
   temp1 <- temp1[!ind,]
   return(temp1)
}



river_closures_plotly <- function(i){
   p1 <- ggplot() +
      geom_line(data = df20 |> filter(River.Name == river_name[i]), 
                aes(x = Time, y = Temp.C, color = "open", group = 1, text = 
                   paste("Date: ", Time, "\n",
                   "Temp: ", round(Temp.C, 2),
                   sep = ""))) +
      {if(nrow(df20 |> filter(River.Name == river_name[i] & status == "closed")) > 0){
         geom_line(data = df20 |> filter(River.Name == river_name[i] & status == "closed"), 
                   aes(x = Time, y = Temp.C, group = period, color = "closed", group = 1, text = 
                          paste("Date: ", Time, "\n",
                                "Temp: ", round(Temp.C, 2),
                                sep = "")))   
      }} +
      {if(nrow(df20 |> filter(River.Name == river_name[i] & status == "monitored")) > 0){
         geom_line(data = df20 |> filter(River.Name == river_name[i] & status == "monitored"), 
                   aes(x = Time, y = Temp.C, group = period, color = "monitored", group = 1, text = 
                          paste("Date: ", Time, "\n",
                                "Temp: ", round(Temp.C, 2),
                                sep = "")))
      }} +
      {if(nrow(water_lev |> filter(River.Name == river_name[i] & !is.na(new))) > 1){
         geom_line(data = water_lev |> filter(River.Name == river_name[i]), 
                   aes(x = Time, y = new, group = 1, text = 
                          paste("Date ", Time, "\n",
                                "Depth ", round(Level.m, 2),
                                sep = "")), 
                   colour = 'blue') 
      }} +
      geom_hline(yintercept = 20, linetype = 'dashed') +
      geom_hline(yintercept = 19, linetype = 'dotdash') +
      geom_hline(yintercept = 18, linetype = 'dotted') +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "mon19_24"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "mon19_24"], linetype = 'dotdash')      
      }} +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "mon18_24"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "mon18_24"], linetype = 'dotted')
      }} +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "close19_24"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "close19_24"], linetype = 'dotdash')
      }} +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "close18_24"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "close18_24"], linetype = 'dotted')
      }} +
      #{if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "mon19_23"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "mon19_23"], linetype = 'dotdash') +
      #}} +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "mon18_23"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "mon18_23"], linetype = 'dotted')
      }} +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "close19_23"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "close19_23"], linetype = 'dotdash')
      }} +
      {if(!is.na(df_stat_all[df_stat_all$River.Name == river_name[i], "close18_23"])){
         geom_vline(xintercept = df_stat_all[df_stat_all$River.Name == river_name[i], "close18_23"], linetype = 'dotted')
      }} +
      theme_bw(base_size = 12) +
      {if(!is.na(df20_23_range[df20_23_range$River.Name == river_name[i], "Date.of.Closure"])) {
         geom_rect(data = df20_23_range |> filter(River.Name == river_name[i]), 
                   aes(xmin = min(Date.of.Closure), xmax = max(Re.open.Date), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = 'darkgrey')    
      }} +
      {if(!is.na(df20_24_range[df20_24_range$River.Name == river_name[i], "Date.of.Closure"])) {
         geom_rect(data = df20_24_range |> filter(River.Name == river_name[i]), 
                   aes(xmin = min(Date.of.Closure), xmax = max(Re.open.Date), ymin = -Inf, ymax = Inf), alpha = 0.3, fill = 'darkgrey')    
      }} +
      
      scale_color_manual(name = "Temperature-based river status",
                         labels = c("open", "monitored", "closed"),
                         breaks = c("open", "monitored", "closed"),
                         values = c('black', 'darkorange', 'red')) +
      labs(x = "Date",
           #y = "Temperature (°C)",
           title = paste0(
              river_name[i], 
              #" at 18-20°C thresholds,",
              ", SFA = ", 
              # "\n SFA = ",
              df20$SFA[df20$River.Name == river_name[i]][1],
              ", River # = ", 
              df20$River.Number[df20$River.Name == river_name[i]][1]
           )) +
      scale_y_continuous(name = "Water Temperature (°C)",
                         sec.axis = sec_axis(trans= ~./15, 
                                             name = "Water Level (meters)"), 
                         limits = c(10, 33)) + # /10 to reduce the axis
      #theme(legend.position = "bottom") + 
      theme(legend.position = "none") + 
      facet_wrap(~Year, scales = "free_x")
   return(plotly::ggplotly(p1, tooltip = "text"))
}