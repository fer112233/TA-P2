require(xts)
library(tidyverse)
library(tsibble)
library(fable)
library(ggplot2)
library(lubridate)
library(cowplot)
library(ggpubr)
library(tibble)
library(stringr)
library(xlsx)

Sys.setlocale(locale="en_US")

db <- read.csv("ecommerce_data.csv")


summary(db)


decompose.xts <- function (x, type = c("additive", "multiplicative"), filter = NULL, frequency = 4) {
    attr(x, 'frequency') <- frequency
    dts <- decompose(as.ts(x), type, filter)
    dts$x <- .xts(dts$x, .index(x))
    dts$seasonal <- .xts(dts$seasonal, .index(x))
    dts$trend <- .xts(dts$trend, .index(x))
    dts$random <- .xts(dts$random, .index(x))
    
    with(dts,
         structure(list(x = x, seasonal = seasonal, trend = trend,
                        random = if (type == "additive") x - seasonal - trend else x/seasonal/trend, 
                        figure = figure, type = type), class = "decomposed.xts"))
  }
plot.decomposed.xts <- function(x, ...) {
  xx <- x$x
  if (is.null(xx))
    xx <- with(x,
               if (type == "additive") random + trend + seasonal
               else random * trend * seasonal)
  p <- cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random)
  p_df <- data.frame(p)
  p1 <- ggplot(p_df, aes(x=as.Date(row.names(p_df)), y=random)) + geom_line(color = "#2d8bba", size = 0.5) + 
    labs(title = "Random",
         y = NULL, x = NULL)
  p2 <- ggplot(p_df, aes(x=as.Date(row.names(p_df)), y=seasonal)) + geom_line(color = "#2d8bba", size = 0.5) + 
    labs(title = "Seasonal",
         y = NULL, x = NULL)
  p3 <- ggplot(p_df, aes(x=as.Date(row.names(p_df)), y=trend)) + geom_line(color = "#2d8bba", size = 0.5) + 
    labs(title = "Trend",
         y = NULL, x = NULL)
  p4 <- ggplot(p_df, aes(x=as.Date(row.names(p_df)), y=observed)) + geom_line(color = "#2d8bba", size = 0.5) + 
    labs(title = "Observed",
         y = NULL, x = NULL)
  
  return(
    ggdraw() +
      draw_plot(p1, x = 0, y = 0, width = 0.98, height = .24) +
      draw_plot(p2, x = 0, y = .23, width = 0.98, height = .24) +
      draw_plot(p3, x = 0, y = .23*2, width = 0.98, height = .24) +
      draw_plot(p4, x = 0, y = .23*3, width = 0.98, height = .24) +
      draw_label("Decomposition of the Time Series | Frequency 4 weeks", x=0.5, y=0.96)
  )
}

forecast_db <- data.frame()
process <- 0
starting_time <- now()
# This will take a while to complete... almost 200 images to save to disk.
for (prod_type in unique(db$product_type)) {
  for (re_id in unique(db$region_id)) {
    
    lbl <- paste(paste(paste("Demand of product category", prod_type), "in region id"), re_id)
    process <- process + 1
    cat(paste(paste("Computing", lbl)), "\n")
    
    ts_subset <- db[db$product_type == as.numeric(prod_type),]
    ts_subset <- ts_subset[ts_subset$region_id == re_id,]
    
    ts_subset$date <- as.Date(ts_subset$date)
    
    full_dates <- seq(min(ymd(db$date)), max(ymd(db$date)), by = "1 day")
    full_dates <- data.frame(date = full_dates)
    
    ts_subset <- merge(full_dates, ts_subset, by = "date", all.x = TRUE)
    ts_subset[is.na(ts_subset)] <- 0
    
    ts_subset$week <- floor_date(ts_subset$date, "week")
      
      aggregated <- aggregate(ts_subset["Quantity"], by=ts_subset["week"], sum)
      
      
      full_dates <- seq(min(aggregated$week), max(aggregated$week), by = "1 week")
      full_dates <- data.frame(week = full_dates)
      
      aggregated <- merge(full_dates, aggregated, by = "week", all.x = TRUE)
      aggregated[is.na(aggregated)] <- 0
      
      
      ts <- xts(aggregated$Quantity, order.by=as.POSIXct(aggregated$week))
      decomposed <- decompose.xts(ts, frequency = 4)
      plot_ts <- plot.decomposed.xts(decomposed)
      
      ts_subset$date <- as.Date(ts_subset$date, "%Y-%m-%d")
      
      plot_std <- ggplot(ts_subset, aes(x=date, y=Quantity)) + geom_line(color = "#2d8bba", size = 0.5) + 
        labs(title = "Quantity by day",
             y = "Quantity", x = "Day")
      plot_agg_w <- ggplot(aggregated, aes(x=week, y=Quantity)) + geom_line(color = "#2d8bba", size = 0.5) + 
        labs(title = "Quantity by aggregated week",
             y = NULL, x = "Week")
      
      aggregated$Month <- paste(months(as.Date(aggregated$week)), year(as.Date(aggregated$week)))
      wk <- function(x) as.numeric(format(x, "%U"))
      aggregated$Week_by_Month <- wk(aggregated$week) - wk(as.Date(cut(aggregated$week, "month"))) + 1
      aggregated$UNIXTime <- as.numeric(aggregated$week)
      
      aggregated$Month <- reorder(aggregated$Month, aggregated$UNIXTime)
      heatmap_plot <- ggplot(aggregated, aes(Month, as.character(Week_by_Month))) + geom_tile(aes(fill = Quantity),colour = "white", na.rm = TRUE) +
        scale_fill_gradient(low = "#2d8bba", high = "#bad9b7") +  
        guides(fill=guide_legend(title="Quantity")) +
        theme_bw() + theme_minimal() + 
        labs(title = "Histogram of Order Quantity by Week nº of month",
             y = "Week nº of the Month", x = "Month") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90))
      
      ts_tibble <- ts %>% fortify.zoo %>% as_tibble(.name_repair = "minimal")
      attr(ts, 'frequency') <- 4
      ts_base <- as.ts(ts)
      ts_tibble_as <- as_tsibble(ts_base)
      paste(paste(paste("Period by Month Quarters from", as.character(min(ymd(db$date)))), "to"), as.character(max(ymd(db$date))))
      ts_tibble_as <- rename(ts_tibble_as, "Period by Month Quarters from 2019-01-12 to 2020-12-10"="index")
      ts_tibble_as <- rename(ts_tibble_as, "Quantity"="value")
      
      model_ts <- model(ts_tibble_as,
                        ets = ETS(box_cox(Quantity, 0.6)),
                        arima = ARIMA(Quantity),
                        snaive = SNAIVE(Quantity)
      )
      forecast <- forecast(model_ts, h = 13)
      prediction_plot <-autoplot(forecast, ts_tibble_as, alpha = 0.5, level=80)
      
      forecast_export <- forecast[forecast$.model == "snaive",]
      forecast_export$.model = NULL
      forecast_export$Quantity = NULL
      
      forecast_export$date_comp_1 = as.numeric(str_extract( forecast_export$`Period by Month Quarters from 2019-01-12 to 2020-12-10`, "^[0-9]+" ))
      forecast_export$date_comp_2 = as.numeric(str_extract( forecast_export$`Period by Month Quarters from 2019-01-12 to 2020-12-10`, "[0-9]+$" ))
      
      ts_tibble_as$date_comp_1 = as.numeric(str_extract( ts_tibble_as$`Period by Month Quarters from 2019-01-12 to 2020-12-10`, "^[0-9]+" ))
      ts_tibble_as$date_comp_2 = as.numeric(str_extract( ts_tibble_as$`Period by Month Quarters from 2019-01-12 to 2020-12-10`, "[0-9]+$" ))
      
      forecast_export$date_comp_1_abs = forecast_export$date_comp_1 - tail(ts_tibble_as, n=1)$date_comp_1
      forecast_export$date_comp_2_abs = forecast_export$date_comp_2 - tail(ts_tibble_as, n=1)$date_comp_2
      
      last_record <- max(aggregated$week)
      forecast_export$date <- last_record + weeks((forecast_export$date_comp_1_abs*4) + forecast_export$date_comp_2_abs)
      
      rowIndex <- nrow(forecast_db) + 1
      forecast_db[rowIndex, "Product Type"] <- prod_type
      forecast_db[rowIndex, "Region ID"] <- re_id
      for (rown in 1:nrow(forecast_export)) {
        forecast_db[rowIndex, format(forecast_export[rown, "date"][[1]], format="%Y-%m-%d")] <- forecast_export[rown, ".mean"][[1]]
      }
      
      aggregated_plot <- ggdraw() +
        draw_plot(plot_std, x = 0, y = .46, width = .48/2, height = .46) +
        draw_plot(plot_agg_w, x = .5/2, y = .46, width = .48/2, height = .46) +
        draw_plot(heatmap_plot, x = 0, y = 0, width = 1/2, height = 0.46) +
        draw_plot(plot_ts, x = .5, y = .34, width = 1/2, height = 0.6) +
        draw_plot(prediction_plot, x = .5, y = 0, width = 1/2, height = 0.34) +
        draw_label(lbl, fontface='bold', x=0.5, y=0.96)
      
      ggsave(paste(paste("Output/", lbl),".png"), aggregated_plot, scale=1.3, height = 1080*(1/150), dpi=150, width = 1920*(1/150))
      
      current_time <- now()
      time_in_process <- interval(starting_time, current_time)
      time_per_iteration_average <- seconds(time_in_process)/process
      remaining_iterations <- length(unique(db$product_type))*length(unique(db$region_id))-process
      
      cat(paste(paste("  -> Overall Process:", paste(round(process*100/remaining_iterations, digits = 2), "%  ->  Estimated Time to Complete:")), paste(duration(round(as.numeric(time_per_iteration_average), digits = 0)*remaining_iterations, "seconds"), "\n")))
      
  }
}

write.csv(forecast_db, "Demand_Forecast_SN.csv", row.names = FALSE)
write.xlsx(forecast_db, "Demand_Forecast_SN.xlsx", sheetName = "3 Months aggregated by Week", col.names = TRUE, row.names = FALSE, append = FALSE)






