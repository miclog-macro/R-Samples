# Key Dates
# Economic Calendar
# Market Crashes

# Working #####
#*20.0 Yield/Vol Ratio           ##### Find + ADD
#*51.0 SPX vs FFR                           ##### Fix + ADD
#*52.0 Consumer Sentiment vs GDP             ##### Fix or sentiment only + ADD
#*55.0 Ratio of Leading to Coincident Index    #### Fix + ADD
#Russia.data.R - Russia MICEX Index - getSymbols("MICEXINDEXCF.ME") #### Fix

from <- Sys.Date() - 360*3   #starting early to capture full effect of moving averages
to <- Sys.Date()

# Key Dates #############################################

cal <- create.calendar("Calendar Rmetrics/NYSE",weekdays=c("saturday", "sunday"))
DateAdj_7days <- adjust.previous(to-7, cal)
DateAdj_1mo <- adjust.previous(to-30, cal)
DateAdj_6mo <- adjust.previous(to-183, cal)
DateAdj_1yr <- adjust.previous(to-365, cal)

#Economic Calendar #####
# #Countries: Argentina, Brazil, Germany, UK, AUS, China, EMU, India, Japan, Russia, Turkey, U.S.
# event_dates.date <- "06/26/2019"
# event_dates <- read.csv('eventdates.csv',header=T)
# event_dates <- data.frame(event_dates)
# # kable_styling(kable(event_dates),
# #               bootstrap_options = c("striped", "hover", "responsive"))

# Market Crashes ####################################
recession08 <- data.frame(xmin=as.Date("2007-12-01"),
                          xmax=as.Date("2009-06-01"),
                          ymin=-Inf,
                          ymax=Inf)

recession01 <- data.frame(xmin=as.Date("2001-03-01"),
                          xmax=as.Date("2001-11-01"),
                          ymin=-Inf,
                          ymax=Inf)

recession91 <- data.frame(xmin=as.Date("1990-07-01"),
                          xmax=as.Date("1991-03-01"),
                          ymin=-Inf,
                          ymax=Inf)

recession82 <- data.frame(xmin=as.Date("1981-07-01"),
                          xmax=as.Date("1982-11-01"),
                          ymin=-Inf,
                          ymax=Inf)

recession80 <- data.frame(xmin=as.Date("1980-01-01"),
                          xmax=as.Date("1980-07-01"),
                          ymin=-Inf,
                          ymax=Inf)

recession75 <- data.frame(xmin=as.Date("1973-11-01"),
                          xmax=as.Date("1975-03-01"),
                          ymin=-Inf,
                          ymax=Inf)

recession70 <- data.frame(xmin=as.Date("1969-12-01"),
                          xmax=as.Date("1970-11-01"),
                          ymin=-Inf,
                          ymax=Inf)

