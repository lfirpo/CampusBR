# Instalação de pacotes
#install.packages("openxlsx")
#install.packages("tidyverse")
#install.packages("plotly")
#install.packages("dplyr")
#install.packages("prophet")
#install.packages("readr")


# Utilização de pacotes
library(openxlsx)
library(tidyverse)
library(plotly)
library(plyr)
library(readr)
library(prophet)


#dataset das escolas públicas de SP
#crianças do ensino fundamental que abandonaram a escola em 2017
###############
df_EF <- read.xlsx('evasaoEF.xlsx')

m <- prophet(df_EF, yearly.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 5, freq = "year" )
head(future)

forecast <- predict(m, future)
head(forecast)

plot(m, forecast)

prophet_plot_components(m, forecast)
#

###############
#Dataset sample para prophet
df_EF <- read.xlsx('Acessos_TV_2015-2018_-_BR-TOTAL.xlsx')

m <- prophet(df_EF, yearly.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 10, freq = "month" )
head(future)

forecast <- predict(m, future)
head(forecast)

plot(m, forecast)

prophet_plot_components(m, forecast)
#
