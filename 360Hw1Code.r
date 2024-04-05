require(data.table)
require(lubridate)
require(forecast)
require(skimr)
require(repr)
require(readxl)
require(ggplot2)

house_index_data <- read_excel("house_price_index.xlsx")
house_index_data <- house_index_data[31:152, ]
house_index_data <- house_index_data[,-3]
colnames(house_index_data) <- c("Date", "House Price Index")
house_index_data$Date <- as.Date(paste(house_index_data$Date, "-01", sep = ""))
house_index_data$"House Price Index" <- as.numeric(house_index_data$"House Price Index")
initial_time <- c(year(min(house_index_data$Date)), month(min(house_index_data$Date)))
house_index_ts <- ts(house_index_data$"House Price Index", start = initial_time, frequency = 12)
house_index_ts

M1_data <- read_excel("M1.xlsx")
M1_data <- M1_data[29:150, ]
M1_data <- M1_data[,-3]
colnames(M1_data) <- c("Date", "M1")
M1_data$Date <- as.Date(paste(M1_data$Date, "-01", sep = ""))
M1_data$"M1" <- as.numeric(M1_data$"M1")
initial_time <- c(year(min(M1_data$Date)), month(min(M1_data$Date)))
M1_ts <- ts(M1_data$"M1", start = initial_time, frequency = 12)
M1_ts

unemployment_rate_data <- read_excel("unemployment_rate.xlsx")
unemployment_rate_data <- unemployment_rate_data[1:122, ]
unemployment_rate_data <- unemployment_rate_data[,-3]
colnames(unemployment_rate_data) <- c("Date", "Rate")
unemployment_rate_data$Date <- as.Date(paste(unemployment_rate_data$Date, "-01", sep = ""))
unemployment_rate_data$"Rate" <- as.numeric(unemployment_rate_data$"Rate")
initial_time <- c(year(min(unemployment_rate_data$Date)), month(min(unemployment_rate_data$Date)))
unemployment_rate_ts <- ts(unemployment_rate_data$"Rate", start = initial_time, frequency = 12)
unemployment_rate_ts

satılık_ev_data = fread("satılık_ev.csv")
colnames(satılık_ev_data) <- c("Date", "Amount")
satılık_ev_data$Date <- as.Date(paste(satılık_ev_data$Date, "-01", sep = ""))
satılık_ev_data <- satılık_ev_data[satılık_ev_data$Date >= as.Date("2014-01-01") & satılık_ev_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(satılık_ev_data$Date)), month(min(satılık_ev_data$Date)))
satılık_ev_ts <- ts(satılık_ev_data$Amount, start = initial_time, frequency = 12)

konut_kredisi_data = fread("konut_kredisi.csv")
colnames(konut_kredisi_data) <- c("Date", "Amount")
konut_kredisi_data$Date <- as.Date(paste(konut_kredisi_data$Date, "-01", sep = ""))
konut_kredisi_data <- konut_kredisi_data[konut_kredisi_data$Date >= as.Date("2014-01-01") & konut_kredisi_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(konut_kredisi_data$Date)), month(min(konut_kredisi_data$Date)))
konut_kredisi_ts <- ts(konut_kredisi_data$Amount, start = initial_time, frequency = 12)

faiz_data = fread("faiz.csv")
colnames(faiz_data) <- c("Date", "Amount")
faiz_data$Date <- as.Date(paste(faiz_data$Date, "-01", sep = ""))
faiz_data <- faiz_data[faiz_data$Date >= as.Date("2014-01-01") & faiz_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(faiz_data$Date)), month(min(faiz_data$Date)))
faiz_ts <- ts(faiz_data$"Amount", start = initial_time, frequency = 12)

enflasyon_data = fread("enflasyon.csv")
colnames(enflasyon_data) <- c("Date", "Amount")
enflasyon_data$Date <- as.Date(paste(enflasyon_data$Date, "-01", sep = ""))
enflasyon_data <- enflasyon_data[enflasyon_data$Date >= as.Date("2014-01-01") & enflasyon_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(enflasyon_data$Date)), month(min(enflasyon_data$Date)))
enflasyon_ts <- ts(enflasyon_data$Amount, start = initial_time, frequency = 12)

darphane_data = fread("darphane.csv")
colnames(darphane_data) <- c("Date", "Amount")
darphane_data$Date <- as.Date(paste(darphane_data$Date, "-01", sep = ""))
darphane_data <- darphane_data[darphane_data$Date >= as.Date("2014-01-01") & darphane_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(darphane_data$Date)), month(min(darphane_data$Date)))
darphane_ts <- ts(darphane_data$Amount, start = initial_time, frequency = 12)

altın_data = fread("altın.csv")
colnames(altın_data) <- c("Date", "Amount")
altın_data$Date <- as.Date(paste(altın_data$Date, "-01", sep = ""))
altın_data <- altın_data[altın_data$Date >= as.Date("2014-01-01") & altın_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(altın_data$Date)), month(min(altın_data$Date)))
altın_ts <- ts(altın_data$Amount, start = initial_time, frequency = 12)

salgın_hastalık_data = fread("salgın_hastalık.csv")
colnames(salgın_hastalık_data) <- c("Date", "Amount")
salgın_hastalık_data$Date <- as.Date(paste(salgın_hastalık_data$Date, "-01", sep = ""))
salgın_hastalık_data <- salgın_hastalık_data[salgın_hastalık_data$Date >= as.Date("2014-01-01") & salgın_hastalık_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(salgın_hastalık_data$Date)), month(min(salgın_hastalık_data$Date)))
salgın_hastalık_ts <- ts(salgın_hastalık_data$Amount, start = initial_time, frequency = 12)

işsizlik_sigortası_data = fread("işsizlik_sigortası.csv")
colnames(işsizlik_sigortası_data) <- c("Date", "Amount")
işsizlik_sigortası_data$Date <- as.Date(paste(işsizlik_sigortası_data$Date, "-01", sep = ""))
işsizlik_sigortası_data <- işsizlik_sigortası_data[işsizlik_sigortası_data$Date >= as.Date("2014-01-01") & işsizlik_sigortası_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(işsizlik_sigortası_data$Date)), month(min(işsizlik_sigortası_data$Date)))
işsizlik_sigortası_ts <- ts(işsizlik_sigortası_data$Amount, start = initial_time, frequency = 12)

iş_ilanları_data = fread("iş_ilanları.csv")
colnames(iş_ilanları_data) <- c("Date", "Amount")
iş_ilanları_data$Date <- as.Date(paste(iş_ilanları_data$Date, "-01", sep = ""))
iş_ilanları_data <- iş_ilanları_data[iş_ilanları_data$Date >= as.Date("2014-01-01") & iş_ilanları_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(iş_ilanları_data$Date)), month(min(iş_ilanları_data$Date)))
iş_ilanları_ts <- ts(iş_ilanları_data$Amount, start = initial_time, frequency = 12)

işsizlik_data = fread("işsizlik.csv")
colnames(işsizlik_data) <- c("Date", "Amount")
işsizlik_data$Date <- as.Date(paste(işsizlik_data$Date, "-01", sep = ""))
işsizlik_data <- işsizlik_data[işsizlik_data$Date >= as.Date("2014-01-01") & işsizlik_data$Date <= as.Date("2024-02-01")]
initial_time <- c(year(min(işsizlik_data$Date)), month(min(işsizlik_data$Date)))
işsizlik_ts <- ts(işsizlik_data$Amount, start = initial_time, frequency = 12)

autoplot(satılık_ev_ts)
autoplot(konut_kredisi_ts)
autoplot(faiz_ts)
autoplot(enflasyon_ts)
autoplot(darphane_ts)
autoplot(altın_ts)
autoplot(salgın_hastalık_ts)
autoplot(işsizlik_sigortası_ts)
autoplot(iş_ilanları_ts)

autoplot(house_index_ts) + ggtitle("House Price Index vs Time") + xlab("Year") + ylab("House Price Index")

ggAcf(house_index_ts, lag.max = 120)
ggAcf(house_index_ts)

df_house_index <- cbind(house_index_data[,-1], satılık_ev_data$Amount, konut_kredisi_data$Amount, faiz_data$Amount)
df_house_index

require(GGally)
ggpairs(df_house_index)

tslm_house_index_1 <- tslm(house_index_ts ~ satılık_ev_ts + konut_kredisi_ts + faiz_ts)
tslm_house_index_2 <- tslm(house_index_ts ~ konut_kredisi_ts + faiz_ts)
tslm_house_index_3 <- tslm(house_index_ts ~ faiz_ts)

summary(tslm_house_index_1)
summary(tslm_house_index_2)
summary(tslm_house_index_3)

CV_data <- data.frame(rbind(CV(tslm_house_index_1), CV(tslm_house_index_2), CV(tslm_house_index_3)))
CV_data

autoplot(house_index_ts, series = "Actual Data") + 
  autolayer(fitted(tslm_house_index_2), series = "Model Result") 

checkresiduals(tslm_house_index_1)
checkresiduals(tslm_house_index_2)
checkresiduals(tslm_house_index_3)

df_house_index[,"Residuals"] <- as.numeric(residuals(tslm_house_index_2))
ggplot(df_house_index, aes(x = fitted(tslm_house_index_2), y = Residuals)) + geom_point()

autoplot(M1_ts) + ggtitle("Money Supply vs Time") + xlab("Year") + ylab("Money Supply")

ggAcf(M1_ts, lag.max = 120)
ggAcf(M1_ts)

require(zoo)
M1_lag1dif <- diff(zoo(M1_data$M1),lag=1,differences = 1,na.pad = TRUE)
M1_lag1dif_ts <- ts(M1_lag1dif, frequency = 12)

ggAcf(M1_lag1dif_ts, lag.max = 120)
ggAcf(M1_lag1dif_ts)

df_M1 <- cbind(M1_data[,-1], faiz_data$Amount, enflasyon_data$Amount, darphane_data$Amount, altın_data$Amount)
df_M1

require(GGally)
ggpairs(df_M1)

tslm_M1_1 <- tslm(M1_ts ~ faiz_ts + enflasyon_ts + darphane_ts + altın_ts)
tslm_M1_2 <- tslm(M1_ts ~ faiz_ts + enflasyon_ts + darphane_ts)
tslm_M1_3 <- tslm(M1_ts ~ enflasyon_ts + darphane_ts)
summary(tslm_M1_1)
summary(tslm_M1_2)
summary(tslm_M1_3)

CV_data <- data.frame(rbind(CV(tslm_M1_1), CV(tslm_M1_2), CV(tslm_M1_3)))
CV_data

autoplot(M1_ts, series = "Actual Data") + 
  autolayer(fitted(tslm_M1_1), series = "Model Result") 

checkresiduals(tslm_M1_1)
checkresiduals(tslm_M1_2)
checkresiduals(tslm_M1_3)

df_M1[,"Residuals"] <- as.numeric(residuals(tslm_M1_1))
ggplot(df_M1, aes(x = M1, y = Residuals)) + geom_point()
ggplot(df_M1, aes(x = fitted(tslm_M1_1), y = Residuals)) + geom_point()

autoplot(unemployment_rate_ts) + ggtitle("Unemployment Rate vs Time") + xlab("Year") + ylab("Unemployment Rate")

ggAcf(unemployment_rate_ts, lag.max = 120)
ggAcf(unemployment_rate_ts)

df_unemployment_rate <- cbind(unemployment_rate_data[,-1], iş_ilanları_data$Amount, enflasyon_data$Amount, işsizlik_sigortası_data$Amount, salgın_hastalık_data$Amount, işsizlik_data$Amount)
df_unemployment_rate

require(GGally)
ggpairs(df_unemployment_rate)

tslm_unemployment_rate_1 <- tslm(unemployment_rate_ts ~ iş_ilanları_ts + enflasyon_ts + salgın_hastalık_ts + işsizlik_sigortası_ts + işsizlik_ts)
tslm_unemployment_rate_2 <- tslm(unemployment_rate_ts ~ iş_ilanları_ts + enflasyon_ts + salgın_hastalık_ts + işsizlik_ts)
tslm_unemployment_rate_3 <- tslm(unemployment_rate_ts ~ iş_ilanları_ts + enflasyon_ts + işsizlik_ts)
summary(tslm_unemployment_rate_1)
summary(tslm_unemployment_rate_2)
summary(tslm_unemployment_rate_3)

CV_data <- data.frame(rbind(CV(tslm_unemployment_rate_1), CV(tslm_unemployment_rate_2), CV(tslm_unemployment_rate_3)))
CV_data

autoplot(unemployment_rate_ts, series = "Actual Data") + 
  autolayer(fitted(tslm_unemployment_rate_2), series = "Model Result") 

checkresiduals(tslm_unemployment_rate_1)
checkresiduals(tslm_unemployment_rate_2)
checkresiduals(tslm_unemployment_rate_3)

df_unemployment_rate[,"Residuals"] <- as.numeric(residuals(tslm_unemployment_rate_2))
ggplot(df_M1, aes(x = fitted(tslm_unemployment_rate_2), y = Residuals)) + geom_point()
