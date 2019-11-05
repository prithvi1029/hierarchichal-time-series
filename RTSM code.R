library(xts)
library(forecast)
library(MLmetrics)
library(matlib)
# Create the dates object as an index for your xts object
train_dates <- seq(as.Date("2014-01-19"), length = 154, by = 7)
train_sales <- xts(train_sales, order.by = train_dates)
test_dates <- seq(as.Date("2017-01-01"), length = 22, by = 7)
test_sales <- xts(valid,order.by = test_dates)

MET_hi <- train_sales[,"MET.hi"]
MET_lo <- train_sales[,"MET.lo"]
MET_sp <- train_sales[,"MET.sp"]
M_hi <- train_sales[,"M.hi"]
M_lo <- train_sales[,"M.lo"]
SEC_hi <- train_sales[,"SEC.hi"]
SEC_lo <- train_sales[,"SEC.lo"]
M<- train_sales[,"M"]
MET = train_sales[,'MET']
SEC = train_sales[,'SEC']
T = train_sales[,'T']


MET_hi_test <- test_sales[,"MET.hi"]
MET_lo_test <- test_sales[,"MET.lo"]
MET_sp_test <- test_sales[,"MET.sp"]
M_hi_test <- test_sales[,"M.hi"]
M_lo_test <- test_sales[,"M.lo"]
SEC_hi_test <- test_sales[,"SEC.hi"]
SEC_lo_test <- test_sales[,"SEC.lo"]
M_test<- test_sales[,"M"]
MET_test = test_sales[,'MET']
SEC_test = test_sales[,'SEC']
T_test = test_sales[,'T']

#**************************************************************************
MET_hi_model_arima <- auto.arima(MET_hi)
for_MET_hi <- forecast(MET_hi_model_arima, h = 22)

MET_lo_model_arima <- auto.arima(MET_lo)
for_MET_lo <- forecast(MET_lo_model_arima, h = 22)# Build a time series model 

MET_sp_model_arima <- auto.arima(MET_sp)
for_MET_sp <- forecast(MET_sp_model_arima, h = 22)# Build a time series model 
#**************************************************************************
M_hi_model_arima <- auto.arima(M_hi)
for_M_hi <- forecast(M_hi_model_arima, h = 22)# Build a time series model 

M_lo_model_arima <- auto.arima(M_lo)
for_M_lo <- forecast(M_lo_model_arima, h = 22)# Build a time series model 
#**************************************************************************
SEC_hi_model_arima <- auto.arima(SEC_hi)
for_SEC_hi <- forecast(SEC_hi_model_arima, h = 22)# Build a time series model 

SEC_lo_model_arima <- auto.arima(SEC_lo)
for_SEC_lo <- forecast(SEC_lo_model_arima, h = 22)# Build a time series model 
#******************************************************************************
MET_model_arima <- auto.arima(MET)
for_MET <- forecast(MET_model_arima, h = 22)# Build a time series model 

M_model_arima <- auto.arima(M)
for_M <- forecast(M_model_arima, h = 22)# Build a time series model 

SEC_model_arima <- auto.arima(SEC)
for_SEC <- forecast(SEC_model_arima, h = 22)# Build a time series model 
#*******************************************************************************
T_model_arima <- auto.arima(T)
for_T <- forecast(T_model_arima, h = 22)# Build a time series model 
#********************************************************************************

for_MET_hi_xts <- xts(for_MET_hi$mean, order.by = test_dates)
MAPE1 <- MAPE(for_MET_hi_xts, MET_hi_test)
print(MAPE1)

for_MET_sp_xts <- xts(for_MET_sp$mean, order.by = test_dates)
MAPE2 <- MAPE(for_MET_sp_xts, MET_sp_test)
print(MAPE2)

for_MET_lo_xts <- xts(for_MET_lo$mean, order.by = test_dates)
MAPE3 <- MAPE(for_MET_lo_xts, MET_lo_test)
print(MAPE3)

for_M_hi_xts <- xts(for_M_hi$mean, order.by = test_dates)
MAPE4 <- MAPE(for_M_hi_xts, M_hi_test)
print(MAPE4)

for_M_lo_xts <- xts(for_M_lo$mean, order.by = test_dates)
MAPE5 <- MAPE(for_M_lo_xts, M_lo_test)
print(MAPE5)

for_SEC_lo_xts <- xts(for_SEC_lo$mean, order.by = test_dates)
MAPE6 <- MAPE(for_SEC_lo_xts, SEC_lo_test)
print(MAPE6)

for_SEC_hi_xts <- xts(for_SEC_hi$mean, order.by = test_dates)
MAPE7 <- MAPE(for_SEC_hi_xts, SEC_hi_test)
print(MAPE7)

for_MET_xts <- xts(for_MET$mean, order.by = test_dates)
MAPE8 <- MAPE(for_MET_xts, MET_test)
print(MAPE8)

for_M_xts <- xts(for_M$mean, order.by = test_dates)
MAPE9 <- MAPE(for_M_xts, M_test)
print(MAPE9)

for_SEC_xts <- xts(for_SEC$mean, order.by = test_dates)
MAPE10 <- MAPE(for_SEC_xts, SEC_test)
print(MAPE10)

for_T_xts <- xts(for_T$mean, order.by = test_dates)
MAPE11 <- MAPE(for_T_xts, T_test)
print(MAPE11)

##BOTTOM UP##******************************************

MET_bu = for_MET_hi_xts + for_MET_lo_xts +for_MET_sp_xts
MAPE_MET_bu <- MAPE(MET_bu, MET_test)
print(MAPE8)
print(MAPE_MET_bu)

M_bu = for_M_hi_xts + for_M_lo_xts
MAPE_M_bu <- MAPE(M_bu, M_test)
print(MAPE9)
print(MAPE_M_bu)

SEC_bu = for_SEC_hi_xts + for_SEC_lo_xts
MAPE_SEC_bu <- MAPE(SEC_bu, SEC_test)
print(MAPE10)
print(MAPE_SEC_bu)

T_bu = for_M_xts + for_MET_xts +for_SEC_xts
MAPE_T_bu <- MAPE(T_bu, T_test)
print(MAPE11)
print(MAPE_T_bu)


##TOP-BOTTOM##******************************************

# Calculate the average historical proportions
prop_MET_hi <- mean(MET_hi/MET)
prop_MET_lo <- mean(MET_lo/MET)
prop_MET_sp <- mean(MET_sp/MET)

prop_M_hi <- mean(M_hi/M)
prop_M_lo <- mean(M_lo/M)

prop_SEC_hi <- mean(SEC_hi/SEC)
prop_SEC_lo <- mean(SEC_lo/SEC)

prop_MET <- mean(MET/T)
prop_M <- mean(M/T)
prop_SEC <- mean(SEC/T)



# Distribute out your forecast to each product
for_prop_MET <- prop_MET*for_T_xts
for_prop_M <- prop_M*for_T_xts
for_prop_SEC <- prop_SEC*for_T_xts

# Distribute out your forecast to each product
for_prop_MET_hi <- prop_MET_hi*for_prop_MET
for_prop_MET_lo <- prop_MET_lo*for_prop_MET
for_prop_MET_sp <- prop_MET_sp*for_prop_MET

# Distribute out your forecast to each product
for_prop_M_hi <- prop_M_hi*for_prop_M
for_prop_M_lo <- prop_M_lo*for_prop_M

# Distribute out your forecast to each product
for_prop_SEC_hi <- prop_SEC_hi*for_prop_SEC
for_prop_SEC_lo <- prop_SEC_lo*for_prop_SEC

# Distribute out your forecast to each product
for_prop_MET_hi <- prop_MET_hi*for_prop_MET
for_prop_MET_lo <- prop_MET_lo*for_prop_MET
for_prop_MET_sp <- prop_MET_sp*for_prop_MET

# Distribute out your forecast to each product
for_prop_M_hi <- prop_M_hi*for_prop_M
for_prop_M_lo <- prop_M_lo*for_prop_M

# Distribute out your forecast to each product
for_prop_SEC_hi <- prop_SEC_hi*for_prop_SEC
for_prop_SEC_lo <- prop_SEC_lo*for_prop_SEC


MAPE_MET_tb <- MAPE(for_prop_MET, MET_test)
print(MAPE_MET_tb)
print(MAPE8)

MAPE_M_tb <- MAPE(for_prop_M, M_test)
print(MAPE_M_tb)
print(MAPE9)

MAPE_SEC_tb <- MAPE(for_prop_SEC, SEC_test)
print(MAPE_SEC_tb)
print(MAPE10)

MAPE_MET_hi_tb <- MAPE(for_prop_MET_hi, MET_hi_test)
print(MAPE_MET_hi_tb)
print(MAPE1)

MAPE_MET_lo_tb <- MAPE(for_prop_MET_lo, MET_lo_test)
print(MAPE_MET_lo_tb)
print(MAPE3)

MAPE_MET_sp_tb <- MAPE(for_prop_MET_sp, MET_sp_test)
print(MAPE_MET_sp_tb)
print(MAPE2)

MAPE_M_hi_tb <- MAPE(for_prop_M_hi, M_hi_test)
print(MAPE_M_hi_tb)
print(MAPE4)

MAPE_M_lo_tb <- MAPE(for_prop_M_lo, M_lo_test)
print(MAPE_M_lo_tb)
print(MAPE5)

MAPE_SEC_hi_tb <- MAPE(for_prop_SEC_hi, SEC_hi_test)
print(MAPE_SEC_hi_tb)
print(MAPE6)

MAPE_SEC_lo_tb <- MAPE(for_prop_SEC_lo, SEC_lo_test)
print(MAPE_SEC_lo_tb)
print(MAPE7)


##***************MIDDLE OUT****************************************************

T_bu = for_M_xts + for_MET_xts +for_SEC_xts
MAPE_T_m <- MAPE(T_bu, T_test)
print(MAPE11)
print(MAPE_T_m)

# Distribute out your forecast to each product
for_prop_MET_hi <- prop_MET_hi*for_prop_MET
for_prop_MET_lo <- prop_MET_lo*for_prop_MET
for_prop_MET_sp <- prop_MET_sp*for_prop_MET

# Distribute out your forecast to each product
for_prop_M_hi <- prop_M_hi*for_prop_M
for_prop_M_lo <- prop_M_lo*for_prop_M

# Distribute out your forecast to each product
for_prop_SEC_hi <- prop_SEC_hi*for_prop_SEC
for_prop_SEC_lo <- prop_SEC_lo*for_prop_SEC


MAPE_MET_tb <- MAPE(for_prop_MET, MET_test)
print(MAPE_MET_tb)
print(MAPE8)

MAPE_M_tb <- MAPE(for_prop_M, M_test)
print(MAPE_M_tb)
print(MAPE9)

MAPE_SEC_tb <- MAPE(for_prop_SEC, SEC_test)
print(MAPE_SEC_tb)
print(MAPE10)

MAPE_MET_hi_tb <- MAPE(for_prop_MET_hi, MET_hi_test)
print(MAPE_MET_hi_tb)
print(MAPE1)

MAPE_MET_lo_tb <- MAPE(for_prop_MET_lo, MET_lo_test)
print(MAPE_MET_lo_tb)
print(MAPE3)

MAPE_MET_sp_tb <- MAPE(for_prop_MET_sp, MET_sp_test)
print(MAPE_MET_sp_tb)
print(MAPE2)

MAPE_M_hi_tb <- MAPE(for_prop_M_hi, M_hi_test)
print(MAPE_M_hi_tb)
print(MAPE4)

MAPE_M_lo_tb <- MAPE(for_prop_M_lo, M_lo_test)
print(MAPE_M_lo_tb)
print(MAPE5)

MAPE_SEC_hi_tb <- MAPE(for_prop_SEC_hi, SEC_hi_test)
print(MAPE_SEC_hi_tb)
print(MAPE6)

MAPE_SEC_lo_tb <- MAPE(for_prop_SEC_lo, SEC_lo_test)
print(MAPE_SEC_lo_tb)
print(MAPE7)

##*************SUMMING MATRIX*****************************

S =matrix(c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                         0,0,0,1,1,0,0,0,0,0,0,0,1,1,
                         1,0,0,0,0,0,0,0,1,0,0,0,0,0,
                         0,0,1,0,0,0,0,0,0,0,1,0,0,0,
                         0,0,0,0,1,0,0,0,0,0,0,0,1,0,
                         0,0,0,0,0,0,1) ,nrow = 11,ncol = 7,byrow = TRUE)
y_final =S%*%inv(t(S)%*%(S))%*%t(S)%*%t(a)

as.matrix(c(for_M_hi_xts[,1],for_M_lo[,1]))

a =matrix(c(for_T$mean,for_MET$mean,for_M$mean, for_SEC$mean,
            for_MET_hi$mean, for_MET_lo$mean,
            for_MET_sp$mean , for_M_hi$mean , for_M_lo$mean ,
            for_SEC_hi$mean, for_SEC_lo$mean), nrow = 22, ncol = 11, byrow = FALSE)
mape_table =(abs((y_final-t(a)))/t(a))
sum(mape_table[11,])