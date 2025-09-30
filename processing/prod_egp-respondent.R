## Ocupación
# Ocupación u oficio actual ISCO 08
frq(df1$ISCO08) # isco08 
# Recode CIUO_encuestado desde ISCO 2008 a ISCO 1988
df1$isco88r <- occupar::isco08to88(df1$ISCO08)
sjmisc::frq(df1$isco88r)

#Note: the package generate identical recodes as my own manual recoding

#Employment relation encuestado
sjmisc::frq(df1$EMPREL)
sjmisc::frq(df1$WORK)
df1$semp <- df1$EMPREL
df1$semp <- as.numeric(car::recode(df1$semp, "c(2,3,4)=1;c(1)=2;c(0,8,9)=NA"))
sjmisc::frq(df1$semp)
df1$semp <- sjlabelled::set_labels(x = df1$semp,labels=c('Self-employed','Employee'))
sjmisc::frq(df1$semp)

# superivison
sjmisc::frq(df1$NEMPLOY) #supervision respondent
df1$supvis <- as.numeric(car::recode(df1$NEMPLOY,"c(9998,9999)=NA"))
sjmisc::frq(df1$supvis)

# head(df1[,c('ISCO08','cod_m22',"isco88",'m06','m25',"supvis")],50)
# View(df1[,c('ISCO08','cod_m22',"isco88",'m06','m25',"supvis")])

# Create a temporary variable `tmpisco88` and assign it the value of `isco88`
df1$tmpisco88 <- df1$isco88r
summary(df1$tmpisco88)
# Replace values in `tmpdf1$isco88` based on specified conditions
df1$tempisco <- ifelse((df1$isco88r >= 6100 & df1$isco88r <= 6133) & df1$supvis >= 1 & !is.na(df1$supvis), 1311, df1$isco88r)
df1$tempisco <- ifelse((df1$isco88r >= 9200 & df1$isco88r <= 9213) & df1$supvis > 1 & !is.na(df1$supvis), 6132, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1311 & df1$supvis > 10 & !is.na(df1$supvis), 1221, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1312 & df1$supvis > 10 & !is.na(df1$supvis), 1222, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1313 & df1$supvis > 10 & !is.na(df1$supvis), 1223, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1314 & df1$supvis > 10 & !is.na(df1$supvis), 1224, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1315 & df1$supvis > 10 & !is.na(df1$supvis), 1225, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1316 & df1$supvis > 10 & !is.na(df1$supvis), 1226, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1317 & df1$supvis > 10 & !is.na(df1$supvis), 1227, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1318 & df1$supvis > 10 & !is.na(df1$supvis), 1228, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1319 & df1$supvis > 10 & !is.na(df1$supvis), 1229, df1$isco88r)
df1$tempisco <- ifelse((df1$isco88r == 1300 | df1$isco88r == 1310) & df1$supvis > 10 & !is.na(df1$supvis), 1220, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1221 & (df1$supvis >= 1 & df1$supvis <= 10), 1311, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1222 & (df1$supvis >= 1 & df1$supvis <= 10), 1312, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1223 & (df1$supvis >= 1 & df1$supvis <= 10), 1313, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1224 & (df1$supvis >= 1 & df1$supvis <= 10), 1314, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1225 & (df1$supvis >= 1 & df1$supvis <= 10), 1315, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1226 & (df1$supvis >= 1 & df1$supvis <= 10), 1316, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1227 & (df1$supvis >= 1 & df1$supvis <= 10), 1317, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1228 & (df1$supvis >= 1 & df1$supvis <= 10), 1318, df1$isco88r)
df1$tempisco <- ifelse(df1$isco88r == 1229 & (df1$supvis >= 1 & df1$supvis <= 10), 1319, df1$isco88r)
df1$tempisco <- ifelse((df1$isco88r == 1200 | df1$isco88r == 1210 | df1$isco88r == 1220) & (df1$supvis >= 1 & df1$supvis <= 10), 1310, df1$isco88r)
df1$tempisco <- ifelse((df1$isco88r == 1220 | (df1$isco88r >= 1222 & df1$isco88r <= 1229)) & df1$semp == 1 & df1$supvis >= 11 & !is.na(df1$supvis), 1210, df1$isco88r)
sjmisc::frq(df1$tempisco) 

# haven::write_dta(data = df1[,c("tempisco","supvis","semp")],
#                  path = here::here("input/data/proc/df118_egp.dta"))

# View(df1[,c("isco88",'tempisco')])

df1$egp10 <- NA 
# recode from ISCO88 to EGP11
{
df1$egp10[df1$tempisco == 1000] <- 1
df1$egp10[df1$tempisco == 1100] <- 1
df1$egp10[df1$tempisco == 1110] <- 1
df1$egp10[df1$tempisco == 1120] <- 1
df1$egp10[df1$tempisco == 1130] <- 2
df1$egp10[df1$tempisco == 1140] <- 2
df1$egp10[df1$tempisco == 1141] <- 2
df1$egp10[df1$tempisco == 1142] <- 2
df1$egp10[df1$tempisco == 1143] <- 2
df1$egp10[df1$tempisco == 1200] <- 1
df1$egp10[df1$tempisco == 1210] <- 1
df1$egp10[df1$tempisco == 1220] <- 1
df1$egp10[df1$tempisco == 1221] <- 11
df1$egp10[df1$tempisco == 1222] <- 1
df1$egp10[df1$tempisco == 1223] <- 1
df1$egp10[df1$tempisco == 1224] <- 1
df1$egp10[df1$tempisco == 1225] <- 1
df1$egp10[df1$tempisco == 1226] <- 1
df1$egp10[df1$tempisco == 1227] <- 1
df1$egp10[df1$tempisco == 1228] <- 1
df1$egp10[df1$tempisco == 1229] <- 1
df1$egp10[df1$tempisco == 1230] <- 1
df1$egp10[df1$tempisco == 1231] <- 1
df1$egp10[df1$tempisco == 1232] <- 1
df1$egp10[df1$tempisco == 1233] <- 1
df1$egp10[df1$tempisco == 1234] <- 1
df1$egp10[df1$tempisco == 1235] <- 1
df1$egp10[df1$tempisco == 1236] <- 1
df1$egp10[df1$tempisco == 1237] <- 1
df1$egp10[df1$tempisco == 1239] <- 1
df1$egp10[df1$tempisco == 1240] <- 2
df1$egp10[df1$tempisco == 1250] <- 1
df1$egp10[df1$tempisco == 1251] <- 1
df1$egp10[df1$tempisco == 1252] <- 2
df1$egp10[df1$tempisco == 1300] <- 2
df1$egp10[df1$tempisco == 1310] <- 2
df1$egp10[df1$tempisco == 1311] <- 11
df1$egp10[df1$tempisco == 1312] <- 2
df1$egp10[df1$tempisco == 1313] <- 2
df1$egp10[df1$tempisco == 1314] <- 2
df1$egp10[df1$tempisco == 1315] <- 2
df1$egp10[df1$tempisco == 1316] <- 2
df1$egp10[df1$tempisco == 1317] <- 2
df1$egp10[df1$tempisco == 1318] <- 2
df1$egp10[df1$tempisco == 1319] <- 2
df1$egp10[df1$tempisco == 2000] <- 1
df1$egp10[df1$tempisco == 2100] <- 1
df1$egp10[df1$tempisco == 2110] <- 1
df1$egp10[df1$tempisco == 2111] <- 1
df1$egp10[df1$tempisco == 2112] <- 1
df1$egp10[df1$tempisco == 2113] <- 1
df1$egp10[df1$tempisco == 2114] <- 1
df1$egp10[df1$tempisco == 2120] <- 1
df1$egp10[df1$tempisco == 2121] <- 1
df1$egp10[df1$tempisco == 2122] <- 1
df1$egp10[df1$tempisco == 2130] <- 1
df1$egp10[df1$tempisco == 2131] <- 1
df1$egp10[df1$tempisco == 2132] <- 2
df1$egp10[df1$tempisco == 2139] <- 2
df1$egp10[df1$tempisco == 2140] <- 1
df1$egp10[df1$tempisco == 2141] <- 1
df1$egp10[df1$tempisco == 2142] <- 1
df1$egp10[df1$tempisco == 2143] <- 1
df1$egp10[df1$tempisco == 2144] <- 1
df1$egp10[df1$tempisco == 2145] <- 1
df1$egp10[df1$tempisco == 2146] <- 1
df1$egp10[df1$tempisco == 2147] <- 1
df1$egp10[df1$tempisco == 2148] <- 2
df1$egp10[df1$tempisco == 2149] <- 1
df1$egp10[df1$tempisco == 2200] <- 1
df1$egp10[df1$tempisco == 2210] <- 1
df1$egp10[df1$tempisco == 2211] <- 1
df1$egp10[df1$tempisco == 2212] <- 1
df1$egp10[df1$tempisco == 2213] <- 1
df1$egp10[df1$tempisco == 2220] <- 1
df1$egp10[df1$tempisco == 2221] <- 1
df1$egp10[df1$tempisco == 2222] <- 1
df1$egp10[df1$tempisco == 2223] <- 1
df1$egp10[df1$tempisco == 2224] <- 1
df1$egp10[df1$tempisco == 2229] <- 1
df1$egp10[df1$tempisco == 2230] <- 2
df1$egp10[df1$tempisco == 2300] <- 2
df1$egp10[df1$tempisco == 2310] <- 1
df1$egp10[df1$tempisco == 2320] <- 2
df1$egp10[df1$tempisco == 2321] <- 2
df1$egp10[df1$tempisco == 2322] <- 2
df1$egp10[df1$tempisco == 2323] <- 2
df1$egp10[df1$tempisco == 2330] <- 2
df1$egp10[df1$tempisco == 2331] <- 2
df1$egp10[df1$tempisco == 2332] <- 2
df1$egp10[df1$tempisco == 2340] <- 2
df1$egp10[df1$tempisco == 2350] <- 1
df1$egp10[df1$tempisco == 2351] <- 1
df1$egp10[df1$tempisco == 2352] <- 1
df1$egp10[df1$tempisco == 2359] <- 2
df1$egp10[df1$tempisco == 2400] <- 1
df1$egp10[df1$tempisco == 2410] <- 2
df1$egp10[df1$tempisco == 2411] <- 1
df1$egp10[df1$tempisco == 2412] <- 2
df1$egp10[df1$tempisco == 2419] <- 2
df1$egp10[df1$tempisco == 2420] <- 1
df1$egp10[df1$tempisco == 2421] <- 1
df1$egp10[df1$tempisco == 2422] <- 1
df1$egp10[df1$tempisco == 2429] <- 1
df1$egp10[df1$tempisco == 2430] <- 2
df1$egp10[df1$tempisco == 2431] <- 2
df1$egp10[df1$tempisco == 2432] <- 2
df1$egp10[df1$tempisco == 2440] <- 1
df1$egp10[df1$tempisco == 2441] <- 1
df1$egp10[df1$tempisco == 2442] <- 1
df1$egp10[df1$tempisco == 2443] <- 1
df1$egp10[df1$tempisco == 2444] <- 2
df1$egp10[df1$tempisco == 2445] <- 1
df1$egp10[df1$tempisco == 2446] <- 2
df1$egp10[df1$tempisco == 2450] <- 2
df1$egp10[df1$tempisco == 2451] <- 2
df1$egp10[df1$tempisco == 2452] <- 2
df1$egp10[df1$tempisco == 2453] <- 2
df1$egp10[df1$tempisco == 2454] <- 2
df1$egp10[df1$tempisco == 2455] <- 2
df1$egp10[df1$tempisco == 2460] <- 2
df1$egp10[df1$tempisco == 3000] <- 2
df1$egp10[df1$tempisco == 3100] <- 2
df1$egp10[df1$tempisco == 3110] <- 2
df1$egp10[df1$tempisco == 3111] <- 2
df1$egp10[df1$tempisco == 3112] <- 2
df1$egp10[df1$tempisco == 3113] <- 2
df1$egp10[df1$tempisco == 3114] <- 2
df1$egp10[df1$tempisco == 3115] <- 2
df1$egp10[df1$tempisco == 3116] <- 2
df1$egp10[df1$tempisco == 3117] <- 2
df1$egp10[df1$tempisco == 3118] <- 2
df1$egp10[df1$tempisco == 3119] <- 2
df1$egp10[df1$tempisco == 3120] <- 2
df1$egp10[df1$tempisco == 3121] <- 2
df1$egp10[df1$tempisco == 3122] <- 2
df1$egp10[df1$tempisco == 3123] <- 2
df1$egp10[df1$tempisco == 3130] <- 2
df1$egp10[df1$tempisco == 3131] <- 2
df1$egp10[df1$tempisco == 3132] <- 2
df1$egp10[df1$tempisco == 3133] <- 2
df1$egp10[df1$tempisco == 3139] <- 2
df1$egp10[df1$tempisco == 3140] <- 2
df1$egp10[df1$tempisco == 3141] <- 2
df1$egp10[df1$tempisco == 3142] <- 2
df1$egp10[df1$tempisco == 3143] <- 1
df1$egp10[df1$tempisco == 3144] <- 1
df1$egp10[df1$tempisco == 3145] <- 2
df1$egp10[df1$tempisco == 3150] <- 2
df1$egp10[df1$tempisco == 3151] <- 2
df1$egp10[df1$tempisco == 3152] <- 2
df1$egp10[df1$tempisco == 3200] <- 2
df1$egp10[df1$tempisco == 3210] <- 2
df1$egp10[df1$tempisco == 3211] <- 2
df1$egp10[df1$tempisco == 3212] <- 2
df1$egp10[df1$tempisco == 3213] <- 2
df1$egp10[df1$tempisco == 3220] <- 2
df1$egp10[df1$tempisco == 3221] <- 2
df1$egp10[df1$tempisco == 3222] <- 2
df1$egp10[df1$tempisco == 3223] <- 2
df1$egp10[df1$tempisco == 3224] <- 2
df1$egp10[df1$tempisco == 3225] <- 2
df1$egp10[df1$tempisco == 3226] <- 2
df1$egp10[df1$tempisco == 3227] <- 2
df1$egp10[df1$tempisco == 3228] <- 2
df1$egp10[df1$tempisco == 3229] <- 2
df1$egp10[df1$tempisco == 3230] <- 3
df1$egp10[df1$tempisco == 3231] <- 3
df1$egp10[df1$tempisco == 3232] <- 3
df1$egp10[df1$tempisco == 3240] <- 2
df1$egp10[df1$tempisco == 3241] <- 2
df1$egp10[df1$tempisco == 3242] <- 2
df1$egp10[df1$tempisco == 3300] <- 3
df1$egp10[df1$tempisco == 3310] <- 3
df1$egp10[df1$tempisco == 3320] <- 3
df1$egp10[df1$tempisco == 3330] <- 3
df1$egp10[df1$tempisco == 3340] <- 3
df1$egp10[df1$tempisco == 3400] <- 2
df1$egp10[df1$tempisco == 3410] <- 2
df1$egp10[df1$tempisco == 3411] <- 2
df1$egp10[df1$tempisco == 3412] <- 2
df1$egp10[df1$tempisco == 3413] <- 2
df1$egp10[df1$tempisco == 3414] <- 2
df1$egp10[df1$tempisco == 3415] <- 2
df1$egp10[df1$tempisco == 3416] <- 2
df1$egp10[df1$tempisco == 3417] <- 2
df1$egp10[df1$tempisco == 3419] <- 2
df1$egp10[df1$tempisco == 3420] <- 2
df1$egp10[df1$tempisco == 3421] <- 2
df1$egp10[df1$tempisco == 3422] <- 2
df1$egp10[df1$tempisco == 3423] <- 2
df1$egp10[df1$tempisco == 3429] <- 2
df1$egp10[df1$tempisco == 3430] <- 3
df1$egp10[df1$tempisco == 3431] <- 2
df1$egp10[df1$tempisco == 3432] <- 2
df1$egp10[df1$tempisco == 3433] <- 3
df1$egp10[df1$tempisco == 3434] <- 2
df1$egp10[df1$tempisco == 3439] <- 3
df1$egp10[df1$tempisco == 3440] <- 2
df1$egp10[df1$tempisco == 3441] <- 2
df1$egp10[df1$tempisco == 3442] <- 2
df1$egp10[df1$tempisco == 3443] <- 2
df1$egp10[df1$tempisco == 3444] <- 2
df1$egp10[df1$tempisco == 3449] <- 2
df1$egp10[df1$tempisco == 3450] <- 2
df1$egp10[df1$tempisco == 3451] <- 2
df1$egp10[df1$tempisco == 3452] <- 7
df1$egp10[df1$tempisco == 3460] <- 3
df1$egp10[df1$tempisco == 3470] <- 2
df1$egp10[df1$tempisco == 3471] <- 2
df1$egp10[df1$tempisco == 3472] <- 2
df1$egp10[df1$tempisco == 3473] <- 2
df1$egp10[df1$tempisco == 3474] <- 2
df1$egp10[df1$tempisco == 3475] <- 2
df1$egp10[df1$tempisco == 3480] <- 3
df1$egp10[df1$tempisco == 4000] <- 3
df1$egp10[df1$tempisco == 4100] <- 3
df1$egp10[df1$tempisco == 4110] <- 3
df1$egp10[df1$tempisco == 4111] <- 3
df1$egp10[df1$tempisco == 4112] <- 3
df1$egp10[df1$tempisco == 4113] <- 3
df1$egp10[df1$tempisco == 4114] <- 3
df1$egp10[df1$tempisco == 4115] <- 3
df1$egp10[df1$tempisco == 4120] <- 3
df1$egp10[df1$tempisco == 4121] <- 3
df1$egp10[df1$tempisco == 4122] <- 3
df1$egp10[df1$tempisco == 4130] <- 3
df1$egp10[df1$tempisco == 4131] <- 3
df1$egp10[df1$tempisco == 4132] <- 3
df1$egp10[df1$tempisco == 4133] <- 3
df1$egp10[df1$tempisco == 4140] <- 3
df1$egp10[df1$tempisco == 4141] <- 3
df1$egp10[df1$tempisco == 4142] <- 9
df1$egp10[df1$tempisco == 4143] <- 3
df1$egp10[df1$tempisco == 4144] <- 3
df1$egp10[df1$tempisco == 4190] <- 3
df1$egp10[df1$tempisco == 4200] <- 3
df1$egp10[df1$tempisco == 4210] <- 3
df1$egp10[df1$tempisco == 4211] <- 3
df1$egp10[df1$tempisco == 4212] <- 3
df1$egp10[df1$tempisco == 4213] <- 3
df1$egp10[df1$tempisco == 4214] <- 3
df1$egp10[df1$tempisco == 4215] <- 3
df1$egp10[df1$tempisco == 4220] <- 3
df1$egp10[df1$tempisco == 4221] <- 3
df1$egp10[df1$tempisco == 4222] <- 3
df1$egp10[df1$tempisco == 4223] <- 3
df1$egp10[df1$tempisco == 5000] <- 3
df1$egp10[df1$tempisco == 5100] <- 3
df1$egp10[df1$tempisco == 5110] <- 3
df1$egp10[df1$tempisco == 5111] <- 3
df1$egp10[df1$tempisco == 5112] <- 3
df1$egp10[df1$tempisco == 5113] <- 3
df1$egp10[df1$tempisco == 5120] <- 3
df1$egp10[df1$tempisco == 5121] <- 2
df1$egp10[df1$tempisco == 5122] <- 8
df1$egp10[df1$tempisco == 5123] <- 9
df1$egp10[df1$tempisco == 5130] <- 9
df1$egp10[df1$tempisco == 5131] <- 3
df1$egp10[df1$tempisco == 5132] <- 9
df1$egp10[df1$tempisco == 5133] <- 3
df1$egp10[df1$tempisco == 5139] <- 9
df1$egp10[df1$tempisco == 5140] <- 8
df1$egp10[df1$tempisco == 5141] <- 8
df1$egp10[df1$tempisco == 5142] <- 9
df1$egp10[df1$tempisco == 5143] <- 8
df1$egp10[df1$tempisco == 5149] <- 9
df1$egp10[df1$tempisco == 5150] <- 2
df1$egp10[df1$tempisco == 5151] <- 2
df1$egp10[df1$tempisco == 5152] <- 2
df1$egp10[df1$tempisco == 5160] <- 9
df1$egp10[df1$tempisco == 5161] <- 8
df1$egp10[df1$tempisco == 5162] <- 8
df1$egp10[df1$tempisco == 5163] <- 9
df1$egp10[df1$tempisco == 5164] <- 8
df1$egp10[df1$tempisco == 5169] <- 9
df1$egp10[df1$tempisco == 5200] <- 3
df1$egp10[df1$tempisco == 5210] <- 3
df1$egp10[df1$tempisco == 5220] <- 3
df1$egp10[df1$tempisco == 5230] <- 3
df1$egp10[df1$tempisco == 6000] <- 10
df1$egp10[df1$tempisco == 6100] <- 10
df1$egp10[df1$tempisco == 6110] <- 10
df1$egp10[df1$tempisco == 6111] <- 10
df1$egp10[df1$tempisco == 6112] <- 10
df1$egp10[df1$tempisco == 6113] <- 10
df1$egp10[df1$tempisco == 6114] <- 10
df1$egp10[df1$tempisco == 6120] <- 10
df1$egp10[df1$tempisco == 6121] <- 10
df1$egp10[df1$tempisco == 6122] <- 10
df1$egp10[df1$tempisco == 6123] <- 10
df1$egp10[df1$tempisco == 6124] <- 10
df1$egp10[df1$tempisco == 6129] <- 10
df1$egp10[df1$tempisco == 6130] <- 10
df1$egp10[df1$tempisco == 6131] <- 11
df1$egp10[df1$tempisco == 6132] <- 11
df1$egp10[df1$tempisco == 6133] <- 11
df1$egp10[df1$tempisco == 6134] <- 10
df1$egp10[df1$tempisco == 6140] <- 10
df1$egp10[df1$tempisco == 6141] <- 10
df1$egp10[df1$tempisco == 6142] <- 10
df1$egp10[df1$tempisco == 6150] <- 10
df1$egp10[df1$tempisco == 6151] <- 10
df1$egp10[df1$tempisco == 6152] <- 10
df1$egp10[df1$tempisco == 6153] <- 10
df1$egp10[df1$tempisco == 6154] <- 10
df1$egp10[df1$tempisco == 6200] <- 11
df1$egp10[df1$tempisco == 6210] <- 11
df1$egp10[df1$tempisco == 7000] <- 8
df1$egp10[df1$tempisco == 7100] <- 8
df1$egp10[df1$tempisco == 7110] <- 8
df1$egp10[df1$tempisco == 7111] <- 8
df1$egp10[df1$tempisco == 7112] <- 8
df1$egp10[df1$tempisco == 7113] <- 8
df1$egp10[df1$tempisco == 7120] <- 8
df1$egp10[df1$tempisco == 7121] <- 9
df1$egp10[df1$tempisco == 7122] <- 9
df1$egp10[df1$tempisco == 7123] <- 9
df1$egp10[df1$tempisco == 7124] <- 8
df1$egp10[df1$tempisco == 7129] <- 8
df1$egp10[df1$tempisco == 7130] <- 8
df1$egp10[df1$tempisco == 7131] <- 9
df1$egp10[df1$tempisco == 7132] <- 8
df1$egp10[df1$tempisco == 7133] <- 8
df1$egp10[df1$tempisco == 7134] <- 8
df1$egp10[df1$tempisco == 7135] <- 9
df1$egp10[df1$tempisco == 7136] <- 8
df1$egp10[df1$tempisco == 7137] <- 8
df1$egp10[df1$tempisco == 7140] <- 8
df1$egp10[df1$tempisco == 7141] <- 8
df1$egp10[df1$tempisco == 7142] <- 9
df1$egp10[df1$tempisco == 7143] <- 9
df1$egp10[df1$tempisco == 7200] <- 8
df1$egp10[df1$tempisco == 7210] <- 8
df1$egp10[df1$tempisco == 7211] <- 8
df1$egp10[df1$tempisco == 7212] <- 8
df1$egp10[df1$tempisco == 7213] <- 8
df1$egp10[df1$tempisco == 7214] <- 8
df1$egp10[df1$tempisco == 7215] <- 8
df1$egp10[df1$tempisco == 7216] <- 8
df1$egp10[df1$tempisco == 7220] <- 8
df1$egp10[df1$tempisco == 7221] <- 8
df1$egp10[df1$tempisco == 7222] <- 8
df1$egp10[df1$tempisco == 7223] <- 8
df1$egp10[df1$tempisco == 7224] <- 8
df1$egp10[df1$tempisco == 7230] <- 8
df1$egp10[df1$tempisco == 7231] <- 8
df1$egp10[df1$tempisco == 7232] <- 8
df1$egp10[df1$tempisco == 7233] <- 8
df1$egp10[df1$tempisco == 7234] <- 9
df1$egp10[df1$tempisco == 7240] <- 8
df1$egp10[df1$tempisco == 7241] <- 8
df1$egp10[df1$tempisco == 7242] <- 8
df1$egp10[df1$tempisco == 7243] <- 8
df1$egp10[df1$tempisco == 7244] <- 8
df1$egp10[df1$tempisco == 7245] <- 8
df1$egp10[df1$tempisco == 7300] <- 8
df1$egp10[df1$tempisco == 7310] <- 8
df1$egp10[df1$tempisco == 7311] <- 8
df1$egp10[df1$tempisco == 7312] <- 8
df1$egp10[df1$tempisco == 7313] <- 8
df1$egp10[df1$tempisco == 7320] <- 9
df1$egp10[df1$tempisco == 7321] <- 9
df1$egp10[df1$tempisco == 7322] <- 9
df1$egp10[df1$tempisco == 7323] <- 8
df1$egp10[df1$tempisco == 7324] <- 8
df1$egp10[df1$tempisco == 7330] <- 9
df1$egp10[df1$tempisco == 7331] <- 9
df1$egp10[df1$tempisco == 7332] <- 9
df1$egp10[df1$tempisco == 7340] <- 8
df1$egp10[df1$tempisco == 7341] <- 8
df1$egp10[df1$tempisco == 7342] <- 8
df1$egp10[df1$tempisco == 7343] <- 8
df1$egp10[df1$tempisco == 7344] <- 8
df1$egp10[df1$tempisco == 7345] <- 8
df1$egp10[df1$tempisco == 7346] <- 8
df1$egp10[df1$tempisco == 7400] <- 8
df1$egp10[df1$tempisco == 7410] <- 8
df1$egp10[df1$tempisco == 7411] <- 8
df1$egp10[df1$tempisco == 7412] <- 8
df1$egp10[df1$tempisco == 7413] <- 8
df1$egp10[df1$tempisco == 7414] <- 8
df1$egp10[df1$tempisco == 7415] <- 8
df1$egp10[df1$tempisco == 7416] <- 8
df1$egp10[df1$tempisco == 7420] <- 8
df1$egp10[df1$tempisco == 7421] <- 9
df1$egp10[df1$tempisco == 7422] <- 8
df1$egp10[df1$tempisco == 7423] <- 8
df1$egp10[df1$tempisco == 7424] <- 9
df1$egp10[df1$tempisco == 7430] <- 8
df1$egp10[df1$tempisco == 7431] <- 9
df1$egp10[df1$tempisco == 7432] <- 9
df1$egp10[df1$tempisco == 7433] <- 8
df1$egp10[df1$tempisco == 7434] <- 8
df1$egp10[df1$tempisco == 7435] <- 8
df1$egp10[df1$tempisco == 7436] <- 8
df1$egp10[df1$tempisco == 7437] <- 8
df1$egp10[df1$tempisco == 7440] <- 8
df1$egp10[df1$tempisco == 7441] <- 8
df1$egp10[df1$tempisco == 7442] <- 8
df1$egp10[df1$tempisco == 7500] <- 8
df1$egp10[df1$tempisco == 7510] <- 7
df1$egp10[df1$tempisco == 7520] <- 8
df1$egp10[df1$tempisco == 7530] <- 9
df1$egp10[df1$tempisco == 8000] <- 9
df1$egp10[df1$tempisco == 8100] <- 9
df1$egp10[df1$tempisco == 8110] <- 8
df1$egp10[df1$tempisco == 8111] <- 8
df1$egp10[df1$tempisco == 8112] <- 8
df1$egp10[df1$tempisco == 8113] <- 8
df1$egp10[df1$tempisco == 8120] <- 8
df1$egp10[df1$tempisco == 8121] <- 8
df1$egp10[df1$tempisco == 8122] <- 8
df1$egp10[df1$tempisco == 8123] <- 8
df1$egp10[df1$tempisco == 8124] <- 8
df1$egp10[df1$tempisco == 8130] <- 9
df1$egp10[df1$tempisco == 8131] <- 9
df1$egp10[df1$tempisco == 8139] <- 9
df1$egp10[df1$tempisco == 8140] <- 9
df1$egp10[df1$tempisco == 8141] <- 9
df1$egp10[df1$tempisco == 8142] <- 9
df1$egp10[df1$tempisco == 8143] <- 9
df1$egp10[df1$tempisco == 8150] <- 8
df1$egp10[df1$tempisco == 8151] <- 8
df1$egp10[df1$tempisco == 8152] <- 8
df1$egp10[df1$tempisco == 8153] <- 8
df1$egp10[df1$tempisco == 8154] <- 8
df1$egp10[df1$tempisco == 8155] <- 8
df1$egp10[df1$tempisco == 8159] <- 8
df1$egp10[df1$tempisco == 8160] <- 8
df1$egp10[df1$tempisco == 8161] <- 8
df1$egp10[df1$tempisco == 8162] <- 8
df1$egp10[df1$tempisco == 8163] <- 8
df1$egp10[df1$tempisco == 8170] <- 8
df1$egp10[df1$tempisco == 8171] <- 8
df1$egp10[df1$tempisco == 8172] <- 8
df1$egp10[df1$tempisco == 8200] <- 9
df1$egp10[df1$tempisco == 8210] <- 8
df1$egp10[df1$tempisco == 8211] <- 8
df1$egp10[df1$tempisco == 8212] <- 9
df1$egp10[df1$tempisco == 8220] <- 9
df1$egp10[df1$tempisco == 8221] <- 9
df1$egp10[df1$tempisco == 8222] <- 9
df1$egp10[df1$tempisco == 8223] <- 9
df1$egp10[df1$tempisco == 8224] <- 9
df1$egp10[df1$tempisco == 8229] <- 9
df1$egp10[df1$tempisco == 8230] <- 9
df1$egp10[df1$tempisco == 8231] <- 9
df1$egp10[df1$tempisco == 8232] <- 9
df1$egp10[df1$tempisco == 8240] <- 9
df1$egp10[df1$tempisco == 8250] <- 9
df1$egp10[df1$tempisco == 8251] <- 9
df1$egp10[df1$tempisco == 8252] <- 9
df1$egp10[df1$tempisco == 8253] <- 9
df1$egp10[df1$tempisco == 8260] <- 9
df1$egp10[df1$tempisco == 8261] <- 9
df1$egp10[df1$tempisco == 8262] <- 9
df1$egp10[df1$tempisco == 8263] <- 9
df1$egp10[df1$tempisco == 8264] <- 9
df1$egp10[df1$tempisco == 8265] <- 9
df1$egp10[df1$tempisco == 8266] <- 9
df1$egp10[df1$tempisco == 8269] <- 9
df1$egp10[df1$tempisco == 8270] <- 9
df1$egp10[df1$tempisco == 8271] <- 9
df1$egp10[df1$tempisco == 8272] <- 9
df1$egp10[df1$tempisco == 8273] <- 9
df1$egp10[df1$tempisco == 8274] <- 9
df1$egp10[df1$tempisco == 8275] <- 9
df1$egp10[df1$tempisco == 8276] <- 9
df1$egp10[df1$tempisco == 8277] <- 9
df1$egp10[df1$tempisco == 8278] <- 9
df1$egp10[df1$tempisco == 8279] <- 9
df1$egp10[df1$tempisco == 8280] <- 9
df1$egp10[df1$tempisco == 8281] <- 9
df1$egp10[df1$tempisco == 8282] <- 9
df1$egp10[df1$tempisco == 8283] <- 9
df1$egp10[df1$tempisco == 8284] <- 9
df1$egp10[df1$tempisco == 8285] <- 9
df1$egp10[df1$tempisco == 8286] <- 9
df1$egp10[df1$tempisco == 8290] <- 9
df1$egp10[df1$tempisco == 8300] <- 9
df1$egp10[df1$tempisco == 8310] <- 9
df1$egp10[df1$tempisco == 8311] <- 8
df1$egp10[df1$tempisco == 8312] <- 9
df1$egp10[df1$tempisco == 8320] <- 9
df1$egp10[df1$tempisco == 8321] <- 9
df1$egp10[df1$tempisco == 8322] <- 9
df1$egp10[df1$tempisco == 8323] <- 9
df1$egp10[df1$tempisco == 8324] <- 9
df1$egp10[df1$tempisco == 8330] <- 9
df1$egp10[df1$tempisco == 8331] <- 10
df1$egp10[df1$tempisco == 8332] <- 8
df1$egp10[df1$tempisco == 8333] <- 8
df1$egp10[df1$tempisco == 8334] <- 9
df1$egp10[df1$tempisco == 8340] <- 9
df1$egp10[df1$tempisco == 8400] <- 9
df1$egp10[df1$tempisco == 9000] <- 9
df1$egp10[df1$tempisco == 9100] <- 3
df1$egp10[df1$tempisco == 9110] <- 3
df1$egp10[df1$tempisco == 9111] <- 3
df1$egp10[df1$tempisco == 9112] <- 3
df1$egp10[df1$tempisco == 9113] <- 3
df1$egp10[df1$tempisco == 9120] <- 9
df1$egp10[df1$tempisco == 9130] <- 9
df1$egp10[df1$tempisco == 9131] <- 9
df1$egp10[df1$tempisco == 9132] <- 9
df1$egp10[df1$tempisco == 9133] <- 9
df1$egp10[df1$tempisco == 9140] <- 9
df1$egp10[df1$tempisco == 9141] <- 9
df1$egp10[df1$tempisco == 9142] <- 9
df1$egp10[df1$tempisco == 9150] <- 9
df1$egp10[df1$tempisco == 9151] <- 9
df1$egp10[df1$tempisco == 9152] <- 9
df1$egp10[df1$tempisco == 9153] <- 9
df1$egp10[df1$tempisco == 9160] <- 9
df1$egp10[df1$tempisco == 9161] <- 9
df1$egp10[df1$tempisco == 9162] <- 9
df1$egp10[df1$tempisco == 9200] <- 9
df1$egp10[df1$tempisco == 9210] <- 10
df1$egp10[df1$tempisco == 9211] <- 10
df1$egp10[df1$tempisco == 9212] <- 10
df1$egp10[df1$tempisco == 9213] <- 10
df1$egp10[df1$tempisco == 9300] <- 9
df1$egp10[df1$tempisco == 9310] <- 9
df1$egp10[df1$tempisco == 9311] <- 9
df1$egp10[df1$tempisco == 9312] <- 9
df1$egp10[df1$tempisco == 9313] <- 9
df1$egp10[df1$tempisco == 9320] <- 9
df1$egp10[df1$tempisco == 9321] <- 9
df1$egp10[df1$tempisco == 9322] <- 9
df1$egp10[df1$tempisco == 9330] <- 9
df1$egp10[df1$tempisco == 9331] <- 9
df1$egp10[df1$tempisco == 9332] <- 9
df1$egp10[df1$tempisco == 9333] <- 9
}

sjmisc::frq(df1$egp10) 

# Start of ISCOEGP.INC section
# `p' codes promotability of certain occupations
df1$p <- rep(NA, length(df1$tempisco))
df1$p[df1$tempisco >= 1000 & df1$tempisco <= 9299] <- 1
sjmisc::frq(df1$p)


# `d' codes degradability of certain occupations
df1$d <- rep(NA, length(df1$tempisco))
df1$d[df1$tempisco >= 1300 & df1$tempisco <= 1319 |
          df1$tempisco >= 3400 & df1$tempisco <= 3439 |
          df1$tempisco >= 4000 & df1$tempisco <= 5230] <- 1
sjmisc::frq(df1$d)

df1$egp10_b <- df1$egp10
sjmisc::frq(df1$egp10_b)

# quietly replace `varlist'=2  if  (`varlist' == 3 & `tmpsv' >= 1 & `tmpsv' ~= .);
df1$egp10_b[df1$egp10_b == 3 & df1$supvis >= 1 & !is.na(df1$supvis)] <- 2

# quietly replace `varlist'=4  if ((`varlist' == 3 | `varlist' == 2) &
# 	                                  `sempl' == 1 &
# 	                                  `d' == 1);
	                                  
df1$egp10_b[df1$egp10_b %in% c(3, 2) & df1$semp == 1 & df1$d == 1] <- 4

# quietly replace `varlist'=5  if ((`varlist' >= 7 & `varlist' <= 9) &
# 	                                  `sempl' == 1 &
# 	                                  `p' == 1);
df1$egp10_b[df1$egp10_b %in% c(7, 8, 9) & df1$semp == 1 & df1$p == 1] <- 5

# quietly replace `varlist'=7  if (`varlist' == 8  & `tmpsv' >= 1 & `tmpsv' ~= .);
df1$egp10_b[df1$egp10_b == 8 & df1$supvis >= 1 & !is.na(df1$supvis)] <- 7

# quietly replace `varlist'=11 if (`varlist' == 10 & `sempl' == 1);
df1$egp10_b[df1$egp10_b == 10 & df1$semp == 1] <- 11

# quietly replace `varlist'=5  if (`varlist' == 4 & `tmpsv' < 1);
df1$egp10_b[df1$egp10_b == 4 & df1$supvis < 1] <- 5


# quietly replace `varlist'=4 if  (`varlist' == 5 & `tmpsv' >= 1 & `tmpsv' ~= .);
df1$egp10_b[df1$egp10_b == 5 & df1$supvis >= 1 & !is.na(df1$supvis)] <- 4

# quietly replace `varlist'=1 if  ((`varlist' == 2 | `varlist' == 3 | `varlist' == 4) &
#                                   `tmpsv' >= 10 & `tmpsv' ~= .);
df1$egp10_b[df1$egp10_b %in% c(2, 3, 4) & df1$supvis >= 10 & !is.na(df1$supvis)] <- 1

sjmisc::frq(df1$egp10_b)

df1$egp10_b <- sjlabelled::set_labels(x = df1$egp10_b,
                                        labels = c("higher controllers"=1 ,
                                                                     "lo controllers"=2,
                                                                     "routine nonmanual"=3 ,
                                                                     "sempl with emp"=4,
                                                                     "sempl without empl"=5,
                                                                     "manual supervisor"=7,
                                                                     "skilled manual"=8,
                                                                     "semi-unskilld manual"=9,
                                                                     "farm labor"=10,
                                                                     "selfempl farm"=11))

sjmisc::frq(df1$egp10_b)
df1$egp10r <- df1$egp10_b
df1$egp10_b <- NULL
sjmisc::frq(df1$egp10r)

# total N=44492 valid N=36698 mean=4.83 sd=3.27
# Value |                Label |    N | Raw % | Valid % | Cum. %
# --------------------------------------------------------------
#     1 |   higher controllers | 5213 | 11.72 |   14.21 |  14.21
#     2 |       lo controllers | 7763 | 17.45 |   21.15 |  35.36
#     3 |    routine nonmanual | 6770 | 15.22 |   18.45 |  53.81
#     4 |       sempl with emp |  751 |  1.69 |    2.05 |  55.85
#     5 |   sempl without empl | 2795 |  6.28 |    7.62 |  63.47
#     7 |    manual supervisor |    7 |  0.02 |    0.02 |  63.49
#     8 |       skilled manual | 4525 | 10.17 |   12.33 |  75.82
#     9 | semi-unskilld manual | 6965 | 15.65 |   18.98 |  94.80
#    10 |           farm labor |  856 |  1.92 |    2.33 |  97.13
#    11 |        selfempl farm | 1053 |  2.37 |    2.87 | 100.00

#              class10 |      Freq.     Percent        Cum.
# ---------------------+-----------------------------------
#   higher controllers |      5,094       13.88       13.88
#       lo controllers |      7,763       21.15       35.03
#    routine nonmanual |      6,770       18.45       53.48
#       sempl with emp |        870        2.37       55.85
#   sempl without empl |      2,795        7.62       63.47
#    manual supervisor |          7        0.02       63.49
#       skilled manual |      4,525       12.33       75.82
# semi-unskilld manual |      6,965       18.98       94.80
#           farm labor |        855        2.33       97.13
#        selfempl farm |      1,054        2.87      100.00
# ---------------------+-----------------------------------
#                Total |     36,698      100.00










