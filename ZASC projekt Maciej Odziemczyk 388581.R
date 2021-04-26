# biblioteki
library('XML')
library("RCurl")
library('rvest')
library('data.table')
library('xts')
library('dygraphs')
library('htmltools')
library('car')
library('FinTS')
library('fGarch')
library('tseries')
library('moments')
library('rugarch')

# Potrzebne do konwertowania daty
Sys.setlocale("LC_TIME", "C")
#Sys.setlocale("LC_TIME", "Polish")
#Sys.setenv(LANG = "pl")

options(stringsAsFactors = FALSE)



# linki do kursów kryptowalut
# M - Maciej
urlMOBI = "http://coinmarketcap.com/currencies/mobius/historical-data/?start=20190403&end=20200620"
urlMUE = "http://coinmarketcap.com/currencies/monetaryunit/historical-data/?start=20190403&end=20200620"
# O - Odziemczyk
urlTRAC = "http://coinmarketcap.com/currencies/origintrail/historical-data/?start=20190403&end=20200620"
urlOK = "http://coinmarketcap.com/currencies/okcash/historical-data/?start=20190403&end=20200620"

########################
# Przygotowanie danych #
########################

# Scrapowanie
MUE <- urlMUE %>%
  html() %>%
  html_nodes(xpath='//*[@id="__next"]/div/div[2]/div[1]/div[2]/div[3]/div/ul[2]/li[5]/div/div/div[2]/div[3]/div/table') %>%
  html_table()
MUE <- MUE[[1]]

MOBI <- urlMOBI %>%
  html() %>%
  html_nodes(xpath='//*[@id="__next"]/div/div[2]/div[1]/div[2]/div[3]/div/ul[2]/li[5]/div/div/div[2]/div[3]/div/table') %>%
  html_table()
MOBI <- MOBI[[1]]

OK <- urlOK %>%
  html() %>%
  html_nodes(xpath='//*[@id="__next"]/div/div[2]/div[1]/div[2]/div[3]/div/ul[2]/li[5]/div/div/div[2]/div[3]/div/table') %>%
  html_table()
OK <- OK[[1]]

TRAC <- urlTRAC %>%
  html() %>%
  html_nodes(xpath='//*[@id="__next"]/div/div[2]/div[1]/div[2]/div[3]/div/ul[2]/li[5]/div/div/div[2]/div[3]/div/table') %>%
  html_table()
TRAC <- TRAC[[1]]

# zmiana formatu daty
MUE$Date = as.Date(MUE$Date, "%b %d, %Y")
MOBI$Date = as.Date(MOBI$Date, "%b %d, %Y")
OK$Date = as.Date(OK$Date, "%b %d, %Y")
TRAC$Date = as.Date(TRAC$Date, "%b %d, %Y")

# pozostawienie tylko interesujacych mnie kolumn (Date, Close, Market Cap)
MUE <- MUE[c(1,5,7)]
MOBI <- MOBI[c(1,5,7)]
OK <- OK[c(1,5,7)]
TRAC <- TRAC[c(1,5,7)]

# zmiana nazw kolumn
colnames(MUE) <- c("Date", "MUE_close", "MUE_cap")
colnames(MOBI) <- c("Date", "MOBI_close", "MOBI_cap")
colnames(OK) <- c("Date", "OK_close", "OK_cap")
colnames(TRAC) <- c("Date", "TRAC_close", "TRAC_cap")

# zmiana formatu kapitalizacji na numeric
MUE$MUE_cap = as.numeric(gsub(',', '', MUE$MUE_cap))
MOBI$MOBI_cap = as.numeric(gsub(',', '', MOBI$MOBI_cap))
OK$OK_cap = as.numeric(gsub(',', '', OK$OK_cap))
TRAC$TRAC_cap = as.numeric(gsub(',', '', TRAC$TRAC_cap))

# ³¹czenie tabel
data = merge(MUE, MOBI, by.x = "Date", by.y = "Date")
data = merge(data, OK, by.x = "Date", by.y = "Date")
data = merge(data, TRAC, by.x = "Date", by.y = "Date")

######################
# budowanie portfela #
######################

# total 
data$total_cap = data$MUE_cap + data$MOBI_cap + data$OK_cap + data$TRAC_cap

# udzia³ procentowy
data$MUE_p = (data$MUE_cap/data$total_cap)
data$MOBI_p = (data$MOBI_cap/data$total_cap)
data$OK_p = (data$OK_cap/data$total_cap)
data$TRAC_p = (data$TRAC_cap/data$total_cap)

# usuwanie niepotrzebnych kolumn
data = data[-c(3, 5, 7, 9, 10)]

# obliczanie logarytmicznych stóp zwrotu
data$MUE_r = diff.xts(log(data$MUE_close))
data$MOBI_r = diff.xts(log(data$MOBI_close))
data$OK_r = diff.xts(log(data$OK_close))
data$TRAC_r = diff.xts(log(data$TRAC_close))

# na portfelu
data$total_r = (data$MUE_p*data$MUE_r
                +data$MOBI_p*data$MOBI_r
                +data$OK_p*data$OK_r
                +data$TRAC_p*data$TRAC_r)

###########
# Analiza #
###########

# Wykres udzialu w czasie
share_xts = xts(data[, c("MUE_p", "MOBI_p", "OK_p", "TRAC_p")], order.by = data$Date)
dygraph(share_xts, main = "Profil portfelowy") %>%
  dyRangeSelector(height = 50)
  
# portfel jest w miare zdywersyfikowany, z zastrzerzeniem okresu po kwietniu 2020, wtedy udzial TRAC
# dochodzi nawet do 80%, ta waluta utrzymuje utrzymuje przez caly okres poziom powyzej 30%. Zblizonym do TRAC
# udzialem do kwietnia 2020 charakteryzuje sie MOBI, po kwietniu 2020 widzimy odwrotny trend niz w przypadku TRAC
# waluty OK i MUE stanowia dopelnienie, warto zauwazyc dosc stabilny poziom MUE ~10% przez wiekszosc okresu, OK natomiast
# wahalo sie od ~0.07 do ~0.36


# wykresy wszystkich cen zamkniêcia i stóp zwrotu z portfela

# obiekty pomocnicze (dygraph musi mieæ obiekt w formacie xts)
close_xts = xts(data[, c("MUE_close", "MOBI_close", "OK_close", "TRAC_close")], order.by = data$Date)
tot_ret_xts = xts(data[, c("total_r")], order.by = data$Date)

# wykres cen zamkniêcia
dygraph(close_xts, main = 'Ceny zamkniêcia dla wszystkich kryptowalut') %>%
  dyRangeSelector(height = 50)
# wraz ze wzrostem udzialu TRAC w portfelu obserwujemy ogromny wzrost ceny tejze kryptowaluty, do stycznia 2020
# ceny zamkniecia wykazuja podobne trendy dla wszystkich walut oczywiscie o zroznicowanej sile, co wynika rowniez
# z wartosci, ktora jak warto zauwazyc nie przekracza 0.1$ w zadnym przypadku

# wykres stóp zwrotu z portfela
dygraph(tot_ret_xts, main="Stopy zwrotów dla ca³ego portfela") %>% 
  dyRangeSelector(height = 50)

# zdecydowanie wariancja portfela po kwietniu 2020 jest wieksza niz wczesniej, najnizsza wariancje obserwujemy od 
# czerwca 2019 do stycznia 2020, wariancja z poczatku okresu (do czerwca 2019) jest jednak nizsza niz ta po kwietniu
# 2020. Latwo zaobserwowac zjawisko grupowania wariancji - wystepowanie efektow ARCH

# wykresy jako subploty (trzeba powiekszyc okno wykresu)
dy_graph <- list(
  dygraphs::dygraph(close_xts, group="temp_close", main='Ceny zamkniêcia dla wszystkich kryptowalut') %>%
    dyRangeSelector(height = 30),
  dygraphs::dygraph(tot_ret_xts, group="temp_tot_ret", main="Stopy zwrotów dla ca³ego portfela") %>%
    dyRangeSelector(height = 30)
)
htmltools::browsable(htmltools::tagList(dy_graph))



# Wykres ACF dla zwrotow z portfela
acf(data$total_r, lag.max = 36, na.action = na.pass,
    ylim = c(-0.3, 0.3), 
    col = "darkblue", lwd = 7,
    main = "Wykres ACF zwrotów z portfela")
# ACF sugeruje wystepowanie zaleznosci sredniej ruchomej - istotne pierwsze opoznienie i stopniowe wygasanie,
#warto to jednak potwierdzic formalnym testem


durbinWatsonTest(lm(data$total_r ~ 1),
                 max.lag = 5) # 5 pierwszych opóŸnieñ
# formalny test potwierdzil wystepowanie autokorelacji dla opoznien 1 i 4 (odrzucona H0 o braku autokorelacji)

# Wykresy ACF^2 dla zwrotow z portfela (efekty ARCH)
acf(data$total_r^2, lag.max = 100, na.action = na.pass,
    ylim = c(-0.3, 0.3),
    col = "darkblue", lwd = 7,
    main = "Wykres ACF kwadratów zwrotów z portfela")
# z wykresu mozna odczytac silne efekty ARCH dla pierwszych 5 opoznien, warto to zweryfikowac formalnymi testami

# test LM ARCH
ArchTest(data$total_r, lags = 5)
# hipoteza o braku efektow ARCH jest w tym przypadku silnie odrzucana, statystyka testowa to 58 z p value niemalze 0

durbinWatsonTest(lm(data$total_r^2 ~ 1),
                 max.lag = 5) # 5 pierwszych opóŸnieñ
# test Durbina Watsona pozwala na odrzucenie H0 o braku autokorelacji dla kwadratow zwrotow dla wszystkich 5 pierwszych
# opoznien

# leptokurtycznosc rozkladu zwrotow
hist(data$total_r, prob = T, breaks = 50, main = "Histogram rozk³adu zwrotów portfela", xlab="returns", col="skyblue1")
curve(dnorm(x, mean = mean(data$total_r, na.rm = T),
            sd  = sd(data$total_r, na.rm = T)),
      col = "darkblue", lwd = 2, add = TRUE,
)
# wykres wskazuje na ewidentna leptokurtycznosc (duzo wieksze maksimum i grubsze ogony rozkladu empirycznego)

# statystyki opisowe rozkladu zwrotu
empstats <- basicStats(data$total_r)
knitr::kable(as.matrix(empstats), digits = 2)
# kurtoza znacznie wyzsza od rozkladu normalnego standaryzowanego, co ciekawe skosnosc zblizona bardzo do 0, co moze
# sugerowac, ze asymetria jednak nie wystepuje

# formalny test (Jarque-Bera)

jbtest <- jarque.bera.test(na.omit(data$total_r)) # brak stopy zwrotu przy pierwszej obserwacji
jbtest
# Hipoteza o normalnosci rozkladu empirycznego jest silnie odrzucana, zatem dotychczasowe wnioski wydaja sie byc poprawne
# warto tutaj pamietac o asymptotycznosci testu jarque bera, niniejsza proba moglaby sie okazac za mala, jednakze z analizy
# statystyk opisowych i histogramu nie ma przeslanek do poddawania tychze wynikow watpliwosci.


X = data$total_r
X = X[!is.na(X)]
kurtosis(X)

# cala powyzsza analiza sugeruje korzystanie z modeli GARCH, EGARCH.

################################
# Przygotowanie do modelowania #
################################

# in sample (314 obserwacji) - zroznicowana wariancja
ins = data[which(data$Date < "2020-02-11"),]

# out of sample (132 obserwacje) - zroznicowana wariancja
outs = data[-which(data$Date < "2020-02-11"),]


# wykresy zwrotów in sample i out of sample
par(mfrow = c(2, 1))
plot(ins$Date, ins$total_r, type = "l", col = "black", lwd = 1, main = "Zwroty z portfela in sample")
plot(outs$Date, outs$total_r, type = "l", col = "blue", lwd = 1, main = "Zwroty z portfela out of sample")
par(mfrow = c(1, 1))
# widaæ grupowanie wariancji

# standaryzacja zwrotow i pierwszy kwantyl empiryczny in sample
ins$total_r_std <- (ins$total_r - mean(ins$total_r, na.rm=T)) /
  sd(ins$total_r, na.rm = T)

total_r_std_q01 <- quantile(ins$total_r_std, 0.01, na.rm = T)
total_r_std_q01
qnorm(0.01, 0, 1)
total_r_std_q01-qnorm(0.01, 0, 1)
# pierwszy kwantyl empiryczny zdecydowanie rozni sie od teoretycznego, bez watpienia lepszym  wyborem przy obliczaniu
# VaR jest kwantyl empiryczny


##############
# GARCH(1,1) #
##############

spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0),
                                     include.mean = T),
                   distribution.model = "norm")

ins.garch11 <- ugarchfit(spec = spec,
                         data = na.omit(ins$total_r))
ins.garch11

par(mfrow = c(2, 1))
plot(ins.garch11, which = 11)
plot(ins.garch11, which = 10)
par(mfrow = c(1, 1))

####################
# MA(1)-GARCH(1,1) #
####################
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1),
                                     include.mean = T),
                   distribution.model = "norm")

ins.garch11ma1 <- ugarchfit(spec = spec,
                            data = na.omit(ins$total_r))
ins.garch11ma1

par(mfrow = c(2, 1))
plot(ins.garch11ma1, which = 11)
plot(ins.garch11ma1, which = 10)
par(mfrow = c(1, 1))

########################
# ARMA(1,1)-GARCH(1,1) #
########################
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1),
                                     include.mean = T),
                   distribution.model = "norm")

ins.garch11arma11 <- ugarchfit(spec = spec,
                               data = na.omit(ins$total_r))
ins.garch11arma11

par(mfrow = c(2, 1))
plot(ins.garch11arma11, which = 11)
plot(ins.garch11arma11, which = 10)
par(mfrow = c(1, 1))

###############################
# ARMA(1,1)-GARCH(1,1) mu = 0 #
###############################
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1),
                                     include.mean = F),
                   distribution.model = "norm")

ins.garch11arma11mu0 <- ugarchfit(spec = spec,
                                  data = na.omit(ins$total_r))
ins.garch11arma11mu0

par(mfrow = c(2, 1))
plot(ins.garch11arma11mu0, which = 11)
plot(ins.garch11arma11mu0, which = 10)
par(mfrow = c(1, 1))

###########################
# MA(1)-GARCH(1,1) mu = 0 #
###########################
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1),
                                     include.mean = F),
                   distribution.model = "norm")

ins.garch11ma1mu0 <- ugarchfit(spec = spec,
                                  data = na.omit(ins$total_r))
ins.garch11ma1mu0

par(mfrow = c(2, 1))
plot(ins.garch11ma1mu0, which = 11)
plot(ins.garch11ma1mu0, which = 10)
par(mfrow = c(1, 1))

###########################
# Porownanie modeli GARCH #
###########################
ICsGARCH = cbind(c(-3.1465,-3.0986,-3.1468,-3.1274),c(-3.1725,-3.1126,-3.1730,-3.1486),
                 c(-3.1787,-3.1308,-3.1790,-3.1595),c(-3.1667,-3.0949,-3.1674,-3.1380),
                 c(-3.1722,-3.1124,-3.1727,-3.1483))
colnames(ICsGARCH) <- c("garch11", "ma1garch11", "ma1garch11mu0", "arma11garch11", "arma11garch11mu0")
rownames(ICsGARCH) <- c("Akaike", "Bayes", "Shibata", "Hannan-Quinn")
ICsGARCH

# najlepszy model MA(1)-GARCH(1,1) mu=0

# ----------------------EGARCH---------------------------------------
###############
# EGARCH(1,1) #
###############
spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0),
                                     include.mean = T),
                   distribution.model = "norm")

ins.egarch11 <- ugarchfit(spec = spec,
                          data = na.omit(ins$total_r))
ins.egarch11

par(mfrow = c(2, 1))
plot(ins.egarch11, which = 11)
plot(ins.egarch11, which = 10)
par(mfrow = c(1, 1))

#####################
# MA(1)-EGARCH(1,1) #
#####################
spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1),
                                     include.mean = T),
                   distribution.model = "norm")

ins.egarch11ma1 <- ugarchfit(spec = spec,
                             data = na.omit(ins$total_r))
ins.egarch11ma1

par(mfrow = c(2, 1))
plot(ins.egarch11ma1, which = 11)
plot(ins.egarch11ma1, which = 10)
par(mfrow = c(1, 1))

##########################
# MA(1)-EGARCH(1,1) mu=0 #
##########################
spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1),
                                     include.mean = F),
                   distribution.model = "norm")

ins.egarch11ma1mu0 <- ugarchfit(spec = spec,
                             data = na.omit(ins$total_r))
ins.egarch11ma1mu0

par(mfrow = c(2, 1))
plot(ins.egarch11ma1mu0, which = 11)
plot(ins.egarch11ma1mu0, which = 10)
par(mfrow = c(1, 1))


#########################
# ARMA(1,1)-EGARCH(1,1) #
#########################
spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1),
                                     include.mean = T),
                   distribution.model = "norm")

ins.egarch11arma11 <- ugarchfit(spec = spec,
                                data = na.omit(ins$total_r))
ins.egarch11arma11

par(mfrow = c(2, 1))
plot(ins.egarch11arma11, which = 11)
plot(ins.egarch11arma11, which = 10)
par(mfrow = c(1, 1))

################################
# ARMA(1,1)-EGARCH(1,1) mu = 0 #
################################
spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1),
                                     include.mean = F),
                   distribution.model = "norm")

ins.egarch11arma11mu0 <- ugarchfit(spec = spec,
                                   data = na.omit(ins$total_r))
ins.egarch11arma11mu0

par(mfrow = c(2, 1))
plot(ins.egarch11arma11mu0, which = 11)
plot(ins.egarch11arma11mu0, which = 10)
par(mfrow = c(1, 1))

############################
# Porownanie modeli EGARCH #
############################
ICsEGARCH = cbind(c(-3.1455,-3.0856,-3.1460,-3.1215),c(-3.1784,-3.1066,-3.1791,-3.1497),
                  c(-3.1772,-3.0935,-3.1782,-3.1438),c(-3.1832,-3.1114,-3.1839,-3.1545),
                  c(-3.1844,-3.1246,-3.1849,-3.1605))
colnames(ICsEGARCH) <- c("egarch11", "ma1egarch11", "arma11egarch11", "arma11egarch11mu0","ma1egarch11mu0")
rownames(ICsEGARCH) <- c("Akaike", "Bayes", "Shibata", "Hannan-Quinn")
ICsEGARCH
ICsGARCH

####################
# Najlepsze modele #
####################
# W modelu MA(1)-GARCH(1,1) wszystkie parametry poza mu sa istotne, po usunieciu sredniej, wszystkie parametry
# sa istotne. Testy Ljunga Boxa i LM wskazuja na wyeliminowanie efektow ARCH i ARMA, pomimo kilu "istotnych na oko"
# patrzac na korelogramy opoznien

# w modelu MA(1)-EGARCH(1,1) mozemy zaobserwowac dodatnie oszacowanie parametru gamma oraz nieistotnosc alfa, co pomimo
# istotnosci alfa wskazuje na brak asymetrii tj. bad news impact volatility more than good news


# model GARCH ma zdecydowanie wyzsze kryterium informacyjne Bayesa, podczas gdy EGARCH ma wyzsze inne kryteria,
# biorac pod uwage jednak "oszczednosc" parametrow przy ocenie Bayesa oraz oszacowania EGARCH to model GARCH wydaje sie
# byc lepszym wyborem

# -----MA(1)-GARCH(1,1)-----
# model
ins.garch11ma1mu0


# warunkowa wariancja
plot(ins.garch11ma1mu0, which = 3)

# korelogramy
par(mfrow = c(2, 1))
plot(ins.garch11ma1mu0, which = 11)
plot(ins.garch11ma1mu0, which = 10)
par(mfrow = c(1, 1))


hist(ins.garch11ma1mu0@fit$residuals, prob = T, breaks = 50,
     main = "Histogram reszt modelu MA(1)-GARCH(1,1)", xlab="residuals", col="skyblue1")
curve(dnorm(x, mean = mean(ins.garch11ma1mu0@fit$residuals, na.rm = T),
            sd  = sd(ins.garch11ma1mu0@fit$residuals, na.rm = T)),
      col = "darkblue", lwd = 2, add = TRUE,
)

empstats <- basicStats(ins.garch11ma1mu0@fit$residuals)
knitr::kable(as.matrix(empstats), digits = 2)
# w resztach wystepuje lepkokurtycznosc

jbtest_residuals <- jarque.bera.test(ins.garch11ma1mu0@fit$residuals)
jbtest_residuals
# reszty nie pochodza z rozkladu normalnego

plot(ins.garch11ma1mu0@fit$residuals, type ="l")

# -----MA(1)-EGARCH(1,1)-----
# model
ins.egarch11ma1mu0

# warunkowa wariancja
plot(ins.egarch11ma1mu0, which = 3)

# korelogramy
par(mfrow = c(2, 1))
plot(ins.egarch11ma1mu0, which = 11)
plot(ins.egarch11ma1mu0, which = 10)
par(mfrow = c(1, 1))


hist(ins.egarch11ma1mu0@fit$residuals, prob = T, breaks = 50,
     main = "Histogram reszt modelu MA(1)-GARCH(1,1)", xlab="residuals", col="skyblue1")
curve(dnorm(x, mean = mean(ins.egarch11ma1mu0@fit$residuals, na.rm = T),
            sd  = sd(ins.egarch11ma1mu0@fit$residuals, na.rm = T)),
      col = "darkblue", lwd = 2, add = TRUE,
)

empstats <- basicStats(ins.egarch11ma1mu0@fit$residuals)
knitr::kable(as.matrix(empstats), digits = 2)

jbtest_residuals <- jarque.bera.test(ins.egarch11ma1mu0@fit$residuals)
jbtest_residuals

plot(ins.egarch11ma1mu0@fit$residuals, type ="l")

#######
# VaR #-----------------------------------------------------------------------
#######

forecast_garch11ma1mu0 <-ugarchforecast(ins.garch11ma1mu0, n.ahead = 1)
forecast2_garch11ma1mu0 <- forecast_garch11ma1mu0@forecast$sigmaFor[1,1]
VaR_garch11ma1mu0 <- total_r_std_q01*forecast2_garch11ma1mu0
VaR_garch11ma1mu0

forecast_egarch11ma1mu0 <-ugarchforecast(ins.egarch11ma1mu0, n.ahead = 1)
forecast2_egarch11ma1mu0 <- forecast_egarch11ma1mu0@forecast$sigmaFor[1,1]
VaR_egarch11ma1mu0 <- total_r_std_q01*forecast2_egarch11ma1mu0
VaR_egarch11ma1mu0

# dodanie numeru obserwacji
data$obs = 1:length(data$total_r)

start <- data$obs[data$Date == as.Date("2020-02-11")] # pierwszy dzien oos
stop <- data$obs[data$Date == as.Date("2020-06-21")] # ostatni dzien oos
data2 <- data[start:stop, ] # df do zapisywania VaR
VaR.garch <- rep(NA, times = stop-start+1) # wektor VaR dla modelu MA(1)-GARCH(1,1), mu = 0
VaR.egarch <- rep(NA, times = stop-start+1) # wektor VaR dla modelu MA(1)-EGARCH(1,1), mu = 0

time1 = Sys.time()
for(k in start:stop){
  # generowanie zbioru danych zawierajacego obserwacje do dnia przed dniem prognozny
  # (prognozy jednodniowe) => powiekszanie proby o jeden dzien z kazda iteracja
  tmp.data <- data[data$obs <= (k-1), ] 
  # reszty standaryzowane
  tmp.data$rstd <- (tmp.data$total_r-mean(tmp.data$total_r, na.rm = T))/sd(tmp.data$total_r, na.rm = T)
  # pierwszy kwantyl empiryczny
  q01 <- quantile(tmp.data$rstd, 0.01, na.rm = T)
  # specyfikacja modelu MA(1)-GARCH(1,1), mu = 0
  spec.garch <- ugarchspec(variance.model = list(model = "sGARCH",
                                                 garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 1),
                                             include.mean = F),
                           distribution.model = "norm")
  # specyfikacja modelu MA(1)-EGARCH(1,1), mu = 0
  spec.egarch <- ugarchspec(variance.model = list(model = "eGARCH",
                                                  garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 1),
                                              include.mean = F),
                            distribution.model = "norm")
  # estymacja modelu (zwroty z portfela z bazy tmp)
    # GARCH
  tmp.garch11ma1mu0 <- ugarchfit(spec = spec.garch, data = na.omit(tmp.data$total_r))
    # EGARCH
  tmp.egarch11ma1mu0 <- ugarchfit(spec = spec.egarch, data = na.omit(tmp.data$total_r))
  # jednodniowa predykcja wariancji
    # GARCH
  sigma_forecast.garch <- ugarchforecast(tmp.garch11ma1mu0, n.ahead = 1)
    # EGARCH
  sigma_forecast.egarch <- ugarchforecast(tmp.egarch11ma1mu0, n.ahead = 1)
  # technikalia - zapis wartosci predykcji wariancji jako numeric
    # GARCH
  sigma_forecast2.garch <- sigma_forecast.garch@forecast$sigmaFor[1,1]
    # EGARCH
  sigma_forecast2.egarch <- sigma_forecast.egarch@forecast$sigmaFor[1,1]
  # obliczenie i zapis VaR do wektora (k = <start; stop>)
    # GARCH
  VaR.garch[k-start+1] <- q01*sigma_forecast2.garch
    # EGARCH
  VaR.egarch[k-start+1] <- q01*sigma_forecast2.egarch
}
time2 = Sys.time()
# czas obliczen
print(time2 - time1)
# dodanie kolumny VaR
data2$VaR.garch <- VaR.garch
data2$VaR.egarch <- VaR.egarch

# wyres zwrotóW vs. VaR w okresie OUT-OF-SAMPLE
plot(data2$Date, data2$total_r, col = "darkblue", lwd = 2, type = 'l', ylab = "zwroty/VaR", xlab = "Data")
abline(h = 0, lty = 2)
lines(data2$Date, data2$VaR.garch, lwd = 2, type = 'l', col = "darkgreen")
lines(data2$Date, data2$VaR.egarch, lwd = 2, type = 'l', col = "darkred")
legend("topleft", c("Returns out of sample", "VaR MA(1)-GARCH(1,1)", "VaR MA(1)-EGARCH(1,1)"), 
        text.col=c('darkblue', 'darkgreen', 'darkred'), cex = 0.75)


# frakcja przypadkow zwrotow mniejszych od 
# wynik idealnego modelu to 1% 
#   VaR MA(1)-GARCH(1,1)
sum(data2$total_r < data2$VaR.garch) / length(data2$VaR.garch)
#   VaR MA(1)-EGARCH(1,1)
sum(data2$total_r < data2$VaR.egarch) / length(data2$VaR.egarch)
# wniosek:
# Model VaR oparte na prognozach MA(1)-GARCH(1,1) ma wyniki du¿o bli¿sze idealowi, nalezy zatem uznac ten model
# za lepszy pod wzgledem kryterium szacowania VaR

#######################
# Analiza wrazliwosci #
#######################

# Proba treningowa (in sample) konczy sie 10-02-2020, dzieki czemu proba testowa (out of sample) zawiera
# w sobie 3 spore skoki wariancji z czego najwiekszy z nich przypada na koniec lutego - poczatek kwietnia, 
# Bardzo mozliwe, ze te szoki zwiazane sa z epidemia koronawirusa. Wydluzenie okresu in sample moze spowodowac spotegowanie
# efektow ARCH i byc moze wyestymowane wczesniej modele nie beda juz tak dobrze szacowac VaR, jednoczesnie skrocony 
# zostanie okres out of sample, jednak analiza wrazliwosci w druga strone tj. skrocenie in sample wydaje sie malo atrakcyjna
# bowiem wariancja zachowuje sie tam dosc stabilnie oraz proba spadlaby ponizej wymaganej liczebnosci 300.
# za nowa granice uznana zostaje data 24 Marca 2020.

ins2 = data[which(data$Date < "2020-03-24"),]
outs2 = data[-which(data$Date < "2020-03-24"),]

# MA(1)-GARCH(1,1), mu = 0
spec <- ugarchspec(variance.model = list(model = "sGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1),
                                     include.mean = F),
                   distribution.model = "norm")

ins2.garch11ma1mu0 <- ugarchfit(spec = spec,
                                data = na.omit(ins2$total_r))
ins2.garch11ma1mu0

par(mfrow = c(2, 1))
plot(ins2.garch11ma1mu0, which = 11)
plot(ins2.garch11ma1mu0, which = 10)
par(mfrow = c(1, 1))

par(mfrow = c(2, 1))
plot(ins.garch11ma1mu0, which = 11)
plot(ins.garch11ma1mu0, which = 10)
par(mfrow = c(1, 1))

# MA(1)-EGARCH(1,1), mu = 0
spec <- ugarchspec(variance.model = list(model = "eGARCH",
                                         garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 1),
                                     include.mean = F),
                   distribution.model = "norm")

ins2.egarch11ma1mu0 <- ugarchfit(spec = spec,
                                 data = na.omit(ins2$total_r))
ins2.egarch11ma1mu0


par(mfrow = c(2, 1))
plot(ins2.egarch11ma1mu0, which = 11)
plot(ins2.egarch11ma1mu0, which = 10)
par(mfrow = c(1, 1))


start <- data$obs[data$Date == as.Date("2020-03-24")] # pierwszy dzien oos
stop <- data$obs[data$Date == as.Date("2020-06-21")] # ostatni dzien oos
data3 <- data[start:stop, ] # df do zapisywania VaR
VaR.garch <- rep(NA, times = stop-start+1) # wektor VaR dla modelu MA(1)-GARCH(1,1), mu = 0
VaR.egarch <- rep(NA, times = stop-start+1) # wektor VaR dla modelu MA(1)-EGARCH(1,1), mu = 0

time1 = Sys.time()
for(k in start:stop){
  # generowanie zbioru danych zawierajacego obserwacje do dnia przed dniem prognozny
  # (prognozy jednodniowe) => powiekszanie proby o jeden dzien z kazda iteracja
  tmp.data <- data[data$obs <= (k-1), ] 
  # reszty standaryzowane
  tmp.data$rstd <- (tmp.data$total_r-mean(tmp.data$total_r, na.rm = T))/sd(tmp.data$total_r, na.rm = T)
  # pierwszy kwantyl empiryczny
  q01 <- quantile(tmp.data$rstd, 0.01, na.rm = T)
  # specyfikacja modelu MA(1)-GARCH(1,1), mu = 0
  spec.garch <- ugarchspec(variance.model = list(model = "sGARCH",
                                                 garchOrder = c(1, 1)),
                           mean.model = list(armaOrder = c(0, 1),
                                             include.mean = F),
                           distribution.model = "norm")
  # specyfikacja modelu MA(1)-EGARCH(1,1), mu = 0
  spec.egarch <- ugarchspec(variance.model = list(model = "eGARCH",
                                                  garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 1),
                                              include.mean = F),
                            distribution.model = "norm")
  # estymacja modelu (zwroty z portfela z bazy tmp)
  # GARCH
  tmp.garch11ma1mu0 <- ugarchfit(spec = spec.garch, data = na.omit(tmp.data$total_r))
  # EGARCH
  tmp.egarch11ma1mu0 <- ugarchfit(spec = spec.egarch, data = na.omit(tmp.data$total_r))
  # jednodniowa predykcja wariancji
  # GARCH
  sigma_forecast.garch <- ugarchforecast(tmp.garch11ma1mu0, n.ahead = 1)
  # EGARCH
  sigma_forecast.egarch <- ugarchforecast(tmp.egarch11ma1mu0, n.ahead = 1)
  # technikalia - zapis wartosci predykcji wariancji jako numeric
  # GARCH
  sigma_forecast2.garch <- sigma_forecast.garch@forecast$sigmaFor[1,1]
  # EGARCH
  sigma_forecast2.egarch <- sigma_forecast.egarch@forecast$sigmaFor[1,1]
  # obliczenie i zapis VaR do wektora (k = <start; stop>)
  # GARCH
  VaR.garch[k-start+1] <- q01*sigma_forecast2.garch
  # EGARCH
  VaR.egarch[k-start+1] <- q01*sigma_forecast2.egarch
}
time2 = Sys.time()
# czas obliczen
print(time2 - time1)
# dodanie kolumny VaR
data3$VaR.garch <- VaR.garch
data3$VaR.egarch <- VaR.egarch

# wyres zwrotóW vs. VaR w okresie OUT-OF-SAMPLE
plot(data3$Date, data3$total_r, col = "darkblue", lwd = 2, type = 'l', ylab = "zwroty/VaR", xlab = "Data")
abline(h = 0, lty = 2)
lines(data3$Date, data3$VaR.garch, lwd = 2, type = 'l', col = "darkgreen")
lines(data3$Date, data3$VaR.egarch, lwd = 2, type = 'l', col = "darkred")
legend("topleft", c("Returns out of sample", "VaR MA(1)-GARCH(1,1)", "VaR MA(1)-EGARCH(1,1)"), 
       text.col=c('darkblue', 'darkgreen', 'darkred'), cex = 0.75)


# frakcja przypadkow zwrotow mniejszych od 
# wynik idealnego modelu to 1% 
#   VaR MA(1)-GARCH(1,1)
sum(data3$total_r < data3$VaR.garch) / length(data3$VaR.garch)
#   VaR MA(1)-EGARCH(1,1)
sum(data3$total_r < data3$VaR.egarch) / length(data3$VaR.egarch)
# wniosek:
# wyniki zgodne z intuicja - model "sie douczyl" jednoczesnie przeuczajac sie na MA(1)-GARCH(1,1) i osiagajac zblizone
# wyniki do MA(1)-GARCH(1,1) na mniejszej probie przy modelu MA(1)-EGARCH(1,1). Oczywiscie ze wzgledu na inny zbior obserwacji
# nie mozemy porownywach modeli z powyzszego eksperymentu z pierwotnymi za pomoca kryteriow informacyjnych, mozemy zauwazyc 
# jedynie, ze zmiana okna wplynela na wyniki szacowania VaR niedoszacowujac go wrecz, na jak sie wczesniej okazalo, lepszym 
# dla tego problemu modelu MA(1)-GARCH(1,1)
# w modelu MA(1)-GARCH(1,1) wydluzenie okresu in sample o dodatkowy duzy skok wariancji odbilo sie na estymacji wyzszych parametrow w przypadku Beta1, 
# reszta pozostala na zblizonym poziomie co jest wytlumaczalne teoria.
# W modelu MA(1)-EGARCH(1,1) wydluzenie okresu in sample spowodowalo zwiekszenie co do wartosci bezwzglednej zarowno parametru alfa
# jak i beta, ponadto parametr alfa stal sie istotny, jednoczesnie reszta gamma zostala na bardzo zblizonym poziomie, zatem nie ma
# w tym przypadku rowniez mowy o asymetrii.
# podsumowujac, zwiekszenie liczby obsrewacji przyczynia sie do przeuczania modeli, byc moze rozszerzenie MA(1) powinno zostac usuniete
# w przypadku zwiekszania sie liczby obserwacji
