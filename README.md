# Cryptocurrencies portfolio
Project created for *Advanced Time Series Analysis* (org. *Zaawansowana Analiza Szeregów Czasowych*) classes at WNE UW

Language:

Polish - classes, report and code comments

Semester: II (MA studies)

## About
The main objective of this project was to build and analyze cryptocurriencies portfolio. Portfolio was built according to the market capitalization weighting principle and contained cryptocurriencies which names begin on first letters of author's first and last name. The data was scrapped from coinmarketcap.com, preprocessed (logarithmic returns and some needed transformations for computations), pre-analyzed (share, close prices, returns, distributions, autocorelation, heteroscedasticity), splitted into in and out of sample (for fitting and forecasting) and modelled (GARCH class models). Considered models were standard GARCH and Exponential GARCH (E-GARCH), the best ones were selected using information criteria (Akaike, Bayes, Shibata, Hannan-Quinn). After that models residuals were inspected and 1% Value at Risk was calculated. Finally estimation window sensitivity analysis was performed. In this project I've learnt a lot of time series analysis, especially on the condtional variance function estimation field what is crucial while dealing with financial instruments and high frequency data. I've practiced programming in R, gained some knowledge about its packages used in time sieries analysis and some markdown reports writing.

Findings:
 * financial instruments returns distributions are characterized by assymetry, variance groupping, leptokurtosis, so GARCH family models are very attractive there
 * GARCH model extensions are nice to try, but not always give better results
 * statistical inference about GARCH process can be biased when in sample dataset is too small (dependend on samples characteristics)
 * coronavirus had a huge impact on cryptocurriencies market (variance shocks)
 
 ## About repository
  * ZASC projekt Maciej Odziemczyk 388581.R - R script
  * ZASC projekt Maciej Odziemczyk 388581.Rmd - R markdown file, can be used to generate an html report
  * ZASC projekt Maciej Odziemczyk 388581.html - html report generated for assignment

Main packages used in this project was rugarch amd fGarch, tseries, dygraph.

## Technologies
 * R 
 * markdown

## Author
Maciej Odziemczyk
