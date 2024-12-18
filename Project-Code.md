Project Code
================
Diego Brule, Nicolas Tamara
27/11/2024

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(ggrepel)
library(readxl)
library(DIMORA)
```

    ## Loading required package: minpack.lm

    ## Loading required package: numDeriv

    ## Loading required package: forecast

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Loading required package: reshape2

    ## Loading required package: deSolve

``` r
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
#All_data <- read_xlsx('Product Data finale.xlsx')
All_data <- read.csv('Product_Data Q3_2022 to Q4_2023.csv')
```

``` r
Date_Converter <- function(month, year){
  Month_name <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  paste(month, year)
}

Date_Converter(3, 2023)
```

    ## [1] "3 2023"

\#Glimpse at the data

First, let’s look at the different time series for the different
medicine brands

``` r
#Creating a column 'mes' to represent total months. i.e. January 2023 is 13 instead of 1. This is to facilitate graphing the time series.

#Creating a column 'Month' to have a more interpretable x axis. Can't figure out how to work this yet.

All_data <- All_data %>% mutate(Mes = ifelse(AnnioCorte == 2022, MesFactura, MesFactura + 12 ))

#All_data <- All_data %>% mutate(Month = Date_Converter(MesFactura, AnnioCorte))

#ggplot(All_data, aes(x=Month, y=TotalUnidades, group = DescripcionComercial, colour = DescripcionComercial)) + geom_line() + scale_x_discrete(guide=guide_axis(check.overlap = TRUE))

ggplot(All_data, aes(x=Mes, y=TotalUnidades, group = DescripcionComercial, colour = DescripcionComercial)) + geom_line()
```

![](Project-Code_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We see that there are three brands: Iclomod, Lebrina, Gilenya That
dominate the market in terms of units sold. Let’s zoom in on those, and
the other lower-selling brands separately.

``` r
#Separate Data by different drug brands to ease analysis, select only ts variables

Aximod <- All_data %>% filter(DescripcionComercial == 'AXIMOD') %>% arrange(Mes)
Fincler <- All_data %>% filter(DescripcionComercial == 'FINCLER') %>% arrange(Mes)
Gilenya <- All_data %>% filter(DescripcionComercial == 'GILENYA') %>% arrange(Mes)
Iclomod <- All_data %>% filter(DescripcionComercial == 'ICLOMOD') %>% arrange(Mes)
Kesimpta <- All_data %>% filter(DescripcionComercial == 'KESIMPTA') %>% arrange(Mes)
Lebrina <- All_data %>% filter(DescripcionComercial == 'LEBRINA') %>% arrange(Mes)
Lemtrada <- All_data %>% filter(DescripcionComercial == 'LEMTRADA') %>% arrange(Mes)
Limostad <- All_data %>% filter(DescripcionComercial == 'LIMOSTAD') %>% arrange(Mes)
Mavenclad <- All_data %>% filter(DescripcionComercial == 'MAVENCLAD') %>% arrange(Mes)
Ocrevus <- All_data %>% filter(DescripcionComercial == 'OCREVUS') %>% arrange(Mes)
Singomod <- All_data %>% filter(DescripcionComercial == 'SINGOMOD') %>% arrange(Mes)
Tysabri <- All_data %>% filter(DescripcionComercial == 'TYSABRI') %>% arrange(Mes)
```

``` r
Trend_Data <- All_data %>% filter((DescripcionComercial == 'GILENYA') | (DescripcionComercial == 'LEBRINA') | (DescripcionComercial == 'ICLOMOD'))

ggplot(Trend_Data, aes(x=Mes, y=TotalUnidades, group = DescripcionComercial, colour = DescripcionComercial)) + geom_line()
```

![](Project-Code_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
Flat_Data <- All_data %>% filter((DescripcionComercial != 'GILENYA') & (DescripcionComercial != 'LEBRINA') & (DescripcionComercial != 'ICLOMOD'))

ggplot(Flat_Data, aes(x=Mes, y=TotalUnidades, group = DescripcionComercial, colour = DescripcionComercial)) + geom_line()
```

![](Project-Code_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

\#First round of model fitting

Since the top three brands have much higer volumes of units sold, the
movements and fluctuations in sales are also bigger, so we will start by
fitting a simple bass model, while we will start with a simple linear
model for the other brands.

``` r
BM_Iclomod <- BM(Iclomod$TotalUnidades, display = T)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
summary(BM_Iclomod)
```

    ## Call: ( Standard Bass Model )
    ## 
    ##   BM(series = Iclomod$TotalUnidades, display = T)
    ## 
    ## Residuals:
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -7603.6 -3950.3 -1192.6  -914.8   441.6 10765.2 
    ## 
    ## Coefficients:
    ##        Estimate    Std.Error        Lower        Upper  p-value    
    ## m 2.539892e+05 1.496121e+04 2.246658e+05 2.833127e+05 2.97e-10 ***
    ## p 1.647355e-02 1.555315e-03 1.342518e-02 1.952191e-02 9.20e-08 ***
    ## q 2.630499e-01 2.837341e-02 2.074390e-01 3.186607e-01 4.30e-07 ***
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  5022.665  on  13  degrees of freedom
    ##  Multiple R-squared:   0.998346  Residual sum of squares:  327953142

``` r
ggplot(Iclomod, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Iclomod$fitted), color='red') + ggtitle('Iclomod Units sold and BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
acf(BM_Iclomod$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ggplot(Iclomod, aes(x=Mes, y=BM_Iclomod$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') + ggtitle('Iclomod residuals of simple BM') + labs(x='Month', y='Residuals')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

At a shallow level, the BM fit for Iclomod data is good, it has high
$R^2$ and a relevant p-value for its variables. However, analysing the
residuals graph we can see that while it contains a more or less
constant variance, which is a good sign, it seems to have an underlying
pattern, which means there is a pattern in the data that was not
captured in the model. (Insert next model idea)

``` r
BM_Gilenya <- BM(Gilenya$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
summary(BM_Gilenya)
```

    ## Call: ( Standard Bass Model )
    ## 
    ##   BM(series = Gilenya$TotalUnidades)
    ## 
    ## Residuals:
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -3957.4 -2432.1   398.0  -131.6  1205.8  5419.8 
    ## 
    ## Coefficients:
    ##        Estimate    Std.Error        Lower        Upper  p-value    
    ## m 7.673444e+05 6.915959e+04 6.317941e+05 9.028947e+05 1.25e-08 ***
    ## p 3.664171e-02 2.581882e-03 3.158131e-02 4.170211e-02 4.23e-10 ***
    ## q 4.409796e-02 1.088727e-02 2.275930e-02 6.543662e-02 1.05e-03  **
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  2885.32  on  15  degrees of freedom
    ##  Multiple R-squared:   0.999902  Residual sum of squares:  124876105

``` r
ggplot(Gilenya, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Gilenya$fitted), color='red') + ggtitle('Gilenya units sold and simple BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
acf(BM_Gilenya$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
ggplot(Gilenya, aes(x=Mes, y=BM_Gilenya$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') + ggtitle('Gilenya residuals for simple BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

Similarly to the BM fit for Iclomod, we have a high $R^2$ and relevant
p-values, and constant variance in the residuals, but there seems to be
an underlying pattern in the residuals.

``` r
BM_Lebrina <- BM(Lebrina$TotalUnidades, display = T)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
summary(BM_Lebrina)
```

    ## Call: ( Standard Bass Model )
    ## 
    ##   BM(series = Lebrina$TotalUnidades, display = T)
    ## 
    ## Residuals:
    ##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -5409.2 -2766.1  -625.1  -118.8  1879.6  7008.9 
    ## 
    ## Coefficients:
    ##         Estimate    Std.Error         Lower        Upper p-value  
    ## m  5.593349e+05 5.032352e+07 -9.807295e+07 9.919162e+07   0.991  
    ## p  3.388489e-02 3.048464e+00 -5.940995e+00 6.008764e+00   0.991  
    ## q -3.389894e-02 3.048638e+00 -6.009120e+00 5.941322e+00   0.991  
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  3901.76  on  15  degrees of freedom
    ##  Multiple R-squared:   0.999253  Residual sum of squares:  228356008

``` r
ggplot(Lebrina, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Lebrina$fitted), color='red') + ggtitle('Lebrina units sold and simple BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
acf(BM_Lebrina$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
ggplot(Lebrina, aes(x=Mes, y=BM_Lebrina$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype = 'dashed') + ggtitle('Lebrina residuals for simple BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

And again, we see a high $R^2$, relevant p-values but less constant
variance and seemingly an underlying residuals pattern.

``` r
# A simple linear model for the smaller selling medications

lm_Kesimpta <- lm(Kesimpta$TotalUnidades ~ Kesimpta$Mes)
acf(Kesimpta$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
summary(lm_Kesimpta)
```

    ## 
    ## Call:
    ## lm(formula = Kesimpta$TotalUnidades ~ Kesimpta$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -269.98  -78.64  -15.93  112.04  264.33 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -664.971    113.784  -5.844 2.49e-05 ***
    ## Kesimpta$Mes   84.693      6.961  12.166 1.69e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 153.2 on 16 degrees of freedom
    ## Multiple R-squared:  0.9025, Adjusted R-squared:  0.8964 
    ## F-statistic:   148 on 1 and 16 DF,  p-value: 1.685e-09

``` r
ggplot(Kesimpta, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Kesimpta$coefficients[1], slope = lm_Kesimpta$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
dwtest(lm_Kesimpta)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Kesimpta
    ## DW = 2.4878, p-value = 0.7887
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Kesimpta, aes(x=Mes, y=lm_Kesimpta$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
acf(Kesimpta$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
acf(lm_Kesimpta$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-5.png)<!-- -->

Kesimpta is one of the lower-selling brands, so we first fit a linear
model to it. The model is good, it has a relatively high $R^2$ and
relevant estimators according to their p-values, but as time goes on the
variance grows. This is a sign of something missing in the model. This
can be further shown by the result of the DW test, which shows residuals
contain a positive autocorrelation as $DW>2$, and the p-value shows that
this autocorrelation is significant.

``` r
#Simple Linear model for Axmimod

lm_Aximod <- lm(Aximod$TotalUnidades ~ Aximod$Mes)
acf(Aximod$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
summary(lm_Aximod)
```

    ## 
    ## Call:
    ## lm(formula = Aximod$TotalUnidades ~ Aximod$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -152.27  -90.89  -29.93   93.33  186.38 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -18.124    145.710  -0.124    0.903
    ## Aximod$Mes     8.828      8.204   1.076    0.307
    ## 
    ## Residual standard error: 120.1 on 10 degrees of freedom
    ## Multiple R-squared:  0.1038, Adjusted R-squared:  0.01415 
    ## F-statistic: 1.158 on 1 and 10 DF,  p-value: 0.3072

``` r
ggplot(Aximod, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Aximod$coefficients[1], slope = lm_Aximod$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
dwtest(lm_Aximod)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Aximod
    ## DW = 1.2617, p-value = 0.0388
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Aximod, aes(x=Mes, y=lm_Aximod$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
acf(Aximod$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
acf(lm_Aximod$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-5.png)<!-- -->

For the Aximod data, we can see from the start by looking at the $R^2$
value that this is not a good model at all. $R^2$ Is very low, the
coefficients table show that month is not a significant estimator, and
the residuals graph shows an underlying pattern, and the DW test shows
negative, significant autocorrelation.

``` r
#Simple linear model for Fincler

lm_Fincler <- lm(Fincler$TotalUnidades ~ Fincler$Mes)
acf(Fincler$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
summary(lm_Fincler)
```

    ## 
    ## Call:
    ## lm(formula = Fincler$TotalUnidades ~ Fincler$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -45.847 -37.477  -2.366  20.328  95.282 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)    8.450     44.611   0.189    0.854
    ## Fincler$Mes    2.435      3.396   0.717    0.492
    ## 
    ## Residual standard error: 43.85 on 9 degrees of freedom
    ## Multiple R-squared:  0.05404,    Adjusted R-squared:  -0.05107 
    ## F-statistic: 0.5141 on 1 and 9 DF,  p-value: 0.4916

``` r
ggplot(Fincler, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Fincler$coefficients[1], slope = lm_Fincler$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
dwtest(lm_Fincler)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Fincler
    ## DW = 1.7438, p-value = 0.1993
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Fincler, aes(x=Mes, y=lm_Fincler$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
acf(lm_Fincler$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

Similar to the Aximod data, the linear model doesn’t fit right to the
Fincler data at all, despite it looking quite linear in the first
glimpse we had of the data.

``` r
lm_Lemtrada <- lm(Lemtrada$TotalUnidades ~ Lemtrada$Mes)
acf(Lemtrada$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
summary(lm_Lemtrada)
```

    ## 
    ## Call:
    ## lm(formula = Lemtrada$TotalUnidades ~ Lemtrada$Mes)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -72.24 -28.73 -14.08  18.78 174.13 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   242.164     42.835   5.653 3.59e-05 ***
    ## Lemtrada$Mes   -3.312      2.621  -1.264    0.224    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.68 on 16 degrees of freedom
    ## Multiple R-squared:  0.09075,    Adjusted R-squared:  0.03392 
    ## F-statistic: 1.597 on 1 and 16 DF,  p-value: 0.2244

``` r
ggplot(Lemtrada, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Lemtrada$coefficients[1], slope = lm_Lemtrada$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
dwtest(lm_Lemtrada)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Lemtrada
    ## DW = 2.392, p-value = 0.7211
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Lemtrada, aes(x=Mes, y=lm_Lemtrada$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
acf(lm_Lemtrada$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-14-4.png)<!-- --> For
the Lemtrada data, we can see again a low $R^2$ value, and in the
coefficient table we can see that the Month variable has a high p-value,
which shows it’s not a significant estimator for sales. The only ‘good’
thing we can see in this model is that there is no significant residual
correlation.

``` r
lm_Limostad <- lm(Limostad$TotalUnidades ~ Limostad$Mes)
acf(Limostad$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
summary(lm_Limostad)
```

    ## 
    ## Call:
    ## lm(formula = Limostad$TotalUnidades ~ Limostad$Mes)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -140.451  -78.893   -9.105   45.722  217.611 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   352.913    118.466   2.979   0.0176 *
    ## Limostad$Mes  -15.646      9.129  -1.714   0.1249  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 110.5 on 8 degrees of freedom
    ## Multiple R-squared:  0.2686, Adjusted R-squared:  0.1771 
    ## F-statistic: 2.937 on 1 and 8 DF,  p-value: 0.1249

``` r
ggplot(Limostad, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Limostad$coefficients[1], slope = lm_Limostad$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
dwtest(lm_Limostad)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Limostad
    ## DW = 1.3326, p-value = 0.05509
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Limostad, aes(x=Mes, y=lm_Limostad$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
acf(lm_Limostad$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-15-4.png)<!-- --> Same
conclusion as Limostad data for all the same reasons.

``` r
lm_Mavenclad <- lm(Mavenclad$TotalUnidades ~ Mavenclad$Mes)

acf(Mavenclad$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
summary(lm_Lemtrada)
```

    ## 
    ## Call:
    ## lm(formula = Lemtrada$TotalUnidades ~ Lemtrada$Mes)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -72.24 -28.73 -14.08  18.78 174.13 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   242.164     42.835   5.653 3.59e-05 ***
    ## Lemtrada$Mes   -3.312      2.621  -1.264    0.224    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.68 on 16 degrees of freedom
    ## Multiple R-squared:  0.09075,    Adjusted R-squared:  0.03392 
    ## F-statistic: 1.597 on 1 and 16 DF,  p-value: 0.2244

``` r
ggplot(Mavenclad, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Mavenclad$coefficients[1], slope = lm_Mavenclad$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
dwtest(lm_Mavenclad)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Mavenclad
    ## DW = 1.4486, p-value = 0.06568
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Mavenclad, aes(x=Mes, y=lm_Mavenclad$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
acf(lm_Mavenclad$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-16-4.png)<!-- --> Again,
same conclusion as the last two models for the same reasons.

``` r
lm_Ocrevus <- lm(Ocrevus$TotalUnidades ~ Ocrevus$Mes)
acf(Ocrevus$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
summary(lm_Ocrevus)
```

    ## 
    ## Call:
    ## lm(formula = Ocrevus$TotalUnidades ~ Ocrevus$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -300.64 -148.72  -24.29  108.31  330.21 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  456.522    139.391   3.275  0.00476 **
    ## Ocrevus$Mes   13.017      8.528   1.526  0.14645   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 187.7 on 16 degrees of freedom
    ## Multiple R-squared:  0.1271, Adjusted R-squared:  0.07254 
    ## F-statistic:  2.33 on 1 and 16 DF,  p-value: 0.1464

``` r
ggplot(Ocrevus, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Ocrevus$coefficients[1], slope = lm_Ocrevus$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
dwtest(lm_Ocrevus)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Ocrevus
    ## DW = 1.3038, p-value = 0.03185
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Ocrevus, aes(x=Mes, y=lm_Ocrevus$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
acf(lm_Ocrevus$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-17-4.png)<!-- --> And
again, same conclusion same reasons.

``` r
lm_Singomod <- lm(Singomod$TotalUnidades ~ Singomod$Mes)
acf(Singomod$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
summary(lm_Singomod)
```

    ## 
    ## Call:
    ## lm(formula = Singomod$TotalUnidades ~ Singomod$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -228.25 -168.06  -74.27    5.84  898.29 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   361.037    211.313   1.709    0.107
    ## Singomod$Mes   -5.515     12.928  -0.427    0.675
    ## 
    ## Residual standard error: 284.6 on 16 degrees of freedom
    ## Multiple R-squared:  0.01125,    Adjusted R-squared:  -0.05055 
    ## F-statistic: 0.182 on 1 and 16 DF,  p-value: 0.6754

``` r
ggplot(Singomod, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Singomod$coefficients[1], slope = lm_Singomod$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
dwtest(lm_Singomod)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Singomod
    ## DW = 1.7027, p-value = 0.1762
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Singomod, aes(x=Mes, y=lm_Singomod$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

``` r
acf(lm_Singomod$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-18-4.png)<!-- --> Same
conclusions same reasons.

``` r
lm_Tysabri <- lm(Tysabri$TotalUnidades ~ Tysabri$Mes)
acf(Tysabri$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
summary(lm_Tysabri)
```

    ## 
    ## Call:
    ## lm(formula = Tysabri$TotalUnidades ~ Tysabri$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -426.08 -143.66   -1.82   85.02  418.36 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1342.12     188.54   7.119 2.44e-06 ***
    ## Tysabri$Mes    25.55      11.53   2.215   0.0416 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 253.9 on 16 degrees of freedom
    ## Multiple R-squared:  0.2347, Adjusted R-squared:  0.1869 
    ## F-statistic: 4.907 on 1 and 16 DF,  p-value: 0.04161

``` r
ggplot(Tysabri, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Tysabri$coefficients[1], slope = lm_Tysabri$coefficients[2], color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
dwtest(lm_Tysabri)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_Tysabri
    ## DW = 1.3977, p-value = 0.05171
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Tysabri, aes(x=Mes, y=lm_Tysabri$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

``` r
acf(lm_Tysabri$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

From the Tysabri data, we can see in the coefficients table that Month
is actually a significant predictor for sales, but $R^2$ is low, the
residuals graph does not have constant variance and shows an underlying
pattern, and the DW test backs this up as it has a low value, indicating
negative significant autocorrelation.

Now, let’s have a deeper look into some models: Iclomod, Aximod and
Kesimpta. A reminder of what their data and their first models look
like:

``` r
ggplot(Iclomod, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Iclomod$fitted), color='red') + ggtitle('Iclomod Units sold and BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ggplot(Kesimpta, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Kesimpta$coefficients[1], slope = lm_Kesimpta$coefficients[2], color='red') +ggtitle('Kesimpta Units Sold and BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
ggplot(Aximod, aes(x=Mes, y=TotalUnidades)) + geom_line(linetype='dashed') + geom_point() +geom_abline(intercept = lm_Aximod$coefficients[1], slope = lm_Aximod$coefficients[2], color='red') + ggtitle('Aximod Units Sold and BM fit')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-20-3.png)<!-- --> For
Iclomod, let’s consider a GBM with an exponential shock.

``` r
GBM_Iclomod <- GBM(Iclomod$TotalUnidades, shock = 'exp', nshock=1, prelimestimates = c(BM_Iclomod$coefficients[1], BM_Iclomod$coefficients[2], BM_Iclomod$coefficients[3], 9, -0.1, 0.4))
```

![](Project-Code_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggplot(Iclomod, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(aes(y=make.instantaneous(GBM_Iclomod$fitted)), color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

``` r
acf(GBM_Iclomod$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->

``` r
ggplot(Iclomod, aes(x=Mes, y=GBM_Iclomod$residuals)) + geom_point() + geom_hline(yintercept=0, linetype = 'dashed') 
```

![](Project-Code_files/figure-gfm/unnamed-chunk-21-4.png)<!-- -->

``` r
summary(GBM_Iclomod)
```

    ## Call: ( Generalized Bass model with 1  Exponential  shock )
    ## 
    ##   GBM(series = Iclomod$TotalUnidades, shock = "exp", nshock = 1, 
    ##     prelimestimates = c(BM_Iclomod$coefficients[1], BM_Iclomod$coefficients[2], 
    ##         BM_Iclomod$coefficients[3], 9, -0.1, 0.4))
    ## 
    ## Residuals:
    ##       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -4915.628 -2638.739     7.217  -368.048  1661.800  3205.493 
    ## 
    ## Coefficients:
    ##            Estimate    Std.Error         Lower         Upper  p-value    
    ## m     2.403888e+05 9.209344e+03  2.223388e+05  2.584388e+05 1.57e-10 ***
    ## p     1.083553e-02 1.516746e-03  7.862761e-03  1.380830e-02 3.13e-05 ***
    ## q     3.868816e-01 3.374908e-02  3.207346e-01  4.530286e-01 4.49e-07 ***
    ## a1    8.468757e+00 3.835213e-01  7.717069e+00  9.220445e+00 8.14e-10 ***
    ## b1   -5.211089e-01 3.627366e-01 -1.232060e+00  1.898417e-01 1.81e-01    
    ## c1   -9.032272e-01 3.798517e-01 -1.647723e+00 -1.587316e-01 3.88e-02   *
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  3172.659  on  10  degrees of freedom
    ##  Multiple R-squared:   0.998761  Residual sum of squares:  100657622

We can see an improvement in the model for the Iclomod data, for
starters, the $R^2$ is higher, secondly, residual variance is much more
stable and less trend and pattern is visible in it, shown in the lower
residual standard error. The shock introduced at month 9 suggests an
event that occured in September 2023 that may have positively shocked
sales in the Iclomod brand. But with a superficial look at data from
other Gilenya and Lebrina, the two other big brands in the market, we
can see that sales also spike up in September 2023, although not as
violently as for Iclomod. This could suggest that there was some event,
campaign or policy that affected the market.

For Kesimpta, let’s consider transforming the data to a log scale, as
there is constantly growing variance shown.

``` r
summary(lm_Kesimpta)
```

    ## 
    ## Call:
    ## lm(formula = Kesimpta$TotalUnidades ~ Kesimpta$Mes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -269.98  -78.64  -15.93  112.04  264.33 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -664.971    113.784  -5.844 2.49e-05 ***
    ## Kesimpta$Mes   84.693      6.961  12.166 1.69e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 153.2 on 16 degrees of freedom
    ## Multiple R-squared:  0.9025, Adjusted R-squared:  0.8964 
    ## F-statistic:   148 on 1 and 16 DF,  p-value: 1.685e-09

``` r
Kesimpta_log <- log(Kesimpta$TotalUnidades)

lm_log_Kesimpta <- lm(Kesimpta_log ~ Kesimpta$Mes)

summary(lm_log_Kesimpta)
```

    ## 
    ## Call:
    ## lm(formula = Kesimpta_log ~ Kesimpta$Mes)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.84833 -0.19277  0.05663  0.24264  0.63845 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3.29571    0.27567   11.96 2.17e-09 ***
    ## Kesimpta$Mes  0.18088    0.01687   10.72 1.03e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3712 on 16 degrees of freedom
    ## Multiple R-squared:  0.8779, Adjusted R-squared:  0.8703 
    ## F-statistic:   115 on 1 and 16 DF,  p-value: 1.028e-08

``` r
ggplot(Kesimpta, aes(x=Mes, y=Kesimpta_log)) + geom_point() + geom_line(linetype='dashed')+ geom_line(aes(x=Mes, y=lm_log_Kesimpta$coefficients[1] + lm_log_Kesimpta$coefficients[2]*Mes), color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
acf(lm_log_Kesimpta$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
ggplot(Kesimpta, aes(x=Mes, y=lm_log_Kesimpta$residuals)) + geom_point() + geom_hline(yintercept = 0, color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->

``` r
dwtest(lm_log_Kesimpta)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  lm_log_Kesimpta
    ## DW = 0.96419, p-value = 0.003044
    ## alternative hypothesis: true autocorrelation is greater than 0

A linear model to the log transformed data for Kesimpta improves the
issue of the constantly growing variance, however, the $R^2$ is smaller,
and there seems to be an underlying pattern in the model’s residuals.
This is confirmed by the dw test, where we reject the null hypothesis
that autocorrelation is 0, due to the p-score being 0.08. A possible
better model here could be a BM, since we know a linear model is
unrealistic, as no product would have unbounded growth in the market.

``` r
BM_log_Kesimpta <- BM(Kesimpta_log)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
summary(BM_log_Kesimpta)
```

    ## Call: ( Standard Bass Model )
    ## 
    ##   BM(series = Kesimpta_log)
    ## 
    ## Residuals:
    ##       Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -0.732314 -0.160979  0.001317 -0.062780  0.067178  0.431559 
    ## 
    ## Coefficients:
    ##        Estimate    Std.Error        Lower        Upper  p-value    
    ## m 235.00245861 1.418854e+01 207.19342697 262.81149026 4.75e-11 ***
    ## p   0.01748555 8.544905e-04   0.01581078   0.01916033 2.26e-12 ***
    ## q   0.08151103 4.582154e-03   0.07253018   0.09049189 1.71e-11 ***
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  0.346862  on  15  degrees of freedom
    ##  Multiple R-squared:   0.999969  Residual sum of squares:  1.804694

``` r
ggplot(Kesimpta, aes(x=Mes, y=Kesimpta_log)) + geom_point() + geom_line(linetype='dashed') + geom_line( y=make.instantaneous(BM_log_Kesimpta$fitted), color='red') + ggtitle('Kesimpta log sales with BM model')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
acf(BM_log_Kesimpta$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->

``` r
ggplot(Kesimpta, aes(x=Mes, y=BM_log_Kesimpta$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-23-4.png)<!-- -->

Here we have so far the best fitting model for the Kesimpta data. $R^2$
is high, the estimators are significant, and variance is much more
stable. However, there is still sinusodial behaviour in the acf for the
model residuals and we can see a clear pattern once we plot the
residuals: There is a steep upward trend in residuals at the start and
then it flattens out. This may suggest a shock at the start of the data,
so one last possible improvement would be to consider a positive shock
at the start.

``` r
GBM_log_Kesimpta <- GBM(Kesimpta_log, shock='exp', nshock=1, prelimestimates = c(BM_log_Kesimpta$coefficients[1], BM_log_Kesimpta$coefficients[2], BM_log_Kesimpta$coefficients[3], 7, -0.5, 0.5))
```

![](Project-Code_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
summary(GBM_log_Kesimpta)
```

    ## Call: ( Generalized Bass model with 1  Exponential  shock )
    ## 
    ##   GBM(series = Kesimpta_log, shock = "exp", nshock = 1, prelimestimates = c(BM_log_Kesimpta$coefficients[1], 
    ##     BM_log_Kesimpta$coefficients[2], BM_log_Kesimpta$coefficients[3], 
    ##     7, -0.5, 0.5))
    ## 
    ## Residuals:
    ##      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.48353 -0.11855 -0.01900 -0.03162  0.08309  0.58094 
    ## 
    ## Coefficients:
    ##           Estimate    Std.Error        Lower        Upper  p-value    
    ## m    251.70879356 22.570652349 207.47112785 295.94645927 1.09e-07 ***
    ## p      0.01588057  0.001304127   0.01332453   0.01843661 4.11e-08 ***
    ## q      0.07843656  0.006011860   0.06665353   0.09021959 1.89e-08 ***
    ## a1     4.17766933  1.610625585   1.02090119   7.33443747 2.35e-02   *
    ## b1    -1.08309325  2.196258295  -5.38768041   3.22149391 6.31e-01    
    ## c1     0.25871178  0.450106064  -0.62347990   1.14090345 5.76e-01    
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  0.282024  on  12  degrees of freedom
    ##  Multiple R-squared:   0.999951  Residual sum of squares:  0.95445

``` r
ggplot(Kesimpta, aes(x=Mes, y=Kesimpta_log)) + geom_point() + geom_line(linetype='dashed') + geom_line( y=make.instantaneous(GBM_log_Kesimpta$fitted), color='red') + ggtitle('Kesimpta log sales with BM model')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

``` r
acf(GBM_log_Kesimpta$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->

``` r
ggplot(Kesimpta, aes(x=Mes, y=GBM_log_Kesimpta$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-24-4.png)<!-- --> This
model actually has a slightly lower $R^2$ than the BM without the shock,
but it greatly reduces variance in the residuals, and gets rid of most
of the underlying pattern in them, implying that the model captures much
more accurately the shape of the data.

Another possibility is to consider an ARIMA model for the Kesimpta data,
since we can see a clear upwards trend in the data.

``` r
Arima_kesimpta <- arima(Kesimpta$TotalUnidades, order = c(0,1,0), seasonal = c(0,0,0))
summary(Arima_kesimpta)
```

    ## 
    ## Call:
    ## arima(x = Kesimpta$TotalUnidades, order = c(0, 1, 0), seasonal = c(0, 0, 0))
    ## 
    ## 
    ## sigma^2 estimated as 63654:  log likelihood = -118.14,  aic = 238.28
    ## 
    ## Training set error measures:
    ##                    ME     RMSE      MAE      MPE     MAPE      MASE       ACF1
    ## Training set 88.39117 245.1882 177.9467 13.51764 28.36819 0.9444565 -0.6515229

``` r
plot(Kesimpta$TotalUnidades)
lines(fitted(Arima_kesimpta), col=2)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
