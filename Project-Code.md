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
```

    ## Warning: package 'ggrepel' was built under R version 4.3.3

``` r
library(readxl)
library(DIMORA)
```

    ## Warning: package 'DIMORA' was built under R version 4.3.3

    ## Loading required package: minpack.lm

    ## Warning: package 'minpack.lm' was built under R version 4.3.3

    ## Loading required package: numDeriv

    ## Loading required package: forecast

    ## Warning: package 'forecast' was built under R version 4.3.3

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Loading required package: reshape2

    ## Warning: package 'reshape2' was built under R version 4.3.3

    ## Loading required package: deSolve

    ## Warning: package 'deSolve' was built under R version 4.3.3

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 4.3.3

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.3.3

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

Aximod <- All_data %>% filter(DescripcionComercial == 'AXIMOD')
Fincler <- All_data %>% filter(DescripcionComercial == 'FINCLER')
Gilenya <- All_data %>% filter(DescripcionComercial == 'GILENYA')
Iclomod <- All_data %>% filter(DescripcionComercial == 'ICLOMOD')
Kesimpta <- All_data %>% filter(DescripcionComercial == 'KESIMPTA')
Lebrina <- All_data %>% filter(DescripcionComercial == 'LEBRINA')
Lemtrada <- All_data %>% filter(DescripcionComercial == 'LEMTRADA')
Limostad <- All_data %>% filter(DescripcionComercial == 'LIMOSTAD')
Mavenclad <- All_data %>% filter(DescripcionComercial == 'MAVENCLAD')
Ocrevus <- All_data %>% filter(DescripcionComercial == 'OCREVUS')
Singomod <- All_data %>% filter(DescripcionComercial == 'SINGOMOD')
Tysabri <- All_data %>% filter(DescripcionComercial == 'TYSABRI')
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
    ## -3911.1 -2663.5 -1651.0  -163.9  1265.3  9305.2 
    ## 
    ## Coefficients:
    ##        Estimate    Std.Error        Lower        Upper  p-value    
    ## m 2.714974e+05 1.712896e+04 2.379252e+05 3.050695e+05 6.98e-10 ***
    ## p 1.831285e-02 1.179325e-03 1.600141e-02 2.062428e-02 9.00e-10 ***
    ## q 2.275404e-01 2.354995e-02 1.813833e-01 2.736974e-01 2.68e-07 ***
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  4091.066  on  13  degrees of freedom
    ##  Multiple R-squared:   0.998902  Residual sum of squares:  217578671

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
    ## -4183.9 -1398.1   569.3   224.9  1765.8  4093.1 
    ## 
    ## Coefficients:
    ##        Estimate    Std.Error        Lower        Upper  p-value    
    ## m 8.486166e+05 1.023466e+05 6.480210e+05 1.049212e+06 5.53e-07 ***
    ## p 3.396120e-02 3.466326e-03 2.716733e-02 4.075508e-02 6.54e-08 ***
    ## q 3.272701e-02 1.115275e-02 1.086803e-02 5.458600e-02 1.03e-02   *
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  2645.307  on  15  degrees of freedom
    ##  Multiple R-squared:   0.999918  Residual sum of squares:  104964701

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
    ##      Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -5362.24 -1729.17  -242.27    86.66  1628.25  6852.90 
    ## 
    ## Coefficients:
    ##         Estimate    Std.Error         Lower        Upper p-value  
    ## m  5.548942e+05 3.092866e+07 -6.006416e+07 6.117395e+07   0.986  
    ## p  3.427797e-02 1.910483e+00 -3.710199e+00 3.778755e+00   0.986  
    ## q -3.427400e-02 1.911016e+00 -3.779797e+00 3.711249e+00   0.986  
    ## ---
    ##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Residual standard error  3716.665  on  15  degrees of freedom
    ##  Multiple R-squared:   0.999322  Residual sum of squares:  207204028

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
    ## DW = 2.6282, p-value = 0.8817
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
ggplot(Kesimpta, aes(x=Mes, y=lm_Kesimpta$residuals)) + geom_point() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
acf(lm_Kesimpta$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

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
acf(lm_Aximod$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

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
    ## DW = 1.6908, p-value = 0.1864
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
    ## DW = 2.1207, p-value = 0.4994
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
    ## DW = 2.176, p-value = 0.4812
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
    ## DW = 1.7776, p-value = 0.2259
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
    ## DW = 1.4412, p-value = 0.0649
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
    ## DW = 1.6716, p-value = 0.1615
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
    ## DW = 0.91473, p-value = 0.002024
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
