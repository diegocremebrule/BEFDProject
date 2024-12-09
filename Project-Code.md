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

``` r
#Creating a column 'mes' to represent total months. i.e. January 2023 is 13 instead of 1. This is to facilitate graphing the time series.

#Creating a column 'Month' to have a more interpretable x axis. Can't figure out how to work this yet.

All_data <- All_data %>% mutate(Mes = ifelse(AnnioCorte == 2022, MesFactura, MesFactura + 12 ))

#All_data <- All_data %>% mutate(Month = Date_Converter(MesFactura, AnnioCorte))

#ggplot(All_data, aes(x=Month, y=TotalUnidades, group = DescripcionComercial, colour = DescripcionComercial)) + geom_line() + scale_x_discrete(guide=guide_axis(check.overlap = TRUE))

ggplot(All_data, aes(x=Mes, y=TotalUnidades, group = DescripcionComercial, colour = DescripcionComercial)) + geom_line()
```

![](Project-Code_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

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

``` r
#For the three medicines that are of bigger trends, Gilenya, Iclomod and Lebrina, we can start with Bass Models, as we see clear non-linearity. Then we evaluate to see if there will be a more appropiate model.

#For the flatter trending medications, we can start with a linear model and go from there.
```

``` r
BM_Iclomod <- BM(Iclomod$TotalUnidades, display = T)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

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
ggplot(Iclomod, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Iclomod$fitted), color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
acf(BM_Iclomod$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
ggplot(Iclomod, aes(x=Mes, y=BM_Iclomod$residuals)) + geom_point() + geom_line() + geom_hline(yintercept = 0, linetype = 'dashed') + ggtitle('Residuals of simple BM') + labs(x='Month', y='Residuals')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->

``` r
BM_Gilenya <- BM(Gilenya$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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
ggplot(Gilenya, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Gilenya$fitted), color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
acf(BM_Gilenya$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->

``` r
ggplot(Gilenya, aes(x=Mes, y=BM_Gilenya$residuals)) + geom_point() + geom_line() + geom_hline(yintercept = 0, linetype = 'dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-10-4.png)<!-- -->

``` r
BM_Lebrina <- BM(Lebrina$TotalUnidades, display = T)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

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
ggplot(Lebrina, aes(x=Mes, y=TotalUnidades)) + geom_point() + geom_line(linetype='dashed') + geom_line(y=make.instantaneous(BM_Lebrina$fitted), color='red')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
acf(BM_Lebrina$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
ggplot(Lebrina, aes(x=Mes, y=BM_Lebrina$residuals)) + geom_point() + geom_line() + geom_hline(yintercept = 0, linetype = 'dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-11-4.png)<!-- -->

``` r
# A simple linear model for the smaller selling medications

acf(Kesimpta$TotalUnidades)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
lm_Kesimpta <- lm(Kesimpta$TotalUnidades ~ Kesimpta$Mes)

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

![](Project-Code_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

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
ggplot(Kesimpta, aes(x=Mes, y=lm_Kesimpta$residuals)) + geom_point() + geom_line() + geom_hline(yintercept = 0, linetype='dashed')
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
acf(lm_Kesimpta$residuals)
```

![](Project-Code_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->
