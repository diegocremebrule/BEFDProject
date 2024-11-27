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
```

``` r
All_data <- read.csv('Product Data Q3_2022 to Q4_2023.csv')
```

``` r
#Separate Data by different drug brands to ease analysis

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
#Creating a time series out of total units sold for two brands

Fincler <- Fincler %>% mutate(Mes = ifelse(AnnioCorte == 2022, MesFactura, MesFactura+12 ))

Fincler_ts <- Fincler %>% group_by(Mes) %>% summarise(TotalUnidadesFacturadas = sum(TotalUnidadesFacturadas))

Gilenya <- Gilenya %>% mutate(Mes = ifelse(AnnioCorte == 2022, MesFactura, MesFactura + 12 ))

Gilenya_ts <- Gilenya %>% group_by(Mes) %>% summarise(TotalUnidadesFacturadas = sum(TotalUnidadesFacturadas))

ggplot(Fincler_ts, aes(x=Mes, y=TotalUnidadesFacturadas)) +geom_line()
```

![](Project-Code_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
ggplot(Gilenya, aes(x=Mes, y=TotalUnidadesFacturadas)) +geom_line()
```

![](Project-Code_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->
