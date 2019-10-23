
---
title: "Determining Optimal Test Stores"
author: "Dustin Rogers"
date: "September 29, 2018"
output:
      html_document:
        keep_md: true
---




####Load Data and look at data structure

```r
setwd("C:\\Users\\user\\Documents\\Datalore Projects\\Test Control Stores\\")
test_cntl <- read.csv("Test Control Stores.csv",stringsAsFactors=FALSE)
str(test_cntl)
```

```
## 'data.frame':	125 obs. of  10 variables:
##  $ Status                         : chr  "Control" "Control" "Control" "Control" ...
##  $ StoreCode                      : int  1003 1113 1161 1629 1699 1855 1924 2224 2238 2261 ...
##  $ CustomerMarket                 : chr  "FG" "FC" "FB" "FG" ...
##  $ Sum.of.Customer.Sales..s       : num  142709 78336 144626 263185 106198 ...
##  $ Sum.of.Customer.Gross.Margin..s: num  41576 19576 36501 83747 32660 ...
##  $ Sum.of.Customer.S.T..          : num  0.71 0.72 0.73 0.76 0.82 0.87 0.73 0.75 0.74 0.74 ...
##  $ Max.of.MEDIAN.HOME.VALUE       : chr  "231750" "193075" "206550" "231757" ...
##  $ Max.of.AGG.HOME.VALUES         : chr  "75638586" "295962580" "112035186" "214320269" ...
##  $ Max.of.INCOME.DENSITY          : chr  "20526370" "78821928" "28587143" "52452386" ...
##  $ Max.of.MEDIAN.AGE              : chr  "32" "40" "28" "35" ...
```
####We can see that the naming conventions are quite poor so I will change those. Also some of the data that should have loaded as numerical actually loaded as chararters. I will fix this too.

```r
test_cntl <- test_cntl%>%
  rename_all(.funs = funs(sub(c("*Sum.of.Customer."), "", names(test_cntl))))
test_cntl <- test_cntl%>%
  rename_all(.funs = funs(sub(c("*Max.of."), "", names(test_cntl))))
numeric_feats <-names(test_cntl[sapply(test_cntl, function(x) length(unique(x)))>12])
test_cntl[numeric_feats] <- lapply(test_cntl[numeric_feats], as.numeric)
```
#### Next I check null values and see that there are 6 rows that are all missing demographic data.
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> x </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Status </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> StoreCode </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CustomerMarket </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sales..s </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gross.Margin..s </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S.T.. </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.HOME.VALUE </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AGG.HOME.VALUES </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> INCOME.DENSITY </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.AGE </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
</tbody>
</table>
#### I have decided to impute the mean value for the rows null values since my data set is already so small. 

```r
for (x in numeric_feats) {   
  mean_value <- mean(test_cntl[[x]],na.rm = TRUE)
  test_cntl[[x]][is.na(test_cntl[[x]])] <- mean_value
}
```
#### Next I look at summary statistics for all my numeric data. I see that AGG.HOME.VALUES & INCOME.DENSITY are the total cost of all homes within a zip code. 
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> SD </th>
   <th style="text-align:right;"> Unique </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> StoreCode </td>
   <td style="text-align:right;"> 1702.67 </td>
   <td style="text-align:right;"> 850.74 </td>
   <td style="text-align:right;"> 125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sales..s </td>
   <td style="text-align:right;"> 153231.10 </td>
   <td style="text-align:right;"> 93056.67 </td>
   <td style="text-align:right;"> 125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gross.Margin..s </td>
   <td style="text-align:right;"> 44247.27 </td>
   <td style="text-align:right;"> 32802.62 </td>
   <td style="text-align:right;"> 125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S.T.. </td>
   <td style="text-align:right;"> 0.78 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.HOME.VALUE </td>
   <td style="text-align:right;"> 190360.81 </td>
   <td style="text-align:right;"> 58192.50 </td>
   <td style="text-align:right;"> 119 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AGG.HOME.VALUES </td>
   <td style="text-align:right;"> 110379693.59 </td>
   <td style="text-align:right;"> 89655323.27 </td>
   <td style="text-align:right;"> 120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> INCOME.DENSITY </td>
   <td style="text-align:right;"> 27636314.41 </td>
   <td style="text-align:right;"> 19190972.73 </td>
   <td style="text-align:right;"> 120 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.AGE </td>
   <td style="text-align:right;"> 41.71 </td>
   <td style="text-align:right;"> 6.19 </td>
   <td style="text-align:right;"> 29 </td>
  </tr>
</tbody>
</table>
#### I created a function that randomly samples 20 stores and then compares their means to the remaining means using T-Tests until all the T-Tests have P-Values of 80% of higher. I did this to make sure that test group would be a representative sample of the entire population.

```r
random.sample <- function(x) {
  success <- FALSE
  while (!success) {
    
    test <- sample_n(test_cntl,20)
    control <- setdiff(test_cntl,test)
    
    cov <- c('Sales..s','Gross.Margin..s','S.T..','MEDIAN.HOME.VALUE','MEDIAN.AGE')
    
    ttest <- ldply(cov,function(x) {
      p_val = t.test(x=test[,x], y=control[,x])$p.value
      return(data.frame(x=x,p_value=p_val))
    })
  
    success <- all(ttest$p_value>.8)
  }
  test_stores <- test$StoreCode
}

test_stores<-random.sample(test_cntl)

test_cntl<-test_cntl%>%
  mutate(Status_2=case_when(StoreCode %in% test_stores ~ 1,
                            TRUE ~ 0
                            ))
```

####Look at difference in means between current test/control groups. The first table represents the newly picked test stores vs. the remain 105 stores. The second table represent the prechosen control and test stores.

```
## Adding missing grouping variables: `Status_3`
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Status_3 </th>
   <th style="text-align:right;"> StoreCode </th>
   <th style="text-align:right;"> Sales..s </th>
   <th style="text-align:right;"> Gross.Margin..s </th>
   <th style="text-align:right;"> S.T.. </th>
   <th style="text-align:right;"> MEDIAN.HOME.VALUE </th>
   <th style="text-align:right;"> AGG.HOME.VALUES </th>
   <th style="text-align:right;"> INCOME.DENSITY </th>
   <th style="text-align:right;"> MEDIAN.AGE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Control </td>
   <td style="text-align:right;"> 1663.381 </td>
   <td style="text-align:right;"> 152670.9 </td>
   <td style="text-align:right;"> 43985.71 </td>
   <td style="text-align:right;"> 0.7787619 </td>
   <td style="text-align:right;"> 190575.2 </td>
   <td style="text-align:right;"> 108171705 </td>
   <td style="text-align:right;"> 27007269 </td>
   <td style="text-align:right;"> 41.68163 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Test </td>
   <td style="text-align:right;"> 1908.950 </td>
   <td style="text-align:right;"> 156172.1 </td>
   <td style="text-align:right;"> 45620.43 </td>
   <td style="text-align:right;"> 0.7760000 </td>
   <td style="text-align:right;"> 189235.3 </td>
   <td style="text-align:right;"> 121971633 </td>
   <td style="text-align:right;"> 30938804 </td>
   <td style="text-align:right;"> 41.88571 </td>
  </tr>
</tbody>
</table>

```
## Adding missing grouping variables: `Status`
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Status </th>
   <th style="text-align:right;"> StoreCode </th>
   <th style="text-align:right;"> Sales..s </th>
   <th style="text-align:right;"> Gross.Margin..s </th>
   <th style="text-align:right;"> S.T.. </th>
   <th style="text-align:right;"> MEDIAN.HOME.VALUE </th>
   <th style="text-align:right;"> AGG.HOME.VALUES </th>
   <th style="text-align:right;"> INCOME.DENSITY </th>
   <th style="text-align:right;"> MEDIAN.AGE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Control </td>
   <td style="text-align:right;"> 1586.85 </td>
   <td style="text-align:right;"> 169811.3 </td>
   <td style="text-align:right;"> 51806.75 </td>
   <td style="text-align:right;"> 0.7685 </td>
   <td style="text-align:right;"> 199948.1 </td>
   <td style="text-align:right;"> 113067650 </td>
   <td style="text-align:right;"> 28379723 </td>
   <td style="text-align:right;"> 39.82143 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Test </td>
   <td style="text-align:right;"> 1704.90 </td>
   <td style="text-align:right;"> 171250.5 </td>
   <td style="text-align:right;"> 49877.23 </td>
   <td style="text-align:right;"> 0.8150 </td>
   <td style="text-align:right;"> 197148.4 </td>
   <td style="text-align:right;"> 134148381 </td>
   <td style="text-align:right;"> 32921391 </td>
   <td style="text-align:right;"> 43.48571 </td>
  </tr>
</tbody>
</table>

####Next I run a t-test for each individual variables to determine if the control and test means are equal. The first table represents the newly picked test stores vs. the remain 105 stores. The second table represent the prechosen control and test stores.
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> x </th>
   <th style="text-align:right;"> t_value </th>
   <th style="text-align:right;"> p_value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sales..s </td>
   <td style="text-align:right;"> -0.1602498 </td>
   <td style="text-align:right;"> 0.8738415 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gross.Margin..s </td>
   <td style="text-align:right;"> -0.2040887 </td>
   <td style="text-align:right;"> 0.8398248 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S.T.. </td>
   <td style="text-align:right;"> 0.1360388 </td>
   <td style="text-align:right;"> 0.8925993 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.HOME.VALUE </td>
   <td style="text-align:right;"> 0.1004953 </td>
   <td style="text-align:right;"> 0.9206537 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.AGE </td>
   <td style="text-align:right;"> -0.1609679 </td>
   <td style="text-align:right;"> 0.8731116 </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> x </th>
   <th style="text-align:right;"> t_value </th>
   <th style="text-align:right;"> p_value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sales..s </td>
   <td style="text-align:right;"> -0.0429213 </td>
   <td style="text-align:right;"> 0.9659893 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gross.Margin..s </td>
   <td style="text-align:right;"> 0.1585418 </td>
   <td style="text-align:right;"> 0.8748715 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S.T.. </td>
   <td style="text-align:right;"> -2.1415602 </td>
   <td style="text-align:right;"> 0.0393627 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.HOME.VALUE </td>
   <td style="text-align:right;"> 0.1168788 </td>
   <td style="text-align:right;"> 0.9075914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.AGE </td>
   <td style="text-align:right;"> -1.7901351 </td>
   <td style="text-align:right;"> 0.0815000 </td>
  </tr>
</tbody>
</table>
####Based on these results, the Sum.of.Customer.S.T.. is the only variable that has a significant difference in means at the 95% confidence level


#### Next, I am going to use the matchit function to determine the optimal control stores based on the test stores that have already been chosen.

```
## 
## Call:
## matchit(formula = Status_2 ~ CustomerMarket + Sales..s + Gross.Margin..s + 
##     S.T.. + MEDIAN.HOME.VALUE + MEDIAN.AGE, data = test_cntl, 
##     method = "nearest", ratio = 2)
## 
## Summary of balance for all data:
##                   Means Treated Means Control SD Control  Mean Diff
## distance                 0.2860        0.1360     0.1118     0.1500
## CustomerMarketEB         0.0500        0.0857     0.2813    -0.0357
## CustomerMarketEF         0.0500        0.0095     0.0976     0.0405
## CustomerMarketFA         0.1000        0.0857     0.2813     0.0143
## CustomerMarketFB         0.1000        0.1905     0.3946    -0.0905
## CustomerMarketFC         0.1000        0.0857     0.2813     0.0143
## CustomerMarketFD         0.1000        0.0762     0.2666     0.0238
## CustomerMarketFE         0.0500        0.0952     0.2950    -0.0452
## CustomerMarketFF         0.0000        0.0190     0.1373    -0.0190
## CustomerMarketFG         0.1000        0.0571     0.2332     0.0429
## CustomerMarketFH         0.0500        0.0381     0.1923     0.0119
## CustomerMarketFJ         0.0500        0.0667     0.2506    -0.0167
## CustomerMarketFL         0.2500        0.1905     0.3946     0.0595
## Sales..s            171250.4505   149798.8459 90123.6497 21451.6046
## Gross.Margin..s      49877.2270    43174.8953 31913.2462  6702.3317
## S.T..                    0.8150        0.7713     0.1041     0.0437
## MEDIAN.HOME.VALUE   197148.4403   189067.9241 52825.0948  8080.5162
## MEDIAN.AGE              43.4857       41.3769     6.0333     2.1088
##                      eQQ Med   eQQ Mean    eQQ Max
## distance              0.1556     0.1431     0.2759
## CustomerMarketEB      0.0000     0.0500     1.0000
## CustomerMarketEF      0.0000     0.0000     0.0000
## CustomerMarketFA      0.0000     0.0000     0.0000
## CustomerMarketFB      0.0000     0.1000     1.0000
## CustomerMarketFC      0.0000     0.0000     0.0000
## CustomerMarketFD      0.0000     0.0000     0.0000
## CustomerMarketFE      0.0000     0.0500     1.0000
## CustomerMarketFF      0.0000     0.0500     1.0000
## CustomerMarketFG      0.0000     0.0500     1.0000
## CustomerMarketFH      0.0000     0.0000     0.0000
## CustomerMarketFJ      0.0000     0.0500     1.0000
## CustomerMarketFL      0.0000     0.0500     1.0000
## Sales..s          18931.1800 18052.0215 36079.5100
## Gross.Margin..s    6411.1600  6434.5615 14326.7000
## S.T..                 0.0400     0.0560     0.3400
## MEDIAN.HOME.VALUE  5972.5000 11220.1000 82254.0000
## MEDIAN.AGE            2.0000     2.5357     9.0000
## 
## 
## Summary of balance for matched data:
##                   Means Treated Means Control SD Control Mean Diff
## distance                 0.2860        0.2302     0.1236    0.0557
## CustomerMarketEB         0.0500        0.0500     0.2207    0.0000
## CustomerMarketEF         0.0500        0.0250     0.1581    0.0250
## CustomerMarketFA         0.1000        0.1250     0.3349   -0.0250
## CustomerMarketFB         0.1000        0.1500     0.3616   -0.0500
## CustomerMarketFC         0.1000        0.0750     0.2667    0.0250
## CustomerMarketFD         0.1000        0.1000     0.3038    0.0000
## CustomerMarketFE         0.0500        0.0250     0.1581    0.0250
## CustomerMarketFF         0.0000        0.0000     0.0000    0.0000
## CustomerMarketFG         0.1000        0.0750     0.2667    0.0250
## CustomerMarketFH         0.0500        0.0500     0.2207    0.0000
## CustomerMarketFJ         0.0500        0.0750     0.2667   -0.0250
## CustomerMarketFL         0.2500        0.2500     0.4385    0.0000
## Sales..s            171250.4505   164772.2065 88674.1526 6478.2440
## Gross.Margin..s      49877.2270    47697.1095 31244.1945 2180.1175
## S.T..                    0.8150        0.8015     0.0880    0.0135
## MEDIAN.HOME.VALUE   197148.4403   191428.0702 56350.6575 5720.3702
## MEDIAN.AGE              43.4857       42.7429     6.2676    0.7429
##                     eQQ Med   eQQ Mean     eQQ Max
## distance             0.0544     0.0564      0.1748
## CustomerMarketEB     0.0000     0.0000      0.0000
## CustomerMarketEF     0.0000     0.0000      0.0000
## CustomerMarketFA     0.0000     0.0000      0.0000
## CustomerMarketFB     0.0000     0.0500      1.0000
## CustomerMarketFC     0.0000     0.0500      1.0000
## CustomerMarketFD     0.0000     0.0000      0.0000
## CustomerMarketFE     0.0000     0.0000      0.0000
## CustomerMarketFF     0.0000     0.0000      0.0000
## CustomerMarketFG     0.0000     0.0500      1.0000
## CustomerMarketFH     0.0000     0.0000      0.0000
## CustomerMarketFJ     0.0000     0.0000      0.0000
## CustomerMarketFL     0.0000     0.0000      0.0000
## Sales..s          8723.1700 14703.4650  98647.7200
## Gross.Margin..s   2544.2100  4814.9670  37766.2800
## S.T..                0.0200     0.0225      0.1100
## MEDIAN.HOME.VALUE 4966.0000 12802.9500 117390.0000
## MEDIAN.AGE           1.0000     1.5500      6.0000
## 
## Percent Balance Improvement:
##                   Mean Diff. eQQ Med eQQ Mean   eQQ Max
## distance             62.8430 65.0060  60.6024   36.6645
## CustomerMarketEB    100.0000  0.0000 100.0000  100.0000
## CustomerMarketEF     38.2353  0.0000   0.0000    0.0000
## CustomerMarketFA    -75.0000  0.0000   0.0000    0.0000
## CustomerMarketFB     44.7368  0.0000  50.0000    0.0000
## CustomerMarketFC    -75.0000  0.0000     -Inf      -Inf
## CustomerMarketFD    100.0000  0.0000   0.0000    0.0000
## CustomerMarketFE     44.7368  0.0000 100.0000  100.0000
## CustomerMarketFF    100.0000  0.0000 100.0000  100.0000
## CustomerMarketFG     41.6667  0.0000   0.0000    0.0000
## CustomerMarketFH    100.0000  0.0000   0.0000    0.0000
## CustomerMarketFJ    -50.0000  0.0000 100.0000  100.0000
## CustomerMarketFL    100.0000  0.0000 100.0000  100.0000
## Sales..s             69.8007 53.9217  18.5495 -173.4176
## Gross.Margin..s      67.4723 60.3159  25.1702 -163.6077
## S.T..                69.0840 50.0000  59.8214   67.6471
## MEDIAN.HOME.VALUE    29.2079 16.8522 -14.1073  -42.7165
## MEDIAN.AGE           64.7742 50.0000  38.8732   33.3333
## 
## Sample sizes:
##           Control Treated
## All           105      20
## Matched        40      20
## Unmatched      65       0
## Discarded       0       0
```
####With the new control groups given by the above matchit model, I used a t-test for each variable to determine if the control and test means were equal.
<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> x </th>
   <th style="text-align:right;"> t_value </th>
   <th style="text-align:right;"> p_value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sales..s </td>
   <td style="text-align:right;"> -0.2321010 </td>
   <td style="text-align:right;"> 0.8179283 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gross.Margin..s </td>
   <td style="text-align:right;"> -0.2239069 </td>
   <td style="text-align:right;"> 0.8242292 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S.T.. </td>
   <td style="text-align:right;"> -0.6015365 </td>
   <td style="text-align:right;"> 0.5507083 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.HOME.VALUE </td>
   <td style="text-align:right;"> -0.2798560 </td>
   <td style="text-align:right;"> 0.7816303 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MEDIAN.AGE </td>
   <td style="text-align:right;"> -0.4084223 </td>
   <td style="text-align:right;"> 0.6854266 </td>
  </tr>
</tbody>
</table>
