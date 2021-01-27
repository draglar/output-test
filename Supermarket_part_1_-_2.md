R Notebook
================
eyan
23/01/2021

``` r
library('tidyverse')
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✔ ggplot2 3.3.3     ✔ purrr   0.3.4
    ## ✔ tibble  3.0.5     ✔ dplyr   1.0.3
    ## ✔ tidyr   1.1.2     ✔ stringr 1.4.0
    ## ✔ readr   1.4.0     ✔ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library('data.table')
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
url <- 'http://bit.ly/CarreFourDataset'
sup_market <- read.csv(url)
head(sup_market)
```

    ##    Invoice.ID Branch Customer.type Gender           Product.line Unit.price
    ## 1 750-67-8428      A        Member Female      Health and beauty      74.69
    ## 2 226-31-3081      C        Normal Female Electronic accessories      15.28
    ## 3 631-41-3108      A        Normal   Male     Home and lifestyle      46.33
    ## 4 123-19-1176      A        Member   Male      Health and beauty      58.22
    ## 5 373-73-7910      A        Normal   Male      Sports and travel      86.31
    ## 6 699-14-3026      C        Normal   Male Electronic accessories      85.39
    ##   Quantity     Tax      Date  Time     Payment   cogs gross.margin.percentage
    ## 1        7 26.1415  1/5/2019 13:08     Ewallet 522.83                4.761905
    ## 2        5  3.8200  3/8/2019 10:29        Cash  76.40                4.761905
    ## 3        7 16.2155  3/3/2019 13:23 Credit card 324.31                4.761905
    ## 4        8 23.2880 1/27/2019 20:33     Ewallet 465.76                4.761905
    ## 5        7 30.2085  2/8/2019 10:37     Ewallet 604.17                4.761905
    ## 6        7 29.8865 3/25/2019 18:30     Ewallet 597.73                4.761905
    ##   gross.income Rating    Total
    ## 1      26.1415    9.1 548.9715
    ## 2       3.8200    9.6  80.2200
    ## 3      16.2155    7.4 340.5255
    ## 4      23.2880    8.4 489.0480
    ## 5      30.2085    5.3 634.3785
    ## 6      29.8865    4.1 627.6165

``` r
tail(sup_market)
```

    ##       Invoice.ID Branch Customer.type Gender           Product.line Unit.price
    ## 995  652-49-6720      C        Member Female Electronic accessories      60.95
    ## 996  233-67-5758      C        Normal   Male      Health and beauty      40.35
    ## 997  303-96-2227      B        Normal Female     Home and lifestyle      97.38
    ## 998  727-02-1313      A        Member   Male     Food and beverages      31.84
    ## 999  347-56-2442      A        Normal   Male     Home and lifestyle      65.82
    ## 1000 849-09-3807      A        Member Female    Fashion accessories      88.34
    ##      Quantity     Tax      Date  Time Payment   cogs gross.margin.percentage
    ## 995         1  3.0475 2/18/2019 11:40 Ewallet  60.95                4.761905
    ## 996         1  2.0175 1/29/2019 13:46 Ewallet  40.35                4.761905
    ## 997        10 48.6900  3/2/2019 17:16 Ewallet 973.80                4.761905
    ## 998         1  1.5920  2/9/2019 13:22    Cash  31.84                4.761905
    ## 999         1  3.2910 2/22/2019 15:33    Cash  65.82                4.761905
    ## 1000        7 30.9190 2/18/2019 13:28    Cash 618.38                4.761905
    ##      gross.income Rating     Total
    ## 995        3.0475    5.9   63.9975
    ## 996        2.0175    6.2   42.3675
    ## 997       48.6900    4.4 1022.4900
    ## 998        1.5920    7.7   33.4320
    ## 999        3.2910    4.1   69.1110
    ## 1000      30.9190    6.6  649.2990

``` r
colSums(is.na(sup_market))
```

    ##              Invoice.ID                  Branch           Customer.type 
    ##                       0                       0                       0 
    ##                  Gender            Product.line              Unit.price 
    ##                       0                       0                       0 
    ##                Quantity                     Tax                    Date 
    ##                       0                       0                       0 
    ##                    Time                 Payment                    cogs 
    ##                       0                       0                       0 
    ## gross.margin.percentage            gross.income                  Rating 
    ##                       0                       0                       0 
    ##                   Total 
    ##                       0

No null values were found

``` r
dim(sup_market)
```

    ## [1] 1000   16

``` r
sup_market[duplicated(sup_market)]
```

    ## data frame with 0 columns and 1000 rows

No duplicates

``` r
summary(sup_market)
```

    ##   Invoice.ID           Branch          Customer.type         Gender         
    ##  Length:1000        Length:1000        Length:1000        Length:1000       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  Product.line         Unit.price       Quantity          Tax         
    ##  Length:1000        Min.   :10.08   Min.   : 1.00   Min.   : 0.5085  
    ##  Class :character   1st Qu.:32.88   1st Qu.: 3.00   1st Qu.: 5.9249  
    ##  Mode  :character   Median :55.23   Median : 5.00   Median :12.0880  
    ##                     Mean   :55.67   Mean   : 5.51   Mean   :15.3794  
    ##                     3rd Qu.:77.94   3rd Qu.: 8.00   3rd Qu.:22.4453  
    ##                     Max.   :99.96   Max.   :10.00   Max.   :49.6500  
    ##      Date               Time             Payment               cogs       
    ##  Length:1000        Length:1000        Length:1000        Min.   : 10.17  
    ##  Class :character   Class :character   Class :character   1st Qu.:118.50  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :241.76  
    ##                                                           Mean   :307.59  
    ##                                                           3rd Qu.:448.90  
    ##                                                           Max.   :993.00  
    ##  gross.margin.percentage  gross.income         Rating           Total        
    ##  Min.   :4.762           Min.   : 0.5085   Min.   : 4.000   Min.   :  10.68  
    ##  1st Qu.:4.762           1st Qu.: 5.9249   1st Qu.: 5.500   1st Qu.: 124.42  
    ##  Median :4.762           Median :12.0880   Median : 7.000   Median : 253.85  
    ##  Mean   :4.762           Mean   :15.3794   Mean   : 6.973   Mean   : 322.97  
    ##  3rd Qu.:4.762           3rd Qu.:22.4453   3rd Qu.: 8.500   3rd Qu.: 471.35  
    ##  Max.   :4.762           Max.   :49.6500   Max.   :10.000   Max.   :1042.65

# Reducing the number of features using PCA

``` r
# importing the necessary library for PCA
library('FactoMineR')
library('ggbiplot')
```

    ## Loading required package: plyr

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

    ## Loading required package: scales

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

    ## Loading required package: grid

``` r
label_encode <- function (column,dataframe){
  data <- dataframe[[column]]
  new_data <- as.integer(factor(data))
  # print(new_data)
  return (new_data)
}
supermarket.org <- sup_market[,-c(1,9,10)]
```

``` r
# Label encoding columns
names_ <- c('Branch','Customer.type','Gender','Product.line','Payment')
for(nm in names_){
  supermarket.org[nm] <- label_encode(nm,supermarket.org)
}
supermarket <- supermarket.org
head(supermarket)
```

    ##   Branch Customer.type Gender Product.line Unit.price Quantity     Tax Payment
    ## 1      1             1      1            4      74.69        7 26.1415       3
    ## 2      3             2      1            1      15.28        5  3.8200       1
    ## 3      1             2      2            5      46.33        7 16.2155       2
    ## 4      1             1      2            4      58.22        8 23.2880       3
    ## 5      1             2      2            6      86.31        7 30.2085       3
    ## 6      3             2      2            1      85.39        7 29.8865       3
    ##     cogs gross.margin.percentage gross.income Rating    Total
    ## 1 522.83                4.761905      26.1415    9.1 548.9715
    ## 2  76.40                4.761905       3.8200    9.6  80.2200
    ## 3 324.31                4.761905      16.2155    7.4 340.5255
    ## 4 465.76                4.761905      23.2880    8.4 489.0480
    ## 5 604.17                4.761905      30.2085    5.3 634.3785
    ## 6 597.73                4.761905      29.8865    4.1 627.6165

``` r
# Geting the principle components from the supermarkets dataset.
results <- prcomp(supermarket[,-c(10)], center = TRUE, scale. = TRUE)
summary(results)
```

    ## Importance of components:
    ##                           PC1     PC2    PC3    PC4     PC5     PC6     PC7
    ## Standard deviation     2.2201 1.06317 1.0317 1.0099 0.99289 0.97714 0.96273
    ## Proportion of Variance 0.4107 0.09419 0.0887 0.0850 0.08215 0.07957 0.07724
    ## Cumulative Proportion  0.4107 0.50493 0.5936 0.6786 0.76078 0.84035 0.91758
    ##                            PC8     PC9      PC10      PC11      PC12
    ## Standard deviation     0.94823 0.29977 2.112e-16 1.456e-16 1.214e-16
    ## Proportion of Variance 0.07493 0.00749 0.000e+00 0.000e+00 0.000e+00
    ## Cumulative Proportion  0.99251 1.00000 1.000e+00 1.000e+00 1.000e+00

The first principal component has the largest proportion explaining 41%
of the variance in the data. The first 9 components explain the all
variance in the data, making the last 3 obsolete

``` r
screeplot(results, type = "l", npcs = 13, main = "Screeplot of the first 12 Principal components")
abline(h = 0.9, col="red", lty=5)
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
The first 9 principal components explain the most in the data, explainig
99% of the variance. These are the most important features in the
dataset.

``` r
ggbiplot(results,labels.size = 5, alpha = 0.5)
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
Unit price,gross income,tax and quantity contribute to PC1
Payment,gender,Product line and customer type contribute to PC2

``` r
clusters <- function (name,pc1=1,pc2=2){
  cl_list <- factor(sup_market[[name]])
  # species <- cl_list$species
  ggbiplot(results,,choices=c(pc1,pc2),ellipse = TRUE,groups = cl_list, obs.scale = 1, var.scale = 1,labels.size = 5, alpha = 0.5)
}
clusters('Gender')
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
The clusters of the gender show: Males are clustered at the top
characterised by all features but the branch, while females are
clustered at the bottom with unitprice,tax,gross income,rating and
branch influencing their buying.

``` r
clusters('Gender',3,4)
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
Since the 3rd and 4th princpal components explain lower variances than
the first two, i doesnt do that good a job at showing the pattern shown
in the previous plot. although it does give a hint on where the
different genders lie.

``` r
clusters('Gender',11,12)
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
The last 2 principle components perform even worse since they ecplain so
little of the variance in the dataset.

# Reducing the attributes using Feature Selection

## Method 1 filter method: high correlation filter

``` r
library('caret')
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library('corrplot')
```

    ## corrplot 0.84 loaded

``` r
supermarket_ <- supermarket.org[,-c(10)]
corr_ <- cor(supermarket_)
corr_
```

    ##                    Branch Customer.type       Gender Product.line   Unit.price
    ## Branch         1.00000000   -0.01960787 -0.056317558 -0.053937557  0.028202440
    ## Customer.type -0.01960787    1.00000000  0.039996160 -0.036800311 -0.020237875
    ## Gender        -0.05631756    0.03999616  1.000000000  0.005193197  0.015444630
    ## Product.line  -0.05393756   -0.03680031  0.005193197  1.000000000  0.019321028
    ## Unit.price     0.02820244   -0.02023787  0.015444630  0.019321028  1.000000000
    ## Quantity       0.01596379   -0.01676271 -0.074258307  0.020256001  0.010777564
    ## Tax            0.04104666   -0.01967028 -0.049450989  0.031620725  0.633962089
    ## Payment       -0.05010429    0.01807344  0.044577609  0.029896383 -0.015941048
    ## cogs           0.04104666   -0.01967028 -0.049450989  0.031620725  0.633962089
    ## gross.income   0.04104666   -0.01967028 -0.049450989  0.031620725  0.633962089
    ## Rating         0.01023848    0.01888867  0.004800208 -0.020528973 -0.008777507
    ## Total          0.04104666   -0.01967028 -0.049450989  0.031620725  0.633962089
    ##                  Quantity         Tax      Payment        cogs gross.income
    ## Branch         0.01596379  0.04104666 -0.050104288  0.04104666   0.04104666
    ## Customer.type -0.01676271 -0.01967028  0.018073436 -0.01967028  -0.01967028
    ## Gender        -0.07425831 -0.04945099  0.044577609 -0.04945099  -0.04945099
    ## Product.line   0.02025600  0.03162072  0.029896383  0.03162072   0.03162072
    ## Unit.price     0.01077756  0.63396209 -0.015941048  0.63396209   0.63396209
    ## Quantity       1.00000000  0.70551019 -0.003920990  0.70551019   0.70551019
    ## Tax            0.70551019  1.00000000 -0.012433637  1.00000000   1.00000000
    ## Payment       -0.00392099 -0.01243364  1.000000000 -0.01243364  -0.01243364
    ## cogs           0.70551019  1.00000000 -0.012433637  1.00000000   1.00000000
    ## gross.income   0.70551019  1.00000000 -0.012433637  1.00000000   1.00000000
    ## Rating        -0.01581490 -0.03644170 -0.005381289 -0.03644170  -0.03644170
    ## Total          0.70551019  1.00000000 -0.012433637  1.00000000   1.00000000
    ##                     Rating       Total
    ## Branch         0.010238476  0.04104666
    ## Customer.type  0.018888672 -0.01967028
    ## Gender         0.004800208 -0.04945099
    ## Product.line  -0.020528973  0.03162072
    ## Unit.price    -0.008777507  0.63396209
    ## Quantity      -0.015814905  0.70551019
    ## Tax           -0.036441705  1.00000000
    ## Payment       -0.005381289 -0.01243364
    ## cogs          -0.036441705  1.00000000
    ## gross.income  -0.036441705  1.00000000
    ## Rating         1.000000000 -0.03644170
    ## Total         -0.036441705  1.00000000

High correlations from tax,gross income ,cogs and total

``` r
high_cor <- findCorrelation(corr_, cutoff=0.75)
high_cor
```

    ## [1]  9 12  7

``` r
# Removing the highly correlated features in index (9,12,7)
final <- supermarket_[-high_cor]
```

``` r
# Plotting the correlations for comparison
par(mfrow = c(1,2))
corrplot(corr_, order = "hclust")
corrplot(cor(final), order = "hclust")
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
Feature selection removes the most correlated features from the dataset
in this case were left with 9 features that are not overly correlated

## Method 2 : Wrapper Methods

``` r
library('clustvarsel')
```

    ## Loading required package: mclust

    ## Package 'mclust' version 5.4.7
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## 
    ## Attaching package: 'mclust'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

    ## Package 'clustvarsel' version 2.3.4

    ## Type 'citation("clustvarsel")' for citing this R package in publications.

``` r
library('mclust')
```

``` r
# Sequential forward greedy search using the default number of groups
out <- clustvarsel(supermarket_)
out
```

    ## ------------------------------------------------------ 
    ## Variable selection for Gaussian model-based clustering
    ## Stepwise (forward/backward) greedy search
    ## ------------------------------------------------------ 
    ## 
    ##  Variable proposed Type of step   BICclust Model G   BICdiff Decision
    ##           Quantity          Add  -4308.761     E 9  687.4466 Accepted
    ##               cogs          Add -16306.851   VEV 9 1083.0132 Accepted
    ##         Unit.price          Add -21393.079   EVV 7 2812.3733 Accepted
    ##         Unit.price       Remove -16306.851   VEV 9 2812.3733 Rejected
    ##             Rating          Add -25510.859   EVV 7 -184.0917 Rejected
    ##         Unit.price       Remove -16306.851   VEV 9 2812.3733 Rejected
    ## 
    ## Selected subset: Quantity, cogs, Unit.price

``` r
final2 <- supermarket_[,out$subset]
head(final2)
```

    ##   Quantity   cogs Unit.price
    ## 1        7 522.83      74.69
    ## 2        5  76.40      15.28
    ## 3        7 324.31      46.33
    ## 4        8 465.76      58.22
    ## 5        7 604.17      86.31
    ## 6        7 597.73      85.39

Using clustvarsel 3 groups were taken as important in creating the
clusters of the dataset

``` r
mod <- Mclust(final2)
summary(mod)
```

    ## ---------------------------------------------------- 
    ## Gaussian finite mixture model fitted by EM algorithm 
    ## ---------------------------------------------------- 
    ## 
    ## Mclust EVV (ellipsoidal, equal volume) model with 7 components: 
    ## 
    ##  log-likelihood    n df       BIC       ICL
    ##       -10478.95 1000 63 -21393.08 -21446.89
    ## 
    ## Clustering table:
    ##   1   2   3   4   5   6   7 
    ## 173 134 126 148 128 142 149

There were achieved 7 clusters in the selected subset of data

``` r
plot(mod,c("classification"))
```

![](/home/eyan/Documents/R/Supermarket_part_1_-_2_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->
The unsupervised clustering shows 7 clusters from the 3 attributes
selected
