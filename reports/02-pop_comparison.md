# Social Care Survey Open Data


#Introduction

Here I will use the newly released `skimr` package to describe each of the three social care datasets. Generally looking for distributions and missing data levels. 

The majority of variables are factors so we probabaly won't get the full benefit of `skimr` but am keen to try it out. 

##Load data
Load in tidied data 

```r
load("produced_data/created_objects/soc_care10.rds")
load("produced_data/created_objects/soc_care11.rds")
load("produced_data/created_objects/soc_care12.rds")
```

##Load packages
And required packages

```r
library(skimr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readr)
library(purrr)
library(stringr)
library(tidyr)
library(magrittr)
library(forcats)
library(DT)
```


#Examine data

##Summaries using 'skimr`

1st thing is to create skim objects for each year. I've printed the 2011 versions here as an example


```r
skim10 <- skim(soc_care10)
skim11 <- skim(soc_care11)
skim12 <- skim(soc_care12)

datatable(skim11)
```

<!--html_preserve--><div id="htmlwidget-ad9e332abd5c90c866d0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ad9e332abd5c90c866d0">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231"],["council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","council","client_id","client_id","client_id","client_id","client_id","client_id","client_id","age_grp","age_grp","age_grp","age_grp","age_grp","age_grp","age_grp","age_grp","age_grp","client_grp","client_grp","client_grp","client_grp","client_grp","client_grp","client_grp","client_grp","client_grp","client_grp","gender","gender","gender","gender","gender","gender","gender","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","la_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","pri_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","vol_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","pc_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","total_hrs","hc_client","hc_client","hc_client","hc_client","hc_client","hc_client","hc_client","comm_alarm","comm_alarm","comm_alarm","comm_alarm","comm_alarm","comm_alarm","comm_alarm","other_telecare","other_telecare","other_telecare","other_telecare","other_telecare","other_telecare","other_telecare","alarm_and_tele","alarm_and_tele","alarm_and_tele","alarm_and_tele","alarm_and_tele","alarm_and_tele","alarm_and_tele","living_arr","living_arr","living_arr","living_arr","living_arr","living_arr","living_arr","multi_staff","multi_staff","multi_staff","multi_staff","multi_staff","multi_staff","multi_staff","housing_type","housing_type","housing_type","housing_type","housing_type","housing_type","housing_type","housing_type","laundry","laundry","laundry","laundry","laundry","laundry","laundry","shopping","shopping","shopping","shopping","shopping","shopping","shopping","housing_supp","housing_supp","housing_supp","housing_supp","housing_supp","housing_supp","housing_supp"],["factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","character","character","character","character","character","character","character","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor"],["missing","complete","n","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","count","n_unique","missing","complete","n","min","max","empty","n_unique","missing","complete","n","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","count","count","count","count","count","count","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","count","count","count","count","count","count","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","count","count","count","count","count","count","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","count","count","count","count","count","count","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","count","count","count","count","count","count","count","count","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique","missing","complete","n","count","count","count","n_unique"],[".all",".all",".all","Aberdeen City","Aberdeenshire","Angus","Argyll &amp; Bute","Clackmannanshire","Dumfries &amp; Galloway","Dundee City","East Ayrshire","East Dunbartonshire","East Lothian","East Renfrewshire","Edinburgh, City of","Eilean Siar","Falkirk","Fife","Glasgow City","Highland","Inverclyde","Midlothian","Moray","North Ayrshire","North Lanarkshire","Orkney Islands","Perth &amp; Kinross","Renfrewshire","Scottish Borders","Shetland Islands","South Ayrshire","South Lanarkshire","Stirling","West Dunbartonshire","West Lothian",null,".all",".all",".all",".all",".all",".all",".all",".all",".all",".all",".all","18-&lt;65","65-&lt;75","75-&lt;85","85+",null,".all",".all",".all",".all","Dementia &amp; Mental Health","Learning Disability","Physical Disability","Infirmity due to Age","Other",null,".all",".all",".all",".all","Male","Female",null,".all",".all",".all",".all","Zero","&lt;1","1-2","2-4","4-6","6-8","8-10","10-15","15-20","20-30","30-40","40-50","over50",null,".all",".all",".all",".all","Zero","&lt;1","1-2","2-4","4-6","6-8","8-10","10-15","15-20","20-30","30-40","40-50","over50",null,".all",".all",".all",".all","Zero","&lt;1","1-2","2-4","4-6","6-8","8-10","10-15","15-20","20-30","30-40","40-50","over50",null,".all",".all",".all",".all","Zero","&lt;1","1-2","2-4","4-6","6-8","8-10","10-15","15-20","20-30","30-40","40-50","over50",null,".all",".all",".all",".all","Zero","&lt;1","1-2","2-4","4-6","6-8","8-10","10-15","15-20","20-30","30-40","40-50","over50",null,".all",".all",".all",".all","No","Yes",null,".all",".all",".all",".all","No","Yes",null,".all",".all",".all",".all","No","Yes",null,".all",".all",".all",".all","No","Yes",null,".all",".all",".all",".all","Lives Alone","Other",null,".all",".all",".all",".all","Single Staff","2 or more Staff",null,".all",".all",".all",".all","Mainstream","Supported Housing","Other",null,".all",".all",".all",".all","No","Yes",null,".all",".all",".all",".all","No","Yes",null,".all",".all",".all",".all","No","Yes",null,".all"],[0,142466,142466,3337,4157,4571,1904,1541,3733,5759,4099,2580,2752,2042,9521,1080,5050,8324,19781,4387,2351,2048,2317,3267,13066,612,2858,3937,4116,877,3330,9414,2478,2572,4605,0,32,0,142466,142466,2,9,0,142303,3399,139067,142466,23085,22897,49857,43228,3399,4,25905,116561,142466,8058,5425,33471,64587,5020,25905,5,2353,140113,142466,46281,93832,2353,2,0,142466,142466,99411,2055,6653,10173,5061,5552,3060,6392,2226,1042,420,126,295,0,13,0,142466,142466,121644,448,1991,4066,2315,3023,1418,3650,1603,1027,492,188,601,0,13,189,142277,142466,138635,16,180,718,435,314,113,356,188,303,226,130,663,189,13,0,142466,142466,88032,2119,5585,11070,6133,8257,3804,9992,3245,2216,1004,338,671,0,13,0,142466,142466,79714,2146,7466,12612,6982,8271,4635,10541,4270,2538,1237,468,1586,0,13,0,142466,142466,79714,62752,0,2,0,142466,142466,49077,93389,0,2,0,142466,142466,136951,5515,0,2,0,142466,142466,130356,12110,0,2,72903,69563,142466,38568,30995,72903,2,0,142466,142466,137414,5052,0,2,5929,136537,142466,111834,22576,2127,5929,3,0,142466,142466,139538,2928,0,2,0,142466,142466,137804,4662,0,2,0,142466,142466,134727,7739,0,2]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>var<\/th>\n      <th>type<\/th>\n      <th>stat<\/th>\n      <th>level<\/th>\n      <th>value<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":5},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
skim11
```

```
## 
## Character Variables
## # A tibble: 1 x 9
##         var      type complete missing empty      n   min   max n_unique
## *     <chr>     <chr>    <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>
## 1 client_id character   142466       0     0 142466     2     9   142303
## 
## Factor Variables
## # A tibble: 19 x 7
##               var   type complete missing      n n_unique
##             <chr>  <chr>    <dbl>   <dbl>  <dbl>    <dbl>
##  1        age_grp factor   139067    3399 142466        4
##  2 alarm_and_tele factor   142466       0 142466        2
##  3     client_grp factor   116561   25905 142466        5
##  4     comm_alarm factor   142466       0 142466        2
##  5        council factor   142466       0 142466       32
##  6         gender factor   140113    2353 142466        2
##  7      hc_client factor   142466       0 142466        2
##  8   housing_supp factor   142466       0 142466        2
##  9   housing_type factor   136537    5929 142466        3
## 10         la_hrs factor   142466       0 142466       13
## 11        laundry factor   142466       0 142466        2
## 12     living_arr factor    69563   72903 142466        2
## 13    multi_staff factor   142466       0 142466        2
## 14 other_telecare factor   142466       0 142466        2
## 15         pc_hrs factor   142466       0 142466       13
## 16        pri_hrs factor   142466       0 142466       13
## 17       shopping factor   142466       0 142466        2
## 18      total_hrs factor   142466       0 142466       13
## 19        vol_hrs factor   142277     189 142466       13
## # ... with 1 more variables: stat <chr>
```

Here we have both the `datatable()` and console version of the skim object. The former is a tidy object that can be manipulated any way we want - the latter is a handy summary of the dataset - I particularly like the counts of complete and missing cases.

###sjmisc

Alternative 16/08/2017


```r
library(sjmisc)

knitr::kable(descr(soc_care10))
```

```
## Warning in psych::describe(dd, fast = FALSE): NAs introduced by coercion
```



variable        type          label               n       NA.prc        mean          sd          se      md        trimmed   min   max   range         skew     kurtosis
--------------  ------------  -------------  ------  -----------  ----------  ----------  ----------  ------  -------------  ----  ----  ------  -----------  -----------
council*        categorical   council         76202    0.0000000   16.045301   9.0959820   0.0329508      16   1.600481e+01     1    32      31    0.0615918   -1.0813682
client_id*      character     client_id       76202    0.0000000         Inf         NaN         NaN   47513   4.244964e+66     0   Inf     Inf          NaN          NaN
age_grp*        categorical   age_grp         68263   10.4183617    2.819609   1.0804331   0.0041353       3   2.899507e+00     1     4       3   -0.4944782   -1.0313806
client_grp*     categorical   client_grp      67483   11.4419569    3.350503   1.0061498   0.0038732       4   3.523645e+00     1     5       4   -1.1462490    0.4929800
gender*         categorical   gender          68277   10.3999895    1.672159   0.4694300   0.0017965       2   1.715193e+00     1     2       1   -0.7334703   -1.4620427
la_hrs*         categorical   la_hrs          76202    0.0000000    3.753773   2.7897056   0.0101059       3   3.429185e+00     1    13      12    0.7515853   -0.3112179
pri_hrs*        categorical   pri_hrs         76202    0.0000000    2.355765   2.7008308   0.0097839       1   1.676733e+00     1    13      12    1.9537750    2.8439107
vol_hrs*        categorical   vol_hrs         76011    0.2506496    1.300930   1.5363787   0.0055726       1   1.000000e+00     1    13      12    5.8596622   35.7512199
total_hrs*      categorical   total_hrs       76202    0.0000000    5.269626   2.9485963   0.0106815       5   5.128539e+00     1    13      12    0.4003527   -0.4369420
hc_client*      categorical   hc_client       76202    0.0000000    1.859754   0.3472440   0.0012579       2   1.949690e+00     1     2       1   -2.0720278    2.2933293
living_arr*     categorical   living_arr      48784   35.9806829    1.353538   0.4780728   0.0021645       1   1.316926e+00     1     2       1    0.6127057   -1.6246251
multi_staff*    categorical   multi_staff     76202    0.0000000    1.064500   0.2456424   0.0008899       1   1.000000e+00     1     2       1    3.5457607   10.5725579
housing_type*   categorical   housing_type    74267    2.5393034    1.252010   0.5573436   0.0020451       1   1.113692e+00     1     3       2    2.1229412    3.3632367
laundry*        categorical   laundry         76202    0.0000000    1.048293   0.2143854   0.0007766       1   1.000000e+00     1     2       1    4.2139185   15.7573161
shopping*       categorical   shopping        76202    0.0000000    1.072925   0.2600144   0.0009419       1   1.000000e+00     1     2       1    3.2849704    8.7911461
housing_supp*   categorical   housing_supp    76202    0.0000000    1.125272   0.3310296   0.0011992       1   1.031594e+00     1     2       1    2.2639836    3.1256627

##Councils

1st thing I want to visualise is the absolute number of records returned for each council and what percentage of the total this is. 

I'm going to stick with 2011 data for now. 


```r
skim11 %>%
  filter(var == "council") %>%
  filter(stat == "count") %>%
  filter(!is.na(level)) %>%
  mutate(percentage = (value/sum(value)* 100)) %>%
  arrange(-value) %>%
  mutate(level = factor(level, level)) %>%
  select(level, value, percentage) %>%
  ggplot() +
  geom_col(aes(x = level, y = value)) + 
    geom_text(aes(label=paste0(round(percentage, 1),"%"), x= level,
                            y= value), size=4, vjust = -0.5, colour = "black") +
    theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1, vjust = 1)) +
    theme_hc() +
  labs(
    title = "Number of records returned for Social Care Survey 2011, with percentage of total",
    subtitle = "by Local Authority", 
    x = "Local Authority",
    y = "Number of records")
```

![](C:\GitHub\social_care_open_data\reports\02-pop_comparison_files/figure-html/skim_coucil11-1.png)<!-- -->


# Population comparison

How do these absolute numbers reflect the differing populations in each local authority?

##Create main table

2011 Popoulation estimates downloaded from the National Records of Scotland here:-  <https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/2011-census-reconciliation-report/list-of-tables>

Create `pop_data_combined` a `data_frame` with population sizes of each Local Authority as a total and for over 65s


```r
#Note I cheated and converted data to numeric in excel to remove the commas as 1000 seperators

#import and tidy raw data
pop_data <- read_csv("raw_data/2011-cen-rec-report-pop-tab1b.csv", skip = 3) %>%
  slice(1:34) %>%
  filter(X1 %in% c("Council areas", "SCOTLAND") == FALSE) %>%
  select(-X3, -X23, -X24) %>%
  map_at(2:21, as.numeric) %>% #coerce from character to numeric
  as_data_frame()

names(pop_data) <- str_replace(names(pop_data), "X1", "council") #rename X variables
names(pop_data) <- str_replace(names(pop_data), "X2", "total")

#tidy data for manipulation
pop_data %<>%
  gather(age_grp, value, 2:21) %>%  
  arrange(council)


#create table of total population per council
pop_data_total <-
  pop_data %>%
  filter(age_grp == "total") %>%  
  select(-age_grp) %>%
  arrange(council)
names(pop_data_total) <- str_replace(names(pop_data_total), "value", "pop_total")

#create table of total over65s per council
pop_data_65plus <-
  pop_data %>%
  filter(age_grp %in% c("65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", 
                        "90+") == TRUE) %>%
  group_by(council) %>%
  mutate(pop_over_65 = sum(value)) %>%    
  arrange(council) %>%
  select(council, pop_over_65) %>%
  unique(.)

#join tables together
pop_data_combined <-
  full_join(pop_data_total, pop_data_65plus)  

pop_data_combined
```

```
## # A tibble: 32 x 3
##                council pop_total pop_over_65
##                  <chr>     <dbl>       <dbl>
##  1       Aberdeen City    222793       32031
##  2       Aberdeenshire    252973       40663
##  3               Angus    115978       23061
##  4       Argyll & Bute     88166       19336
##  5    Clackmannanshire     51442        8212
##  6 Dumfries & Galloway    151324       33050
##  7         Dundee City    147268       24597
##  8       East Ayrshire    122767       21307
##  9 East Dunbartonshire    105026       20428
## 10        East Lothian     99717       17763
## # ... with 22 more rows
```

```r
rm(list = c("pop_data_65plus", "pop_data_total"))
```

##Add social care

Add a column with total amount of records returned from each council i.e. the total number of indiviudals receiving some form of social care


```r
#count number of records from each council
any_care_11 <- fct_count(soc_care11$council)

#rename tibble columns
names(any_care_11) <- 
  str_replace(names(any_care_11), "n", "any_care_total") %>%
  str_replace(., "f", "council")

#join this table to pop_data_combined
pop_data_combined %<>%
  full_join(., any_care_11)
```

```
## Joining, by = "council"
```

```
## Warning: Column `council` joining character vector and factor, coercing
## into character vector
```

```r
pop_data_combined
```

```
## # A tibble: 32 x 4
##                council pop_total pop_over_65 any_care_total
##                  <chr>     <dbl>       <dbl>          <int>
##  1       Aberdeen City    222793       32031           3337
##  2       Aberdeenshire    252973       40663           4157
##  3               Angus    115978       23061           4571
##  4       Argyll & Bute     88166       19336           1904
##  5    Clackmannanshire     51442        8212           1541
##  6 Dumfries & Galloway    151324       33050           3733
##  7         Dundee City    147268       24597           5759
##  8       East Ayrshire    122767       21307           4099
##  9 East Dunbartonshire    105026       20428           2580
## 10        East Lothian     99717       17763           2752
## # ... with 22 more rows
```

```r
rm(any_care_11)
```

##Add social care over 65s

Add a column with total amount of records of over 65s from each local authority


```r
#subset over 65s from 2011 data
any_care65s <-
  soc_care11 %>%
  filter(age_grp != "18-<65") 

#now count by local authority
care65s <- fct_count(any_care65s$council)
names(care65s) <- 
  str_replace(names(care65s), "n", "any_care_over_65s") %>%
  str_replace(., "f", "council")

#and add this to pop_data_combined
pop_data_combined %<>%
  full_join(., care65s)
```

```
## Joining, by = "council"
```

```
## Warning: Column `council` joining character vector and factor, coercing
## into character vector
```

```r
pop_data_combined
```

```
## # A tibble: 32 x 5
##                council pop_total pop_over_65 any_care_total
##                  <chr>     <dbl>       <dbl>          <int>
##  1       Aberdeen City    222793       32031           3337
##  2       Aberdeenshire    252973       40663           4157
##  3               Angus    115978       23061           4571
##  4       Argyll & Bute     88166       19336           1904
##  5    Clackmannanshire     51442        8212           1541
##  6 Dumfries & Galloway    151324       33050           3733
##  7         Dundee City    147268       24597           5759
##  8       East Ayrshire    122767       21307           4099
##  9 East Dunbartonshire    105026       20428           2580
## 10        East Lothian     99717       17763           2752
## # ... with 22 more rows, and 1 more variables: any_care_over_65s <int>
```

```r
rm(care65s)
```

##calculate ratios per 1000 population


```r
#calculate per 1000
pop_data_combined %<>%
  mutate(total_per_thousand = (any_care_total / pop_total) * 1000) %>%
  mutate(over_65s_per_thousand = (any_care_over_65s / pop_over_65) * 1000)

pop_data_combined
```

```
## # A tibble: 32 x 7
##                council pop_total pop_over_65 any_care_total
##                  <chr>     <dbl>       <dbl>          <int>
##  1       Aberdeen City    222793       32031           3337
##  2       Aberdeenshire    252973       40663           4157
##  3               Angus    115978       23061           4571
##  4       Argyll & Bute     88166       19336           1904
##  5    Clackmannanshire     51442        8212           1541
##  6 Dumfries & Galloway    151324       33050           3733
##  7         Dundee City    147268       24597           5759
##  8       East Ayrshire    122767       21307           4099
##  9 East Dunbartonshire    105026       20428           2580
## 10        East Lothian     99717       17763           2752
## # ... with 22 more rows, and 3 more variables: any_care_over_65s <int>,
## #   total_per_thousand <dbl>, over_65s_per_thousand <dbl>
```

##Visualise over 65s


```r
pop_data_combined %>%
  arrange(-over_65s_per_thousand) %>%
  mutate(council = factor(council, council)) %>%
  ggplot() +
  geom_col(aes(x = council, y = over_65s_per_thousand)) +
  theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1, vjust = 1)) +
  theme_hc() +
  labs(
    title = "Number of over 65s per 1000 population receiving any form of social care, 2011",
    x = "Local Authority area",
    y = "rate per 1000 population"
  )
```

![](C:\GitHub\social_care_open_data\reports\02-pop_comparison_files/figure-html/vis_pop-1.png)<!-- -->

##Highland


```r
highland <- 
  soc_care11 %>%
  filter(council == "Highland")

nrow(highland)
```

```
## [1] 4387
```

```r
fct_count(highland$age_grp)
```

```
## # A tibble: 5 x 2
##        f     n
##   <fctr> <int>
## 1 18-<65   304
## 2 65-<75   288
## 3 75-<85   682
## 4    85+   769
## 5   <NA>  2344
```

Highland is an outlier because 53.4% of its records have missing data for Age group. It is likely most of this missing data will be over 65s.



```r
skim11_highland <- 
  skim(highland) %>%
  filter(stat == "missing") %>%
  filter(value != 0)

skim11_highland
```

```
## # A tibble: 5 x 5
##            var   type    stat level value
##          <chr>  <chr>   <chr> <chr> <dbl>
## 1      age_grp factor missing  .all  2344
## 2   client_grp factor missing  .all  2344
## 3       gender factor missing  .all  2344
## 4   living_arr factor missing  .all  2443
## 5 housing_type factor missing  .all   105
```

Turns out (most likely) the same 53.4% of records are also missing data for `client-grp`, `gender`, and `living_arr`.

##Visualise totals


```r
pop_data_combined %>%
  arrange(-total_per_thousand) %>%
  mutate(council = factor(council, council)) %>%
  ggplot() +
  geom_col(aes(x = council, y = total_per_thousand)) +
  theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1, vjust = 1)) +
  theme_hc() +
  labs(
    title = "Number per 1000 population receiving any form of social care, 2011",
    x = "Local Authority area",
    y = "rate per 1000 population"
  )
```

![](C:\GitHub\social_care_open_data\reports\02-pop_comparison_files/figure-html/vis_totals-1.png)<!-- -->

##save created objects

```r
save(pop_data, file = "produced_data/created_objects/pop_data.rds")
save(pop_data_combined, file = "produced_data/created_objects/pop_data_combined.rds")
save(skim10, file = "produced_data/created_objects/skim10.rds")
save(skim11, file = "produced_data/created_objects/skim11.rds")
save(skim12, file= "produced_data/created_objects/skim12.rds")
save(any_care65s, file = "produced_data/created_objects/any_care65s.rds")
```

#Variable by variable visualisation

##Councils

## Age Group

## Client Group

## Gender


## Meals

## Local Authority Provided Home Care Hours

## Private Company Provided Home Care Hours

## Voluntary Orgnaisation Provided Home Care Hours

## Personal Care Home Care Hours

## Total Home Care Hours

## Home Care Client Flad

## Living Arrangements

## Mutli Staffing

## Housing Type

## Laundry

## Shopping

## Housing Support

## IoRN Score

#Interesting comparison
