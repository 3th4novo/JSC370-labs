Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10

# Relative Humidity
met$rh <- 100*((112-0.1*met$temp+met$dew.point)/(112+0.9*met$temp))^8

median_by_station <- met %>%
  group_by(USAFID) %>%
  summarise(
    median_temp = median(temp, na.rm = TRUE),
    median_wind.sp = median(wind.sp, na.rm = TRUE),
    median_atm.press = median(atm.press, na.rm = TRUE)
  )
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
joint_df <- left_join(stations, median_by_station, by = join_by(USAF == USAFID))
```

``` r
head(joint_df)
```

    ##     USAF   CTRY  STATE   LAT    LON median_temp median_wind.sp median_atm.press
    ##    <int> <char> <char> <num>  <num>       <num>          <num>            <num>
    ## 1:  7018   <NA>   <NA>  0.00  0.000          NA             NA               NA
    ## 2:  7026     AF   <NA>  0.00  0.000          NA             NA               NA
    ## 3:  7070     AF   <NA>  0.00  0.000          NA             NA               NA
    ## 4:  8260   <NA>   <NA>  0.00  0.000          NA             NA               NA
    ## 5:  8268     AF   <NA> 32.95 65.567          NA             NA               NA
    ## 6:  8307     AF   <NA>  0.00  0.000          NA             NA               NA

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
quantile(joint_df$median_temp, na.rm = TRUE)
```

    ##   0%  25%  50%  75% 100% 
    ##  1.0 18.3 21.1 23.9 34.0

``` r
quantile(joint_df$median_wind.sp, na.rm = TRUE)
```

    ##   0%  25%  50%  75% 100% 
    ##  1.5  2.6  3.1  3.6  9.3

``` r
quantile(joint_df$median_atm.press, na.rm = TRUE)
```

    ##     0%    25%    50%    75%   100% 
    ##  985.7 1010.7 1011.6 1012.7 1041.3

Next identify the stations have these median values.

``` r
station_med <- met[, .(
  temp_50 = quantile(met$temp, probs = 0.5, na.rm = TRUE),
  wind.sp_50 = quantile(met$wind.sp, probs = 0.5, na.rm = TRUE),
  atm.press_50 = quantile(met$atm.press, probs = 0.5, na.rm = TRUE)
)]
```

``` r
joint_df %>% 
  filter((joint_df$median_temp - station_med$temp_50 == 0) & (joint_df$median_wind.sp - station_med$wind.sp_50 == 0) & (joint_df$median_atm.press - station_med$atm.press_50 == 0))
```

    ##      USAF   CTRY  STATE    LAT    LON median_temp median_wind.sp
    ##     <int> <char> <char>  <num>  <num>       <num>          <num>
    ## 1: 723119     US     SC 34.849 -82.35        21.7            3.1
    ##    median_atm.press
    ##               <num>
    ## 1:           1011.7

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
state_med <- joint_df %>% 
  group_by(STATE) %>% 
  summarise(state_temp_med = median(median_temp, na.rm = TRUE),
            state_ws_med = median(median_wind.sp, na.rm = TRUE))
```

``` r
joint_1 <- left_join(joint_df, state_med, by = join_by(STATE == STATE))
```

``` r
joint_1 <- joint_1 %>% 
  mutate(euclidean_distance = sqrt((median_temp - state_temp_med)^2 + (median_wind.sp - state_ws_med)^2), by = STATE)
```

``` r
representative_stations <- joint_1[, .SD[which.min(euclidean_distance)], by = STATE]
representative_stations
```

    ##      STATE   USAF   CTRY    LAT      LON median_temp median_wind.sp
    ##     <char>  <int> <char>  <num>    <num>       <num>          <num>
    ##  1:     NM 723658     US 36.744 -108.229        21.7           3.60
    ##  2:     CA 722909     US 32.568 -117.117        17.8           3.60
    ##  3:     FL 720735     US 30.349  -85.788        26.1           3.60
    ##  4:     MI 725395     US 42.264  -84.456        18.3           3.10
    ##  5:     NV 725805     US 40.068 -118.570        18.9           3.60
    ##  6:     TX 722448     US 32.359  -95.404        27.8           3.60
    ##  7:     NC 725294     US 35.017  -80.083        22.1           2.60
    ##  8:     WA 727924     US 46.117 -122.894        15.0           3.10
    ##  9:     AZ 722747     US 32.857 -109.636        28.3           3.60
    ## 10:     OR 726875     US 42.590 -117.865        16.1           3.10
    ## 11:     AR 722188     US 35.212  -91.737        23.9           2.60
    ## 12:     AL 720505     US 33.902  -87.314        23.0           2.60
    ## 13:     MD 725514     US 38.142  -76.429        21.1           3.10
    ## 14:     NE 725525     US 40.600  -98.426        21.1           3.60
    ## 15:     UT 725724     US 40.219 -111.723        19.4           3.10
    ## 16:     KY 724233     US 38.185  -84.904        20.6           3.10
    ## 17:     IN 744660     US 40.948  -87.183        20.0           3.60
    ## 18:     GA 720257     US 31.397  -84.895        23.0           2.60
    ## 19:     MS 722364     US 34.681  -90.347        24.9           2.60
    ## 20:     VA 720501     US 37.850  -76.883        21.0           2.60
    ## 21:     CO 724695     US 39.717 -104.750        15.1           3.60
    ## 22:     SD 726515     US 44.300  -96.800        21.7           4.10
    ## 23:     LA 720467     US 30.817  -89.867        27.2           2.60
    ## 24:     SC 723106     US 34.188  -79.731        22.8           3.10
    ## 25:     MT 726770     US 45.807 -108.546        16.1           3.10
    ## 26:     IL 722149     US 41.352  -89.153        22.0           3.10
    ## 27:     WV 724273     US 39.339  -81.444        18.9           2.60
    ## 28:     TN 723240     US 35.034  -85.200        22.2           3.10
    ## 29:     MO 723290     US 37.087  -84.077        22.8           3.10
    ## 30:     OK 723540     US 35.417  -97.383        24.5           3.60
    ## 31:     MN 726550     US 45.544  -94.052        21.1           3.10
    ## 32:     IA 720309     US 40.947  -91.511        22.0           3.10
    ## 33:     PA 720324     US 40.435  -75.382        19.0           2.85
    ## 34:     ID 725867     US 42.545 -113.769        16.1           3.10
    ## 35:     WI 722059     US 42.690  -88.304        19.1           3.10
    ## 36:     KS 724505     US 37.617  -97.267        23.1           3.60
    ## 37:     WY 725690     US 42.898 -106.474        13.9           3.60
    ## 38:     OH 724294     US 39.757  -82.663        19.2           3.60
    ## 39:     NJ 720581     US 40.617  -74.250        20.0           3.10
    ## 40:     ME 726060     US 43.642  -70.304        15.0           3.10
    ## 41:     ND 727675     US 48.417 -101.350        20.7           3.60
    ## 42:     VT 720493     US 44.883  -72.233        16.3           2.60
    ## 43:     CT 725027     US 41.510  -72.828        20.0           3.10
    ## 44:     NY 725190     US 43.111  -76.104        18.3           3.10
    ## 45:     RI 725079     US 41.530  -71.283        18.0           3.60
    ## 46:     MA 725059     US 42.470  -71.289        17.8           3.10
    ## 47:     DE 724093     US 38.690  -75.362        21.1           3.60
    ## 48:     NH 726056     US 43.279  -70.924        16.7           2.60
    ##      STATE   USAF   CTRY    LAT      LON median_temp median_wind.sp
    ##     median_atm.press state_temp_med state_ws_med euclidean_distance     by
    ##                <num>          <num>        <num>              <num> <char>
    ##  1:          1011.70          21.55         4.10          0.5220153     NM
    ##  2:          1014.65          17.50         3.60          0.3000000     CA
    ##  3:          1012.60          26.10         3.60          0.0000000     FL
    ##  4:          1012.70          18.30         3.10          0.0000000     MI
    ##  5:          1012.30          18.90         3.60          0.0000000     NV
    ##  6:          1010.20          27.80         3.60          0.0000000     TX
    ##  7:               NA          22.15         2.60          0.0500000     NC
    ##  8:          1018.60          15.00         3.10          0.0000000     WA
    ##  9:          1007.20          28.00         3.60          0.3000000     AZ
    ## 10:          1013.20          16.10         3.10          0.0000000     OR
    ## 11:          1011.80          23.90         2.60          0.0000000     AR
    ## 12:               NA          23.00         2.60          0.0000000     AL
    ## 13:          1010.70          21.10         3.10          0.0000000     MD
    ## 14:          1012.20          21.40         3.60          0.3000000     NE
    ## 15:          1011.50          19.40         3.60          0.5000000     UT
    ## 16:          1011.50          20.60         3.10          0.0000000     KY
    ## 17:               NA          20.00         3.60          0.0000000     IN
    ## 18:               NA          23.00         2.60          0.0000000     GA
    ## 19:               NA          25.00         2.60          0.1000000     MS
    ## 20:               NA          21.00         2.60          0.0000000     VA
    ## 21:          1011.60          15.00         3.60          0.1000000     CO
    ## 22:          1013.10          21.60         4.10          0.1000000     SD
    ## 23:               NA          27.20         2.60          0.0000000     LA
    ## 24:          1011.40          22.80         3.10          0.0000000     SC
    ## 25:          1012.80          16.10         3.10          0.0000000     MT
    ## 26:               NA          22.00         3.10          0.0000000     IL
    ## 27:          1012.20          18.90         2.60          0.0000000     WV
    ## 28:          1011.20          22.10         2.85          0.2692582     TN
    ## 29:          1011.40          23.00         3.10          0.2000000     MO
    ## 30:          1010.40          24.50         3.60          0.0000000     OK
    ## 31:          1013.50          21.10         3.10          0.0000000     MN
    ## 32:               NA          22.00         3.10          0.0000000     IA
    ## 33:               NA          18.90         3.10          0.2692582     PA
    ## 34:          1012.20          16.10         2.60          0.5000000     ID
    ## 35:               NA          19.10         3.10          0.0000000     WI
    ## 36:          1010.70          23.00         3.60          0.1000000     KS
    ## 37:          1012.20          13.60         3.60          0.3000000     WY
    ## 38:          1012.10          19.20         3.60          0.0000000     OH
    ## 39:               NA          20.00         3.10          0.0000000     NJ
    ## 40:          1010.60          15.00         3.10          0.0000000     ME
    ## 41:          1012.55          20.60         3.60          0.1000000     ND
    ## 42:               NA          16.30         2.60          0.0000000     VT
    ## 43:          1009.70          20.00         3.10          0.0000000     CT
    ## 44:          1009.60          18.30         3.10          0.0000000     NY
    ## 45:          1010.40          18.00         3.60          0.0000000     RI
    ## 46:          1010.35          17.80         3.10          0.0000000     MA
    ## 47:          1010.50          21.10         3.60          0.0000000     DE
    ## 48:          1009.70          16.70         2.60          0.0000000     NH
    ##     median_atm.press state_temp_med state_ws_med euclidean_distance     by

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
geographic_midpoint <- joint_1[, .(median_lat = median(LAT, na.rm = TRUE), 
                               median_lon = median(LON, na.rm = TRUE)), by = .(STATE)]
```

``` r
joint_2 <- merge(joint_df, geographic_midpoint, by = "STATE")
```

``` r
joint_2[, distance_to_midpoint := sqrt((LAT - median_lat)^2 + (LON - median_lon)^2), by = STATE]
```

``` r
closest_stations_geo <- joint_2[, .SD[which.min(distance_to_midpoint)], by = STATE]
closest_stations_geo
```

    ## Key: <STATE>
    ##      STATE   USAF   CTRY     LAT      LON median_temp median_wind.sp
    ##     <char>  <int> <char>   <num>    <num>       <num>          <num>
    ##  1:   <NA> 136290     AL  40.600   20.767          NA             NA
    ##  2:     AK 749221     US  61.150 -150.233          NA             NA
    ##  3:     AL 722266     US  32.350  -86.983          NA             NA
    ##  4:     AR 720401     US  35.600  -92.450        23.0            2.6
    ##  5:     AS 917650     AQ -14.331 -170.714          NA             NA
    ##  6:     AZ 722783     US  33.466 -111.721        30.6            2.6
    ##  7:     BC 711070     CA  51.933 -131.000          NA             NA
    ##  8:     CA 723940     US  34.894 -120.452        13.9            3.1
    ##  9:     CO 726396     US  39.050 -105.516         8.0            3.1
    ## 10:     CR 167464     GR  35.533   24.150          NA             NA
    ## 11:     CT 997284     US  41.280  -72.880          NA             NA
    ## 12:     DC 997314     US  38.870  -77.020          NA             NA
    ## 13:     DE 998169     US  39.083  -75.433          NA             NA
    ## 14:     FL 722123     US  27.950  -81.783        24.0            3.1
    ## 15:     FM 913340     FM   7.467  151.850          NA             NA
    ## 16:     GA 720300     US  32.683  -83.350          NA             NA
    ## 17:     GU 912120     GQ  13.483  144.800          NA             NA
    ## 18:     HI 911860     US  21.154 -157.096          NA             NA
    ## 19:     IA 725466     US  41.691  -93.566        22.0            3.1
    ## 20:     ID 725865     US  43.500 -114.300        11.0            4.1
    ## 21:     IL 724397     US  40.483  -88.950        21.7            4.1
    ## 22:     IN 720575     US  40.031  -86.251        20.0            3.1
    ## 23:     KS 720432     US  38.352  -97.691          NA             NA
    ## 24:     KY 720448     US  37.578  -84.770        20.0            3.6
    ## 25:     LA 722403     US  29.559  -91.525        28.9            2.6
    ## 26:     MA 725097     US  42.150  -70.933          NA             NA
    ## 27:     MB 719540     CA  54.683 -101.683          NA             NA
    ## 28:     MD 998409     US  38.983  -76.480          NA             NA
    ## 29:     ME 726157     US  44.080  -69.030          NA             NA
    ## 30:     MH 913660     RM   8.733  167.733          NA             NA
    ## 31:     MI 726380     US  44.359  -84.674        16.7            3.1
    ## 32:     MN 726550     US  45.544  -94.052        21.1            3.1
    ## 33:     MO 720869     US  38.947  -92.683        23.9            2.6
    ## 34:     MP 912310     CQ  14.999  145.621          NA             NA
    ## 35:     MS 722340     US  32.335  -88.751        24.0            2.6
    ## 36:     MT 720994     US  47.283 -110.800          NA             NA
    ## 37:     NB 717110     CA  47.983  -66.333          NA             NA
    ## 38:     NC 722131     US  35.541  -78.390        22.0            2.6
    ## 39:     ND 721012     US  47.909 -100.927          NA             NA
    ## 40:     NE 725524     US  41.623  -98.949        20.6            3.6
    ## 41:     NH 726050     US  43.205  -71.503        17.2            2.6
    ## 42:     NJ 724090     US  40.033  -74.350        19.4            3.6
    ## 43:     NM 722685     US  34.100 -105.667          NA             NA
    ## 44:     NS 719590     CA  46.633  -60.933          NA             NA
    ## 45:     NT 710513     CA  71.950 -124.733          NA             NA
    ## 46:     NU 719390     CA  68.933 -116.917          NA             NA
    ## 47:     NV 724770     US  39.601 -116.006        14.4            3.1
    ## 48:     NY 997991     US  42.017  -73.917          NA             NA
    ## 49:     OH 724200     US  40.820  -82.518        18.0            3.6
    ## 50:     OK 723540     US  35.417  -97.383        24.5            3.6
    ## 51:     ON 710445     CA  48.367  -89.333          NA             NA
    ## 52:     OR 726945     US  44.500 -123.283        15.6            4.1
    ## 53:     PA 725118     US  40.218  -76.855        21.1            3.1
    ## 54:     PC 912450     WQ  19.283  166.650          NA             NA
    ## 55:     PR 785260     US  18.433  -66.011          NA             NA
    ## 56:     PW 914081     PS   7.337  134.477          NA             NA
    ## 57:     QC 716270     CA  45.467  -73.733          NA             NA
    ## 58:     RI 997288     US  41.580  -71.400          NA             NA
    ## 59:     SC 747920     US  33.850  -80.480          NA             NA
    ## 60:     SD 726686     US  44.381 -100.286          NA             NA
    ## 61:     TN 749083     US  36.183  -86.300          NA             NA
    ## 62:     TX 720648     US  30.395  -97.567        27.0            3.6
    ## 63:     UM 912750     JQ  16.733 -169.533          NA             NA
    ## 64:     UT 724795     US  39.330 -112.580          NA             NA
    ## 65:     VA 720297     US  37.239  -76.716          NA             NA
    ## 66:     VI 785470     VQ  17.700  -64.813          NA             NA
    ## 67:     VT 726145     US  44.205  -72.565        15.6            2.6
    ## 68:     WA 994014     US  47.605 -122.338          NA             NA
    ## 69:     WI 726426     US  44.550  -89.533        20.0            3.1
    ## 70:     WV 720366     US  38.687  -80.652          NA             NA
    ## 71:     WY 726690     US  41.806 -107.195        13.3            4.6
    ## 72:     YK 693870     CA  62.117 -136.183          NA             NA
    ## 73:     YT 719690     CA  69.583 -140.183          NA             NA
    ##      STATE   USAF   CTRY     LAT      LON median_temp median_wind.sp
    ##     median_atm.press median_lat median_lon distance_to_midpoint
    ##                <num>      <num>      <num>                <num>
    ##  1:               NA    40.1485    20.6330           0.47096523
    ##  2:               NA    61.2595  -150.6500           0.43113716
    ##  3:               NA    32.3915   -86.7745           0.21258998
    ##  4:               NA    35.2345   -92.5310           0.37436780
    ##  5:               NA   -14.3310  -170.7140           0.00000000
    ##  6:           1007.2    33.4170  -111.7830           0.07902531
    ##  7:               NA    53.0915  -128.4750           2.77808338
    ##  8:           1015.3    35.3500  -120.0145           0.63193532
    ##  9:               NA    39.2455  -105.1420           0.42201451
    ## 10:               NA    35.5330    24.1500           0.00000000
    ## 11:               NA    41.2750   -72.8540           0.02647640
    ## 12:               NA    38.8700   -77.0200           0.00000000
    ## 13:               NA    39.0830   -75.4330           0.00000000
    ## 14:               NA    27.9500   -81.7570           0.02600000
    ## 15:               NA     7.2250   155.0250           3.18420932
    ## 16:               NA    32.5885   -83.4195           0.11730516
    ## 17:               NA    13.5330   144.8585           0.07695616
    ## 18:               NA    21.1540  -157.0170           0.07900000
    ## 19:               NA    41.7090   -93.5660           0.01800000
    ## 20:               NA    43.7875  -114.6220           0.43167146
    ## 21:           1012.7    40.2965   -88.6660           0.33976205
    ## 22:               NA    40.1760   -86.2665           0.14582610
    ## 23:               NA    38.3020   -97.4300           0.26574612
    ## 24:               NA    37.5120   -84.7210           0.08220097
    ## 25:           1011.0    29.4450   -91.5170           0.11428036
    ## 26:               NA    42.0990   -70.9460           0.05263079
    ## 27:               NA    54.6830  -101.6830           0.00000000
    ## 28:               NA    38.9830   -76.4700           0.01000000
    ## 29:               NA    44.4000   -68.8190           0.38330275
    ## 30:               NA     7.8990   169.5025           1.95619177
    ## 31:           1013.4    44.4500   -84.9130           0.25573815
    ## 32:           1013.5    45.6050   -94.1655           0.12885360
    ## 33:               NA    38.5830   -92.5530           0.38651779
    ## 34:               NA    14.9990   145.6210           0.00000000
    ## 35:           1011.1    32.2330   -89.2560           0.51519802
    ## 36:               NA    46.9690  -111.1085           0.44019115
    ## 37:               NA    47.9830   -66.3330           0.00000000
    ## 38:               NA    35.3440   -78.3300           0.20593446
    ## 39:               NA    47.9430  -101.0100           0.08969392
    ## 40:           1012.2    41.2715   -99.0750           0.37340092
    ## 41:           1009.7    43.2420   -71.4345           0.07785403
    ## 42:           1009.2    40.0800   -74.4500           0.11049434
    ## 43:               NA    34.1000  -106.0960           0.42900000
    ## 44:               NA    46.6330   -60.9330           0.00000000
    ## 45:               NA    71.9500  -124.7000           0.03300000
    ## 46:               NA    68.9330  -116.9170           0.00000000
    ## 47:           1011.8    39.1830  -116.0060           0.41800000
    ## 48:               NA    42.2240   -74.1230           0.29203596
    ## 49:           1012.5    40.9180   -82.6630           0.17501143
    ## 50:           1010.4    35.4780   -97.1730           0.21868013
    ## 51:               NA    48.5830   -89.3330           0.21600000
    ## 52:           1017.3    44.5415  -122.9785           0.30731498
    ## 53:           1010.7    40.4350   -76.8770           0.21811236
    ## 54:               NA    13.3080   150.5665          17.15749391
    ## 55:               NA    18.4330   -66.0545           0.04350000
    ## 56:               NA     7.3370   134.4770           0.00000000
    ## 57:               NA    45.4670   -73.7330           0.00000000
    ## 58:               NA    41.5885   -71.4000           0.00850000
    ## 59:               NA    33.8280   -80.5000           0.02973214
    ## 60:               NA    44.3800  -100.3475           0.06150813
    ## 61:               NA    35.9640   -86.3000           0.21900000
    ## 62:               NA    30.3710   -97.3200           0.24816325
    ## 63:               NA    19.2830  -169.5330           2.55000000
    ## 64:               NA    39.6090  -112.0620           0.58835788
    ## 65:               NA    37.2360   -76.9520           0.23601907
    ## 66:               NA    17.7000   -64.8130           0.00000000
    ## 67:           1011.3    44.3125   -72.7825           0.24261595
    ## 68:               NA    47.6110  -122.5120           0.17410342
    ## 69:               NA    44.5150   -89.5325           0.03500357
    ## 70:               NA    38.9025   -80.6480           0.21553712
    ## 71:           1011.7    42.5780  -107.1950           0.77200000
    ## 72:               NA    62.1170  -136.1830           0.00000000
    ## 73:               NA    69.5830  -140.1830           0.00000000
    ##     median_atm.press median_lat median_lon distance_to_midpoint

``` r
leaflet(data = geographic_midpoint) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(~median_lat, ~median_lon)
```

<div class="leaflet html-widget html-fill-item" id="htmlwidget-cd5161b3c88fae9b80fc" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-cd5161b3c88fae9b80fc">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[20.633,24.15,-106.096,-120.0145,-81.75700000000001,-84.913,-116.006,-97.31999999999999,-78.33,-122.512,-111.783,-122.9785,-92.53100000000001,-86.77449999999999,-76.47,-99.075,-112.062,-84.721,-86.26650000000001,-83.4195,-150.65,-136.183,-89.256,-76.952,-105.142,-89.333,-124.7,-128.475,-100.3475,-73.733,-66.333,-116.917,-101.683,-60.933,-140.183,-91.517,-80.5,-111.1085,-88.666,-80.648,-86.3,-92.553,-97.173,-94.16549999999999,-93.566,-76.877,-114.622,-89.5325,-97.43000000000001,-107.195,-82.663,-74.45,-68.819,-101.01,-72.7825,-72.854,-74.12299999999999,-71.40000000000001,-70.946,-75.43300000000001,-71.43450000000001,-157.017,-66.05449999999999,-64.813,-169.533,144.8585,145.621,150.5665,155.025,169.5025,134.477,-170.714,-77.02],[40.1485,35.533,34.1,35.34999999999999,27.95,44.45,39.183,30.371,35.344,47.611,33.417,44.5415,35.2345,32.3915,38.983,41.2715,39.609,37.512,40.176,32.5885,61.2595,62.117,32.233,37.236,39.2455,48.583,71.95,53.0915,44.38,45.467,47.983,68.93300000000001,54.683,46.633,69.583,29.445,33.828,46.969,40.29649999999999,38.9025,35.964,38.583,35.47799999999999,45.605,41.709,40.435,43.7875,44.515,38.302,42.578,40.918,40.08,44.4,47.943,44.3125,41.27500000000001,42.224,41.5885,42.099,39.083,43.242,21.154,18.433,17.7,19.283,13.533,14.999,13.308,7.225,7.899000000000001,7.337,-14.331,38.87],10,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-170.714,169.5025],"lng":[-14.331,71.95]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
joint_df4 <- left_join(met, stations, by = join_by(USAFID == USAF))
joint_df4 <- joint_df4 %>% mutate(elev_cat = case_when(
  elev < 93 ~ "low",
  elev >= 93 & elev < 401 ~ "mid",
  elev >= 401 ~ "high")
)

joint_df4 %>% 
  group_by(STATE, elev_cat) %>% 
  summarise(temp_mean = mean(temp, na.rm = TRUE), .groups = "drop")
```

    ## # A tibble: 101 × 3
    ##    STATE elev_cat temp_mean
    ##    <chr> <chr>        <dbl>
    ##  1 AL    low           25.1
    ##  2 AL    mid           23.8
    ##  3 AR    high          23.7
    ##  4 AR    low           25.6
    ##  5 AR    mid           24.4
    ##  6 AZ    high          23.9
    ##  7 AZ    low           29.3
    ##  8 AZ    mid           30.4
    ##  9 CA    high          18.1
    ## 10 CA    low           18.3
    ## # ℹ 91 more rows

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
filtered_data <- median_by_station %>%
  filter(median_atm.press >= 1000 & median_atm.press <= 1020)

ggplot(filtered_data, aes(x = median_atm.press, y = median_temp)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  geom_smooth(method = "gam", formula = y ~ s(x), color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Temperature vs. Atmospheric Pressure",
       x = "Atmospheric Pressure",
       y = "Temperature")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
linear_model <- lm(median_temp ~ median_atm.press, data = filtered_data)

spline_model <- gam(median_temp ~ s(median_wind.sp), data = filtered_data)

summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = median_temp ~ median_atm.press, data = filtered_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3696  -2.6173   0.1844   2.3012  11.6394 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      1175.93898   58.53149   20.09   <2e-16 ***
    ## median_atm.press   -1.14162    0.05785  -19.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.762 on 1083 degrees of freedom
    ## Multiple R-squared:  0.2645, Adjusted R-squared:  0.2638 
    ## F-statistic: 389.4 on 1 and 1083 DF,  p-value: < 2.2e-16

``` r
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## median_temp ~ s(median_wind.sp)
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.9391     0.1297   161.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                     edf Ref.df     F p-value    
    ## s(median_wind.sp) 3.071  3.886 15.96  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0529   Deviance explained = 5.56%
    ## GCV = 18.296  Scale est. = 18.228    n = 1083

We can see from the graph that GAM captures the non-linear feature of
the data. I think the spline model is a better fit because there isn’t a
clear linear relationship between the points.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
