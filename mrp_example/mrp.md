# MRP for Raleigh Community Survey 2018
Gio Circo
2023-06-12

## MRP Example Code

First, need to pull census data from ACS, link responses to census tract
level. NOTE: Can likely do even block group but not sure on how many
strata we can do. Will also have to rely somewhat on spatial weighting
as well (see ICAR models).

``` r
library(tidyverse)
library(brms)
library(tidycensus)
library(sf)

# load survey data
svy <- read_csv("Ral18_Survey.csv")

# LOAD ACS DATA 
# ------------------------- #
vars <- filter(load_variables(2018, "acs5"), grepl("B01001", name))
demo_vars <- vars %>% select(name) %>% pull()

census <- get_acs(
  geography = "tract",
  state = "NC",
  county = "Wake",
  variables = demo_vars
) %>%
  left_join(vars, by = c("variable" = "name"))

# GEOGRAPHY
# ------------------------- #

# city limits of Raleigh
city_limits <- st_read("Raleigh_City_Council_Districts.shp") %>%
  st_transform(crs = 2264)

# Wake County tracts
raleigh_tract <-
  get_acs(
    geography = "tract",
    state = "NC",
    county = "Wake",
    variables = "B01001_001",
    geometry = TRUE
  ) %>%
  select(geoid = GEOID) %>%
  st_transform(crs = st_crs(city_limits))

# survey coords, spatial join to tract
svy_coords <- svy %>%
  select(ID, BLOCK_LAT, BLOCK_LON) %>%
  filter(!is.na(BLOCK_LAT)) %>%
  st_as_sf(coords = c('BLOCK_LON', 'BLOCK_LAT'), crs = 4326) %>%
  st_transform(crs = st_crs(city_limits)) %>%
  st_join(raleigh_tract) %>%
  tibble() %>%
  select(ID, geoid)
```

## Data Setup

### Recode census data

``` r
# Select variables of interest, grouping up by relevant strata
# gender = 2, race = 4, age = 6
census_demos <- census %>%
  select(geoid = GEOID, estimate, label, concept) %>%
  group_by(geoid, concept) %>%
  slice(-1:-2) %>%
  mutate(
    gender = case_when(grepl("Female", label) ~ "female",
                       grepl("Male", label) ~ "male"),
    race = case_when(
      grepl("WHITE ALONE, NOT HISPANIC OR LATINO", concept) ~ "white",
      grepl("BLACK OR AFRICAN AMERICAN ALONE", concept) ~ "black",
      grepl("HISPANIC OR LATINO", concept) ~ "hispanic",
      grepl("NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE|ASIAN ALONE", concept) ~ "asian",
      grepl("TWO OR MORE RACES|SOME OTHER RACE ALONE", concept) ~ "other"
    ),
    age = case_when(
      grepl("18 and 19|20 to 24|25 to 29|30 to 34", label) ~ "18-34",
      grepl("35 to 44", label) ~ "35-44",
      grepl("45 to 54", label) ~ "45-54",
      grepl("55 to 64", label) ~ "55-64",
      grepl("65 to 74|75 to 84|85 years and over", label) ~ "65+"
    )
  ) %>%
  na.omit()

raleigh <-
  census_demos %>%
  group_by(geoid, gender, race, age) %>%
  summarise(count = sum(estimate)) %>%
  right_join(raleigh_tract) %>%
  st_as_sf()
```

### Recode survey data

``` r
# mrp svy
# this looks insane, but its just because the field names are wrong
mrp_svy <- svy %>%
  right_join(svy_coords) %>%
  rename(age = `Which_of_the_following_best_describes_your_race_or_ethnicity___28_01`,
         sex = `Household_Income__32`,
         pol_qual = `Quality_of_police_services__12_01`) %>%
  mutate(age = case_when(
    age == 1 ~ "18-34",
    age == 2 ~ "35-44",
    age == 3 ~ "45-54",
    age == 4 ~ "55-64",
    age == 5 ~ "65+"
  ),
  race = case_when(
    Ancestry__29 == 4 ~ "White",
    Which_of_the_following_is_the_highest_level_of_education_you_have_completed___31 == 1 ~ "Hispanic",
    !is.na(`Are_you_of_Spanish__Hispanic__or_Latino_Ancestry___29`) ~ "Black",
    !is.na(`Other__Answer__28_02`) ~ "Asian",
    TRUE ~ "Other"
  ),
  pol_qual = case_when(
    pol_qual %in% 4:5 ~ 1,
    pol_qual %in% 1:3 ~ 0
  )) %>%
  select(age,race,sex,geoid, pol_qual) %>% 
  na.omit()
```

    Joining, by = "ID"

### Strata to post-stratify to

``` r
# post strat table
post_strat <-
  mrp_svy %>%
  expand(age,race,sex,geoid)

head(post_strat)
```

    # A tibble: 6 × 4
      age   race  sex    geoid      
      <chr> <chr> <chr>  <chr>      
    1 18-34 Asian Female 37183050100
    2 18-34 Asian Female 37183050300
    3 18-34 Asian Female 37183050400
    4 18-34 Asian Female 37183050500
    5 18-34 Asian Female 37183050600
    6 18-34 Asian Female 37183050700

### Run MRP model

``` r
# MULTI-LEVEL REGRESSION
# ------------------------- #

# set tight priors for more regularization
bprior <- c(prior(normal(0, 1), class = "Intercept"),
            prior(normal(0, 1), class = "sd"))

# Regression Step
# Multi-level regression predicting the probability a respondent
# says police service is "excellent" or "good"

fit1 <- brm(pol_qual ~ sex +
              (1|age) +
              (1|race) +
              (1|age:race) +
              (1|geoid),
            family = bernoulli(),
            prior = bprior,
            data = mrp_svy,
            chains = 4,
            cores = 4,
            iter = 2000,
            control = list(adapt_delta = .95))
```

    Compiling Stan program...

    Start sampling

``` r
summary(fit1)
```

     Family: bernoulli 
      Links: mu = logit 
    Formula: pol_qual ~ sex + (1 | age) + (1 | race) + (1 | age:race) + (1 | geoid) 
       Data: mrp_svy (Number of observations: 842) 
      Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
             total post-warmup draws = 4000

    Group-Level Effects: 
    ~age (Number of levels: 5) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.26      0.21     0.01     0.85 1.00     1475     1614

    ~age:race (Number of levels: 25) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.23      0.16     0.01     0.60 1.00     1237     1659

    ~geoid (Number of levels: 107) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.33      0.17     0.03     0.65 1.00      835     1289

    ~race (Number of levels: 5) 
                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    sd(Intercept)     0.68      0.30     0.26     1.43 1.00     1602     1434

    Population-Level Effects: 
              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    Intercept     0.80      0.39    -0.03     1.51 1.00     2015     2127
    sexMale      -0.08      0.17    -0.43     0.26 1.00     5345     2999

    Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    and Tail_ESS are effective sample size measures, and Rhat is the potential
    scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# extract point estimates 
pp <- posterior_predict(fit1, post_strat)
pred_df <- tibble(post_strat, pred = apply(pp, 2, mean))
```

## Post Stratified Estimates

``` r
# POST STRATIFICATION
# ------------------------- #
post_strat_est <-
  pred_df %>%
  rename(gender = sex) %>%
  mutate(across(where(is.character), tolower)) %>%
  left_join(raleigh) %>%
  na.omit() %>%
  mutate(pred = pred * count) %>%
  group_by(geoid) %>%
  summarise(count = sum(count),
            pred = sum(pred)) %>%
  mutate(prop = pred / count)
```

    Joining, by = c("age", "race", "gender", "geoid")

``` r
# predictions
head(post_strat_est)
```

    # A tibble: 6 × 4
      geoid       count  pred  prop
      <chr>       <dbl> <dbl> <dbl>
    1 37183050100  5043 3761. 0.746
    2 37183050300  3231 2549. 0.789
    3 37183050400  1736 1354. 0.780
    4 37183050500  3210 2296. 0.715
    5 37183050600  3038 2004. 0.660
    6 37183050700  2673 1761. 0.659

``` r
# Map of proportion
post_strat_est %>%
  left_join(raleigh_tract) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = prop)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Raleigh Community Survey (2018)",
       subtitle = "Proportion rating quality of police services as 'Excellent' or 'Good'",
       fill = "MRP Est")
```

    Joining, by = "geoid"

![](mrp_files/figure-commonmark/unnamed-chunk-6-1.png)
