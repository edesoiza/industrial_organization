### SEPARATING OUT INDIVIDUAL DATASETS & ADDING DIRT ###

tic()

### Loading data
  master <- read_parquet(FOLDER("data/Master data.parquet"))

### Separating out data & making it absolutely filthy
  # Dominant price dataset
  
  # Defendants' price datasets
  
  # Temperature datasets
  temperature <- master %>%
    select(date, state, temp) %>%
    mutate(temp = case_when(state == "RI" ~ (temp - 32) * (5 / 9),
                            TRUE ~ temp)) %>%
    remove_random_dates(n = 10) %>%
    duplicate_random_dates(n = 10)
  
  # Ingredients price schedule time series dataset
  ingredients <- master %>%
    mutate(egg_price = round(egg_price / 12, 2)) %>%
    pivot_longer(cols = c(milk_price, egg_price, sugar_price),
                 names_to = "ingredient",
                 values_to = "price") %>%
    mutate(ingredient = str_to_upper(str_replace(str_remove(ingredient, "_price"), "egg", " one  egg"))) %>%
    select(c(date, ingredient, price)) %>%
    group_by(ingredient) %>%
    filter(price != lag(price)) %>%
    ungroup() %>%
    remove_random_dates(n = 100) %>%
    duplicate_random_dates(n = 100) %>%
    rename(date_recorded = date)
  
  # Shipping cost time series dataset
  
  # Flavor preference survey dataset
  
### Outputting results
  
  
toc()