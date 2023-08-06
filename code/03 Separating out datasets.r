### SEPARATING OUT INDIVIDUAL DATASETS & ADDING DIRT ###

tic()

### Loading data
  master <- read_parquet(FOLDER("Master data.parquet"))
  
### Separating out data & making it absolutely filthy
  # Dominant price dataset
  
  # Defendants' price datasets
  
  # Temperature datasets
  temperature <- master %>%
    select(date, state, temp) %>%
    mutate(temp = case_when(state == "RI" ~ round((temp - 32) * (5 / 9), 1),
                            TRUE ~ temp)) %>%
    mutate(temp = case_when(state != "RI" ~ paste(as.character(temp), " F", sep = ""),
                            TRUE ~ paste(as.character(temp), " C", sep = ""))) %>%
    arrange(state, date) %>%
    filter(date != lag(date)) %>%
    arrange(desc(date)) %>%
    pivot_wider(id_cols = date,
                names_from = state,
                values_from = temp,
                values_fill = "(NO DATA AVAILABLE) ") %>%
    remove_random_dates(n = 10) %>%
    duplicate_random_dates(n = 10)
  
  # Ingredients price schedule time series dataset
  ingredients <- master %>%
    mutate(egg_price = round(eggs_price / 12, 2),
           state = case_when(state == "ME" ~ "Maine",
                             state == "NH" ~ " New Hampshire",
                             state == "VT" ~ " Vermont",
                             state == "MA" ~ " Massachusetts",
                             state == "CT" ~ " Connecticut",
                             state == "RI" ~ " Rhode Island",
                             state == "NY" ~ " New York",
                             state == "PA" ~ " Pennsylvania",
                             state == "NJ" ~ " New Jersey",
                             state == "DE" ~ " Delaware",
                             state == "MA" ~ "Maryland",
                             state == "VA" ~ "Vriginia")) %>%
    arrange(state, date) %>%
    filter(date != lag(date)) %>%
    pivot_wider(id_cols = date,
                names_from = state,
                values_from = c(milk_price, eggs_price, sugar_price),
                names_glue = "{state}: price of {.value}") %>%
    pivot_longer(cols = contains(c("milk_price", "eggs_price", "sugar_price")),
                 names_to = "ingredient",
                 values_to = "price") %>%
    mutate(ingredient = str_to_upper(str_replace(str_remove(ingredient, "_price"), "eggs", " one  egg"))) %>%
    select(c(date, ingredient, price)) %>%
    group_by(ingredient) %>%
    filter(price != lag(price)) %>%
    ungroup() %>%
    remove_random_dates(n = 100) %>%
    duplicate_random_dates(n = 100) %>%
    rename("Date Recorded" = date,
           "State & Ingredient " = ingredient,
           "Avg. Price (US$ [NOT ADJUSTED FOR INFLATION])" = price)
  
  # Shipping cost time series dataset
  shipping_costs <- master %>%
    select(date, state, diesel_price, gasoline_price) %>%
    mutate(across(c(diesel_price, gasoline_price),
                  ~str_replace_all(paste("$", as.character(.x), " / gallon", sep = ""),
                                   c("(\\d.\\d) /" = "\\10 /", "(\\$\\d) /" = "\\1.00")))) %>%
    remove_random_dates(n = 100) %>%
    duplicate_random_dates(n = 100) %>%
    rename("For trucks using diesel" = diesel_price,
           "For trucks using gasoline" = gasoline_price)
  
  # Flavor preference survey dataset
  
### Outputting results
  # Temperature
  write.xlsx(temperature, FOLDER("Daily temperature data.xlsx"))
  
  # Ingredients
  write.xlsx(ingredients, FOLDER("Ingredients price schedule time series.xlsx"))
  
  # Shipping costs
  write.xlsx(shipping_costs, FOLDER("Shipping costs time series.xlsx"))
  
toc()