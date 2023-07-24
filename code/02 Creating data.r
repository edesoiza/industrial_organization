### CREATING DATA ###

tic()

### Initializing dataframe
  # Creating dates
  start_date <- as.Date("1997-06-02")
  end_date <- as.Date("2012-11-23")
  conduct_start <- as.Date("2004-08-01")
  conduct_end <- as.Date("2011-04-01")
  
  master <- data.frame("date" = seq(start_date, end_date, by = "days"))
  
  # Removing/duplicating random dates
  master <- master %>%
    remove_random_dates(n = 500) %>%
    duplicate_random_dates(n = 500)

### Creating states
  # Create state row
  master <- master %>%
    mutate(ME = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           NH = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           VT = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           MA = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           RI = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           CT = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           NY = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           PA = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           NJ = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           DE = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           MD = seq(1, 100, (100 - 1) / (nrow(master) - 1)),
           VA = seq(1, 100, (100 - 1) / (nrow(master) - 1))
    ) %>%
    pivot_longer(cols = c("ME", "NH", "VT", "MA", "RI", "CT",
                          "NY", "PA", "NJ", "DE", "MD", "VA"),
                 names_to = "state",
                 values_to = "random_value")
  
  # Drop rows prior to a state's entry period for dominant
  master <- master %>%
    mutate(entry_date = case_when(state == "ME" ~ as.Date("2003-06-02"),
                                  state == "NH" ~ as.Date("2003-06-06"),
                                  state == "VT" ~ as.Date("2005-05-03"),
                                  state == "MA" ~ as.Date("1997-06-02"),
                                  state == "RI" ~ as.Date("1999-07-05"),
                                  state == "CT" ~ as.Date("1999-05-01"),
                                  state == "NY" ~ as.Date("2000-04-01"),
                                  state == "PA" ~ as.Date("2000-04-01"),
                                  state == "NJ" ~ as.Date("2000-04-01"),
                                  state == "DE" ~ as.Date("2000-05-01"),
                                  state == "MD" ~ as.Date("2000-05-01"),
                                  state == "VA" ~ as.Date("2000-05-01"),
                                  TRUE ~ as.Date("1997-06-02"))) %>%
    filter(date >= entry_date) %>%
    select(-c("entry_date"))
  
### Creating seasonal temperature changes
  # Identifying seasons and regions
  spring <- c(3, 4, 5)
  summer <- c(6, 7, 8)
  autumn <- c(9, 10, 11)
  winter <- c(12, 1, 2)
  
  northern_northeast <- c("ME", "NH", "VT", "MA")
  southern_northeast <- c("RI", "CT", "NY", "PA", "NJ")
  mid_atlantic <- c("DE", "MD", "VA")
  
  # Generating temperatures from season and region
  master <- master %>%
    mutate(season = case_when(month(date) %in% spring ~ "spring",
                              month(date) %in% summer ~ "summer",
                              month(date) %in% autumn ~ "autumn",
                              month(date) %in% winter ~ "winter"),
           region = case_when(state %in% northern_northeast ~ "NN",
                              state %in% southern_northeast ~ "SN",
                              state %in% mid_atlantic ~ "MA")) %>%
    group_by(region, season) %>%
    mutate(id = cur_group_id()) %>%
    mutate(temp = case_when(id == 1 ~ rnorm(n(), 72.4, 5),
                            id == 2 ~ rnorm(n(), 67.2, 7),
                            id == 3 ~ rnorm(n(), 81.3, 2),
                            id == 4 ~ rnorm(n(), 37.5, 2),
                            id == 5 ~ rnorm(n(), 54.8, 7),
                            id == 6 ~ rnorm(n(), 48.6, 5),
                            id == 7 ~ rnorm(n(), 68.0, 5),
                            id == 8 ~ rnorm(n(), 29.8, 8),
                            id == 9 ~ rnorm(n(), 54.5, 7),
                            id == 10 ~ rnorm(n(), 51.1, 7),
                            id == 11 ~ rnorm(n(), 74.6, 5),
                            id == 12 ~ rnorm(n(), 30.1, 8))) %>%
    ungroup() %>%
    select(-c("season", "region", "id"))
  
  rm(spring, summer, autumn, winter,
     northern_northeast, southern_northeast, mid_atlantic)
  
  # Graphing to view results
  master %>%
    ggplot(aes(date, temp), group = state) +
    geom_line(aes(col = state)) +
    geom_vline(xintercept = conduct_start, alpha = 0.63, size = 1, linetype = "dashed") +
    geom_vline(xintercept = conduct_end, alpha = 0.63, size = 1, linetype = "dashed") +
    ggtitle("Seasonal Fluctuations in Temperature") +
    xlab("") +
    ylab("Price (USD per unit)") +
    theme_pubr() +
    theme(text = element_text(family = "serif"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")

### Creating input price data
  # Milk
  milk_data <- read_excel("data/milk prices.xlsx")
  
  milk_data <- milk_data %>%
    rename(date = Year,
           milk_price = "Average Milk Price by Year*",
           milk_cpi = "Average Annual CPI for Milk**",
           milk_price_real = "Milk Prices Adjusted for Inflation In 2022 Dollars") %>%
    select(-milk_price_real) %>%
    mutate(date = as.Date(paste(date, "01", "01", sep = "-"))) %>%
    rdts()
  
  master <- master %>%
    left_join(milk_data, by = "date")
  
  rm(milk_data)
  
  # Sugar
  master <- master %>%
    group_by(state, year(date), week(date)) %>%
    mutate(sugar_price = round(rnorm(1, 0.41, 0.09), 2)) %>%
    mutate(sugar_price = sugar_price + (year(date) - mean(year(date))) / 0.25) %>%
    ungroup() %>%
    select(-c("year(date)", "week(date)"))
  
  # Eggs
  master <- master %>%
    group_by(state, year(date), month(date)) %>%
    mutate(egg_price = round(rnorm(1, 4.68, 0.98), 2)) %>%
    mutate(egg_price = egg_price + (year(date) - mean(year(date))) / 1.25) %>%
    ungroup() %>%
    select(-c("year(date)", "month(date)"))
  
  # Changing prices during Great Recession
  start_of_recession <- as.Date("2008-11-01")
  end_of_recession <- as.Date("2010-05-31")
  recession_period <- seq(start_of_recession, end_of_recession, by = "days")
  mean_date_num <- mean(as.numeric(start_of_recession), as.numeric(end_of_recession))
  
  new_vals_df <- master %>%
    filter(date %in% recession_period) %>%
    mutate(egg_price = egg_price * (1.102 * as.numeric(date) ^ 2 - 3 * as.numeric(date)) / mean_date_num ^ 2,
           sugar_price = sugar_price * (2.079 * as.numeric(date) ^ 2 - 7 * as.numeric(date)) / mean_date_num ^ 2)
  
  master <- master %>%
    filter(! date %in% recession_period)
  
  master <- rbind(master, new_vals_df)
  
  rm(start_of_recession, end_of_recession, recession_period, mean_date_num, new_vals_df)
  
  # Graphing results to check ingredient prices over time
  master %>%
    pivot_longer(cols = ends_with("_price"),
                 names_to = "ingredient",
                 values_to = "price") %>%
    mutate(ingredient = str_to_upper(str_remove(ingredient, "_price"))) %>%
    ggplot(aes(date, price), group = c(ingredient, state)) +
    geom_line(aes(col = ingredient)) +
    geom_vline(xintercept = conduct_start, alpha = 0.63, size = 1, linetype = "dashed") +
    geom_vline(xintercept = conduct_end, alpha = 0.63, size = 1, linetype = "dashed") +
    ggtitle("Ingredients' Costs") +
    xlab("") +
    ylab("Price (USD per unit)") +
    labs(color = "") +
    theme_pubr() +
    theme(text = element_text(family = "serif"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(family = "serif"))
  
### Creating shipping costs data
  # Creating oil price data
  gasoline_data <- read_csv("data/annual-regular-grade-gasoline-prices.csv")
  
  gasoline_data <- gasoline_data %>%
    mutate(date = as.Date(paste(year, "01", "01", sep = "-"))) %>%
    select(-c("year")) %>%
    rdts(TRUE)
  
  master <- master %>%
    left_join(gasoline_data, by = "date") %>%
    group_by(state) %>%
    mutate(diesel_price = round(rnorm(n(), real_price, 0.04), 2),
           gasoline_price = round(rnorm(n(), nominal_price, 0.04), 2)) %>%
    ungroup() %>%
    select(-c(real_price, nominal_price))
  
  rm(gasoline_data)
    
  # Graphing results to check shipping costs over time
  master %>%
    ggplot(aes(date, diesel_price), group = state) +
    geom_line(aes(col = state)) +
    geom_vline(xintercept = conduct_start, alpha = 0.63, size = 1, linetype = "dashed") +
    geom_vline(xintercept = conduct_end, alpha = 0.63, size = 1, linetype = "dashed") +
    ggtitle("Diesel Costs") +
    xlab("") +
    ylab("Price (USD per gallon)") +
    theme_pubr() +
    theme(text = element_text(family = "serif"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  master %>%
    ggplot(aes(date, gasoline_price), group = state) +
    geom_line(aes(col = state)) +
    geom_vline(xintercept = conduct_start, alpha = 0.63, size = 1, linetype = "dashed") +
    geom_vline(xintercept = conduct_end, alpha = 0.63, size = 1, linetype = "dashed") +
    ggtitle("Gasoline Costs") +
    xlab("") +
    ylab("Price (USD per gallon)") +
    theme_pubr() +
    theme(text = element_text(family = "serif"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
### Creating flavor preference survey data
  # Creating random
  
  # Vegan/Allergen-free option
  
### Creating "complement" and "substitute" data
  # Complement (waffle cones)
  
  # Substitute (frozen yogurt)
  
### Creating advertising spending data
  
### Creating price data
  # Dominant
  
  # Defendants
  
### Saving out master dataset ###
  write.csv(master, FOLDER("data/Master data.csv"))
  write_parquet(master, FOLDER("data/Master data.parquet"))
  
toc()