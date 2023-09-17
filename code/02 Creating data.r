### CREATING DATA ###

tic()

#### Initializing dataframe ####
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
  
  # Identifying conduct period
  master <- master %>%
    mutate(conduct_period = 1 * (date %in% seq(conduct_start,
                                               conduct_end,
                                               by = "days")))
 
#### Creating states ####
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
  
#### Creating seasonal temperature changes ####
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
    mutate(temp = round(temp, 1)) %>%
    ungroup() %>%
    select(-c("season", "region", "id"))
  
  rm(spring, summer, autumn, winter,
     northern_northeast, southern_northeast, mid_atlantic)
  
#### Creating input price data ####
  # Milk
  milk_data <- read_excel("data/raw_data/milk prices.xlsx")
  
  milk_data <- milk_data %>%
    rename(date = Year,
           milk_price = "Average Milk Price by Year*",
           milk_cpi = "Average Annual CPI for Milk**",
           milk_price_real = "Milk Prices Adjusted for Inflation In 2022 Dollars") %>%
    select(-milk_price_real, -milk_cpi) %>%
    mutate(date = as.Date(paste(date, "01", "01", sep = "-"))) %>%
    rdts()
  
  master <- master %>%
    left_join(milk_data, by = "date") %>%
    mutate(milk_price = abs(round(milk_price, 2))) %>%
    mutate(milk_price = ifelse(milk_price == 0, round(mean(milk_price), 2), milk_price))
  
  rm(milk_data)
  
  # Sugar
  sugar_data <- read.csv("data/raw_data/sugar-prices-historical-chart-data.csv")
  
  sugar_data <- sugar_data %>%
    mutate(date = as.Date(date, "%m/%d/%Y")) %>%
    rename(sugar_price = value) %>%
    rdts()
  
  master <- master %>%
    left_join(sugar_data) %>%
    group_by(state) %>%
    mutate(sugar_price = abs(round(rnorm(n(), sugar_price, 0.04), 2))) %>%
    mutate(sugar_price = ifelse(sugar_price == 0, round(mean(sugar_price), 2), sugar_price)) %>%
    ungroup()
  
  rm(sugar_data)
  
  # Eggs
  eggs_data <- read_excel("data/raw_data/egg prices.xlsx")
  
  eggs_data <- eggs_data %>%
    rename(date = Year,
           eggs_price = "Average Egg Prices by Year*",
           eggs_cpi = "Average Annual CPI for Egg**",
           eggs_price_real = "Egg Prices Adjusted for Inflation in 2022 Dollars") %>%
    select(-eggs_price_real, -eggs_cpi) %>%
    mutate(date = as.Date(paste(date, "01", "01", sep = "-"))) %>%
    rdts(TRUE)
  
  master <- master %>%
    left_join(eggs_data) %>%
    group_by(state) %>%
    mutate(eggs_price = abs(round(rnorm(n(), eggs_price, 0.04), 2))) %>%
    mutate(eggs_price = ifelse(eggs_price == 0, round(mean(eggs_price), 2), eggs_price)) %>%
    ungroup()
  
  rm(eggs_data)
  
#### Creating shipping costs data for dominant's capital cost advantage ####
  # Creating oil price data
  gasoline_data <- read_csv("data/raw_data/annual-regular-grade-gasoline-prices.csv")
  
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
  
#### Creating labor costs data for dominant raising rivals' cost ####
  # Minimum wage panel data
  minimum_wage_data <- read_excel("data/raw_data/historial_minimum_wage_data.xlsx")
  
  names(minimum_wage_data) <- str_squish(str_replace(str_remove_all(names(minimum_wage_data), "\\([a-z]\\)"), "State or other", "state"))
  temp <- data.frame(t(minimum_wage_data))
  names(temp) <- temp[1,]
  temp <- temp[2:nrow(temp), 2:ncol(temp)]
  minimum_wage_data <- temp
  
  rm(temp)
  
  all_years_df <- data.frame("year" = c(1968:2022))
  
  minimum_wage_data <- minimum_wage_data %>%
    mutate(across(everything(), ~as.numeric(str_squish(str_replace_all(., c("\\$" = "",
                                                                            "&[\\s\\w\\d\\$.]*" = "",
                                                                            "\\.\\.\\." = "",
                                                                            "\\(\\w*\\)" = "",
                                                                            "-[\\s\\w\\d\\$.]*" = "",
                                                                            "\\/[\\s\\w\\d\\$.]*" = "",
                                                                            "^(.\\d\\d)" = "0\\1")))))) %>%
    mutate(year = as.numeric(row.names(.)))
    
  minimum_wage_data <- all_years_df %>%
    left_join(minimum_wage_data, by = "year") %>%
    pivot_longer(cols = -year,
                 names_to = "state",
                 values_to = "minimum_wage") %>%
    group_by(state) %>%
    arrange(year) %>%
    fill(minimum_wage, .direction = "down") %>%
    ungroup() %>%
    mutate(state = case_when(state == "Maine" ~ "ME",
                             state == "New Hampshire" ~ "NH",
                             state == "Vermont" ~ "VT",
                             state == "Massachusetts" ~ "MA",
                             state == "Rhode Island" ~ "RI",
                             state == "Connecticut" ~ "CT",
                             state == "New York" ~ "NY",
                             state == "Pennsylvania" ~ "PA",
                             state == "New Jersey" ~ "NJ",
                             state == "Delaware" ~ "DE",
                             state == "Maryland" ~ "MD",
                             state == "Virginia" ~ "VA"))
  
  rm(all_years_df)
  
  master <- master %>%
    mutate(year = year(date)) %>%
    left_join(minimum_wage_data, by = c("year", "state")) %>%
    select(-year)
  
  rm(minimum_wage_data)
    
  # Labor costs data
  union_success_date <- as.Date("2004-08-23")
  
  master <- master %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(dominant_wage = round(minimum_wage + rnorm(1, 1, 0.05) + cur_group_id() / 30, 2),
           defendant_1_wage = round(minimum_wage + rnorm(1, 1.5, 0.30) + cur_group_id() / 30, 2),
           defendant_2_wage = round(minimum_wage + rnorm(1, 1.3, 0.30) + cur_group_id() / 30, 2),
           defendant_3_wage = case_when(minimum_wage <= 10.0 ~ round(minimum_wage + rnorm(1, 5, 0.25), 2),
                                        TRUE ~ round(minimum_wage + rnorm(1, 2, 0.25), 2)),
           defendant_4_wage = round(minimum_wage + rnorm(1, 0.5, 0.05) + cur_group_id() / 32, 2),
           defendant_5_wage = round(minimum_wage + rnorm(1, 1, 0.05) + cur_group_id() / 30, 2),
           defendant_6_wage = round(minimum_wage + rnorm(1, 0.2, 0.01) + cur_group_id() / 60, 2),
           defendant_7_wage = 50) %>%
    ungroup() %>%
    group_by(state, year) %>%
    arrange(year) %>%
    mutate(dominant_wage = case_when(dominant_wage > lead(dominant_wage) ~ lead(dominant_wage),
                            TRUE ~ dominant_wage)) %>%
    ungroup() %>%
    mutate(dominant_wage = case_when(date >= union_success_date ~ dominant_wage + 2.86,
                            TRUE ~ dominant_wage)) %>%
    mutate(across(contains("defendant_"), ~case_when(date >= union_success_date + years(6) + months(3) + days(round(rnorm(1, 6, 7), 0)) ~ round(dominant_wage + rnorm(1, 1, 0.1), 2),
                                                    TRUE ~ .))) %>%
    select(-year) %>%
    arrange(date)
  
  rm(union_success_date)
  
#### Creating flavor preference survey data ####
  # Creating random preference data
  
  # Vegan/Allergen-free option
  
#### Creating "complement" and "substitute" data ####
  temp_na_as_zero <- master %>%
    replace(is.na(.), 0)
  
  # Complement (waffle cones)
  temp_na_as_zero <- temp_na_as_zero %>%
    mutate(waffle_cone_price = round(0.02 + 0.285 * milk_price + 0.054 * sugar_price + 0.611 * eggs_price + 0.030 * gasoline_price ^ 2, 2))
  
  master[, "waffle_cone_price"] <- temp_na_as_zero[, "waffle_cone_price"]
  
  # Substitute (frozen yogurt)
  temp_na_as_zero <- temp_na_as_zero %>%
    mutate(frozen_yogurt_price = round(0.11 + 0.100 * log(temp) + 0.245 * milk_price + 0.254 * 1 / sugar_price + 0.326 * eggs_price + 0.025 * gasoline_price ^ 2, 2))
  
  master[, "frozen_yogurt_price"] <- temp_na_as_zero[, "frozen_yogurt_price"]
  
  rm(temp_na_as_zero)
  
#### Removing defendants in states where they do not operate ####
  defendant_1_states <- c("ME", "NH", "VT", "MA", "RI", "CT")
  defendant_2_states <- c("CT", "RI", "NY", "NJ")
  defendant_3_states <- c("NY", "PA", "NJ", "DE", "MD", "VA")
  defendant_4_states <- c("CT", "NY", "NJ")
  defendant_5_states <- c("MA", "RI", "CT", "NY", "PA", "MD", "VA", "OH")
  defendant_6_states <- c("NY", "NJ", "DE", "MD", "VA", "NC", "SC")
  defendant_7_states <- c("ME", "NH", "VT")
  
  master <- master %>%
    mutate(defendant_1_wage = case_when(state %in% defendant_1_states ~ defendant_1_wage,
                                        TRUE ~ NA),
           defendant_2_wage = case_when(state %in% defendant_2_states ~ defendant_2_wage,
                                        TRUE ~ NA),
           defendant_3_wage = case_when(state %in% defendant_3_states ~ defendant_3_wage,
                                        TRUE ~ NA),
           defendant_4_wage = case_when(state %in% defendant_4_states ~ defendant_4_wage,
                                        TRUE ~ NA),
           defendant_5_wage = case_when(state %in% defendant_5_states ~ defendant_5_wage,
                                        TRUE ~ NA),
           defendant_6_wage = case_when(state %in% defendant_6_states ~ defendant_6_wage,
                                        TRUE ~ NA),
           defendant_7_wage = case_when(state %in% defendant_7_states ~ defendant_7_wage,
                                        TRUE ~ NA))
  
  rm(defendant_1_states, defendant_2_states, defendant_3_states, defendant_4_states, defendant_5_states, defendant_6_states, defendant_7_states)
  
#### Creating price data ####
  master <- master %>%
    select(-random_value)
  
  temp_na_as_zero <- master %>%
    mutate(temp = (60 - temp) / 10) %>%
    replace(is.na(.), 1)
  
  all_dates <- seq(start_date, end_date, by = "days")
  
  # Dominant
  temp_na_as_zero <- temp_na_as_zero %>%
    mutate(dominant_price = round(0.005 * log(temp) +
                                        0.085 * (log(temp)) ^ 2 +
                                        0.190 * log(milk_price) +
                                        0.090 * log(sugar_price) +
                                        0.130 * log(eggs_price) +
                                        0.330 * log(gasoline_price) +
                                        0.030 * log(dominant_wage) +
                                        0.030 * 3.00 +
                                        0.110 * conduct_period +
                                        rnorm(1, 0.01, 0.01), 2))
  
  for (day in 2:length(all_dates)) {
    temp_na_as_zero <- temp_na_as_zero %>%
      group_by(state) %>%
      arrange(date) %>%
      mutate(log_dominant_price = case_when(date == all_dates[day] ~ round(0.005 * log(temp) +
                                                                             0.085 * (log(temp)) ^ 2 +
                                                                             0.190 * log(milk_price) +
                                                                             0.090 * log(sugar_price) +
                                                                             0.130 * log(eggs_price) +
                                                                             0.330 * log(gasoline_price) +
                                                                             0.030 * log(dominant_wage) +
                                                                             0.030 * lag(log_dominant_price) +
                                                                             0.110 * conduct_period +
                                                                             rnorm(1, 0.01, 0.01), 2),
                                            TRUE ~ log_dominant_price)) %>%
      mutate(dominant_price = exp(log_dominant_price)) %>%
      ungroup() %>%
      arrange(date)
    
  }
  
  master[, "dominant_price"] <- temp_na_as_zero[, "dominant_price"]
  
  # Defendants
  for (defendant in 1:7) {
    temp_na_as_zero <- temp_na_as_zero %>%
      mutate("log_defendant_{defendant}_price" := round(0.005 * log(temp) +
                                          0.085 * (log(temp)) ^ 2 +
                                          0.190 * log(milk_price) +
                                          0.090 * log(sugar_price) +
                                          0.130 * log(eggs_price) +
                                          0.330 * log(diesel_price) +
                                          0.030 * log(.data[[paste("defendant_", defendant, "_wage", sep = "")]]) +
                                          0.030 * 4.00 +
                                          0.110 * conduct_period +
                                          rnorm(1, 0.01, 0.01), 2))
    
    for (day in 2:length(all_dates)) {
      temp_na_as_zero <- temp_na_as_zero %>%
        group_by(state) %>%
        arrange(date) %>%
        mutate("log_defendant_{defendant}_price" := case_when(date == all_dates[day] ~ round(0.005 * log(temp) +
                                                                               0.085 * (log(temp)) ^ 2 +
                                                                               0.190 * log(milk_price) +
                                                                               0.090 * log(sugar_price) +
                                                                               0.130 * log(eggs_price) +
                                                                               0.330 * log(diesel_price) +
                                                                               0.030 * log(.data[[paste("defendant_", defendant, "_wage", sep = "")]]) +
                                                                               0.030 * lag(.data[[paste("log_defendant_", defendant, "_price", sep = "")]]) +
                                                                               0.110 * conduct_period +
                                                                               rnorm(1, 0.01, 0.01), 2),
                                              TRUE ~ .data[[paste("log_defendant_", defendant, "_price", sep = "")]])) %>%
        mutate("defendant_{defendant}_price" := exp(.data[[paste("log_defendant_", defendant, "_price", sep = "")]])) %>%
        ungroup() %>%
        arrange(date)
      
    }
    
    master[, paste("defendant_", defendant, "_price", sep = "")] <- temp_na_as_zero[, paste("defendant_", defendant, "_price", sep = "")]
    
  }
  
  rm(temp_na_as_zero, all_dates, day, defendant)

#### Saving out master dataset ####
  write.csv(master, FOLDER("data/Master data.csv"))
  write_parquet(master, FOLDER("data/Master data.parquet"))

#### EOF ####
  
toc()