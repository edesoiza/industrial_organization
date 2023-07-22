### SETTING UP FUNCTIONS ***

### Creating a list of random dates within a specified range
  rdate <- function(n = 1,
                    start_date = as.Date("1960-01-01"),
                    end_date = as.Date("2060-12-31")) {
    # Type and feasibility checks
    stopifnot(is.Date(start_date))
    stopifnot(is.Date(end_date))
    stopifnot(n <= nrow(seq(start_date, end_date, by = "days")))
    
    # Obtaining date range
    date_range <- seq(start_date, end_date, by = "days")
    
    # Obtaining date components
    .years <- as.character(abs(round(rnorm(n,
                                           mean(unique(year(date_range))),
                                           (max(unique(year(date_range))) -
                                              min(unique(year(date_range)))) /
                                             2.5),
                                     0)))
    .months <- as.character(abs(round(rnorm(n,
                                            mean(unique(month(date_range))),
                                            (max(unique(month(date_range))) -
                                               min(unique(month(date_range)))) /
                                              6),
                                      0)))
    .days <- as.character(abs(round(rnorm(n,
                                          mean(unique(day(date_range))),
                                          (max(unique(day(date_range))) -
                                             min(unique(day(date_range)))) /
                                            4.13),
                                    0)))
    
    # Obtaining date values
    .dates <- c()
    for (i in 1:n) {
      if (as.numeric(.months[i]) == 0) {
        .months[i] = "1"
        
      } else if (as.numeric(.months[i]) > 12) {
        .months[i] = "12"
        
      }
      
      if (as.numeric(.days[i]) == 0) {
        .days[i] = "1"
        
      } else if (as.numeric(.days[i]) > 28) {
        if (.months[i] == "2") {
          .days[i] <- "28"
          
        } else if (.months[i] %in% c("4", "6", "9", "11")) {
          .days[i] <- "30"
          
        } else {
          .days[i] <- "31"
          
        }
        
      }
      
      if (as.numeric(.months[i]) < 10) {
        .months[i] <- paste("0", .months[i], sep = "")
        
      }
      
      if (as.numeric(.days[i]) < 10) {
        .days[i] <- paste("0", .days[i], sep = "")
        
      }
      
      .dates <- append(.dates, as.Date(paste(.years[i],
                                                         .months[i],
                                                         .days[i],
                                                         sep = "-")))
      
    }
    
    return(.dates)
    
  }

### Removing random dates from a dataframe
  remove_random_dates <- function(df, n = 1) {
    # Type and feasibility checks
    stopifnot(is.data.frame(df))
    stopifnot(n <= nrow(df))
    
    # Obtaining date values to remove
    remove_dates <- rdate(n = n, min(df$date), max(df$date))
    
    # Removing values
    df <- df %>%
      filter(! date %in% remove_dates)
    
    return(df)
    
  }
  
### Duplicating random rows in a dataframe
  duplicate_random_dates <- function(df, n = 1) {
    # Type and feasibility checks
    stopifnot(is.data.frame(df))
    stopifnot(n <= nrow(df))
    
    # Obtaining date values to duplicate
    duplicate_dates <- rdate(n = n, min(df$date), max(df$date))
    
    # Duplicating dates
    duplicate_dates_df <- df %>%
      filter(date %in% duplicate_dates)
    
    df <- rbind(df, duplicate_dates_df)
    
    return(df)
    
  }
  
### Creating smooth daily-level time series with random fluctuations
  rdts <- function(df, heteroskedastic = FALSE, homoskedastic_sd = 0.04) {
    # Type and feasibility checks
    stopifnot(is.data.frame(df))
    
    # Identifying non-date variables to be smoothed in time series
    variables <- df %>% select(-date) %>% names()
    
    # Creating dataset with values lagged to allow smooth fluctuation
    df_additions <- df %>%
      mutate(additional_dates = date - ddays(1)) %>%
      select(-date) %>%
      rename(date = additional_dates)
    
    # Initializing dataset with all days
    df_full <- data.frame("date" = seq(min(df_additions$date),
                                       max(df$date),
                                       by = "days"))
    
    # Combining original dataset with lagged dataset, missing days remaining NA
    df_full <- df_full %>%
      left_join(df, by = "date") %>%
      full_join(df_additions, by = "date")
    
    rm(df, df_additions)
    
    for (variable in variables) {
      df_full <- df_full %>%
        mutate("{variable}" := pmax(.data[[paste(variable, ".x", sep = "")]],
                                    .data[[paste(variable, ".y", sep = "")]],
                                    na.rm = TRUE))

    }
    
    df_full <- df_full %>%
      select(date, !contains(c(".x", ".y")))
    
    # Filling in NAs with smooth time series
    for (variable in variables) {
      df_full <- df_full %>%
        group_by(date) %>%
        mutate(id = cur_group_id()) %>%
        ungroup() %>%
        filter(!is.na(year(date))) %>%
        group_by(year(date)) %>%
        mutate("{variable}_min" := min(.data[[variable]], na.rm = TRUE),
               "{variable}_max" := max(.data[[variable]], na.rm = TRUE),
               "{variable}_min_id" := case_when(.data[[variable]] ==
                                                  .data[[paste(variable,
                                                               "_min",
                                                               sep = "")]] ~
                                                  id,
                                                TRUE ~ NA),
               "{variable}_max_id" := case_when(.data[[variable]] ==
                                                  .data[[paste(variable,
                                                               "_max",
                                                               sep = "")]] ~
                                                  id,
                                                TRUE ~ NA)) %>%
        fill(.data[[paste(variable, "_min_id", sep = "")]],
             .direction = "updown") %>%
        fill(.data[[paste(variable, "_max_id", sep = "")]],
             .direction = "updown") %>%
        ungroup() %>%
        filter(.data[[paste(variable, "_min_id", sep = "")]] != .data[[paste(variable, "_max_id", sep = "")]]) %>%
        group_by(year(date)) %>%
        mutate("{variable}" := case_when(.data[[paste(variable, "_min_id", sep = "")]] <
                                           .data[[paste(variable, "_max_id", sep = "")]] ~
                                           seq(min(.data[[variable]], na.rm = TRUE),
                                               max(.data[[variable]], na.rm = TRUE),
                                               by = (max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE)) / (n() - 1)),
                                         .data[[paste(variable, "_min_id", sep = "")]] >
                                           .data[[paste(variable, "_max_id", sep = "")]] ~
                                           seq(max(.data[[variable]], na.rm = TRUE),
                                               min(.data[[variable]], na.rm = TRUE),
                                               by = -1 * (max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE)) / (n() - 1)),
                                         TRUE ~ .data[[variable]])) %>%
        ungroup() %>%
        group_by(week(date)) %>%
        mutate(sd = case_when(heteroskedastic ~ sum((.data[[variable]] - mean(.data[[variable]])) ^ 2) / (20 * (n() + 2)),
                              TRUE ~ homoskedastic_sd)) %>%
        ungroup() %>%
        mutate("{variable}" := rnorm(n(), .data[[variable]], sd))
      
    }
    
    df_full <- df_full %>%
      select(!contains(c("_min", "_max", "id", "sd", "year(date)", "week(date)")))
    
    return(df_full)
    
  }