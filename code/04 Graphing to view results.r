### GRAPHING TO VIEW RESULTS ###

tic()

#### Loading data ####
  master <- read_parquet(FOLDER("data/Master data.parquet"))
  
#### Temperature ####
  master %>%
    ggplot(aes(date, temp), group = state) +
    geom_line(aes(col = state)) +
    ggtitle("Seasonal Fluctuations in Temperature") +
    xlab("") +
    ylab("Temperature (degrees Farenheit)") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(n.breaks = 50,
                       labels = c(-10, rep(c(""), 4),
                                  0, rep(c(""), 4),
                                  10, rep(c(""), 4),
                                  20, rep(c(""), 4),
                                  30, rep(c(""), 4),
                                  40, rep(c(""), 4),
                                  50, rep(c(""), 4),
                                  60, rep(c(""), 4),
                                  70, rep(c(""), 4),
                                  80, rep(c(""), 4),
                                  90),
                       limits = c(-10, 90),
                       expand = c(0, 0)) +
    theme_dc() +
    # Conduct period
    geom_ribbon(aes(ymin = -10,
                    ymax = conduct_period * 90,
                    fill = (conduct_period == 1),
                    alpha = 0.05)) +
    scale_fill_manual(values = c("#2A2A2F", "#A7A7A7"),
                      name = "fill",
                      guide = "none")
  
#### Ingredients cost ####
  master %>%
    pivot_longer(cols = c(milk_price, eggs_price, sugar_price),
                 names_to = "ingredient",
                 values_to = "price") %>%
    mutate(ingredient = str_to_upper(str_remove(ingredient, "_price"))) %>%
    ggplot(aes(date, price), group = c(ingredient, state)) +
    geom_line(aes(col = ingredient)) +
    geom_rangeframe(sides = "bl") +
    ggtitle("Ingredients' Costs") +
    xlab("") +
    ylab("Price (USD per unit)") +
    labs(color = "Ingredient") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0.5, 5.0, 0.5),
                       limits = c(0, 5.0),
                       labels = currency(c(seq(0.5, 5.0, 0.5))),
                       expand = c(0, 0)) +
    theme_dc("legend") +
    # Conduct period
    geom_ribbon(aes(ymin = 0,
                    ymax = conduct_period * 5,
                    fill = (conduct_period == 1)),
                alpha = 0.05) +
    scale_fill_manual(values = c("#000000", "#A7A7A7"),
                      name = "fill",
                      guide = "none")
  
#### Oil prices ####
  # Diesel prices
  master %>%
    ggplot(aes(date, diesel_price), group = state) +
    geom_line(aes(col = state)) +
    ggtitle("Diesel Costs") +
    xlab("") +
    ylab("Price (USD per gallon)") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0.5, 5.0, 0.5),
                       limits = c(0, 5.0),
                       labels = currency(c(seq(0.5, 5.0, 0.5))),
                       expand = c(0, 0)) +
    theme_dc() +
    # Conduct period
    geom_ribbon(aes(ymin = 0,
                    ymax = conduct_period * 5,
                    fill = (conduct_period == 1),
                    alpha = 0.05)) +
    scale_fill_manual(values = c("#000000", "#A7A7A7"),
                      name = "fill")
  
  # Gasoline prices
  master %>%
    ggplot(aes(x = date), group = state) +
    geom_line(aes(y = gasoline_price, col = state)) +
    ggtitle("Gasoline Costs") +
    xlab("") +
    ylab("Price (USD per gallon)") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0.5, 5.0, 0.5),
                       limits = c(0, 5.0),
                       labels = currency(c(seq(0.5, 5.0, 0.5))),
                       expand = c(0, 0)) +
    theme_dc() +
    # Conduct period
    geom_ribbon(aes(ymin = 0,
                    ymax = conduct_period * 5,
                    fill = (conduct_period == 1),
                    alpha = 0.05)) +
    scale_fill_manual(values = c("#000000", "#A7A7A7"),
                      name = "fill")

#### Wages ####
  # By firm in each state
  states <- c("ME", "NH", "VT", "MA", "RI", "CT",
              "NY", "PA", "NJ", "DE", "MD", "VA")
  for (state_code in states) {
    temp <- master %>%
      filter(state == state_code) %>%
      pivot_longer(cols = contains("_wage"),
                   names_to = "firm",
                   values_to = "price") %>%
      ggplot(aes(x = date), group = firm) +
      geom_line(aes(y = price, col = firm)) +
      ggtitle(paste("Wage in ", state_code, sep = "")) +
      xlab("") +
      ylab("Hourly wage (USD)") +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y") +
      scale_y_continuous(breaks = seq(2.0, 20.0, 2.0),
                         limits = c(0, 20.0),
                         labels = currency(c(seq(2.0, 20.0, 2.0))),
                         expand = c(0, 0)) +
      theme_dc() +
      theme(plot.title = element_text(size = 12),
            axis.text.x.bottom = element_text(size = 7),
            axis.text.y.left = element_text(size = 7),
            axis.title.x.bottom = element_text(size = 9),
            axis.title.y.left = element_text(size = 9)) +
      # Conduct period
      geom_ribbon(aes(ymin = 0,
                      ymax = conduct_period * 20,
                      fill = (conduct_period == 1),
                      alpha = 0.05)) +
      scale_fill_manual(values = c("#000000", "#A7A7A7"),
                        name = "fill")
    
    assign(paste("plot_", state_code, sep = ""), temp)
    
    rm(temp)
    
  }
  
  ggarrange(plot_ME, plot_NH, plot_VT, plot_MA, plot_RI, plot_CT,
            plot_NY, plot_PA, plot_NJ, plot_DE, plot_MD, plot_VA)
  
  rm(states, state_code, plot_ME, plot_NH, plot_VT, plot_MA, plot_RI, plot_CT,
     plot_NY, plot_PA, plot_NJ, plot_DE, plot_MD, plot_VA)
  
  # Minimum wage in each state
  master %>%
    ggplot(aes(x = date), group = state) +
    geom_point(aes(y = minimum_wage, col = state)) +
    ggtitle("Minimum Wage by State ") +
    xlab("") +
    ylab("Hourly minimum wage (USD)") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(1.0, 10.0, 1.0),
                       limits = c(0, 10.0),
                       labels = currency(c(seq(1.0, 10.0, 1.0))),
                       expand = c(0, 0)) +
    labs(color = "Ingredient") +
    theme_dc("legend") +
    # Conduct period
    geom_ribbon(aes(ymin = 0,
                    ymax = conduct_period * 20,
                    fill = (conduct_period == 1)),
                alpha = 0.05) +
    scale_fill_manual(values = c("#000000", "#A7A7A7"),
                      name = "fill", 
                      guide = "none")
  
#### Price schedules ####
  states <- c("ME", "NH", "VT", "MA", "RI", "CT",
              "NY", "PA", "NJ", "DE", "MD", "VA")
  for (state_code in states) {
    temp <- master %>%
      filter(state == state_code) %>%
      pivot_longer(cols = contains("_price") &
                     (starts_with("do") | starts_with("de")),
                   names_to = "firm",
                   values_to = "price") %>%
      ggplot(aes(x = date), group = firm) +
      geom_line(aes(y = price, col = firm)) +
      ggtitle(paste("Ice Cream Prices in ", state_code, sep = "")) +
      xlab("") +
      ylab("Price per bucket (USD)") +
      scale_x_date(date_breaks = "1 year",
                   date_labels = "%Y") +
      scale_y_continuous(breaks = seq(0.5, 6.0, 0.5),
                         limits = c(0, 6.0),
                         labels = currency(c(seq(0.5, 6.0, 0.5))),
                         expand = c(0, 0)) +
      theme_dc() +
      theme(plot.title = element_text(size = 12),
            axis.text.x.bottom = element_text(size = 7),
            axis.text.y.left = element_text(size = 7),
            axis.title.x.bottom = element_text(size = 9),
            axis.title.y.left = element_text(size = 9)) +
      # Conduct period
      geom_ribbon(aes(ymin = 0,
                      ymax = conduct_period * 6,
                      fill = (conduct_period == 1),
                      alpha = 0.05)) +
      scale_fill_manual(values = c("#000000", "#A7A7A7"),
                        name = "fill")
    
    assign(paste("plot_", state_code, sep = ""), temp)
    
    rm(temp)
    
  }
  
  ggarrange(plot_ME, plot_NH, plot_VT, plot_MA, plot_RI, plot_CT,
            plot_NY, plot_PA, plot_NJ, plot_DE, plot_MD, plot_VA)
  
  rm(states, state_code, plot_ME, plot_NH, plot_VT, plot_MA, plot_RI, plot_CT,
     plot_NY, plot_PA, plot_NJ, plot_DE, plot_MD, plot_VA)
  
#### Sales data ####
  master %>%
    ggplot(aes(date, dominant_sales), group = state) +
    geom_line(aes(col = state)) +
    ggtitle("Dominant Firm's Sales") +
    xlab("") +
    ylab("Buckets of ice cream sold") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq(5000, 45000, 5000),
                       limits = c(0, 45000),
                       labels = (c(seq(5000, 45000, 5000))),
                       expand = c(0, 0)) +
    theme_dc() +
    # Conduct period
    geom_ribbon(aes(ymin = 0,
                    ymax = conduct_period * 45000,
                    fill = (conduct_period == 1),
                    alpha = 0.05)) +
    scale_fill_manual(values = c("#000000", "#A7A7A7"),
                      name = "fill")
   
#### EOF ####  
    
toc()