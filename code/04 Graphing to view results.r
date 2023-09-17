### GRAPHING TO VIEW RESULTS ###

tic()

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
  
#### Prices ####
  
#### EOF ####  
    
toc()