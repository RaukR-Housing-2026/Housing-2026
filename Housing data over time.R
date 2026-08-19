
library(tidyverse)
library(plotly)
library(scales)

## Median plot

# % change in median price since 2000
price_index <- brf_lan_tidy |>
  filter(!is.na(median_price)) |>
  group_by(region) |>
  arrange(year) |>
  mutate(
    price_2000 = median_price[year == 2000][1],
    pct_change = 100 * (median_price / price_2000 - 1)) |>
  ungroup()

# Overall trend across lan
sweden_trend <- price_index |>
  group_by(year) |>
  summarise(
    pct_change = median(pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p1 <- ggplot() +
  geom_line(data = price_index,
    aes(
      x = year,
      y = pct_change,
      group = region,
      text = paste0(
        "<b>", region, "</b>",
        "<br>Year: ", year,
        "<br>Median price: ", comma(median_price, big.mark = " "), " kr",
        "<br>Homes sold: ", comma(n_sales, big.mark = " "),
        "<br>Change since 2000: ", round(pct_change, 1), "%"
      )
    ),
    color = "#5652A4",
    alpha = 0.45,
    linewidth = 0.8
  ) +
  geom_line(data = sweden_trend,
    aes(
      x = year,
      y = pct_change,
      group = 1,
      text = paste0(
        "<b>Median across län</b>",
        "<br>Year: ", year,
        "<br>Change: ", round(pct_change, 1), "%"
      )
    ),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_vline(
    xintercept = 2000,
    linetype = "dashed",
    color = "grey60"
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), "%")
  ) +
  labs(
    title = "Housing Price Development Across Swedish Lan",
    subtitle = "Median sale price, 2000 = 0%",
    x = NULL,
    y = "% Change in median price"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 4. Make interactive
ggplotly(p1, tooltip = "text") |>
  layout(hovermode = "closest",
    xaxis = list(rangeslider = list(visible = TRUE)
    )
  )

## Mean plot 

# % change in mean price since 2000
mean_price_index <- brf_lan_tidy |>
  filter(!is.na(mean_price)) |>
  group_by(region) |>
  arrange(year) |>
  mutate(
    mean_price_2000 = mean_price[year == 2000][1],
    mean_pct_change = 100 * (mean_price / mean_price_2000 - 1)) |>
  ungroup()

# Overall trend across län
mean_sweden_trend <- mean_price_index |>
  group_by(year) |>
  summarise(
    mean_pct_change = mean(mean_pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p2 <- ggplot() +
  geom_line(data = mean_price_index,
    aes(
      x = year,
      y = mean_pct_change,
      group = region,
      text = paste0(
        "<b>", region, "</b>",
        "<br>Year: ", year,
        "<br>Mean price: ",
        comma(mean_price, big.mark = " "), " kr",
        "<br>Homes sold: ",
        comma(n_sales, big.mark = " "),
        "<br>Change since 2000: ",
        round(mean_pct_change, 1), "%"
      )
    ),
    color = "#5652A4",
    alpha = 0.45,
    linewidth = 0.8
  ) +
  geom_line(
    data = mean_sweden_trend, 
    aes(
      x = year,
      y = mean_pct_change,
      group = 1,
      text = paste0(
        "<b>Mean across län</b>",
        "<br>Year: ", year,
        "<br>Change: ", round(mean_pct_change, 1), "%"
      )
    ),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60" ) +
  geom_vline(
    xintercept = 2000,
    linetype = "dashed",
    color = "grey60"
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), "%")
  ) +
  labs(
    title = "Housing Price Development Across Swedish Lan",
    subtitle = "Mean sale price, 2000 = 0%",
    x = NULL,
    y = "% Change in mean price"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 4. Make interactive
ggplotly(p2, tooltip = "text") |>
  layout(hovermode = "closest",
    xaxis = list(
      rangeslider = list(visible = TRUE)
    )
  )


## Houses sold

# % change in mean price since 2000
sales_index <- brf_lan_tidy |>
  filter(!is.na(n_sales)) |>
  group_by(region) |>
  arrange(year) |>
  mutate(
    sales_2000 = n_sales [year == 2000][1],
    sales_pct_change = 100 * (n_sales / sales_2000 - 1)) |>
  ungroup()

# Overall trend across län
sales_sweden_trend <- sales_index |>
  group_by(year) |>
  summarise(
    sales_pct_change = mean(sales_pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p3 <- ggplot() +
  geom_line(data = sales_index,
    aes(
      x = year,
      y = sales_pct_change,
      group = region,
      text = paste0(
        "<b>", region, "</b>",
        "<br>Year: ", year,
        "<br>Houses sold: ",
        comma(n_sales, big.mark = " "),
        "<br>Change since 2000: ",
        round(sales_pct_change, 1), "%"
      )
    ),
    color = "#5652A4",
    alpha = 0.45,
    linewidth = 0.8
  ) +
  geom_line(
    data = sales_sweden_trend, 
    aes(
      x = year,
      y = sales_pct_change,
      group = 1,
      text = paste0(
        "<b>Homes sold across län</b>",
        "<br>Year: ", year,
        "<br>Change: ", round(sales_pct_change, 1), "%"
      )
    ),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60" ) +
  geom_vline(
    xintercept = 2000,
    linetype = "dashed",
    color = "grey60"
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), "%")
  ) +
  labs(
    title = "Housing Price Development Across Swedish Lan",
    subtitle = "Homes sold, 2000 = 0%",
    x = NULL,
    y = "% Change in homes sold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 4. Make interactive
ggplotly(p3, tooltip = "text") |>
  layout(hovermode = "closest",
    xaxis = list(
      rangeslider = list(visible = TRUE)
    )
  )

###Median salary#########################

# Total earned income
salary_earned <- salary_lan_tidy |>
  filter(
    income == "total_earned_income",
    sex == "total"
  )

# % change in median price since 2000
median_salary_index <- salary_earned |>
  filter(!is.na(median_salary)) |>
  group_by(region) |>
  arrange(year) |>
  mutate(
    median_salary_2011 = median_salary[year == 2011][1],
    median_s_pct_change = 100 * (median_salary / median_salary_2011 - 1)) |>
  ungroup()

# Overall trend across lan
median_salary_trend <- median_salary_index |>
  group_by(year) |>
  summarise(
    median_s_pct_change = median(median_s_pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p4 <- ggplot() +
  geom_line(data = median_salary_index,
    aes(
      x = year,
      y = median_s_pct_change,
      group = region,
      text = paste0(
        "<b>", region, "</b>",
        "<br>Year: ", year,
        "<br>Median earned income: ", comma(median_salary, big.mark = " "), " kr",
        "<br>Working population: ", comma(n_persons, big.mark = " "),
        "<br>Change since 2011: ", round(median_s_pct_change, 1), "%"
      )
    ),
    color = "#5652A4",
    alpha = 0.45,
    linewidth = 0.8
  ) +
  geom_line(data = median_salary_trend,
    aes(
      x = year,
      y = median_s_pct_change,
      group = 1,
      text = paste0(
        "<b>Median across län</b>",
        "<br>Year: ", year,
        "<br>Change: ", round(median_s_pct_change, 1), "%"
      )
    ),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_vline(
    xintercept = 2011,
    linetype = "dashed",
    color = "grey60"
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), "%")
  ) +
  scale_x_continuous(
  limits = c(2010, 2025),
  breaks = seq(2010, 2025, by = 5)
) +
  labs(
    title = "Earned Income Increase Across Swedish Län",
    subtitle = "Median earned income, 2011 = 0%",
    x = NULL,
    y = "% Median earned income increase"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 4. Make interactive
ggplotly(p4, tooltip = "text") |>
  layout(hovermode = "closest",
    xaxis = list(rangeslider = list(visible = TRUE)
    )
  )


###Mean salary#########################

# Total earned income
mean_salary_earned <- salary_lan_tidy |>
  filter(
    income == "total_earned_income",
    sex == "total"
  )

# % change in median price since 2000
mean_salary_index <- mean_salary_earned |>
  filter(!is.na(mean_salary)) |>
  group_by(region) |>
  arrange(year) |>
  mutate(
    mean_salary_2011 = mean_salary[year == 2011][1],
    mean_s_pct_change = 100 * (mean_salary / mean_salary_2011 - 1)) |>
  ungroup()

# Overall trend across lan
mean_salary_trend <- mean_salary_index |>
  group_by(year) |>
  summarise(
    mean_s_pct_change = mean(mean_s_pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p5 <- ggplot() +
  geom_line(data = mean_salary_index,
    aes(
      x = year,
      y = mean_s_pct_change,
      group = region,
      text = paste0(
        "<b>", region, "</b>",
        "<br>Year: ", year,
        "<br>Median earned income: ", comma(mean_salary, big.mark = " "), " kr",
        "<br>Working population: ", comma(n_persons, big.mark = " "),
        "<br>Change since 2011: ", round(mean_s_pct_change, 1), "%"
      )
    ),
    color = "#5652A4",
    alpha = 0.45,
    linewidth = 0.8
  ) +
  geom_line(data = mean_salary_trend,
    aes(
      x = year,
      y = mean_s_pct_change,
      group = 1,
      text = paste0(
        "<b>Mean across län</b>",
        "<br>Year: ", year,
        "<br>Change: ", round(mean_s_pct_change, 1), "%"
      )
    ),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_vline(
    xintercept = 2011,
    linetype = "dashed",
    color = "grey60"
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), "%")
  ) +
  scale_x_continuous(
  limits = c(2010, 2025),
  breaks = seq(2010, 2025, by = 5)
) +
  labs(
    title = "Earned Income Increase Across Swedish Län",
    subtitle = "Mean earned income, 2011 = 0%",
    x = NULL,
    y = "% Mean earned income increase"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 4. Make interactive
ggplotly(p5, tooltip = "text") |>
  layout(hovermode = "closest",
    xaxis = list(rangeslider = list(visible = TRUE)
    )
  )


## Houses sold from 2011

# % change in mean price since 2011
sales11_index <- brf_lan_tidy |>
  filter(!is.na(n_sales), year >= 2011) |>
  filter(!is.na(n_sales)) |>
  group_by(region) |>
  arrange(year) |>
  mutate(
    sales_2011 = n_sales [year == 2011][1],
    sales11_pct_change = 100 * (n_sales / sales_2011 - 1)) |>
  ungroup()


# Overall trend across län
sales11_sweden_trend <- sales11_index |>
  group_by(year) |>
  summarise(
    sales11_pct_change = mean(sales11_pct_change, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
p6 <- ggplot() +
  geom_line(data = sales11_index,
    aes(
      x = year,
      y = sales11_pct_change,
      group = region,
      text = paste0(
        "<b>", region, "</b>",
        "<br>Year: ", year,
        "<br>Houses sold: ",
        comma(n_sales, big.mark = " "),
        "<br>Change since 2011: ",
        round(sales11_pct_change, 1), "%"
      )
    ),
    color = "#5652A4",
    alpha = 0.45,
    linewidth = 0.8
  ) +
  geom_line(
    data = sales11_sweden_trend, 
    aes(
      x = year,
      y = sales11_pct_change,
      group = 1,
      text = paste0(
        "<b>Homes sold across län</b>",
        "<br>Year: ", year,
        "<br>Change: ", round(sales11_pct_change, 1), "%"
      )
    ),
    color = "black",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60" ) +
  geom_vline(
    xintercept = 2011,
    linetype = "dashed",
    color = "grey60"
  ) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), "%")
  ) +
  labs(
    title = "Housing Price Development Across Swedish Lan",
    subtitle = "Homes sold, 2011 = 0%",
    x = NULL,
    y = "% Change in homes sold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# 4. Make interactive
ggplotly(p6, tooltip = "text") |>
  layout(hovermode = "closest",
    xaxis = list(
      rangeslider = list(visible = TRUE)
    )
  )




combined <- subplot(
  ggplotly(p4, tooltip = "text"),
  ggplotly(p6, tooltip = "text"),
  nrows = 2,
  shareX = FALSE,
  margin = 0.08
) |>
  layout(
    hovermode = "closest",

    # Top plot
    xaxis = list(
      matches = "x2",
      rangeslider = list(visible = FALSE)
    ),
    yaxis = list(
      title = "% Median earned income increase"
    ),

    # Bottom plot
    xaxis2 = list(
      title = "Year",
      rangeslider = list(visible = TRUE)
    ),
    yaxis2 = list(
      title = "% Change in homes sold"
    )
  )

combined