options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

df <- read.csv("us_power_data.csv")
df <- subset(df, tolower(trimws(df$sector)) %in% "residential")

avgs_month <- df |>
  group_by(year, month, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)))

avgs_3_month <- df |>
  mutate(period_3mo = floor((month - 1) / 3) + 1) |>
  group_by(year, period_3mo, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mid_month = c(2, 5, 8, 11)[period_3mo],
    date = as.Date(sprintf("%d-%02d-01", year, mid_month))
  )

avgs_6_month <- df |>
  mutate(period_6mo = floor((month - 1) / 6) + 1) |>
  group_by(year, period_6mo, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    mid_month = c(3, 9)[period_6mo],
    date = as.Date(sprintf("%d-%02d-01", year, mid_month))
  )

avgs_year <- df |>
  group_by(year, sector) |>
  summarise(
    price = mean(price, na.rm = TRUE),
    sales = mean(sales, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(sprintf("%d-07-01", year)))

start <- as.Date("2001-01-01")
end <- as.Date("2024-12-01")

plot_style <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#111827", hjust = 0),
    plot.subtitle = element_text(size = 11, color = "#6B7280", hjust = 0),
    axis.title = element_text(color = "#374151"),
    axis.text = element_text(color = "#6B7280"),
    panel.grid.major = element_line(color = "#E5E7EB", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F7F6F3", color = NA),
    panel.background = element_rect(fill = "#F7F6F3", color = NA),
    legend.title = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(color = "#374151"),
    plot.margin = margin(14, 16, 14, 14)
  )

p1 <- ggplot() +
  geom_line(data = avgs_6_month, aes(x = date, y = price, color = "6 Month Avg"), linewidth = 1.1) +
  geom_line(data = avgs_3_month, aes(x = date, y = price, color = "3 Month Avg"), linewidth = 1.1) +
  scale_color_manual(values = c("6 Month Avg" = "#111111", "3 Month Avg" = "#D9485F")) +
  scale_x_date(limits = c(start, end), date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Mean Price of Power in the US",
    subtitle = "3 month vs 6 month averages",
    x = NULL,
    y = "Price per kWh (cents)"
  ) +
  plot_style

ggsave("output/plot_3mo_vs_6mo.png", p1, width = 11, height = 6.5, dpi = 300)

p2 <- ggplot() +
  geom_line(data = avgs_month, aes(x = date, y = price, color = "Monthly Avg"), linewidth = 1.1) +
  geom_line(data = avgs_year, aes(x = date, y = price, color = "Year Avg"), linewidth = 1.1) +
  scale_color_manual(values = c("Monthly Avg" = "#111111", "Year Avg" = "#D9485F")) +
  scale_x_date(limits = c(start, end), date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Mean Price of Power in the US",
    subtitle = "Monthly vs yearly averages",
    x = NULL,
    y = "Price per kWh (cents)"
  ) +
  plot_style

ggsave("output/plot_monthly_vs_year.png", p2, width = 11, height = 6.5, dpi = 300)