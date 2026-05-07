df <- read.csv("us_power_data.csv")
names(df) <- trimws(names(df))
df$date <- as.Date(sprintf("%d-%02d-01", df$year, df$month))
df <- df[, c("date", "year", "month", "sector", "state", "price", "sales")]
df <- subset(df, !is.na(df$date) & !is.na(df$price))

print(colnames(df))
#write.csv(df, "new_data.csv", row.names = FALSE)