getwd()
setwd ("Uni/Bachelorarbeit")

#import libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(Kendall)
library(dunn.test)




full_data <- read.csv("Airline_Delay_Cause.csv")
head(full_data)


#filter data for top 200 airports
group_data<- full_data %>%
  group_by(airport) %>%
  summarise(total_flights = sum(arr_flights, na.rm = TRUE)) %>%
  arrange(desc(total_flights))

airports <- group_data[1:200,]

data <- full_data %>%
  filter(airport %in% airports$airport)
data <- data %>%
  mutate(carrier_name = recode(carrier_name,
                               "AirTran Airways Corporation" = "Southwest Airlines Co.",
                               "ExpressJet Airlines LLC" = "ExpressJet Airlines Inc.",
                               "US Airways Inc." = "American Airlines Inc."))
unique(data$carrier_name)
unique(full_data$airport)=="HNL"

nrow(full_data)
nrow(data)


lowcost <- data %>%
  filter(carrier_name %in% c("JetBlue Airways", "Frontier Airlines Inc.", "Allegiant Air", 
                             "Spirit Air Lines", "PSA Airlines Inc.", "Southwest Airlines Co.", "Virgin America"))
highcost <- data %>%
  filter(!carrier_name %in% c("JetBlue Airways", "Frontier Airlines Inc.", "Allegiant Air", 
                              "Spirit Air Lines", "PSA Airlines Inc.", "Southwest Airlines Co.","Virgin America"))
american <- data %>%
  filter(carrier_name %in% c("American Airlines Inc.", "American Eagle Airlines Inc.", "Envoy Air", 
                             "PSA Airlines Inc."))
alaska <- data %>%
  filter(carrier_name %in% c("Alaska Airlines Inc.","Horizon Air", 
                             "Virgin America"))
delta <- data %>%
  filter(carrier_name %in% c("Delta Air Lines Inc.", "Endeavor Air Inc."))

united <- data %>%
  filter(carrier_name %in% c("United Air Lines Inc.", "Mesa Airlines Inc."))

independent <- data %>%
  filter(!carrier_name %in% c("United Air Lines Inc.", "Mesa Airlines Inc.","Delta Air Lines Inc.", "Endeavor Air Inc.","Alaska Airlines Inc.", "Hawaiian Airlines Inc.", "Horizon Air", 
                              "Virgin America","American Airlines Inc.", "American Eagle Airlines Inc.", "Envoy Air", 
                              "PSA Airlines Inc.", "Hawaiian Airlines Inc."))

overonem <- data %>%
  filter(airport %in% airports$airport[airports$total_flights >= 1000000])

underonem <- data %>%
  filter(airport %in% airports$airport[airports$total_flights <= 1000000])

#Average domestic flights per year
tot_avg <- sum(group_data$total_flights)/10
#Average arriving flights per year for the Top 10 airports
top10_avg <- sum(group_data[1:10, "total_flights"] / 10)
top10_avg/tot_avg


# Plot of development per year
yearly_total <- full_data %>%
  filter(airport %in% airports$airport, year >= 2013 & year <= 2023) %>%
  group_by(year) %>%
  summarise(total_arr_flights = sum(arr_flights, na.rm = TRUE))

total_2023<- yearly_total$total_arr_flights[11]/(8/12)
total_2013<- yearly_total$total_arr_flights[1]/(5/12)

ggplot(yearly_total, aes(x = year, y = total_arr_flights)) +
  geom_line(color = "cyan4", size = 1.2) +
  geom_point(color = "cyan4", size = 2) +
  
  geom_point(aes(x = 2023, y = total_2023), color = "chartreuse3", size = 3, shape = 16) +
  geom_point(aes(x = 2013, y = total_2013), color = "chartreuse3", size = 3, shape = 16) +
  
  geom_segment(aes(x = 2022, xend = 2023,
                   y = yearly_total$total_arr_flights[yearly_total$year == 2022],
                   yend = total_2023),
               linetype = "dashed", color = "chartreuse3") +
  
  geom_segment(aes(x = 2014, xend = 2013,
                   y = yearly_total$total_arr_flights[yearly_total$year == 2014],
                   yend = total_2013),
               linetype = "dashed", color = "chartreuse3") +
  
  scale_x_continuous(breaks = 2013:2023) +
  labs(
    title = "Development of arriving flights",
    x = "Year",
    y = "Amount of arriving flights") +
  theme_minimal()

# 1. Prepare the data
lowcost_names <- c("JetBlue Airways", "Frontier Airlines Inc.", "Allegiant Air", 
                   "Spirit Air Lines", "PSA Airlines Inc.", "Southwest Airlines Co.", "Virgin America")

lowcost_highcost_per_year <- data %>%
  mutate(category = ifelse(carrier_name %in% lowcost_names, "Low-Cost", "Full-Service")) %>%
  group_by(year, category) %>%
  summarise(total_flights = sum(arr_flights, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(percentage = total_flights / sum(total_flights) * 100) %>%
  ungroup()


ggplot(lowcost_highcost_per_year, aes(x = year, y = percentage, color = category)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Low-Cost" = "skyblue", "Full-Service" = "orange")) +
  labs(
    title = "Development of Low-Cost vs Full-Service Airline Share",
    x = "Year",
    y = "Percentage of Flights",
    color = "Airline Type"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 2013:2023)




#  SECTION KEYSTATISTICS

# Percentage of lowcost/highcost flights
lowcost_flights <- sum(lowcost$arr_flights, na.rm = TRUE)
highcost_flights <- sum(highcost$arr_flights, na.rm = TRUE)
total_flights_all <- sum(data$arr_flights, na.rm = TRUE)

lowcost_percentage <- (lowcost_flights / total_flights_all) * 100
highcost_percentage <- (highcost_flights / total_flights_all) * 100
lowcost_percentage
highcost_percentage


lowcost_cancelled <- sum(lowcost$arr_cancelled, na.rm = TRUE)
highcost_cancelled <- sum(highcost$arr_cancelled, na.rm = TRUE)
lowcost_diverted <- sum(lowcost$arr_diverted, na.rm = TRUE)
highcost_diverted <- sum(highcost$arr_diverted, na.rm = TRUE)
total_cancelled <- sum(data$arr_cancelled, na.rm = TRUE)
total_diverted <- sum(data$arr_diverted, na.rm = TRUE)
lowcost_delayed <- sum(lowcost$arr_del15, na.rm=TRUE)
highcost_delayed <- sum(highcost$arr_del15, na.rm=TRUE)
lowcost_ontime <- sum(lowcost$arr_flights-lowcost$arr_del15-lowcost$arr_diverted-lowcost$arr_cancelled, na.rm=TRUE)
highcost_ontime <- sum(highcost$arr_flights-highcost$arr_del15-highcost$arr_diverted-highcost$arr_cancelled, na.rm=TRUE)
lowcost_delayed_min <- sum(lowcost$arr_delay, na.rm=TRUE)/sum(lowcost$arr_flights, na.rm=TRUE)
highcost_delayed_min <- sum(highcost$arr_delay, na.rm=TRUE)/sum(highcost$arr_flights, na.rm=TRUE)



american_ct <- sum(american$arr_flights, na.rm = TRUE)
delta_ct <- sum(delta$arr_flights, na.rm = TRUE)
alaska_ct <- sum(alaska$arr_flights, na.rm = TRUE)
united_ct <- sum(united$arr_flights, na.rm = TRUE)
independent_ct <- sum(independent$arr_flights, na.rm = TRUE)
sum(american_ct, delta_ct, alaska_ct, united_ct, independent_ct, na.rm=TRUE)

american_ct/sum(american_ct, delta_ct, alaska_ct, united_ct, independent_ct, na.rm=TRUE)
delta_ct/sum(american_ct, delta_ct, alaska_ct, united_ct, independent_ct, na.rm=TRUE)
alaska_ct/sum(american_ct, delta_ct, alaska_ct, united_ct, independent_ct, na.rm=TRUE)
united_ct/sum(american_ct, delta_ct, alaska_ct, united_ct, independent_ct, na.rm=TRUE)
independent_ct/sum(american_ct, delta_ct, alaska_ct, united_ct, independent_ct, na.rm=TRUE)


# Cancelled flights per airport
cancelled_per_airport <- data %>%
  group_by(airport) %>%
  summarise(
    cancelled_flights = sum(arr_cancelled, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    cancel_rate = cancelled_flights / total_flights
  ) %>%
  arrange(desc(cancel_rate))

# Diverted flights per airport
diverted_per_airport <- data %>%
  group_by(airport) %>%
  summarise(
    diverted_flights = sum(arr_diverted, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    divert_rate = diverted_flights / total_flights
  ) %>%
  arrange(desc(divert_rate))

# Delayed flights per airport
delayed_per_airport <- data %>%
  group_by(airport) %>%
  summarise(
    delayed_flights = sum(arr_del15, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    delay_rate = delayed_flights / total_flights
  ) %>%
  arrange(desc(delay_rate))


head(cancelled_per_airport)  
head(diverted_per_airport)   
head(delayed_per_airport)



# Flight delay causes
causes_summary <- data %>%
  summarise(
    carrier = sum(carrier_delay, na.rm = TRUE),
    weather = sum(weather_delay, na.rm = TRUE),
    nas = sum(nas_delay, na.rm = TRUE),
    security = sum(security_delay, na.rm = TRUE),
    late_aircraft = sum(late_aircraft_delay, na.rm = TRUE))

causes_long <- pivot_longer(causes_summary, cols = everything(), names_to = "Cause", values_to = "total")
causes_long
causes_long$total/sum(causes_long$total)

ggplot(causes_long, aes(x = "", y = total, fill = Cause)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Delay Causes by minute") +
  scale_fill_manual(values = c(
    "carrier" = "darkorchid",
    "weather" = "darkslateblue",
    "nas" = "deepskyblue",      
    "security" = "deeppink2",    
    "late_aircraft" = "cornflowerblue" )) +
  theme_void()

causes_summary <- data %>%
  summarise(
    carrier = sum(carrier_ct, na.rm = TRUE),
    weather = sum(weather_ct, na.rm = TRUE),
    nas = sum(nas_ct, na.rm = TRUE),
    security = sum(security_ct, na.rm = TRUE),
    late_aircraft = sum(late_aircraft_ct, na.rm = TRUE))

causes_long <- pivot_longer(causes_summary, cols = everything(), names_to = "Cause", values_to = "total")
causes_long
causes_long$total/sum(causes_long$total)

ggplot(causes_long, aes(x = "", y = total, fill = Cause)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Delay Causes by count") +
  scale_fill_manual(values = c(
    "carrier" = "darkorchid",
    "weather" = "darkslateblue",
    "nas" = "deepskyblue",      
    "security" = "deeppink2",    
    "late_aircraft" = "cornflowerblue" )) +
  theme_void()

#top5 late airports causes
# Airports of interest
top_airports <- c("ASE", "SFB", "EGE", "EWR")

# Filter data for relevant airports
filtered_data <- data %>%
  filter(airport %in% top_airports)


# Summarise delay causes per airport
causes_per_airport <- filtered_data %>%
  group_by(airport) %>%
  summarise(
    carrier = sum(carrier_delay, na.rm = TRUE),
    weather = sum(weather_delay, na.rm = TRUE),
    nas = sum(nas_delay, na.rm = TRUE),
    security = sum(security_delay, na.rm = TRUE),
    late_aircraft = sum(late_aircraft_delay, na.rm = TRUE)
  )

# Convert to long format
causes_long <- causes_per_airport %>%
  pivot_longer(cols = -airport, names_to = "Cause", values_to = "Total_Minutes")

# Calculate proportions per airport
causes_long <- causes_long %>%
  group_by(airport) %>%
  mutate(Proportion = Total_Minutes / sum(Total_Minutes)) %>%
  ungroup()

# View final table
print(causes_long)




# TRENDS

delay_per_year <- data %>%
  group_by(year) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    carrier_delay = sum(carrier_delay, na.rm = TRUE),
    weather_delay = sum(weather_delay, na.rm = TRUE),
    nas_delay = sum(nas_delay, na.rm = TRUE),
    security_delay = sum(security_delay, na.rm = TRUE),
    late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE))

delay_per_year_percent <- delay_per_year %>%
  mutate(
    carrier_delay_pct = (carrier_delay / total_delay) * 100,
    weather_delay_pct = (weather_delay / total_delay) * 100,
    nas_delay_pct = (nas_delay / total_delay) * 100,
    security_delay_pct = (security_delay / total_delay) * 100,
    late_aircraft_delay_pct = (late_aircraft_delay / total_delay) * 100)

delay_long <- delay_per_year_percent %>%
  pivot_longer(cols = c("carrier_delay_pct", "weather_delay_pct", "nas_delay_pct", 
                        "security_delay_pct", "late_aircraft_delay_pct"),
               names_to = "Delay_Type", 
               values_to = "percentage") %>%
  mutate(Delay_Type = recode(Delay_Type,
                             "carrier_delay_pct" = "Carrier Delay",
                             "weather_delay_pct" = "Weather Delay",
                             "nas_delay_pct" = "NAS Delay",
                             "security_delay_pct" = "Security Delay",
                             "late_aircraft_delay_pct" = "Late Aircraft Delay"))

ggplot(delay_long, aes(x = year, y = percentage, color = Delay_Type, group = Delay_Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Percentage of Delay Causes by Year",
       x = "Year",
       y = "Percentage of Total Delays (%)") +
  scale_color_manual(values = c("Carrier Delay" = "darkorchid",
                                "Weather Delay" = "darkslateblue",
                                "NAS Delay" = "deepskyblue", 
                                "Security Delay" = "deeppink2", 
                                "Late Aircraft Delay" = "cornflowerblue")) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  theme_minimal()

late_data <- delay_long %>% 
  filter(Delay_Type == "Late Aircraft Delay")
mk_result <- MannKendall(late_data$percentage)
print(mk_result)

late_data <- delay_long %>% 
  filter(Delay_Type == "NAS Delay")
mk_result <- MannKendall(late_data$percentage)
print(mk_result)



#### seasonal trends
monthly_delays <- data %>%
  group_by(year, month) %>%
  summarise(
    total_delay_min = sum(arr_delay, na.rm = TRUE),  # Gesamtverspätungen in Minuten
    mean_delay_min = mean(arr_delay, na.rm = TRUE),  # Durchschnitt pro Flug
    n_flights = sum(arr_flights, na.rm = TRUE),       # Anzahl Flüge
    .groups = "drop"
  ) %>%
  mutate(
    month_name = factor(month.abb[month], levels = month.abb)  # Monatsnamen für Plots
  )

ggplot(monthly_delays, aes(x = month_name, y = year, fill = total_delay_min)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red", name = "Delays (Min)") +
  labs(
    title = "Seasonal Delays (2013-2023)",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Kruskal Wallis test to compare central tendencies
kruskal.test(mean_delay_min ~ factor(year), data = monthly_delays)
#posthoc to find out differences 
dunn.test(monthly_delays$mean_delay_min, factor(monthly_delays$year))


print(monthly_delays, n=121)


ggplot(monthly_delays, aes(x = month_name, y = total_delay_min)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Distribution by months",
    x = "Month",
    y = "Delay (Minutes)"
  ) +
  theme_minimal()

library(forecast)

# Zeitreihe erstellen (monatliche Daten)
ts_data <- ts(
  monthly_delays$total_delay_min,
  start = c(2013, 1),
  frequency = 12  # 12 Monate = 1 Jahr
)

# Zerlegung in Trend, Saisonalität und Residuen
decomposed <- decompose(ts_data, type = "additive")

# Plot der Zerlegung
plot(decomposed)


kruskal.test(total_delay_min ~ month_name, data = monthly_delays)







#trend for low/highcost
# Berechnung für Low-Cost Carriers
lowcost_delay_per_year <- lowcost %>%
  group_by(year) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE),
    carrier_delay = sum(carrier_delay, na.rm = TRUE)
  ) %>%
  mutate(
    late_aircraft_pct = (late_aircraft_delay / total_delay) * 100,
    carrier_delay_pct = (carrier_delay / total_delay) * 100
  )

# Berechnung für Full-Service Carriers
highcost_delay_per_year <- highcost %>%
  group_by(year) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE),
    carrier_delay = sum(carrier_delay, na.rm = TRUE)
  ) %>%
  mutate(
    late_aircraft_pct = (late_aircraft_delay / total_delay) * 100,
    carrier_delay_pct = (carrier_delay / total_delay) * 100
  )

# Plot 1: Late Aircraft Delay (nur Late Aircraft)
ggplot() +
  geom_line(data = lowcost_delay_per_year, aes(x = year, y = late_aircraft_pct, color = "Low-Cost"), size = 1.2) +
  geom_point(data = lowcost_delay_per_year, aes(x = year, y = late_aircraft_pct), color = "darkorchid", size = 3) +
  
  geom_line(data = highcost_delay_per_year, aes(x = year, y = late_aircraft_pct, color = "Full-Service"), size = 1.2) +
  geom_point(data = highcost_delay_per_year, aes(x = year, y = late_aircraft_pct), color = "darkslateblue", size = 3) +
  
  labs(title = "Late Aircraft Delay Percentage by Year",
       x = "Year", y = "Percentage of Late Aircraft Delays (%)",
       color = "Carrier Type") +
  
  scale_color_manual(values = c("Low-Cost" = "darkorchid", 
                                "Full-Service" = "darkslateblue")) +
  
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  theme_minimal()

# Plot 2: Carrier Delay (nur Carrier Delay)
ggplot() +
  geom_line(data = lowcost_delay_per_year, aes(x = year, y = carrier_delay_pct, color = "Low-Cost"), size = 1.2) +
  geom_point(data = lowcost_delay_per_year, aes(x = year, y = carrier_delay_pct), color = "darkorchid", size = 3) +
  
  geom_line(data = highcost_delay_per_year, aes(x = year, y = carrier_delay_pct, color = "Full-Service"), size = 1.2) +
  geom_point(data = highcost_delay_per_year, aes(x = year, y = carrier_delay_pct), color = "darkslateblue", size = 3) +
  
  labs(title = "Carrier Delay Percentage by Year",
       x = "Year", y = "Percentage of Carrier Delays (%)",
       color = "Carrier Type") +
  
  scale_color_manual(values = c("Low-Cost" = "darkorchid", 
                                "Full-Service" = "darkslateblue")) +
  
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  theme_minimal()






























#trend for holding companies
prepare_delay_data <- function(df, group_name) {
  df %>%
    group_by(year) %>%
    summarise(
      total_delay = sum(arr_delay, na.rm = TRUE),
      late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE),
      carrier_delay = sum(carrier_delay, na.rm = TRUE)) %>%
    mutate(
      late_aircraft_pct = (late_aircraft_delay / total_delay) * 100,
      carrier_delay_pct = (carrier_delay / total_delay) * 100,
      group = group_name)}

american_delay <- prepare_delay_data(american, "American")
alaska_delay <- prepare_delay_data(alaska, "Alaska")
delta_delay <- prepare_delay_data(delta, "Delta")
united_delay <- prepare_delay_data(united, "United")
independent_delay <- prepare_delay_data(independent, "Independent")

holding_delay_per_year <- bind_rows(
  american_delay,
  alaska_delay,
  delta_delay,
  united_delay,
  independent_delay)


ggplot(holding_delay_per_year, aes(x = year, y = late_aircraft_pct, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Late Aircraft Delay Percentage by Year (Holding Companies)",
       x = "Year",
       y = "Percentage of Late Aircraft Delays (%)",
       color = "Holding Company") +
  scale_color_manual(values = c(
    "American" = "darkslateblue",
    "Alaska" = "darkorange",
    "Delta" = "deeppink2",
    "United" = "dodgerblue2",
    "Independent" = "mediumorchid3"
  )) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  theme_minimal()

ggplot(holding_delay_per_year, aes(x = year, y = carrier_delay_pct, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Carrier Delay Percentage by Year (Holding Companies)",
       x = "Year",
       y = "Percentage of Carrier Delays (%)",
       color = "Holding Company") +
  scale_color_manual(values = c(
    "American" = "darkslateblue",
    "Alaska" = "darkorange",
    "Delta" = "deeppink2",
    "United" = "dodgerblue2",
    "Independent" = "mediumorchid3"
  )) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  theme_minimal()









