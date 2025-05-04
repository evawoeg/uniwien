getwd()

#import libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(Kendall)
library(dunn.test)
library(purrr) 
library(stringr)
library(forecast)
library(scales)




#importing the data
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

nrow(full_data)
nrow(data)

#creating groups for the following analysis
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




###############################not displayed in paper###########################

# plots to check the distribution of flight amounts per airport
under_1m <- ggplot(group_data %>% filter(total_flights < 1e6), 
                   aes(x = total_flights)) +
  geom_histogram(binwidth = 50000,
                 fill = "#1b9e77",
                 color = "white") +
  labs(title = "Distribution smaller airports (<1M Flights)",
       x = "Total flights (2003-2023)",
       y = "Amount of airports") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"),
                     limits = c(0, 1e6)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

over_1m <- ggplot(group_data %>% filter(total_flights >= 1e6), 
                  aes(x = total_flights)) +
  geom_histogram(binwidth = 250000, 
                 fill = "#d95f02",
                 color = "white") +
  labs(title = "Distribution smaller airports (1M+ Flights)",
       x = "Total flights (2003-2023)",
       y = "Amount of airports") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M"),
                     breaks = seq(1e6, max(group_data$total_flights), 2e6)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

under_1m
over_1m
################################################################################

overthreehtsd <- data %>%
  filter(airport %in% airports$airport[airports$total_flights > 300000])

underthreehtsd <- data %>%
  filter(airport %in% airports$airport[airports$total_flights <= 300000])







##  INTRODUCTION  ##
#Avg domestic flights per year
tot_avg <- sum(group_data$total_flights)/10
#Average arriving flights per year for the Top 10 airports
top10_avg <- sum(group_data[1:10, "total_flights"] / 10)
top10_avg/tot_avg


# Plot of development per year
yearly_total <- full_data %>%
  filter(airport %in% airports$airport, year >= 2013 & year <= 2023) %>%
  group_by(year) %>%
  summarise(total_arr_flights = sum(arr_flights, na.rm = TRUE))

#adding the points for 2013 and 2013
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

# Flight delay causes pie charts
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







##  LOW_COST VS LEGACY  ##
# Percentage of lowcost/highcost flights
lowcost_flights <- sum(lowcost$arr_flights, na.rm = TRUE)
highcost_flights <- sum(highcost$arr_flights, na.rm = TRUE)
total_flights_all <- sum(data$arr_flights, na.rm = TRUE)

lowcost_percentage <- (lowcost_flights / total_flights_all) * 100
highcost_percentage <- (highcost_flights / total_flights_all) * 100
lowcost_percentage
highcost_percentage

#plot for low/legacy development
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
  scale_color_manual(values = c("Low-Cost" = "cornflowerblue", "Full-Service" = "deeppink2")) +
  labs(
    title = "Development of Low-Cost vs Full-Service Airline Share",
    x = "Year",
    y = "Percentage of Flights",
    color = "Airline Type"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 2013:2023)


#values for cancellation/diversion/dely table with lowcost and legacy
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

#test to see whether difference between cancellations/diversions is significant
prop.test(
  x = c(lowcost_diverted, highcost_diverted),
  n = c(sum(lowcost$arr_flights, na.rm = TRUE), sum(highcost$arr_flights, na.rm = TRUE)),
  correct = FALSE)
prop.test(
  x = c(lowcost_cancelled, highcost_cancelled),
  n = c(sum(lowcost$arr_flights, na.rm = TRUE), sum(highcost$arr_flights, na.rm = TRUE)),
  correct = FALSE)



#yearly avg delay per flight low vs legacy
delay_by_year <- data %>%
  mutate(category = ifelse(carrier_name %in% lowcost_names, "Low-Cost", "Full-Service")) %>%
  group_by(year, category) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    avg_delay_per_flight = total_delay / total_flights) %>% ungroup()

#  plot
ggplot(delay_by_year, aes(x = year, y = avg_delay_per_flight, color = category)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Low-Cost" = "cornflowerblue", "Full-Service" = "deeppink2")) +
  labs(
    title = "Average Arrival Delay per Flight ",
    x = "Year",
    y = "Avg Delay per Flight (minutes)",
    color = "Airline Type") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 2013:2023) +
  scale_y_continuous(limits = c(0, NA)) 

#avg minutes
avg_delay_by_category <- data %>%
  mutate(category = ifelse(carrier_name %in% lowcost_names, "Low-Cost", "Full-Service")) %>%
  group_by(category) %>%  
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    avg_delay_per_flight = total_delay / total_flights)



# Monthly avg delay per flight
delay_by_month <- data %>%
  mutate(category = ifelse(carrier_name %in% lowcost_names, "Low-Cost", "Full-Service")) %>%
  group_by(month, category) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    avg_delay_per_flight = total_delay / total_flights
  ) %>% 
  ungroup()

#plot
ggplot(delay_by_month, aes(x = month, y = avg_delay_per_flight, 
                           color = category, group = category)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Low-Cost" = "cornflowerblue", "Full-Service" = "deeppink2")) +
  labs(
    title = "Average Arrival Delay per Flight by Month",
    subtitle = "Aggregated across all years in dataset",
    x = "Month",
    y = "Avg Delay per Flight (minutes)",
    color = "Airline Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 1:12, labels = month.abb)  # Monatsabkürzungen (Jan, Feb, etc.)


#tests
#monthly-test
wilcox.test(avg_delay_per_flight ~ category, data = delay_by_month)
#yearly-test
wilcox.test(avg_delay_per_flight ~ category, data = delay_by_year)
#not aggregated over months
data_with_category <- data %>%
  mutate(
    category = ifelse(carrier_name %in% lowcost_names, "Low-Cost", "Full-Service"))

filtered_data <- data_with_category %>%
  filter(!is.na(arr_delay), !is.na(arr_flights), arr_flights > 0)

filtered_data <- filtered_data %>%
  mutate(delay_per_flight = arr_delay / arr_flights)


wilcox.test(delay_per_flight ~ category, data = filtered_data)
tapply(filtered_data$delay_per_flight, filtered_data$category, median, na.rm = TRUE)







##  A LOOK AT AIRLINES  ##

#calculating market shares
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


# creating the plot with four subplots of all four "categories"
calculate_normalized_stats <- function(df) {
  df %>%
    summarise(
      total_flights = n(),
      avg_arr_delay_per_flight = sum(arr_delay, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE),
      pct_diverted_per_flight = sum(arr_diverted, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
      pct_canceled_per_flight = sum(arr_cancelled, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
      pct_delayed_per_flight = sum(arr_delay > 15, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100)}

american_stats <- calculate_normalized_stats(american)
alaska_stats <- calculate_normalized_stats(alaska)
delta_stats <- calculate_normalized_stats(delta)
united_stats <- calculate_normalized_stats(united)
independent_stats <- calculate_normalized_stats(independent)

all_stats_normalized <- bind_rows(
  "American" = american_stats,
  "Alaska" = alaska_stats,
  "Delta" = delta_stats,
  "United" = united_stats,
  "Independent" = independent_stats,
  .id = "airline_group")

stats_long <- all_stats_normalized %>%
  select(airline_group, 
         ends_with("_per_flight"), 
         ends_with("_per_flight")) %>%
  pivot_longer(cols = -airline_group, 
               names_to = "metric", 
               values_to = "value")

ggplot(stats_long, aes(x = airline_group, y = value, fill = airline_group)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y", ncol = 2,
             labeller = labeller(metric = c(
               "avg_arr_delay_per_flight" = "Avg Delay (Min./Flight)",
               "pct_diverted_per_flight" = "Diversions in %",
               "pct_canceled_per_flight" = "Cancellations in %",
               "pct_delayed_per_flight" = "Delay >15min in %"
             ))) +
  labs(title = "Airline-Performance (normalized per Flights)",
       x = "", y = "Value") +
  scale_fill_manual(  # Changed from scale_color_manual to scale_fill_manual
    values = c(
      "American" = "darkslateblue",
      "Alaska" = "darkorange",
      "Delta" = "deeppink2",
      "United" = "dodgerblue2",
      "Independent" = "mediumorchid3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#creating a line chart with absolute numbers of flights taken per group
data_with_groups <- bind_rows(
  american %>% mutate(airline_group = "American"),
  alaska %>% mutate(airline_group = "Alaska"),
  delta %>% mutate(airline_group = "Delta"),
  united %>% mutate(airline_group = "United"),
  independent %>% mutate(airline_group = "Independent"))

flights_per_year <- data_with_groups %>%
  group_by(airline_group, year) %>%
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),
    .groups = "drop")

# Linechart
ggplot(flights_per_year, 
       aes(x = year, y = total_flights, 
           color = airline_group, 
           group = airline_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  labs(
    title = "Development of flight counts per Airline Group",
    x = "Year",
    y = "Count",
    color = "Airline-Group") +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0.05, 0.1))  ) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()) +
  scale_color_manual(
    values = c(
      "American" = "darkslateblue",
      "Alaska" = "darkorange",
      "Delta" = "deeppink2",
      "United" = "dodgerblue2",
      "Independent" = "mediumorchid3"))



###############################not displayed in paper###########################

###quick look at alaska
alaska_delays <- alaska %>%
  group_by(year) %>%
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),
    carrier_delay = sum(carrier_delay, na.rm = TRUE) / total_flights,
    weather_delay = sum(weather_delay, na.rm = TRUE) / total_flights,
    nas_delay = sum(nas_delay, na.rm = TRUE) / total_flights,
    security_delay = sum(security_delay, na.rm = TRUE) / total_flights,
    late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE) / total_flights,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(year, total_flights),
    names_to = "delay_type",
    values_to = "minutes_per_flight")

ggplot(alaska_delays, aes(x = year, y = minutes_per_flight, color = delay_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(alaska_delays$year), max(alaska_delays$year), by = 1)) +
  labs(
    title = "Alaska Airlines - avg delay by cause",
    x = "Jahr",
    y = "delay min per flight",
    color = "cause"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(
    values = c(
      "carrier_delay" = "#E69F00",
      "weather_delay" = "#56B4E9",
      "nas_delay" = "#009E73",
      "security_delay" = "#F0E442",
      "late_aircraft_delay" = "#D55E00"
    ),
    labels = c(
      "carrier_delay" = "carrier",
      "weather_delay" = "weather",
      "nas_delay" = "NAS",
      "security_delay" = "security",
      "late_aircraft_delay" = "late aircraft"
    )
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


delta_delays <- delta %>%
  group_by(year) %>%
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),
    carrier_delay = sum(carrier_delay, na.rm = TRUE) / total_flights,
    weather_delay = sum(weather_delay, na.rm = TRUE) / total_flights,
    nas_delay = sum(nas_delay, na.rm = TRUE) / total_flights,
    security_delay = sum(security_delay, na.rm = TRUE) / total_flights,
    late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE) / total_flights,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(year, total_flights),
    names_to = "delay_type",
    values_to = "minutes_per_flight"
  )

# Liniendiagramm für Alaska Verspätungen nach Ursache
ggplot(delta_delays, aes(x = year, y = minutes_per_flight, color = delay_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(delta_delays$year), max(delta_delays$year), by = 1)) +
  labs(
    title = "delta Airlines - avg delay by cause",
    x = "year",
    y = "delay min per flight",
    color = "cause"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(
    values = c(
      "carrier_delay" = "#E69F00",
      "weather_delay" = "#56B4E9",
      "nas_delay" = "#009E73",
      "security_delay" = "#F0E442",
      "late_aircraft_delay" = "#D55E00"
    ),
    labels = c(
      "carrier_delay" = "carrier",
      "weather_delay" = "weather",
      "nas_delay" = "NAS",
      "security_delay" = "security",
      "late_aircraft_delay" = "late aircraft"
    )
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

################################################################################



lowcost_stats <- lowcost %>%
  group_by(carrier_name) %>%
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),
    avg_delay = sum(arr_delay, na.rm = TRUE) / total_flights,
    cancellation_rate = sum(arr_cancelled, na.rm = TRUE) / total_flights * 100,
    diversion_rate = sum(arr_diverted, na.rm = TRUE) / total_flights * 100,
    del15 = sum(arr_del15 , na.rm = TRUE) / total_flights * 100,
    .groups = "drop"
  ) %>%
  arrange(avg_delay)


lowcost_stats_normalized <- lowcost %>%
  group_by(carrier_name) %>%
  summarise(
    avg_arr_delay_per_flight = sum(arr_delay, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE),
    pct_diverted_per_flight = sum(arr_diverted, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
    pct_canceled_per_flight = sum(arr_cancelled, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
    pct_delayed_per_flight = sum(arr_delay > 15, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100)

lowcost_stats_long <- lowcost_stats_normalized %>%
  pivot_longer(
    cols = -carrier_name,
    names_to = "metric",
    values_to = "value")

#lowcost colors
lowcost_colors <- c(
  "JetBlue Airways" = "blue3",
  "Southwest Airlines Co." = "#FFB300",
  "Spirit Air Lines" = "yellow", 
  "Frontier Airlines Inc." = "seagreen", 
  "Allegiant Air" = "dodgerblue1", 
  "Virgin America" = "red2",  
  "PSA Airlines Inc." = "lightpink"  )

#plot with 4 subplots (like before)
ggplot(lowcost_stats_long, aes(x = carrier_name, y = value, fill = carrier_name)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y", ncol = 2,
             labeller = labeller(metric = c(
               "avg_arr_delay_per_flight" = "Avg Delay (Min./Flight)",
               "pct_diverted_per_flight" = "Diversions in %",
               "pct_canceled_per_flight" = "Cancellations in %",
               "pct_delayed_per_flight" = "Delay >15min in %"
             ))) +
  labs(
    title = "Low-Cost Airline Performance (normalized per Flight)",
    x = "",
    y = "Value",
    fill = "Airline"
  ) +
  scale_fill_manual(values = lowcost_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))



#same for legacy airlines
highcost_stats_normalized <- highcost %>%
  group_by(carrier_name) %>%
  summarise(
    avg_arr_delay_per_flight = sum(arr_delay, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE),
    pct_diverted_per_flight = sum(arr_diverted, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
    pct_canceled_per_flight = sum(arr_cancelled, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
    pct_delayed_per_flight = sum(arr_delay > 15, na.rm = TRUE) / sum(arr_flights, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(avg_arr_delay_per_flight)

highcost_stats_long <- highcost_stats_normalized %>%
  pivot_longer(
    cols = -carrier_name,
    names_to = "metric",
    values_to = "value"
  )

#legacy colors (their actual colors)
highcost_colors <- c(
  "American Airlines Inc." = "firebrick1", 
  "Delta Air Lines Inc." = "blue4", 
  "United Air Lines Inc." = "royalblue2", 
  "Hawaiian Airlines Inc." = "blueviolet", 
  "Alaska Airlines Inc." = "cadetblue",  
  "Envoy Air" = "#6CACE4",         
  "American Eagle Airlines Inc." = "brown3",
  "ExpressJet Airlines Inc." = "springgreen4",
  "Horizon Air" = "coral1",
  "Mesa Airlines Inc." = "ivory3",
  "Republic Airline" = "navyblue",
  "SkyWest Airlines Inc." = "dodgerblue")

ggplot(highcost_stats_long, aes(x = carrier_name, y = value, fill = carrier_name)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y", ncol = 2,
             labeller = labeller(metric = c(
               "avg_arr_delay_per_flight" = "Avg Delay (Min./Flight)",
               "pct_diverted_per_flight" = "Diversions in %",
               "pct_canceled_per_flight" = "Cancellations in %",
               "pct_delayed_per_flight" = "Delay >15min in %"
             ))) +
  labs(
    title = "High-Cost Airline Performance (normalized per Flight)",
    x = "",
    y = "Value",
    fill = "Airline"
  ) +
  scale_fill_manual(values = highcost_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3)))



# investigating hawaiian
highcost_for_test <- highcost %>%
  mutate(is_hawaiian = ifelse(carrier_name == "Hawaiian Airlines Inc.", 
                              "Hawaiian", "Other High-Cost"))

# delays per flight
highcost_for_test <- highcost_for_test %>%
  mutate(delay_per_flight = arr_delay / arr_flights)
highcost_for_test %>%
  group_by(is_hawaiian) %>%
  summarise(
    mean_delay = mean(delay_per_flight, na.rm = TRUE),
    median_delay = median(delay_per_flight, na.rm = TRUE),
    sd_delay = sd(delay_per_flight, na.rm = TRUE),
    n_flights = n()
  ) %>%
  knitr::kable(digits = 2)
wilcox_test_result <- wilcox.test(delay_per_flight ~ is_hawaiian, 
                                  data = highcost_for_test,
                                  alternative = "less")
print(wilcox_test_result)







##  AIRPORTS  ##

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

#for the top 6 table
head(cancelled_per_airport)  
head(diverted_per_airport)   
head(delayed_per_airport)

# total flights
total_flights_over <- overthreehtsd %>%
  group_by(year) %>%
  summarise(total_flights_over = sum(arr_flights, na.rm = TRUE))  # Renamed for clarity

total_flights_under <- underthreehtsd %>%
  group_by(year) %>%
  summarise(total_flights_under = sum(arr_flights, na.rm = TRUE))  # Renamed for clarity

# Join and calculate delay minutes per flight
delay_causes_over <- delay_causes_over %>%
  left_join(total_flights_over, by = "year") %>%
  mutate(minutes_per_flight = minutes / total_flights_over)  # Use the correct column name

delay_causes_under <- delay_causes_under %>%
  left_join(total_flights_under, by = "year") %>%
  mutate(minutes_per_flight = minutes / total_flights)

avg_delay_over <- overthreehtsd %>%
  group_by(year) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    avg_delay = total_delay / total_flights
  ) %>%
  mutate(category = ">300k flights")

avg_delay_under <- underthreehtsd %>%
  group_by(year) %>%
  summarise(
    total_delay = sum(arr_delay, na.rm = TRUE),
    total_flights = sum(arr_flights, na.rm = TRUE),
    avg_delay = total_delay / total_flights
  ) %>%
  mutate(category = "???300k flights")

combined_avg_delay <- bind_rows(avg_delay_over, avg_delay_under)

ggplot(combined_avg_delay, aes(x = year, y = avg_delay, color = category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Arrival Delay per Flight",
       x = "Year",
       y = "Average Delay (minutes)",
       color = "Airport Size Category") +  # Titel der Legende angepasst
  scale_color_manual(
    values = c(">300k flights" = "dodgerblue", "???300k flights" = "tomato"),
    labels = c(">300k flights" = "Large Airports (>300k)", "???300k flights" = "Small Airports (<=300k)")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90")) +
  scale_y_continuous(limits = c(0, NA))

summary(avg_delay_over$avg_delay)

summary(avg_delay_under$avg_delay)


plot_over_norm <- ggplot(delay_causes_over, 
                         aes(x = year, y = minutes_per_flight, color = delay_cause)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Normalized Delay Causes: Airports with >300k Flights",
       x = "Year",
       y = "Delay Minutes per Flight",
       color = "Cause") +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_color_manual(
    values = c("darkorchid", "cornflowerblue", "deepskyblue", "deeppink2", "darkslateblue"),
    labels = c("Carrier", "Late Aircraft", "NAS", "Security", "Weather")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_under_norm <- ggplot(delay_causes_under, 
                          aes(x = year, y = minutes_per_flight, color = delay_cause)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Normalized Delay Causes: Airports with <=300k Flights",
       x = "Year",
       y = "Delay Minutes per Flight",
       color = "Cause") +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_color_manual(
    values = c("darkorchid", "cornflowerblue", "deepskyblue", "deeppink2", "darkslateblue"),
    labels = c("Carrier", "Late Aircraft", "NAS", "Security", "Weather")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plots 
plot_over_norm
plot_under_norm


#investigating top5 late airports causes
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







##  TRENDS  ##

delay_per_flight <- data %>%
  group_by(year) %>%
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),  # Gesamtzahl der Flüge
    total_delay_per_flight = sum(arr_delay, na.rm = TRUE) / total_flights,
    carrier_delay_per_flight = sum(carrier_delay, na.rm = TRUE) / total_flights,
    weather_delay_per_flight = sum(weather_delay, na.rm = TRUE) / total_flights,
    nas_delay_per_flight = sum(nas_delay, na.rm = TRUE) / total_flights,
    security_delay_per_flight = sum(security_delay, na.rm = TRUE) / total_flights,
    late_aircraft_delay_per_flight = sum(late_aircraft_delay, na.rm = TRUE) / total_flights,
    mean_delay_min = mean(arr_delay, na.rm = TRUE))

ggplot(delay_per_flight, aes(x = year, y = total_delay_per_flight)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  labs(
    title = "Average Arrival Delay per Flight by Year",
    x = "Year", 
    y = "Delay per flight (minutes)"
  ) +
  theme_minimal()

#test whether significant
MannKendall(delay_per_flight$total_delay_per_flight) %>% 
  print()

#test whether significant without 2020
delay_per_flight %>%
  filter(year != 2020) %>% 
  pull(total_delay_per_flight) %>%
  MannKendall() %>% 
  print()


# trends of causes
delay_per_year <- data %>%
  group_by(year) %>%  # Falls Kategorie berücksichtigt werden soll
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),
    total_delay = sum(arr_delay, na.rm = TRUE),
    carrier_delay = sum(carrier_delay, na.rm = TRUE),
    weather_delay = sum(weather_delay, na.rm = TRUE),
    nas_delay = sum(nas_delay, na.rm = TRUE),
    security_delay = sum(security_delay, na.rm = TRUE),
    late_aircraft_delay = sum(late_aircraft_delay, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    total_delay_per_flight = total_delay / total_flights)

delay_per_year_percent <- delay_per_year %>%
  mutate(
    carrier_delay_pct = (carrier_delay / total_delay) * 100,
    weather_delay_pct = (weather_delay / total_delay) * 100,
    nas_delay_pct = (nas_delay / total_delay) * 100,
    security_delay_pct = (security_delay / total_delay) * 100,
    late_aircraft_delay_pct = (late_aircraft_delay / total_delay) * 100)

ggplot(delay_per_flight, aes(x = year, y = total_delay_per_flight)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  labs(
    title = "Average Arrival Delay per Flight by Year",
    x = "Year", 
    y = "Delay per flight (minutes)"
  ) +
  theme_minimal()

# Mann-Kendall-Test for significance
MannKendall(delay_per_flight$total_delay_per_flight) %>% 
  print()

# test without 2020 (COVID-Year)
delay_per_flight %>%
  filter(year != 2020) %>% 
  pull(total_delay_per_flight) %>%
  MannKendall() %>% 
  print()

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
  labs(
    title = "Percentage of Delay Causes by Year",
    x = "Year",
    y = "Percentage of Total Delays (%)",
    color = "Delay Type"
  ) +
  scale_color_manual(
    values = c(
      "Carrier Delay" = "darkorchid",
      "Weather Delay" = "darkslateblue",
      "NAS Delay" = "deepskyblue", 
      "Security Delay" = "deeppink2", 
      "Late Aircraft Delay" = "cornflowerblue"
    ),
    labels = c(
      "Carrier Delay" = "Airline",
      "Weather Delay" = "Weather",
      "NAS Delay" = "Air Traffic", 
      "Security Delay" = "Security", 
      "Late Aircraft Delay" = "Late Aircraft"
    )
  ) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1)) +
  scale_y_continuous(limits = c(0, NA)) +  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )



# testing for significant trends with and without 2020
analyze_delay_trends <- function(include_2020 = TRUE) {
  filtered_data <- if (include_2020) {
    delay_per_flight
  } else {
    delay_per_flight %>% filter(year != 2020)
  }
  
  delay_types <- c(
    "total_delay_per_flight",
    "carrier_delay_per_flight", 
    "weather_delay_per_flight",
    "nas_delay_per_flight",
    "security_delay_per_flight",
    "late_aircraft_delay_per_flight"
  )
  
  results <- map(delay_types, ~{
    mk_test <- MannKendall(filtered_data[[.x]])
    tibble(
      Delay_Type = str_remove(.x, "_per_flight") %>% 
        str_replace_all("_", " ") %>% 
        str_to_title(),
      Tau = mk_test$tau,
      p_value = mk_test$sl,
      Significance = ifelse(p_value < 0.05, "Significant", "Not significant"),
      Years = ifelse(include_2020, "2013-2023", "2013-2019 & 2021-2023")
    )
  }) %>% bind_rows()
  
  return(results)
}

delay_per_flight <- data %>%
  group_by(year) %>%
  summarise(
    total_flights = sum(arr_flights, na.rm = TRUE),
    total_delay_per_flight = sum(arr_delay, na.rm = TRUE) / total_flights,
    carrier_delay_per_flight = sum(carrier_delay, na.rm = TRUE) / total_flights,
    weather_delay_per_flight = sum(weather_delay, na.rm = TRUE) / total_flights,
    nas_delay_per_flight = sum(nas_delay, na.rm = TRUE) / total_flights,
    security_delay_per_flight = sum(security_delay, na.rm = TRUE) / total_flights,
    late_aircraft_delay_per_flight = sum(late_aircraft_delay, na.rm = TRUE) / total_flights
  )

trends_with_2020 <- analyze_delay_trends(include_2020 = TRUE)
trends_without_2020 <- analyze_delay_trends(include_2020 = FALSE)

combined_trends <- bind_rows(trends_with_2020, trends_without_2020) %>%
  arrange(Delay_Type, Years)

print(combined_trends, n = Inf)







##  SEASONAL TRENDS  ##
monthly_delays <- data %>%
  group_by(year, month) %>%
  summarise(
    total_delay_min = sum(arr_delay, na.rm = TRUE),
    mean_delay_min = mean(arr_delay, na.rm = TRUE),
    n_flights = sum(arr_flights, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month_name = factor(month.abb[month], levels = month.abb) 
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


monthly_delays <- monthly_delays %>%
  mutate(season = ifelse(month %in% 6:8, "Summer", "Rest of year"))
# Wilcoxon-Test 
wilcox.test(
  total_delay_min ~ season,
  data = monthly_delays,
  alternative = "two.sided")

monthly_delays %>%
  group_by(season) %>%
  summarise(mean_delay = sum(total_delay_min, na.rm = TRUE) / sum(n_flights, na.rm = TRUE))



###############################not displayed in paper###########################

ggplot(monthly_delays, aes(x = month_name, y = total_delay_min)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Distribution by months",
    x = "Month",
    y = "Delay (Minutes)"
  ) +
  theme_minimal()
################################################################################



monthly_delays <- monthly_delays %>%
  mutate(
    season = case_when(
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Fall",
      month %in% c(12,1,2) ~ "Winter",
      TRUE ~ NA_character_
    ),
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))
  )

seasonal_stats <- monthly_delays %>%
  group_by(season) %>%
  summarise(
    mean_delay_per_flight = sum(total_delay_min) / sum(n_flights),
    median_delay = median(total_delay_min / n_flights),
    total_flights = sum(n_flights),
    n_months = n(),
    .groups = "drop"
  )

kruskal.test(total_delay_min ~ season, data = monthly_delays)

# Seasonal trends plot
ggplot(monthly_delays, aes(x = year, y = total_delay_min / n_flights, color = season)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~season, nrow = 1) +
  labs(
    title = "Average Delay per Flight by Season",
    y = "Delay (minutes per flight)",
    x = "Year"
  ) +
  scale_x_continuous(breaks = seq(2013, 2023, 1), labels = seq(2013, 2023, 1)) + 
  theme_minimal() +
  theme(
    legend.position = "none", 
    strip.text = element_text(size = 12)
  )

pairwise_result <- pairwise.wilcox.test(monthly_delays$total_delay_min / monthly_delays$n_flights, 
                                        monthly_delays$season, 
                                        p.adjust.method = "bonferroni")
pairwise_result


###############################not displayed in paper###########################

# time series
ts_data <- ts(
  monthly_delays$total_delay_min,
  start = c(2013, 1),
  frequency = 12)
decomposed <- decompose(ts_data, type = "additive")

plot(decomposed)


kruskal.test(total_delay_min ~ month_name, data = monthly_delays)

################################################################################
