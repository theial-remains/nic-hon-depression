setwd("C:/Users/Preet/OneDrive - Ursinus College/paid_labor/Summer Fellows 2024")
rm(list = ls())

# function to ensure required packages are installed
check_library <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) { # identify missing packages
      install.packages(pkg)
    }
  }
}

# list of required packages to check (make sure lists are same and correct)
required_packages <- c(
  "readxl",
  "dplyr",
  "janitor",
  "tidyr",
  "CausalImpact",
  "zoo",
  "tidyverse",
  "ggplot2",
  "ggthemes",
  "patchwork",
  "stringr"
)

# call check_library
check_library(required_packages)

# load required libraries
library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(CausalImpact)
library(zoo)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(stringr)

# urls of the data files
urls <- list(
  ihme_dalys = "https://github.com/Azidoe/nic-hon-depression/raw/main/ihme_dalys_2.xlsx",
  nicaragua_rain_cm = "https://github.com/Azidoe/nic-hon-depression/raw/main/nicaragua_rain_cm.xlsx",
  honduras_rain_cm = "https://github.com/Azidoe/nic-hon-depression/raw/main/honduras_annual_rain_cm.xlsx",
  mean_sat_nic_hon = "https://github.com/Azidoe/nic-hon-depression/raw/main/mean_surface_air_temp_nic_hon.xlsx",
  efotw_2023 = "https://github.com/Azidoe/nic-hon-depression/raw/main/efotw-2023.xlsx",
  stringency = "https://github.com/Azidoe/nic-hon-depression/raw/main/stringency_new.xlsx",
  population = "https://github.com/Azidoe/nic-hon-depression/raw/main/age_strat_population.xlsx",
  population_structure = "https://github.com/Azidoe/nic-hon-depression/raw/main/population_proportions.xlsx"
)

# download and read excel files
read_excel_from_url <- function(url, ...) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, temp_file, mode = "wb")
  read_excel(temp_file, ...)
}

# read data from urls
ihme_dalys_2 <- read_excel_from_url(urls$ihme_dalys)
stratified_population <- read_excel_from_url(urls$population)
population_structure <- read_excel_from_url(urls$population_structure)
nicaragua_rain_cm <- read_excel_from_url(urls$nicaragua_rain_cm)
honduras_rain_cm <- read_excel_from_url(urls$honduras_rain_cm)
mean_sat_nic_hon <- read_excel_from_url(urls$mean_sat_nic_hon)
efotw_2023 <- read_excel_from_url(urls$efotw_2023, skip = 4)
stringency <- read_excel_from_url(urls$stringency)

# formatting for ihme_dalys
ihme_dalys <- ihme_dalys_2 %>%
  mutate(measure = ifelse(grepl("DALY", measure), "DALYs", measure)) %>%
  filter((measure == "DALYs" & metric == "Number") | (measure == "Prevalence" & metric == "Percent")) %>%
  clean_names()

# formatting for population and adding age-standardized row
population_structure2 <- population_structure %>%
  group_by(region, year, sex) %>%
  summarize(population_proportion = sum(population_proportion, na.rm = TRUE), .groups = "drop") %>%
  mutate(age = "All ages") %>%
  bind_rows(population_structure, .)

stratified_population2 <- stratified_population %>%
  group_by(region, year, sex) %>%
  summarize(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
  mutate(age = "All ages") %>%
  bind_rows(stratified_population, .)

population <- stratified_population2 %>%
  filter(age != "All ages") %>% # exclude "All ages" for standardization
  inner_join(
    population_structure2 %>%
      filter(age != "All ages") %>% # exclude "All ages" for standardization
      mutate(population_proportion = population_proportion / 100),
    by = c("region", "year", "age", "sex")
  ) %>%
  filter(region %in% c("Nicaragua", "Honduras")) %>%
  mutate(weighted_population = population * population_proportion) %>%
  group_by(region, year, sex) %>%
  summarize(population = sum(weighted_population), .groups = "drop") %>%
  mutate(age = "Age-standardized") %>%
  bind_rows( # bring back "All ages"
    stratified_population2 %>%
      filter(region %in% c("Nicaragua", "Honduras")), .
  ) %>%
  clean_names() %>%
  rename(location = region) %>% # population now has age-standardized and all ages rows
  mutate(population = round(population))

# formatting for rain data
nicaragua_rain_yr <- nicaragua_rain_cm %>%
  clean_names() %>%
  select(-`x5_yr_smooth`) %>%
  mutate(
    year = as.numeric(category),
    nicaracgua_rain_cm = annual_mean
  ) %>%
  select(-category, -annual_mean)

honduras_rain_yr <- honduras_rain_cm %>%
  clean_names() %>%
  select(-`x5_yr_smooth`) %>%
  mutate(
    year = as.numeric(category),
    honduras_rain_cm = annual_mean
  ) %>%
  select(-category, -annual_mean)

# formatting for gender disparity index data
efotw2 <- efotw_2023 %>%
  clean_names() %>%
  filter(countries == "Nicaragua" | countries == "Honduras") %>%
  rename(country = countries) %>%
  select(year, country, gender_disparity_index) %>%
  pivot_wider(names_from = country, values_from = gender_disparity_index) %>%
  rename(
    honduras_gdi = Honduras,
    nicaragua_gdi = Nicaragua
  )

# function to fix age groups in adga2 to match population
clean_age_groups <- function(age) {
  age <- str_trim(age) # error prevention
  age <- gsub(" years", "", age)
  age <- case_when(
    age == "0-4" ~ "0-4",
    age == "5-9" ~ "5-9",
    age == "10-14" ~ "10-14",
    age == "15-19" ~ "15-19",
    age == "20-24" ~ "20-24",
    age == "25-29" ~ "25-29",
    age == "30-34" ~ "30-34",
    age == "35-39" ~ "35-39",
    age == "40-44" ~ "40-44",
    age == "45-49" ~ "45-49",
    age == "50-54" ~ "50-54",
    age == "55-59" ~ "55-59",
    age == "60-64" ~ "60-64",
    age == "65-69" ~ "65-69",
    age == "70-74" ~ "70-74",
    age == "75-79" ~ "75-79",
    age == "80-84" ~ "80-84",
    age == "85-89" ~ "85-89",
    age == "90-94" ~ "90-94",
    age == "95-99" ~ "95-99",
    age == "Age-standardized" ~ "Age-standardized",
    age == "All ages" ~ "All ages"
  )
  return(age)
}

# Formatting for depression/anxiety data with DALYs per capita
# FIXME remove per capita after changing dataset
adga2 <- ihme_dalys %>%
  select(-c("upper", "lower")) %>%
  mutate(age = clean_age_groups(age)) %>%
  filter(year >= 2005) %>%
  filter(!is.na(age)) %>%
  left_join(
    population %>%
      mutate(age = clean_age_groups(age)) %>%
      filter(year >= 2005),
    by = c("location", "year", "age", "sex")
  ) %>%
  mutate(
    val = as.numeric(val) * 100,
    # Calculate DALYs per capita for each country
    val_per_capita = case_when(
      location == "Nicaragua" ~ val / population,
      location == "Honduras" ~ val / population,
      TRUE ~ NA_real_
    ),
    date = as.Date(paste0(year, "-01-01"))
  ) %>%
  pivot_wider(names_from = location, values_from = c(val, val_per_capita, population)) %>%
  rename(
    hon_population = population_Honduras,
    nic_population = population_Nicaragua,
    honduras_per_capita = val_per_capita_Honduras,
    nicaragua_per_capita = val_per_capita_Nicaragua,
    honduras = val_Honduras,
    nicaragua = val_Nicaragua
  ) %>%
  pivot_wider(names_from = cause,
              values_from = c(nicaragua,
                              honduras,
                              honduras_per_capita,
                              nicaragua_per_capita)) %>%
  clean_names()

# add variables
merged_df <- adga2 %>%
  left_join(nicaragua_rain_yr, by = "year") %>%
  left_join(honduras_rain_yr, by = "year") %>%
  left_join(mean_sat_nic_hon, by = "year") %>%
  left_join(efotw2, by = "year") %>%
  clean_names() %>%
  filter(year >= 2005) %>%
  select(-date)

# function to remove mispellings if needed
correction_dict <- c("nicarac" = "nicara") # add more potential misspellings

correct_column_names <- function(col_names, corrections) {
  str_replace_all(col_names, corrections)
}
corrected_merged_df <- merged_df %>%
  rename_with(~ correct_column_names(., correction_dict))

# f-m diff, difference calculated for all changed columns
diff_df <- corrected_merged_df %>%
  filter(sex %in% c("Male", "Female")) %>%
  group_by(measure, metric, age, year) %>%
  mutate(
    # diffs for disorder columns
    nicaragua_major_depressive_disorder = as.numeric(nicaragua_major_depressive_disorder[sex == "Female"]) -
      as.numeric(nicaragua_major_depressive_disorder[sex == "Male"]),
    honduras_major_depressive_disorder = as.numeric(honduras_major_depressive_disorder[sex == "Female"]) -
      as.numeric(honduras_major_depressive_disorder[sex == "Male"]),
    nicaragua_anxiety_disorders = as.numeric(nicaragua_anxiety_disorders[sex == "Female"]) -
      as.numeric(nicaragua_anxiety_disorders[sex == "Male"]),
    honduras_anxiety_disorders = as.numeric(honduras_anxiety_disorders[sex == "Female"]) -
      as.numeric(honduras_anxiety_disorders[sex == "Male"]),
    # diffs for population
    hon_population = as.numeric(hon_population[sex == "Female"]) -
      as.numeric(hon_population[sex == "Male"]),
    nic_population = as.numeric(nic_population[sex == "Female"]) -
      as.numeric(nic_population[sex == "Male"]),
    # diffs for per capita columns
    nicaragua_per_capita_major_depressive_disorder = as.numeric(nicaragua_per_capita_major_depressive_disorder[sex == "Female"]) -
      as.numeric(nicaragua_per_capita_major_depressive_disorder[sex == "Male"]),
    honduras_per_capita_major_depressive_disorder = as.numeric(honduras_per_capita_major_depressive_disorder[sex == "Female"]) -
      as.numeric(honduras_per_capita_major_depressive_disorder[sex == "Male"]),
    nicaragua_per_capita_anxiety_disorders = as.numeric(nicaragua_per_capita_anxiety_disorders[sex == "Female"]) -
      as.numeric(nicaragua_per_capita_anxiety_disorders[sex == "Male"]),
    honduras_per_capita_anxiety_disorders = as.numeric(honduras_per_capita_anxiety_disorders[sex == "Female"]) -
      as.numeric(honduras_per_capita_anxiety_disorders[sex == "Male"]),
    # set the sex column to "f-m_diff"
    sex = "f-m_diff"
  ) %>%
  filter(sex == "f-m_diff") %>%
  ungroup() %>%
  distinct()

# bind diff_df
updated_df <- bind_rows(
  corrected_merged_df %>%
    mutate(
      # convert to numeric
      nicaragua_major_depressive_disorder = as.numeric(nicaragua_major_depressive_disorder),
      honduras_major_depressive_disorder = as.numeric(honduras_major_depressive_disorder),
      nicaragua_anxiety_disorders = as.numeric(nicaragua_anxiety_disorders),
      honduras_anxiety_disorders = as.numeric(honduras_anxiety_disorders),
      nicaragua_per_capita_major_depressive_disorder = as.numeric(nicaragua_per_capita_major_depressive_disorder),
      honduras_per_capita_major_depressive_disorder = as.numeric(honduras_per_capita_major_depressive_disorder),
      nicaragua_per_capita_anxiety_disorders = as.numeric(nicaragua_per_capita_anxiety_disorders),
      honduras_per_capita_anxiety_disorders = as.numeric(honduras_per_capita_anxiety_disorders)
    ),
  diff_df
) %>%
  mutate(age = str_trim(age)) %>% # error prevention
  mutate(date = as.Date(paste0(year, "-01-01"))) %>%
  relocate(date)

# function to create zoo object
create_zoo <- function(df1,
                       yvar,
                       sexname,
                       agename,
                       exclude_cols = NULL,
                       measurename,
                       metricname,
                       pre_start = "2005-01-01", pre_end = "2019-01-01",
                       post_start = "2020-01-01", post_end = "2021-01-01") {
  # select options
  df1_filtered <- df1 %>%
    filter(
      sex == sexname,
      age == agename,
      measure == measurename,
      metric == metricname
    ) %>%
    select(
      -measure,
      -sex,
      -age,
      -metric
    ) %>%
    relocate(all_of(yvar))

  # exclude columns
  if (!is.null(exclude_cols)) {
    df1_filtered <- df1_filtered %>%
      select(-matches(exclude_cols))
  }

  # create zoo object
  zoo_df <- df1_filtered %>%
    select(-date) %>%
    zoo(order.by = as.Date(df1_filtered$date))

  original_series <- zoo_df[, yvar]

  # Perform causal impact analysis
  pre_period <- as.Date(c(pre_start, pre_end))
  post_period <- as.Date(c(post_start, post_end))

  impact <- CausalImpact(zoo_df, pre_period, post_period)

  # Extract lower and upper bounds from CausalImpact result
  lower_bound <- impact$series$point.pred.lower
  upper_bound <- impact$series$point.pred.upper

  return(list(
    impact = impact,
    original = original_series,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  ))
}

# updated function to generate long data
generate_long_data <- function(dataframe,
                               gender,
                               age_name,
                               measure,
                               metric,
                               yvars) {
  # loop over the list of yvars
  data_list <- lapply(yvars, function(yvar) {
    # make exclude_cols based on yvar content
    exclude_cols <- c()
    if (grepl("nic", yvar)) {
      exclude_cols <- c(exclude_cols, "hon")
    }
    if (grepl("hon", yvar)) {
      exclude_cols <- c(exclude_cols, "nic")
    }
    if (grepl("anx", yvar)) {
      exclude_cols <- c(exclude_cols, "dep")
    }
    if (grepl("dep", yvar)) {
      exclude_cols <- c(exclude_cols, "anx")
    }

    # create zoo object for the current yvar
    zoo_result <- create_zoo(dataframe,
                             yvar = yvar,
                             sexname = gender,
                             agename = age_name,
                             exclude_cols = exclude_cols,
                             measurename = measure,
                             metricname = metric)

    # get data from CausalImpact result
    zoo_data <- zoo_result$impact$series

    # return df for each yvar
    data.frame(
      time = index(zoo_data),
      series = yvar,
      actual = zoo_data$response,
      predicted = zoo_data$point.pred,
      lower = zoo_result$lower_bound,   # Add lower bound
      upper = zoo_result$upper_bound    # Add upper bound
    )
  })

  # Combine all data frames into one
  combined_data <- do.call(rbind, data_list)

  # Convert to long format for ggplot2
  long_data <- pivot_longer(combined_data,
                            cols = c("actual", "predicted", "lower", "upper"),
                            names_to = ".value",
                            values_drop_na = TRUE)

  return(long_data)
}

# Updated plotting function
generate_plot <- function(df,
                          genders,
                          title,
                          yvars, # Added parameter to pass the list of yvars
                          intervention_date = as.Date("2019-01-01"),
                          line_colors = c(
                            "Female" = "#000000",
                            "Male" = "#000000",
                            "Both" = "#000000",
                            "f-m_diff" = "#000000"
                          ),
                          plot_ages = c("Age-standardized"),
                          measurename = "Prevalence",
                          metricname = "Percent",
                          x_axis_label = "Time",
                          y_axis_label = "Value") {
  long_data2 <- lapply(genders, function(gender) {
    lapply(plot_ages, function(plot_age) {
      long_data <- generate_long_data(
        dataframe = df,
        gender = gender,
        age_name = plot_age,
        measure = measurename,
        metric = metricname,
        yvars = yvars # Pass the list of yvars to generate_long_data
      )
      long_data %>%
        mutate(
          # Extract country and disease based on specific patterns
          country = case_when(
            grepl("nicaragua", series) ~ "Nicaragua",
            grepl("honduras", series) ~ "Honduras"
          ),
          disease = case_when(
            grepl("anxiety", series) ~ "Anxiety",
            grepl("depressive", series) ~ "Depression"
          ),
          gender = gender, # create gender and age cols
          age = plot_age
        )
    }) %>%
      bind_rows() # combine age dfs for each gender
  }) %>%
    bind_rows() # combine all dfs

  # create visual
  ggplot(long_data2, aes(x = time, color = gender)) +
    # Add translucent ribbon for error bounds
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender), alpha = 0.2, color = NA) +
    geom_line(aes(y = actual, linetype = "Actual")) +
    geom_line(aes(y = predicted, linetype = "Predicted")) +
    geom_vline(xintercept = intervention_date, linetype = "dashed", color = "#000000") +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = "Gender",
      fill = "Gender",
      linetype = "Line Type"
    ) +
    scale_color_manual(values = line_colors) + # custom colors here
    scale_linetype_manual(
      values = c(
        "Actual" = "solid",
        "Predicted" = "dotted"
      ),
      labels = c(
        "Actual" = "Actual Values",
        "Predicted" = "Predicted Values"
      )
    ) +
    theme_bw() +
    facet_grid(disease ~ country + age) # facet by disease, country, and age
}

# test plot
generate_plot(
  df = updated_df,
  yvars = c(
    "nicaragua_per_capita_anxiety_disorders",
    "honduras_per_capita_anxiety_disorders"
  ),
  genders = c("Female", "Male"),
  title = "test",
  line_colors = c(
    "Female" = "#FD4F6C",
    "Male" = "#0000FF"
  ),
  plot_ages = c("All ages"),
  measurename = "DALYs",
  metricname = "Number"
)


# generates plot for multiple ages
# now with geon_ribbon error bars
generate_plot2 <- function(df,
                           genders,
                           title,
                           yvars, # added yvars parameter
                           intervention_date = as.Date("2019-01-01"),
                           line_colors,
                           plot_ages = c("Age-standardized"),
                           measurename = "Prevalence",
                           metricname = "Percent",
                           x_axis_label = "Time",
                           y_axis_label = "Value") {
  long_data2 <- lapply(genders, function(gender) {
    message(paste("Processing gender:", gender))

    lapply(plot_ages, function(plot_age) {
      message(paste("Processing age group:", plot_age))

      long_data <- generate_long_data(
        dataframe = df,
        gender = gender,
        age_name = plot_age,
        measure = measurename,
        metric = metricname,
        yvars = yvars # pass the list of yvars to generate_long_data
      )

      # Check if long_data is NULL or empty
      if (is.null(long_data) || nrow(long_data) == 0) {
        message(paste("No data returned for gender:",
                      gender,
                      "and age group:", plot_age))
        return(NULL)
      }

      # Display the first few rows of long_data for debugging
      message("Long data preview:")
      print(head(long_data))

      long_data %>%
        separate(series,
                 into = c("country", "disease"),
                 sep = "_",
                 extra = "merge",
                 fill = "right") %>%
        mutate(
          country = case_when(
            grepl("nicaragua", country) ~ "Nicaragua",
            grepl("honduras", country) ~ "Honduras"
          ),
          disease = case_when(
            grepl("anxiety", disease) ~ "Anxiety",
            grepl("depressive", disease) ~ "Depression"
          ),
          gender = gender,
          age = plot_age
        )
    }) %>%
      bind_rows()
  }) %>%
    bind_rows()

  # Check if long_data2 is NULL or empty
  if (is.null(long_data2) || nrow(long_data2) == 0) {
    stop("No data available for the specified criteria.")
  }

  # Display the first few rows of long_data2 for debugging
  message("Final long_data2 preview:")
  print(head(long_data2))

  # Create the plot
  message("Creating the plot...")
  ggplot(long_data2, aes(x = time, color = age)) +
    ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender, group = interaction(age, gender)),
                alpha = 0.2, color = NA) + # Add 'group' to geom_ribbon
    ggplot2::geom_line(aes(y = actual, linetype = "Actual")) +
    ggplot2::geom_line(aes(y = predicted, linetype = "Predicted")) +
    ggplot2::geom_vline(xintercept = intervention_date,
               linetype = "dashed",
               color = "#000000") +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    labs(
      title = title,
      x = x_axis_label,
      y = y_axis_label,
      color = "Age Group",
      linetype = "Line Type"
    ) +
    scale_color_manual(values = line_colors) +
    scale_linetype_manual(
      values = c(
        "Actual" = "solid",
        "Predicted" = "dotted"
      ),
      labels = c(
        "Actual" = "Actual Values",
        "Predicted" = "Predicted Values"
      )
    ) +
    ggplot2::theme_bw() +
    facet_grid(disease ~ country + gender)
}

# test
generate_plot2(
  df = updated_df,
  genders = c("Female", "Male"),
  title = "test",
  yvars = c(
    "nicaragua_per_capita_anxiety_disorders",
    "honduras_per_capita_anxiety_disorders",
    "nicaragua_per_capita_major_depressive_disorder",
    "honduras_per_capita_major_depressive_disorder"
  ),
  intervention_date = as.Date("2019-01-01"),
  line_colors = c(
    "All ages" = "#000000",
    "10-14" = "purple"
  ),
  plot_ages = list(
    "All ages",
    "10-14"
  ),
  measurename = "DALYs",
  metricname = "Number"
)


# aggregate_rows function that returns new rows
aggregate_rows <- function(df, age_values) {
  new_value <- paste0(sub("-.*", "", age_values[1]), "-", sub(".*-", "", age_values[length(age_values)]))
  new_rows <- df %>%
    group_by(measure, sex, year) %>%
    group_modify(~ {
      group_data <- .x
      if (any(group_data$age %in% age_values)) {
        new_row <- group_data[group_data$age == age_values[1], ]
        new_row$age <- new_value
        # identify columns containing "disorder", "population", "per_capita" to sum
        sum_cols <- grep("disorder|population|per_capita", names(group_data), value = TRUE)
        # sum identified columns for all age groups in age_values
        for (col in sum_cols) {
          new_row[[col]] <- mean(group_data[[col]][group_data$age %in% age_values], na.rm = TRUE)
        }
        return(new_row)
      } else {
        stop("Age groups not found in the dataset")
      }
    }) %>%
    ungroup()
  return(new_rows)
}

# generate_more_ages_plot function
# works for multiple aggregate or non-aggregate age groups
# works for multiple genders
generate_more_ages_plot <- function(df,
                                    genders,
                                    title = "No title",
                                    yvars, # added yvars parameter
                                    intervention_date = as.Date("2019-01-01"),
                                    line_colors = c(
                                      "Female" = "#000000",
                                      "Male" = "#000000",
                                      "Both" = "#000000",
                                      "f-m_diff" = "#000000"
                                    ),
                                    plot_ages = c("Age-standardized"),
                                    measurename = "Prevalence",
                                    metricname = "Percent",
                                    x_axis_label = "Time",
                                    y_axis_label = "Value") {
  new_df <- NULL
  plot_ages2 <- c()
  for (age_group in plot_ages) {
    if (length(age_group) > 1) {
      aggregated_rows <- aggregate_rows(df, age_group)
      new_df <- rbind(new_df, aggregated_rows)
      aggregated_label <- paste0(sub("-.*", "", age_group[[1]]), "-", sub(".*-", "", age_group[[length(age_group)]]))
      plot_ages2 <- c(plot_ages2, aggregated_label)
    } else {
      plot_ages2 <- c(plot_ages2, age_group)
    }
  }
  if (is.null(new_df)) {
    new_df2 <- df
    plot_ages2 <- plot_ages
  } else {
    new_df2 <- rbind(df, new_df)
  }
  plot <- generate_plot2(
    df = new_df2,
    genders = genders,
    title = title,
    yvars = yvars, # pass yvars to generate_plot2
    intervention_date = intervention_date,
    line_colors = line_colors,
    plot_ages = plot_ages2,
    measurename = measurename,
    metricname = metricname,
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label
  )
  return(plot)
}

# test
generate_more_ages_plot(
  df = updated_df,
  genders = c("Female", "Male"),
  title = "Test3",
  yvars = c(
    "nicaragua_per_capita_anxiety_disorders",
    "honduras_per_capita_anxiety_disorders",
    "nicaragua_per_capita_major_depressive_disorder",
    "honduras_per_capita_major_depressive_disorder"
  ), # new columns
  plot_ages = list(c("10-14", "15-19"), "20-24", "25-29"),
  measurename = "DALYs",
  metricname = "Number",
  x_axis_label = "Time",
  y_axis_label = "DALYs",
  line_colors = c(
    "10-19" = "red",
    "15-19" = "#00c700",
    "20-24" = "blue")
)



# tests
library(tidyverse)
updated_df %>%
filter(measure == "DALYs", age %in% c("20-24", "25-29", "60-64"), sex == "Female") %>%
select(year, honduras_per_capita_major_depressive_disorder, age) %>%
ggplot( aes(x = year, y = honduras_per_capita_major_depressive_disorder, col = age))+
ggplot2::geom_line()+theme_bw()

names(updated_df)


updated_df %>% filter(sex=="Female",year==2021, measure=="DALYs")%>%
select(year, honduras_per_capita_major_depressive_disorder, age, hon_population, honduras_major_depressive_disorder)
