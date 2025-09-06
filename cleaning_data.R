# Clearing Environment
rm(list = ls())

# Packages
library(dplyr)
library(httr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# Loading Data.
main_repos_meta_data = read_csv("main_repos_meta_data.csv")
main_repos_commits = read_csv("main_repos_commits.csv")

enhanced_teams = read_csv("dataset_Sep_1_2025/DeepFunding Repos Enhanced via OpenQ - ENHANCED TEAMS.csv")
enhanced_contributors = read_csv("dataset_Sep_1_2025/DeepFunding Repos Enhanced via OpenQ - ENHANCED CONTRIBUTORS.csv")

# Adding logical flags for keywords in the topic and description.
keywords = c("ethereum", "smart contract", "proof of stake", "typescript", "java",
             "python", "javascript", "go", "cpp", "blockchain", "virtual machine",
             "wallet", "web3")

main_repos_meta_data = main_repos_meta_data |>  mutate(description = replace_na(description, ""),
                                                       topics = replace_na(topics, ""), 
                                                       txt = str_to_lower(paste(description, topics, sep = " "))) |>
  mutate(txt = str_replace_all(txt, "-", " ")) |>
  mutate(!!! set_names(lapply(keywords, function(k) {kl <- tolower(k)
  # expression evaluated inside mutate() data mask
  expr(str_detect(.data$txt, fixed(!!kl)))}), paste0("has_", str_replace_all(tolower(keywords), "\\s+", "_")))) |>
  select(-txt)

# Creating commit network.
main_repos_commits = main_repos_commits |> filter(!is.na(user))

main_repos_commits_count = main_repos_commits |> group_by(user, repo) |>
  summarize(commits = n(), .groups = "drop") |> pivot_wider(names_from = repo, values_from = commits, values_fill = list(commits = 0))

write_csv(main_repos_commits_count, "main_repos_commits_count.csv")

# Loading closeness centrality data.
closeness_centrality = read_csv("closeness_centrality.csv")

closeness_centrality = closeness_centrality |> select(repo, closeness)

# Joining with meta data.
main_repos_meta_data = main_repos_meta_data |> inner_join(closeness_centrality, join_by(repo))

# Average commits per day and the mean time between commits.
main_repos_commits = main_repos_commits |> mutate(date = ymd(date))
main_repos_commits_2020 = main_repos_commits |> filter(date >= ymd("2020-01-01"))

main_repos_commits_frequency = main_repos_commits_2020 |> arrange(repo, date) |>
  group_by(repo) |> summarize(total_commits = n(), first_date = min(date),
                              last_date = max(date), days_active = as.numeric(difftime(last_date, first_date, units = "days")) +1,
                              avg_commits_per_day = total_commits / days_active,
                              mean_days_between_commits = if (n_distinct(date) > 1) mean(diff(sort(unique(date)))) else NA_real_, 
                              .groups = "drop")

# Switching logical flags to 0 and 1.
main_repos_meta_data = main_repos_meta_data |> mutate(across(starts_with("has_"), as.integer))

# Selecting wanted columns before combining into a final table.
main_repos_meta_data = main_repos_meta_data |> select(!c(description, topics))

main_repos_commits_frequency = main_repos_commits_frequency |> select(!c(total_commits, first_date, last_date, days_active))

# Final table.
master_data_file = inner_join(main_repos_meta_data, main_repos_commits_frequency, join_by(repo))

# Graphing some of the data.
ggplot(data = master_data_file, aes(stars)) + geom_histogram()
ggplot(data = master_data_file, aes(watchers)) + geom_histogram()
ggplot(data = master_data_file, aes(forks)) + geom_histogram()
ggplot(data = master_data_file, aes(contributor_num)) + geom_histogram()
ggplot(data = master_data_file, aes(size)) + geom_histogram()
ggplot(data = master_data_file, aes(closeness)) + geom_histogram()
ggplot(data = master_data_file, aes(avg_commits_per_day)) + geom_histogram()
ggplot(data = master_data_file, aes(mean_days_between_commits)) + geom_histogram()

# Most have large outliers. Now I'm going to normalize the data. Currently using
# the standard score. Not sure I want to use this given how these distributions
# look.
master_data_file = master_data_file |> mutate(stars = (stars - mean(stars))/(sd(stars))) |>
  mutate(watchers = (watchers - mean(watchers))/sd(watchers)) |>
  mutate(forks = (forks - mean(forks))/sd(forks)) |>
  mutate(contributor_num = (contributor_num - mean(contributor_num))/sd(contributor_num)) |>
  mutate(size = (size - mean(size))/sd(size)) |>
  mutate(avg_commits_per_day = (avg_commits_per_day - mean(avg_commits_per_day))/sd(avg_commits_per_day)) |>
  mutate(mean_days_between_commits = (mean_days_between_commits - mean(mean_days_between_commits))/sd(mean_days_between_commits))

# Saving.
write_csv(master_data_file, "master_data_file.csv")
