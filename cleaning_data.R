# Clearing Environment
rm(list = ls())

# Packages
library(dplyr)
library(httr)
library(ggplot2)
library(jsonlite)
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

# Average commits per day.