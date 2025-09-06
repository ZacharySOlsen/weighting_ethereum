# Clearing enviroment
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

# Loading list of main repos
main_repos = fromJSON("seedRepos.json") |> unname()
main_repos = tibble(repo = main_repos)
main_repos = main_repos |> mutate(repo = str_remove(repo, "https://github.com/"))

# Token so that I can do 5000 requests per hour.
# token = "add here"

# Fuctions for Github API downloads

# Helper function for the API meta data function
get_contributor_count = function(contributors_url, token = NULL){
  page = 1
  per_page = 100
  total = 0
  
  repeat {
    # Constructing the paginated URL.
    url_2 = paste0(contributors_url, "?per_page=", per_page, "&page=", page)
    
    # Adding token
    res = if(!is.null(token)) {
      GET(url_2, add_headers(Authorization = paste("token", token)))
    } else {
      GET(url_2)
    }
    
    # Handle rate limit
    if (status_code(res) == 403 && grepl("rate limit", content(res, "text", encoding = "UTF-8"))) {
      reset_time <- as.numeric(headers(res)$`x-ratelimit-reset`)
      wait_time <- max(5, reset_time - as.numeric(Sys.time()))
      message("Rate limit hit. Sleeping for ", round(wait_time), " seconds...")
      Sys.sleep(wait_time + 1)
      next
    }
    
    stop_for_status(res)
    parsed = content(res, as = "parsed")
    
    count = length(parsed)
    total = total + count
    
    # If fewer than requested, we've reached the end.
    if(count < per_page) break
    
    page = page + 1
  }
  return(total)
}

# API meta data function
api_data = function(repo, token = NULL) {
  # Pasting owner to API link.
  url = paste0("https://api.github.com/repos/", repo)
  
  # API Call
  res = GET(url, add_headers(Authorization = paste("token", token)))
  
  # Handle 404 and continue
  if (status_code(res) == 404) {
    message("Repo not found: ", repo)
    return(tibble(repo = repo, stars = NA_character_, watchers = NA_character_,
                  forks = NA_character_, contributor_num = NA_character_,
                  size = NA_character_, topic = NA_character_, description = NA_character_))
  }
  
  # Handle rate limit
  if (status_code(res) == 403 && grepl("rate limit", content(res, "text", encoding = "UTF-8"))) {
    reset_time <- as.numeric(headers(res)$`x-ratelimit-reset`)
    wait_time <- max(5, reset_time - as.numeric(Sys.time()))
    message("Rate limit hit. Sleeping for ", round(wait_time), " seconds...")
    Sys.sleep(wait_time + 1)
    next
  }
  
  stop_for_status(res)
  data = content(res, as = "parsed", type = "application/json")
  
  contributor_num = get_contributor_count(data$contributors_url, token)
  
  # Repo Description
  description = if(!is.null(data$description)) data$description else NA_character_
  
  # Repo Topics
  topics_url = paste0("https://api.github.com/repos/", repo, "/topics")
  res_topics = GET(topics_url, add_headers(Authorization = paste("token", token), Accept = "application/vnd.github.mercy-preview+json"))
  
  topics = if (status_code(res_topics) == 200){
    parsed = content(res_topics, as = "parsed", type = "application/json") 
    if(!is.null(parsed$names) && length(parsed$names) > 0){
      paste(parsed$names, collapse = ", ")
    }
  } else{
    NA_character_
  }
  
  # Tibble
  tibble(
    repo = repo,
    stars = data$stargazers_count,
    watchers = data$subscribers_count,
    forks = data$forks_count,
    contributor_num = contributor_num,
    size = data$size,
    description = description,
    topics = topics
  )
}

# Getting commits, the user, and the date for the repos.
get_commit_dates = function(repo, token = NULL, per_page = 100){
  # Initializing Variables to hold the dates and what page to start on.
  all_data = list()
  page = 1
  
  # Repeat is for going over all pages of commits.
  repeat{
    # Putting together the URL for the api.
    url = paste0("https://api.github.com/repos/", repo, "/commits?per_page=", per_page, "&page=", page)
    
    # Getting the information. If else statement is for using a token to not be
    # rate limited.
    res = if(!is.null(token)){
      GET(url, add_headers(Authorization = paste("token", token)))
    } else{
      GET(url)
    }
    
    # Handle 404 (repo not found)
    if (status_code(res) == 404) {
      message("Repository not found: ", repo)
      return(tibble(repo = repo, date = as.Date(NA), user = NA_character_))
    }
    
    # Checking for rate limiting
    if(status_code(res) == 403 && grepl("rate limit", content(res, "text", encoding = "UTF-8"))) {
      reset_time = as.numeric(headers(res)$`x-ratelimit-reset`)
      wait_time = max(5, reset_time - as.numeric(Sys.time()))
      message("Rate limit hit. Sleeping for", round(wait_time), " seconds...")
      Sys.sleep(wait_time + 1)
      next
    }
    
    # Checking if our API request has any errors. If it does this function
    # has R display them.
    stop_for_status(res)
    
    # Grabbing the data as a character string.
    commits = content(res, as = "parsed")
    
    # Quit if the repo has no commits.
    if(length(commits) == 0) break
    
    # Extracting commit data and username.
    page_data = map_dfr(commits, function(commit){
      date = commit$commit$author$date
      user = if(!is.null(commit$author) && !is.null(commit$author$login)){
        commit$author$login
      } else {
        NA_character_
      }
      tibble(
        repo = repo,
        date = as.Date(date),
        user = user
        
      )
    })
    
    all_data[[page]] = page_data
    
    if(length(commits) < per_page) break
    page = page + 1
  }
  return(bind_rows(all_data))
}

# Downloading the two sets of data.
main_repos_meta_data = map_dfr(main_repos$repo, api_data, token = token)
write_csv(main_repos_meta_data, "main_repos_meta_data.csv")

main_repos_commits = map_dfr(main_repos$repo, get_commit_dates, token = token)
write_csv(main_repos_commits, "main_repos_commits.csv")