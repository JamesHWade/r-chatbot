chat <- function(user_message, 
                 history = NULL,
                 system_prompt = c("general", "code"),
                 api_key = Sys.getenv("OPENAI_API_KEY")) {
  system   <- get_system_prompt(system_prompt)
  prompt   <- prepare_prompt(user_message, system, history)
  base_url <- "https://api.openai.com/v1"
  body     <- list(model = "gpt-3.5-turbo",
                   messages = prompt)
  req <- 
    resp <-
    request(base_url) |>  
    req_url_path_append("chat/completions") |> 
    req_auth_bearer_token(token = api_key) |> 
    req_headers("Content-Type" = "application/json") |> 
    req_user_agent("James Wade @jameshwade | OpenAI tutorial") |> 
    req_body_json(body) |> 
    req_retry(max_tries = 4) |> 
    req_throttle(rate = 15) |> 
    req_perform()
  
  openai_chat_response <- resp |> resp_body_json(simplifyVector = TRUE)
  
  openai_chat_response$choices$message$content
}

get_system_prompt <- function(system = c("general", "code")) {
  rlang::arg_match(system)
  instructions <- 
    switch(system,
           "general" = "You are a helpful assistant.",
           "code"    = "You are a helpful chat bot that answers questions for an R programmer working in the RStudio IDE.")
  list(list(role = "system", content = instructions))
}

prepare_prompt <- function(user_message, system_prompt, history) {
  user_prompt <-  list(list(role = "user", content = user_message))
  c(system_prompt, history, user_prompt) |> purrr::compact()
}

update_history <- function(history, user_message, response) {
  c(history,
    list(
      list(role = "user", content = user_message),
      list(role = "assistant", content = response)
    )
  ) |> purrr::compact()
}