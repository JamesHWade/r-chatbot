chat <- function(message, 
                 system, 
                 model    = c("gpt-3.5-turbo", "gpt-4"),
                 api_key  = Sys.getenv("OPENAI_API_KEY"),
                 base_url = Sys.getenv("OPENAI_BASE_URL")) {
  system_prompt <- get_system_prompt()
  model  <- rlang::arg_match(model)
  prompt <- list(
    list(role = "system", content = system_prompt),
    list(role = "user", content = message)
  )
  body <- list(model    = model,
               messages = prompt)
  cli::cli_inform("Sending API query")
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
  cli::cli_inform("Received response")
  response <- 
    resp |> 
    resp_body_json(simplifyVector = TRUE) |> 
    parse_openai_response()
  
  openai_chat_response$choices$message$content
}

get_system_prompt <- function(system = c("general", "code")) {
  rlang::arg_match(system)
  switch(system,
         "general" = "You are a helpful assistant.",
         "code"    = "You are a helpful chat bot that answers questions for an R programmer working in the RStudio IDE.")
}

parse_openai_response <- function(response) {
  
}