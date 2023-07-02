library(shiny)
library(bslib)
library(httr2)
library(purrr)
library(glue)

source("helper.R")

ui <- page_sidebar(
  title = "Chatbot with R & OpenAI",
  theme = bs_theme(bootswatch = "vapor"),
  sidebar = sidebar(
    open = "closed",
    selectInput("model", "Model",
                choices = c("gpt-3.5-turbo", "gpt-4")),
    selectInput("task", "Task",
                choices = c("general", "code"))
  ),
  textAreaInput("prompt", NULL, width = "400px"),
  actionButton("chat", NULL, icon = icon("paper-plane"),
               width = "75px",
               class = "m-2 btn-secondary"),
  uiOutput("chat_history")
)

server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$chat_history <- NULL
  observe({
    req(input$prompt != "")
    response <- chat(input$prompt,
                     history = rv$chat_history,
                     system_prompt = input$task)
    rv$chat_history <- update_history(rv$chat_history, input$prompt, response)
    output$chat_history <- renderUI(
      map(rv$chat_history, \(x) markdown(glue("{x$role}: {x$content}")))
    )
  }) |> bindEvent(input$chat)
}

shinyApp(ui, server)
