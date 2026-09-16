library(shiny)
library(reactable)
library(shinyWidgets)
library(shinycssloaders)

source("R/feed_similarity.R")
feed_data <- read_feed_data()
feed_audits <- lapply(feed_data, audit_nutrients)

ui <- fluidPage(
  titlePanel("Feed Similarity Calculator"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("db", "Select the database:", names(feed_data)),
      uiOutput("feed_selected"),
      uiOutput("nutrients"),
      helpText("Z-score Euclidean distance; only candidates with all selected",
               "nutrients are ranked. The reference itself is excluded."),
      helpText("Similarity is not proof of safe substitution. Databases include",
               "different feed roles, additives and processing variants."),
      uiOutput("dataset_kind"),
      actionButton("action", "Calculate"),
      verbatimTextOutput("summary")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Comparison",
                 textOutput("reference"),
                 withSpinner(reactableOutput("result"), type = 8)),
        tabPanel("Excluded candidates", reactableOutput("excluded")),
        tabPanel("Feed Table", withSpinner(reactableOutput("table"), type = 8)),
        tabPanel("Data quality", reactableOutput("quality"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$db %in% names(feed_data))
    feed_data[[input$db]]
  })

  output$dataset_kind <- renderUI({
    kind <- attr(data(), "dataset_kind")
    if (identical(kind, "demo")) {
      helpText(strong("Demo dataset in use."),
               "Values are synthetic (randomly perturbed within \u00b110%).",
               "Place the original file in data/ to analyze real data.")
    } else {
      helpText("Original dataset in use.")
    }
  })

  output$feed_selected <- renderUI({
    x <- data()
    labels <- paste0(x$feed, " [", sub(".*-s1-", "", x$feed_id), "]")
    pickerInput("feed_selected", "Select the reference feed:",
                choices = setNames(x$feed_id, labels),
                options = list(`live-search` = TRUE))
  })

  output$nutrients <- renderUI({
    audit <- feed_audits[[input$db]]
    choices <- audit$nutrient[audit$status == "eligible"]
    selectInput("nutrients", "Select nutrients:",
                choices = choices,
                selected = intersect(default_nutrients(input$db), choices),
                multiple = TRUE)
  })

  calculation <- eventReactive(input$action, {
    result <- tryCatch(
      feed_distance(data(), input$feed_selected, input$nutrients),
      feed_input_error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        validate(need(FALSE, conditionMessage(e)))
      }
    )
    list(db = input$db, reference_id = input$feed_selected,
         nutrients = input$nutrients, result = result)
  }, ignoreNULL = TRUE)

  current_result <- reactive({
    req(input$action > 0)
    saved <- calculation()
    validate(need(identical(saved$db, input$db) &&
                    identical(saved$reference_id, input$feed_selected) &&
                    identical(saved$nutrients, input$nutrients),
                  "Inputs changed. Click Calculate to refresh the comparison."))
    saved$result
  })

  make_table <- function(x, columns = NULL) {
    reactable(x, filterable = TRUE, defaultPageSize = 30,
              showPageSizeOptions = TRUE, striped = TRUE, highlight = TRUE,
              fullWidth = TRUE, bordered = TRUE,
              defaultColDef = colDef(minWidth = 120), columns = columns)
  }

  output$result <- renderReactable({
    result <- current_result()$ranked
    validate(need(nrow(result) > 0, "No other feed has all selected nutrients."))
    display <- result[c("rank", "feed", "distance", "feed_id", input$nutrients)]
    make_table(display, list(
      feed = colDef(minWidth = 240),
      distance = colDef(format = colFormat(digits = 3)),
      feed_id = colDef(show = FALSE)))
  })

  output$reference <- renderText({
    result <- current_result()
    paste("Reference:", result$reference$feed, "|", result$reference$feed_id)
  })

  output$summary <- renderText({
    result <- current_result()
    paste0("Database: ", input$db,
           "\nScale pool: ", length(result$pool_ids), " records",
           "\nSelected nutrients: ", length(result$nutrients),
           "\nRanked candidates: ", nrow(result$ranked),
           "\nExcluded candidates: ", nrow(result$excluded),
           "\nSelf excluded; ties share rank.",
           "\nValues are rounded for display only.")
  })

  output$excluded <- renderReactable(make_table(current_result()$excluded))
  output$table <- renderReactable(make_table(data(), list(feed = colDef(minWidth = 240))))
  output$quality <- renderReactable(make_table(feed_audits[[input$db]]))
}

shinyApp(ui = ui, server = server)
