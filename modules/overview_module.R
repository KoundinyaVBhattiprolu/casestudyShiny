overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("box_total")),
      valueBoxOutput(ns("box_avg")),
      valueBoxOutput(ns("box_count"))
    ),
    fluidRow(
      box(
        title = "Sales Over Time",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput(ns("trend_overview"))
      )
    )
  )
}

overview_server <- function(id, fact) {
  moduleServer(id, function(input, output, session) {
    output$box_total <- renderValueBox({
      valueBox(dollar(sum(fact$LineTotal, na.rm = TRUE)), "Total Sales", icon = icon("dollar-sign"), color = "green")
    })
    output$box_avg <- renderValueBox({
      valueBox(dollar(mean(fact$LineTotal, na.rm = TRUE)), "Average Sale", icon = icon("calculator"), color = "yellow")
    })
    output$box_count <- renderValueBox({
      valueBox(nrow(fact), "Transactions", icon = icon("shopping-cart"), color = "blue")
    })
    output$trend_overview <- renderPlot({
      fact %>%
        group_by(OrderDate) %>%
        summarize(DailySales = sum(LineTotal, na.rm = TRUE)) %>%
        ggplot(aes(x = OrderDate, y = DailySales)) +
        geom_line(color = "steelblue") +
        labs(title = "Sales Over Time", x = "Date", y = "Sales")
    })
  })
}
