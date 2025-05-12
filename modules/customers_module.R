customers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Customer Type Filter",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        selectInput(ns("type_filter"), "Customer Type", choices = c("All", "Store", "Individual"))
      )
    ),
    fluidRow(
      box(
        title = "Top Customers by Sales",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput(ns("top_customers_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Customers Overview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        download_ui(ns("download"), label = "Download Customers csv"),
        br(), br(),
        DTOutput(ns("customers_table"))
      )
    )
  )
}

customers_server <- function(id, customers_dim, fact_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive customer table based on filter
    filtered_customers <- reactive({
      if (input$type_filter == "All") {
        customers_dim
      } else if (input$type_filter == "Store") {
        customers_dim %>% filter(isStoreCustomer == TRUE)
      } else {
        customers_dim %>% filter(isStoreCustomer == FALSE)
      }
      customers_dim %>% filter(!is.na(CustomerName))
    })
    
    # Customer table
    output$customers_table <- renderDT({
      datatable(filtered_customers(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    # Download handler
    download_server("download", data = filtered_customers(), sheet_name = "Customers")
    
    # Plot: Top customers by sales
    output$top_customers_plot <- renderPlot({
      fact <- fact_reactive() %>% filter(!is.na(CustomerName))
      fact %>%
        group_by(CustomerName) %>%
        summarise(TotalSales = sum(LineTotal, na.rm = TRUE)) %>%
        slice_max(TotalSales, n = 10, with_ties = FALSE) %>%
        ggplot(aes(x = reorder(CustomerName, TotalSales), y = TotalSales)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 10 Customers by Sales", x = "Customer", y = "Total Sales")
    })
  })
}
