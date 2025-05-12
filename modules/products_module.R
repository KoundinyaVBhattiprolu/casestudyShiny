products_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Product Filters",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        selectInput(ns("category_filter"), "Product Category", choices = NULL),
        selectInput(ns("subcategory_filter"), "Product Subcategory", choices = NULL)
      )
    ),
    fluidRow(
      box(
        title = "Top Products by Sales",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput(ns("top_products_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Products Overview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        download_ui(ns("download"), label = "Download Products csv"),
        br(), br(),
        DTOutput(ns("products_table"))
      )
    )
  )
}

products_server <- function(id, products_dim, fact_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      updateSelectInput(session, "category_filter",
                        choices = c("All", unique(products_dim$ProductCategoryName)))
    })
    
    observeEvent(input$category_filter, {
      subcats <- if (input$category_filter == "All") {
        unique(products_dim$ProductSubcategoryName)
      } else {
        products_dim %>%
          filter(ProductCategoryName == input$category_filter) %>%
          pull(ProductSubcategoryName) %>%
          unique()
      }
      
      updateSelectInput(session, "subcategory_filter",
                        choices = c("All", subcats))
    })
    
    filtered_products <- reactive({
      df <- products_dim
      if (input$category_filter != "All") {
        df <- df %>% filter(ProductCategoryName == input$category_filter)
      }
      if (input$subcategory_filter != "All") {
        df <- df %>% filter(ProductSubcategoryName == input$subcategory_filter)
      }
      df
    })
    
    output$products_table <- renderDT({
      datatable(filtered_products(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    download_server("download", data = filtered_products(), sheet_name = "Products")
    
    output$top_products_plot <- renderPlot({
      df <- filtered_products()
      topProducts <- fact_reactive() %>%
        semi_join(df, by = "ProductName") %>%
        group_by(ProductName) %>%
        summarise(TotalSales = sum(LineTotal, na.rm = TRUE)) %>%
        slice_max(TotalSales, n = 10, with_ties = FALSE)
      
      ggplot(topProducts, aes(x = reorder(ProductName, TotalSales), y = TotalSales)) +
        geom_col(fill = "darkgreen") +
        coord_flip() +
        labs(title = "Top 10 Products by Sales", x = "Product", y = "Total Sales")
    })
  })
}