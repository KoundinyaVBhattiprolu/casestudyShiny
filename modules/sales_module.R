sales_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Filters & Grouping",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        dateRangeInput(
          ns("sales_date"),
          "Order Date:",
          start = as.Date("2011-01-01"),
          end = Sys.Date()
        ),
        select_group_ui(
          id = ns("filteredData"),
          btn_reset_label = NULL,
          params  = list(
            list(inputId = "ProductCategoryName",     label = "Product Category"),
            list(inputId = "ProductSubcategoryName", label = "Product Subcategory"),
            list(inputId = "ProductName",            label = "Product Name"),
            list(inputId = "CustomerName",           label = "Customer Name")
          ),
          vs_args = list(
            search = TRUE,
            multiple = TRUE,
            disableSelectAll = FALSE,
            disableOptionGroupCheckbox = TRUE
          )
        ),
        fluidRow(
          column(width = 2,
                 actionButton(ns("add_sale_btn"), "Add New Sale", icon = icon("plus"))
          ),
          column(width = 2,
                 actionButton(ns("filteredData-reset_all"), "Reset All")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Sales Summary",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput(ns("sales_plot"))
      )
    ),
    fluidRow(
      box(title = "Detailed Sales", status = "info", solidHeader = TRUE, width = 12,
          download_ui(ns("download")), br(), br(),
          DTOutput(ns("sales_table")))
    )
  )
}

sales_server <- function(id, fact, products_dim, customers_dim) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Add new sale
    observeEvent(input$add_sale_btn, {
      customer_names <- readRDS("./data/customer_names.rds")
      showModal(modalDialog(
        title = "Add New Sale",
        selectInput(ns("new_category"), "Category", choices = unique(products_dim$ProductCategoryName)),
        selectInput(ns("new_subcategory"), "Subcategory", choices = NULL),
        selectInput(ns("new_product"), "Product", choices = NULL),
        selectizeInput(ns("new_customer"), "Customer", choices = customer_names, options = list(maxOptions = 1000)),
        numericInput(ns("new_quantity"), "Order Quantity", value = 1, min = 1),
        textInput(ns("new_order_id"), "Sales Order ID"),
        textInput(ns("new_order_detail_id"), "Sales Order Detail ID"),
        textInput(ns("new_order_number"), "Sales Order Number"),
        actionButton(ns("confirm_add_sale"), "Submit Sale"),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$new_category, {
      updateSelectInput(session, "new_subcategory",
                        choices = unique(products_dim$ProductSubcategoryName[
                          products_dim$ProductCategoryName == input$new_category]))
    })
    
    observeEvent(input$new_subcategory, {
      updateSelectInput(session, "new_product",
                        choices = unique(products_dim$ProductName[
                          products_dim$ProductSubcategoryName == input$new_subcategory]))
    })
    
    # Confirm add sale logic
    observeEvent(input$confirm_add_sale, {
      # Validation: required fields
      if (input$new_order_id == "" || input$new_order_detail_id == "" || input$new_order_number == "") {
        showNotification("Please fill in all Sales Order fields.", type = "error")
        return(NULL)
      }
      
      # Check for duplicates
      if (input$new_order_detail_id %in% fact$SalesOrderDetailID ||
          input$new_order_number %in% fact$SalesOrderNumber ||
          input$new_order_id %in% fact$SalesOrderID) {
        showNotification("Duplicate Sales Order ID, Detail ID, or Number.", type = "error")
        return(NULL)
      }
      
      # Get ProductID and CustomerID
      new_product_id <- products_dim %>%
        filter(ProductName == input$new_product) %>%
        pull(ProductID) %>%
        first()
      
      new_customer_id <- customers_dim %>%
        filter(CustomerName == input$new_customer) %>%
        pull(BusinessEntityID) %>%
        first()
      
      # Construct and append new row
      new_sale <- data.frame(
        SalesOrderID = as.numeric(input$new_order_id),
        SalesOrderDetailID = as.numeric(input$new_order_detail_id),
        SalesOrderNumber = as.character(input$new_order_number),
        ProductID = new_product_id,
        ProductName = input$new_product,
        ProductCategoryName = input$new_category,
        ProductSubcategoryName = input$new_subcategory,
        CustomerID = new_customer_id,
        CustomerName = input$new_customer,
        OrderQty = input$new_quantity,
        LineTotal = input$new_quantity * 100,
        OrderDate = Sys.Date()
      )
      
      fact <<- bind_rows(fact, new_sale)
      removeModal()
      showNotification("Sale successfully added!", type = "message")
      # Save updated fact data to RDS and XLSX
      data_list$fact <- fact
      saveRDS(data_list, rds_file)
      write.xlsx(fact, xlsx_file, sheetName = "SalesOrderDetail", rowNames = FALSE)
    })
    
    # Sales filtering by date
    filtered_sales <- reactive({
      req(input$sales_date)
      fact %>% filter(OrderDate >= input$sales_date[1] & OrderDate <= input$sales_date[2])
    })
    
    data_filtered <- select_group_server(
      id = "filteredData",
      data = reactive(filtered_sales()),
      vars = reactive(c("ProductCategoryName", "ProductSubcategoryName", "ProductName", "CustomerName"))
    )
    
    # Sales plot
    output$sales_plot <- renderPlot({
      df <- data_filtered()
      ggplot(df, aes(x = OrderDate, y = LineTotal)) +
        geom_line(color = "steelblue") +
        labs(title = "Filtered Sales", x = "Order Date", y = "Sales")
    })
    
    observeEvent(input[["filteredData-reset_all"]], {
      updateDateRangeInput(session, "sales_date",
                           start = as.Date("2011-01-01"),
                           end = Sys.Date())
    })
    
    output$sales_table <- renderDT({
      datatable(data_filtered(),options=list(pageLength=10,scrollX=TRUE),rownames=FALSE)
    })

    download_server("download", data = data_filtered(), sheet_name = "SalesData")
  })
}
