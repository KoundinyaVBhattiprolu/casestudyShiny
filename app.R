library(shiny)
library(shinydashboard)
library(dplyr)
library(openxlsx)
library(DT)
library(ggplot2)
library(scales)
library(datamods)

source("modules/overview_module.R")
source("modules/download_module.R")
source("modules/sales_module.R")
source("modules/products_module.R")
source("modules/customers_module.R")
source("utils.R")

ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      img(src = "stryker_logo.png", height = "32px"),
      "Sales Intelligence"
    ),
    titleWidth = 250
  ),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview",  tabName = "overview",  icon = icon("dashboard")),
    menuItem("Sales",     tabName = "sales",     icon = icon("chart-bar")),
    menuItem("Customers", tabName = "customers", icon = icon("users")),
    menuItem("Products",  tabName = "products",  icon = icon("boxes"))
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "overview",
      overview_ui("overview")
    ),
    # Sales Tab
    tabItem(
      tabName = "sales",
      sales_ui("sales")
    ),
    # Customers Tab
    tabItem(
      tabName = "customers",
      customers_ui("customers")
    ),
    # Products Tab
    tabItem(
      tabName = "products",
      products_ui("products")
    )
  ))
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    fact = fact
  )
  overview_server("overview", rv$fact)
  sales_server("sales", rv$fact, products_dim, customers_dim)
  customers_server("customers", customers_dim, reactive(rv$fact))
  products_server("products", products_dim, reactive(rv$fact))
}

shinyApp(ui, server)