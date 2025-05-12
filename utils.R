xlsx_file <- "./data/Case_Study_Data.xlsx"
rds_file  <- "./data/Case_Study_Data.rds"

# centralized data prep
load_data <- function(xlsx_file) {

  products <- read.xlsx(xlsx_file, sheet = "Product") %>% rename(ProductName = Name)
  subcats  <- read.xlsx(xlsx_file, sheet = "ProductSubCategory") %>% rename(ProductSubcategoryName = Name)
  cats     <- read.xlsx(xlsx_file, sheet = "ProductCategory") %>% rename(ProductCategoryName = Name)
  products_dim <- products %>%
    left_join(subcats, by = "ProductSubcategoryID") %>%
    left_join(cats,    by = "ProductCategoryID")

  store_cust <- read.xlsx(xlsx_file, sheet = "StoreCustomers") %>%
    mutate(
      CustomerName = Name,
      isStoreCustomer = TRUE
    )
  
  indiv_cust <- read.xlsx(xlsx_file, sheet = "IndividualCustomers") %>%
    mutate(
      CustomerName = ifelse(
        is.na(MiddleName) | MiddleName == "",
        paste(FirstName, LastName),
        paste(FirstName, MiddleName, LastName)
      ),
      isStoreCustomer = FALSE
    )
  
  customers_dim <- bind_rows(store_cust, indiv_cust) %>%
    distinct(BusinessEntityID, .keep_all = TRUE)
  
  # Fact table
  sales_det <- read.xlsx(xlsx_file, sheet = "SalesOrderDetail", detectDates = TRUE)
  orders    <- read.xlsx(xlsx_file, sheet = "SalesOrderHeader", detectDates = TRUE)
  fact <- sales_det %>%
    left_join(orders, by = "SalesOrderID", multiple = "all") %>%
    left_join(
      select(products_dim, ProductID, ProductName, ProductSubcategoryName, ProductCategoryName),
      by = "ProductID", multiple = "all"
    ) %>%
    # join on CustomerID present in both
    left_join(
      select(customers_dim, BusinessEntityID, CustomerName),
      by = c("CustomerID" = "BusinessEntityID"), multiple = "all"
    ) %>%
    mutate(OrderDate = as.Date(OrderDate, origin = "1899-12-30"))
  
  list(
    fact          = fact,
    products_dim  = products_dim,
    customers_dim = customers_dim
  )
}

# Load and cache data using load_data()
if (file.exists(rds_file)) {
  data_list <- readRDS(rds_file)
} else {
  data_list <- load_data(xlsx_file)
  saveRDS(data_list, rds_file)
}

fact          <- data_list$fact
products_dim  <- data_list$products_dim
customers_dim <- data_list$customers_dim

customer_names <- unique(customers_dim$CustomerName)
saveRDS(customer_names, "./data/customer_names.rds")

