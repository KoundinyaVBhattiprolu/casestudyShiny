download_ui <- function(id, label = "Download CSV") {
  ns <- NS(id)
  downloadButton(ns("download_btn"), label)
}

download_server <- function(id, data, sheet_name = "data") {
  moduleServer(id, function(input, output, session) {
    output$download_btn <- downloadHandler(
      filename = function() paste0(sheet_name, "_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}
