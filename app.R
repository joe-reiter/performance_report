library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(googlesheets4)
library(RMySQL)
library(odbc)
library(DBI)
library(RSQL)

# setup
#gs4_auth("j.reiter@pinehurstcoins.com")
# set up database connection
mydb = dbConnect(MySQL(),
                 user='ae9f0d09_magento',
                 password='PoplarCortexAortasCancan',
                 dbname='ae9f0d09_live',
                 host='Pinehurstcoins.com')



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weekly Performance Report"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tableOutput('table')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("sales")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # query database
  customers.query = dbSendQuery(mydb, "SELECT *
FROM customer_entity")
  customers.original <- DBI::dbFetch(customers.query, n = -1)
  
  # orders query
  orders.query = dbSendQuery(mydb, "SELECT *
FROM sales_flat_order
WHERE created_at > '2021-01-01'")
  orders.original <- DBI::dbFetch(orders.query, n = -1)
  
  # listings query
  magento.query = dbSendQuery(mydb, "SELECT 
    e.entity_id AS 'id',
    e.sku,
    v1.value AS 'name',
    si.qty AS 'stock qty',
    ea.value AS 'status',
    ss.stock_status,
    e.created_at as 'created_at'
FROM
    catalog_product_entity e
        LEFT JOIN
    cataloginventory_stock_item si ON e.entity_id = si.product_id
        LEFT JOIN
    catalog_product_entity_varchar v1 ON e.entity_id = v1.entity_id
    	LEFT JOIN
    catalog_product_entity_int ea ON e.entity_id = ea.entity_id
        LEFT JOIN cataloginventory_stock_status ss ON e.entity_id = ss.product_id
    WHERE ea.attribute_id = 80
        AND v1.store_id = 0
        AND v1.attribute_id = (SELECT 
            attribute_id
        FROM
            eav_attribute
        WHERE
            attribute_code = 'name'
                AND entity_type_id = (SELECT 
                    entity_type_id
                FROM
                    eav_entity_type
                WHERE
                    entity_type_code = 'catalog_product'))")
  magento.original <- DBI::dbFetch(magento.query, n = -1)
  
  # first check to see if today is monday
  monday <- floor_date(Sys.Date(), "week") + 1
  
  # clean customers
  customers <- customers.original %>%
    select(entity_id, group_id, created_at) %>%
    mutate(start_date = as.Date(created_at))
  
  # all wholesale
  all.wholesale <- customers.original %>%
    filter(group_id == 2)
  
  # all retail
  all.retail <- customers.original %>%
    filter(group_id == 3)
  
  # new wholesale customers
  new.customers <- customers %>%
    filter(start_date >= monday)
  
  new.wholesale <- new.customers %>%
    filter(group_id == 2)
  new_wholesale_clients_temp <- nrow(new.wholesale)
  
  new.retail <- new.customers %>%
    filter(group_id == 3)
  new_retail_clients_temp <- nrow(new.retail)
  
  # number of sales Wholesale (qty)
  orders <- orders.original %>%
    select(entity_id, state, customer_id, grand_total, total_qty_ordered, customer_group_id, increment_id, created_at) %>%
    mutate(date = as.Date(created_at))
  
  wholesale.orders <- orders %>%
    filter(customer_group_id == 2,
           state != "cancelled",
           date >= monday)
  
  wholesale_orders <- nrow(wholesale.orders)
  
  wholesale_qty_ordered <- sum(wholesale.orders$total_qty_ordered)
  
  wholesale_sales <- sum(wholesale.orders$grand_total)
  
  ave_wholesale_ticket <- mean(wholesale.orders$grand_total)
  
  # retail sales
  retail.orders <- orders %>%
    filter(customer_group_id == 3,
           state != "cancelled",
           date >= monday)
  
  retail_orders <- nrow(retail.orders)
  
  retail_qty_ordered <- sum(retail.orders$total_qty_ordered)
  
  retail_sales <- sum(retail.orders$grand_total)
  
  ave_retail_ticket <- mean(retail.orders$grand_total)
  
  # number of SKU's added
  magento <- magento.original %>%
    mutate(date = as.Date(created_at)) %>%
    filter(date >= monday)
  
  new_sku <- nrow(magento)
  
  # number of dollars lost to issues fraud/shipping
  #fraud.original <- read_sheet("1jazDlgCW8XiW2nXzdDZpLFTTqOto3eJ_6PUdLoMGJaw", sheet = "USPS Problem Order Archive", range = "A:R", col_types = "ccccccccDccccccccc")
  
  #fraud.temp <- fraud.original[,18]
  
  #fraud_total <- colnames(fraud.temp)
  
  # create results table
  new.wholesale.clients.temp <- data.frame(metric = "New Wholesale Clients",
                                           measure = as.character(new_wholesale_clients_temp))
  
  new.retail.clients.temp <- data.frame(metric = "New Retail Clients",
                                        measure = as.character(new_retail_clients_temp))
  
  wholesale.orders.temp <- data.frame(metric = "Wholesale Orders",
                                      measure = as.character(wholesale_orders))
  
  retail.orders.temp <- data.frame(metric = "Retail Orders",
                                   measure = as.character(retail_orders))
  
  wholesale.qty.ordered.temp <- data.frame(metric = "Wholesale Qty Ordered",
                                           measure = as.character(wholesale_qty_ordered))
  
  retail.qty.ordered.temp <- data.frame(metric = "Retail Qty Ordered",
                                        measure = as.character(retail_qty_ordered))
  
  wholesale.total.sales.temp <- data.frame(metric = "Wholesale Total Sales",
                                           measure = paste("$", wholesale_sales, sep = ""))
  
  retail.total.sales.temp <- data.frame(metric = "Retail Total Sales",
                                        measure = paste("$", round(retail_sales, digits = 2), sep = ""))
  
  wholesale.ave.order <- data.frame(metric = "Average Wholesale Order Amount",
                                    measure = paste("$", round(ave_wholesale_ticket, digits = 2), sep = ""))
  
  retail.ave.order <- data.frame(metric = "Average Wholesale Order Amount",
                                 measure = paste("$", round(ave_retail_ticket, digits = 2), sep = ""))
  
 # fraud.total.temp <- data.frame(metric = "Fraud",
                                 #measure = as.character(fraud_total))
  
  new.sku.temp <- data.frame(metric = "New SKUs",
                             measure = as.character(new_sku))
  
  results0 <- data.frame() 
  results <- results0 %>%
    bind_rows(new.wholesale.clients.temp, new.retail.clients.temp, wholesale.orders.temp, retail.orders.temp, wholesale.qty.ordered.temp,  retail.qty.ordered.temp, wholesale.total.sales.temp, retail.total.sales.temp, wholesale.ave.order, retail.ave.order, new.sku.temp) %>%
    rename(Measure = measure,
           Metric = metric)

    output$sales <- renderPlot({
      # plots
      plot.data <- results0 %>%
        bind_rows(wholesale.total.sales.temp, retail.total.sales.temp) %>%
        mutate(measure = str_replace(measure, "\\$", "")) %>%
        mutate(measure = as.numeric(measure))
      
      sales_plot <- ggplot(plot.data, aes(x = metric, y = measure, fill = metric)) + 
        geom_bar(stat = "identity") + 
        theme_minimal() +
        xlab("Customer Type") +
        ylab("Sales in Dollars") + 
        ggtitle("This Week's Sales") +
        theme(legend.position = "none")
      sales_plot
    })
    
    output$table <- renderTable(results)
}

# Run the application 
shinyApp(ui = ui, server = server)
