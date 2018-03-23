ordersUI <- function(id) {
  ns <- NS(id)

  tagList(
    CannaModules::box(
      h1("Active Orders"),
      DT::dataTableOutput(ns("orders"))
    ),
    CannaModules::box(
      h1("Completed Orders"),
      DT::dataTableOutput(ns("complete"))
    )
  )
}

orders <- function(input, output, session, pool, onfleet_orders, trigger_transactions) {
  trigger_orders <- reactiveVal(0)
  orders <- reactiveVal(q_d_orders(pool))
  observe({
    trigger_orders()
    trigger_transactions()
    invalidateLater(10000)
    new <- q_d_orders(pool)
    if (!identical(orders(), new)) {
      orders(new)
    }
  })

  completed <- reactiveVal(q_d_completed_orders(pool))

  observe({
    trigger_orders()
    trigger_transactions()
    invalidateLater(10000)
    new <- q_d_completed_orders(pool)
    if (!identical(completed(), new)) {
      completed(new)
    }
  })

  output$orders <- DT::renderDataTable({
    isolate(orders()) %>%
      mutate_(index = ~as.numeric(idtransaction),
              status = ~if_else(status == 5, "Unconfirmed", "Confirmed"),
              timeIn = ~as.character(as.POSIXct(
                hms::as.hms(timeIn)
              ), "%I:%M %p")) %>%
      select_(~name, ~businessName, ~date, ~timeIn, ~items, ~revenue, ~status, ~index)
  }, server = TRUE, rownames = TRUE, colnames = c("Name", "Business", "Date", "Time", "Items", "Total", "Status", ""),
  selection = "none", options = list(dom = "tp", drawCallback = JS(
    'function() {
        $(".even").removeClass("even").addClass("odd");
  } '
  ), columnDefs = list(
    list(targets = 0:8, className = "dt-center"),
    list(targets = c(0, 8), orderable = FALSE),
    list(targets = 6, render = JS('function(data, type, row, meta) {
                                    return "$" + data.toFixed(2);
                                  }')),
    list(targets = 0, width = "2%"),
    list(targets = 1:2, width = "10%"),
    list(targets = 2, visible = isTRUE(getOption("CannaData_type") == "distribution")),
    list(targets = 3:7, width = "7%"),
    list(targets = 8, width = "5%", render = JS(
      'function(data, type, row, meta) {
        return "<button class = \'btn btn-info add-queue-btn\' onclick = \'CannaDistribution.select(\\"T" + data + "\\")\'>Info</button>";
      }'
    ))
  )
                                                     ))

  ordersTable <- DT::dataTableProxy("orders")

  observe({
    DT::replaceData(ordersTable, orders() %>%
                      mutate_(index = ~idtransaction,
                              status = ~if_else(status == 5, "Unconfirmed", "Confirmed"),
                              timeIn = ~as.character(as.POSIXct(
                                hms::as.hms(timeIn)
                              ), "%I:%M %p")) %>%
                      select_(~name, ~businessName, ~date, ~timeIn, ~items, ~revenue, ~status, ~index), resetPaging = FALSE, rownames = TRUE)
  })


  output$complete <- DT::renderDataTable({
    isolate(completed()) %>%
      mutate_(timeIn = ~as.character(as.POSIXct(
        hms::as.hms(timeIn)
      ), "%I:%M %p")) %>%
      select_(~name, ~businessName, ~timeIn, ~items, ~revenue, ~driver)
  }, server = TRUE, rownames = FALSE, colnames = c("Name", "Business", "Time", "Items", "Total", "Driver"),
  selection = "none", options = list(dom = "tp", drawCallback = JS(
    'function() {
    $(".even").removeClass("even").addClass("odd");
} '
  ), columnDefs = list(
    list(targets = 0:5, className = "dt-center"),
    list(targets = 4, render = JS('function(data, type, row, meta) {
                                  return "$" + data.toFixed(2);
                                  }')),
    list(targets = c(0:1,5), width = "10%"),
    list(targets = 1, visible = isTRUE(getOption("CannaData_type") == "distribution")),
    list(targets = 2:4, width = "7%")
    )
  ))

  completeTable <- DT::dataTableProxy("complete", deferUntilFlush = TRUE)

  observe({
    replaceData(completeTable, completed() %>%
                  mutate_(timeIn = ~as.character(as.POSIXct(
                    hms::as.hms(timeIn)
                  ), "%I:%M %p")) %>%
                  select_(~name, ~businessName, ~timeIn, ~items, ~revenue, ~driver), rownames = FALSE)
  })

  return(orders)
}

customersUI <- function(id) {
  ns <- NS(id)

  tagList(
    CannaModules::box(
      tableTitle("Customers", "plus"),
      DT::dataTableOutput(ns("customers"))
    )
  )
}

customers <- function(input, output, session, pool, trigger_patients) {
  trigger_customers <- reactiveVal(0)
  customers <- reactive({
    trigger_customers()
    trigger_patients()
    q_d_customers(pool)
  })

  observeEvent(input$add_customer, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container').css('text-align', 'center');$('.modal-body').css('overflow','auto');"
        ),
        h1("New Customer"),
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$form(
          id = session$ns("customer-form"),
          textInput(session$ns("name"), "Name", placeholder = "Name"),
          shinyCleave::phoneInput(session$ns("phone"), "Phone #", placeholder = "Phone #"),
          tags$script(
            paste0(
              "$('#",session$ns("name"), ", #", session$ns("phone"),"').prop('required',true);"
            )
          ),
          if (getOption("CannaData_type") == "distribution") {
            textInput(session$ns("businessName"), "Business Name", placeholder = "Business Name")
          } else {
            tagList(
              textInput(session$ns("id"), "ID #", placeholder = "ID #"),
              dateInput(session$ns("birthday"), "Birthday"),
              tags$script(paste0(
                "$('#",session$ns("id"),"').prop('required',true);
                $('#",session$ns("birthday"),"').children('input').prop('required',true).attr('data-parsley-age','');"
              )))
          }
        ),
        footer = parsleyr::submit_form(session$ns("new_customer"), "Submit", class = "btn btn-info add-queue-btn", formId = session$ns("customer-form"))
      )
    )
  })

  observeEvent(input$new_customer, {
    req(input$name, input$phone)
    if (getOption("CannaData_type") == "distribution") {
     req(input$businessName)
    } else {
      req(input$birthday,input$id)
    }
    phone <- as.numeric(gsub("[ ()]", "", input$phone))
    # remove leading 1?
    req(nchar(phone) %in% 10:11, !is.na(phone))

    if (substr(phone, 1, 1) == "1") {
      phone <- substr(phone, 2, nchar(phone))
    }

    if (phone %in% customers()$phone) {
      showModal(
        modalDialog(
          easyClose = TRUE,
          fade = FALSE,
          tags$script(
            "$('.modal-content').addClass('table-container').css('text-align', 'center');$('.modal-body').css('overflow','auto');"
          ),
          h1("Phone number already used by another customer!"),
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal")
        )
      )
      req(FALSE)
    }

    name <- strsplit(input$name, " ")[[1]]
    DBI::dbWriteTable(pool, "patient", data.frame(
      firstName = name[1],
      lastName = if (isTruthy(name[-1])) paste(name[-1]) else NA_character_,
      phone = phone, businessName = if (isTruthy(input$businessName)) input$businessName else NA_character_,
      birthday = if (isTruthy(input$birthday)) mySql_date(input$birthday) else NA_character_,
      id = if (isTruthy(input$id)) input$id else NA_character_,
      verified = 1, addDate = mySql_date(Sys.Date())
    ), append = TRUE)

    trigger_customers(trigger_customers() + 1)
    removeModal()
  })

  output$customers <-  DT::renderDataTable({
    isolate(customers()) %>%
      mutate_(index = ~idpatient) %>%
      select_(~name, ~businessName, ~phone, ~transactions, ~index)
  }, server = TRUE, rownames = FALSE, colnames = c("Name", "Business", "Phone", "Transactions", ""),
  selection = "none", options = list(dom = "tp", drawCallback = JS(
    'function() {
        $(".even").removeClass("even").addClass("odd");
  } '
  ), columnDefs = list(
    list(targets = 0:4, className = "dt-center"),
    list(targets = 0, width = "10%"),
    list(targets = 2, width = "8%", render = JS("
                            function(data, type, row, meta) {
                              return '(' + data.substring(0, 3) + ')' + ' ' + data.substring(3, 6) + '-' + data.substring(6);
                            }")), list(targets = 1, width = "10%"),
    list(targets = 3, width = "3%"), list(targets = 4, width = "5%",
                                          orderable = FALSE, render = JS(
      'function(data, type, row, meta) {
        return "<button class = \'btn btn-info add-queue-btn\' onclick = \'CannaDistribution.select(\\"P" + data + "\\")\'>Info</button>";
      }'
    ))
  )))

  customersTable <- DT::dataTableProxy("customers")

  observe({
    DT::replaceData(customersTable, customers() %>%
                      mutate_(index = ~idpatient) %>%
                      select_(~name, ~businessName,  ~phone, ~transactions, ~index), resetPaging = FALSE, rownames = FALSE)
  })

  return(customers)
}

driversUI <- function(id) {
  ns <- NS(id)

  tagList(
    col(8,
    CannaModules::box(
      tableTitle("Map", "refresh"),
      leafletOutput(ns("map"))
    )),
    col(4,
    CannaModules::box(
      tableTitle("Drivers", "plus"),
      DT::dataTableOutput(ns("drivers"))
    )),
    col(12,
    CannaModules::box(
      h1("Tasks"),
      DT::dataTableOutput(ns("orders"))
    ))
  )
}

drivers <- function(input, output, session, pool, onfleet_orders, teams, orders, trigger_transactions) {
  trigger_drivers <- reactiveVal(0)
  onfleet_drivers <- reactive({
    input$sync
    trigger_drivers()
    onfleetR::onfleet_get_workers(states = 2)
  })
  drivers <- reactive({
    trigger_drivers()
    input$sync
    q_d_drivers(pool) %>%
      mutate_(active = ~onfleetId %in% onfleet_drivers()$id)
  })

  employees <- reactive({
    trigger_drivers()
    input$sync
    q_d_employees(pool)
  })

  observeEvent(input$add_driver, {
    employees <- employees()
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ),
        h1("New Driver"),
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$form(
          id = session$ns("driver-form"),
          selectizeInput(session$ns("employee"), "Employee", choices = structure(employees$idbudtender, names = employees$name), options= list(
            onInitialize = I("function() { this.setValue('');}"),
            placeholder = "Employee"
          )),
          textInput(session$ns("name"), "Name", placeholder = "Name"),
          shinyCleave::phoneInput(session$ns("phone"), "Phone #", placeholder = "Phone #"),
          tags$script("$('#drivers-name, #drivers-phone').prop('readonly', true);")
        ),
        footer = parsleyr::submit_form(session$ns("new_driver"), "Submit", class = "btn btn-info add-queue-btn", formId = session$ns("driver-form"))
      )
    )
  })

  observe({
    req(input$employee)
    employee <- employees()[employees()$idbudtender == input$employee, ]
    updateTextInput(session, "name", value = employee$name)
    updateTextInput(session, "phone", value = employee$phone)
  })

  observeEvent(input$new_driver, {
    req(input$employee)
    employee <- employees()[employees()$idbudtender == input$employee, ]
    onfleet <- onfleet_post_workers(employee$name, employee$phone, teams = teams$id)
    u_d_employee(pool, input$employee, onfleet$id)
    trigger_drivers(trigger_drivers() + 1)
    removeModal()
  })

  observeEvent(input$assign, {
    trigger_drivers(trigger_drivers() + 1)
    req(input$assign$id)
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');"
        ),
        h1("Assign Driver"),
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$form(
        selectizeInput(session$ns("driver"), "Driver", structure(drivers()$onfleetId[drivers()$active], names = drivers()$name[drivers()$active]), options = list(onInitialize = I("function() {this.setValue('');}")))),
        footer = actionButton(session$ns("assign_driver"), "Assign", class = "btn-info add-queue-btn")
      )
    )
  })

  observeEvent(input$assign_driver, {
    req(input$driver)
    req(input$assign$id)
    transaction <- orders()[orders()$idtransaction == input$assign$id, ]
    ### assign task to driver
    if (detectCores() > 1) {
      mcparallel({
        onfleet_put_insert_tasks("workers", input$driver, -1, transaction$onfleetId)
      })
    } else {
      onfleet_put_insert_tasks("workers", input$driver, -1, transaction$onfleetId)
    }

    u_d_driver(pool, transaction$idtransaction, drivers()$idbudtender[drivers()$onfleetId == input$driver])
    trigger_drivers(trigger_drivers() + 1)
    trigger_transactions(trigger_transactions() + 1)
    removeModal()
  })

  output$map <- leaflet::renderLeaflet({

    tasks <- isolate(onfleet_orders()) %>% filter_(~state != 3) %>% pull("location") %>% purrr::map("location") %>%
      unlist

    states <- isolate(onfleet_orders()) %>% filter_(~state != 3) %>% mutate_(state = ~ state + 1) %>% pull("state")

    map <- leaflet() %>% addTiles()

      if (length(tasks) > 0) {
        tasks <- tasks %>% matrix(ncol = 2, byrow = TRUE) %>% as.data.frame() %>%
          rename_(long=~V1, lat=~V2)

      map <- addAwesomeMarkers(map, lng = tasks$long, lat = tasks$lat, popup = c(
        isolate(onfleet_orders())$recipientName
      ), group = "Deliveries", icon = awesomeIcons("child", "fa", markerColor = c("red", "lightgreen", "green")[states])
      )
      }

      if (length(isolate(onfleet_drivers())$location) > 0) {
        tasks <-
          isolate(onfleet_drivers())$location[[1]] %>% {
            data.frame(long = map_dbl(., 1), lat = map_dbl(., 2))
          }

      map <- addAwesomeMarkers(map, lng = tasks$long, lat = tasks$lat, popup = c(
          isolate(onfleet_drivers())$name
        ), group = "Drivers", icon = awesomeIcons("truck", "fa")
      )
      }

    map
  })

  leaflet_proxy <- leafletProxy("map")

  observeEvent(input$sync, {
    trigger_transactions(trigger_transactions() + 1)
  })
  observe({
    input$sync
    clearMarkers(leaflet_proxy)
    tasks <- onfleet_orders() %>% filter_(~state != 3) %>% pull("location") %>% purrr::map("location") %>%
      unlist
    states <- isolate(onfleet_orders()) %>% filter_(~state != 3) %>% mutate_(state = ~ state + 1) %>% pull("state")
    map <- leaflet_proxy

    if (length(tasks) > 0) {
      tasks <- tasks %>% matrix(ncol = 2, byrow = TRUE) %>% as.data.frame() %>%
        rename_(long=~V1, lat=~V2)

      map <- addAwesomeMarkers(map, lng = tasks$long, lat = tasks$lat, popup = c(
        onfleet_orders()$recipientName
      ), group = "Deliveries", icon = awesomeIcons("child", "fa", markerColor = c("red", "lightgreen", "green")[states])
      )
    }

    if (length(onfleet_drivers()$location) > 0) {
      tasks <-
        onfleet_drivers()$location[[1]] %>% {
          data.frame(long = map_dbl(., 1), lat = map_dbl(., 2))
        }

      map <- addAwesomeMarkers(map, lng = tasks$long, lat = tasks$lat, popup = c(
        onfleet_drivers()$name
      ), group = "Drivers", icon = awesomeIcons("truck", "fa")
      )
    }

    map
  })

  output$drivers <-  DT::renderDataTable({
    isolate(drivers()) %>% select_( ~active, ~name, ~phone) %>%
      arrange_(~desc(active))
  }, server = TRUE, rownames = FALSE, colnames = c("", "Name", "Phone"),
  selection = "single", options = list(dom = "tp", pageLength = 6,
                 drawCallback = JS(
    'function() {
        $(".even").removeClass("even").addClass("odd");
  } '
  ), columnDefs = list(
    list(targets = 0:2, className = "dt-center"),
    list(targets = 0, width = "6%", orderable = FALSE,
                                render = JS("
                                  function(data, type, row, meta) {
                                    return '<svg height=\"25\" width=\"25\"><circle cx=\"12.5\" cy=\"12.5\" r=\"10\" stroke-width=\"3\" fill=\"' + (data ? 'green' : 'red') + '\" /></svg>';
                                  }
                                  ")),
    list(targets = 1:2, width = "47%"),
    list(targets = 2, render = JS("
                            function(data, type, row, meta) {
                              return '(' + data.substring(0, 3) + ')' + ' ' + data.substring(3, 6) + '-' + data.substring(6);
                            }"))
  )))

  driversTable <- DT::dataTableProxy("drivers")

  observe({
    DT::replaceData(driversTable, drivers() %>% select_( ~active, ~name, ~phone) %>%
                      arrange_(~desc(active)), resetPaging = FALSE, rownames = FALSE)
  })

  output$orders <- DT::renderDataTable({
    isolate(orders()) %>%
      mutate_(index = ~as.numeric(idtransaction),
              status = ~if_else(status == 5, "Unconfirmed", "Confirmed"),
              timeIn = ~as.character(as.POSIXct(
                hms::as.hms(timeIn)
              ), "%I:%M %p")) %>%
      left_join(isolate(onfleet_orders()), by = c("onfleetId" = "id")) %>%
      left_join(isolate(drivers()) %>% rename_(driver = ~name), by = c("worker" = "onfleetId")) %>%
      select_(~name, ~driver, ~businessName, ~date, ~timeIn, ~status, ~index)
  }, server = TRUE, rownames = FALSE, colnames = c("Name", "Driver", "Business", "Date", "Time", "Status", ""),
  selection = "none", options = list(dom = "tp", drawCallback = JS(
    'function() {
    $(".even").removeClass("even").addClass("odd");
} '
  ), columnDefs = list(
    list(targets = 0:6, className = "dt-center"),
    list(targets = 6, orderable = FALSE),
    list(targets = 0:3, width = "10%"),
    list(targets = 2, visible = isTRUE(getOption("CannaData_type") == "distribution")),
    list(targets = 4:5, width = "5%"),
    list(targets = 1, render = JS(
      'function(data, type, row, meta) {
        return data ? (data + " <i class = \'fa fa-pencil-square-o fa-lg table-icons\' onclick = \'CannaDistribution.assign(" + row[6] + ")\'/>") : ("<button class = \'btn btn-info add-queue-btn\' onclick = \'CannaDistribution.assign(" + row[6] + ")\'>Assign</button>");
      }'
    )),
    list(targets = 6, width = "3%", render = JS(
      'function(data, type, row, meta) {
        return "<button class = \'btn btn-info add-queue-btn\' onclick = \'CannaDistribution.select(\\"T" + data + "\\")\'>Info</button>";
      }'
    ))
  )
  ))

  ordersTable <- DT::dataTableProxy("orders")

  observe({
    DT::replaceData(ordersTable, orders() %>%
                      mutate_(index = ~as.numeric(idtransaction),
                              status = ~if_else(status == 5, "Unconfirmed", "Confirmed"),
                              timeIn = ~as.character(as.POSIXct(
                                hms::as.hms(timeIn)
                              ), "%I:%M %p")) %>%
                      left_join(onfleet_orders(), by = c("onfleetId" = "id")) %>%
                      left_join(drivers() %>% rename_(driver = ~name), by = c("worker" = "onfleetId")) %>%
                      select_(~name, ~driver, ~businessName, ~date, ~timeIn, ~status, ~index), resetPaging = FALSE, rownames = FALSE)
  })

  return(drivers)
}

orderInfoUI <- function(id) {
  ns <- NS(id)

  tagList(div(class = "content",
              tags$form(
                id = ns("onlineSale"),
                class = "form",
                col(6,
                    div(
                      div(class = "name-container",
                          uiOutput(ns(
                            "name"
                          )))),
                    box(h1("Patient Info", style = "width:100%"),
                        DT::dataTableOutput(ns("patient_info")))
                ),
                col(6,
                    div(
                      class = "row",
                      div(
                        class = "add-delete-btn-container",
                        tags$button(id = ns("cancel"), "Cancel",
                                    class = "btn btn-info delete-btn action-button",
                                    style = "width:25%", formnovalidate = NA),
                        parsleyr::submit_form(
                          ns("submit"),
                          "Confirm",
                          formId = ns("onlineSale"),
                          class = "btn btn-info add-queue-btn",
                          style = "width:25%"
                        ),
                        tags$button(id = ns("print"), "Labels",
                                    class = "btn btn-info add-queue-btn action-button",
                                    style = "width: 25%", formnovalidate = NA)
                      )
                    ),
                    box(h1("Order Info", style = "width:100%"),
                        DT::dataTableOutput(ns("order_info")),
                        actionButton(ns("apply_discount"), "Apply Discount",
                                     class = "btn-info add-queue-btn"))
                ),
                col(12,
                    box(
                      tableTitle("Cart", icon = "plus"),
                      DT::dataTableOutput(ns("cart"))
                    ))
              )))
}

orderInfo <- function(input, output, session, pool, transactionId, msg_service_sid,
                      order_info, reload_select, trigger_transactions, printers, settings,
                      bucket, budtenderId) {
  trigger_order_info <- reactiveVal(0)
  sales <- reactive({
    req(transactionId())
    trigger_order_info()
    q_f_online_sale(pool, transactionId())
  })

  discount <- reactive({
    req(transactionId())
    trigger_order_info()
    q_c_active_discounts(pool, transactionId())
  })

  coupon <- reactive({
    x <- q_c_coupons(pool)
    structure(x$id, names = x$name)
  })

  subtotal <- reactive({
    sales() %>% left_join(
      discount(), by = "idsale"
    ) %>% summarise_(
      rev = ~sum(revenue, na.rm = TRUE),
      dis = ~sum(totalDiscount, na.rm = TRUE),
      total = ~ rev - dis
    ) %>%
      pull("total")
  })

  trigger_inventory <- reactiveVal(0)
  inventory <- reactive({
    req(transactionId())
    req(order_info()$facilityNumber)
    input$add_item
    trigger_inventory()
    q_c_current_inventory(pool, order_info()$facilityNumber)
  })

  total <- reactive({
    round(subtotal() * (1 + (settings$tax)/100), 2)
  })

  output$name <- renderUI({
    if (isTruthy(order_info()$name)) {
      h1(order_info()$name[1])
    } else {
      h1("Select an order")
    }
  })

  output$patient_info <- DT::renderDataTable({
    req(transactionId())
    req(order_info())
    order_info() %>%
      mutate_(phone = ~ paste0("(", substring(phone, 1, 3), ") ", substring(phone, 4, 6), "-", substring(phone, 7))) %>%
      select_(Name = ~name, Business = ~businessName, Phone = ~phone, Email = ~email) %>% select_if(function(x) !(all(is.na(x)))) %>% slice(1) %>%
      t()
  }, rownames = TRUE, class = "table dt-row", selection = 'none',
  options = list(dom = "t",
                 columnDefs = list(
                   list(
                     targets = 0,
                     render = JS(
                       "function(data, type, row, meta) {
                       return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
}"
      )
                     )
                     )))

  output$order_info <- DT::renderDataTable({
    req(transactionId())
    req(order_info())

    data.frame(
      Time = as.character(as.POSIXct(
        hms::as.hms(order_info()$timeIn[1])
      ), "%I:%M %p"),
      Status = order_info()$status[1],
      Total = total(),
      Address = paste(order_info()$address[1], order_info()$city[1], order_info()$zip[1], sep = ", ")
    ) %>% t()
  }, rownames = TRUE, class = "table dt-row", selection = 'none',
  options = list(dom = "t", columnDefs = list(
    list(
      targets = 0,
      render = JS(
        "function(data, type, row, meta) {
        return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
}"
      )
      ),
    list(targets = 1,
         render = JS(
           "function(data, type, row, meta) {
           return row[0] === 'Status' ? (parseInt(data) === 5 ? '<span =class \"unconfirmed\">Unconfirmed</span>' : '<span =class \"confirmed\">Confirmed</span>') :
           row[0] === 'Total' ? '$' + parseFloat(data).toFixed(2) : data;
}"
         ))
         )))

  output$cart <- DT::renderDataTable({
    req(transactionId())
    discount <- discount() %>% mutate_(index = ~if_else(is.na(idsale), row_number(), NA_integer_),
                                       name = ~if_else(is.na(name), reason, name)) %>%
      select_( ~ idsale, ~ iddiscount, ~ name, price = ~ totalDiscount, ~ index)

    sales() %>%
      mutate_(
        index = ~ row_number()
      ) %>%
      select_( ~ index, ~ name, ~ type, ~ quantity, price =  ~ revenue, ~idsale) %>%
      bind_rows(discount) %>% arrange_(~idsale, ~desc(price)) %>%
      select_( ~ index, ~ name, ~ type, ~ quantity, ~ price, ~idsale)
  }, rownames = FALSE, colnames = c('', 'Product', 'Type', 'Quantity', 'Price', ''),
  selection = "none", options = list(dom = "tp",
                                     rowCallback = JS(
                                       'function(row, data, index) {
                                       if (!data[3]) {
                                       $(row).addClass("discount");
                                       }
                                       }'
                                     ),
                                     drawCallback = JS(
                                       'function() {
                                       $(".even").removeClass("even").addClass("odd");
                                       } '),
                                      columnDefs = list(list(
                                        targets = 0:4, className = "dt-center"
                                     ),
                                     list(
                                       targets = 5, visible = FALSE
                                     ),
                                     list(
                                       targets = 0, orderable = FALSE,
                                       width = "2.5%",
                                       render = JS(
                                         "function(data, type, row, meta) {
                                         return data ? '<a id = \"edit' + data + '\" ><i class = \"fa fa-pencil fa-2x\" row = \"' + data + '\" onclick = \"CannaDistribution.edit_item(this)\"></i></a>' : '';
                                       }"
                                        )
                                       ),
                                     list(
                                       targets = 1, width = "25%"
                                     ),
                                     list(
                                       targets = 2,
                                       width = "50px",
                                       render = JS(
                                         'function(data, type, row, meta) {
                                         return data ? "<img class=\\"product-image cart-image\\" src = \\"https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/" + data.toLowerCase() + ".svg\\">" : "";
                                       }'
                                        )
                                       ),
                                     list(
                                       targets = 3, width = "50px", render = JS(
                                         "function(data, type, row, meta) {
                                         return data ? ['flower', 'concentrate'].indexOf(row[2]) >= 0 ? data + ' g' : data + ' pkg': '';
                                         }"
                                        )
                                       ),
                                     list(
                                       targets = 4, width = "50px", render = JS(
                                         "function(data, type, row, meta) {
                                         return data ? row[3] ? '$' + data.toFixed(2) : '<span style = \"color:red\">-$' + Math.abs(data).toFixed(2) + '</span>' : '$0';
}"
                                        )
                                       )
                                       )))

  observeEvent(input$add_item, {
    req(transactionId())
    if (order_info()$status == 6) {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script("$('.modal-content').addClass('table-container');"),
        h1("Order has already been confirmed. Cannot change cart.")
      ))
      req(FALSE)
    }
    # add sale
    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        class = "sales-modal",
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');
          $('.sales-modal').parents('.modal-dialog').addClass('sales-dialog');"
        ),
        h1("Add Item"),
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$div(class = "row inventory-container",
                 div(class = "table-container",
                     tags$div(
                       class = "modal-item-list",
                       DT::dataTableOutput(session$ns("inventory"))
                     )
                     )
      )
    )
    )
  })

  observeEvent(input$back_cart, {
    req(transactionId())
    # add sale
    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        fade = FALSE,
        class = "sales-modal",
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');
          $('.sales-modal').parents('.modal-dialog').addClass('sales-dialog');"
        ),
        h1("Add Item"),
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$div(class = "row inventory-container",
                 div(class = "table-container",
                     tags$div(
                       class = "modal-item-list",
                       DT::dataTableOutput(session$ns("inventory"))
                     )
                 )
        )
      )
    )
  })

  observeEvent(input$selected_inventory, {
    req(transactionId())
    info <- inventory()[input$selected_inventory$row, ]
    showModal(
      modalDialog(
        size = 'l',
        easyClose = TRUE,
        fade = FALSE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').css('background-color', '#061726');$('.modal-body').css('overflow','auto');$('.modal-dialog').css('width', '70%');"
        ),
        actionButton(
          session$ns("back_cart"),
          icon("arrow-left", class = "back-modal fa-lg"),
          class = "cart-btn"
        ),
        tags$form(id = session$ns("newItem-frm"),
        add_to_cartUI(session$ns("newItem"), reactive(info$type), reactive(info$name), c("hybrid","indica","sativa")[as.logical(c(
          info$hybrid, info$indica, info$sativa
        ))], coupon = coupon, margin_top = 30)),
        footer = parsleyr::submit_form(session$ns("submit_item"), "Submit", class = "btn-info add-queue-btn", formId = session$ns("newItem-frm"))
      )
    )
  })

  new_item <- callModule(add_to_cart, "newItem", pool, reactive({
    inventory()[input$selected_inventory$row, "type"]
  }), reactive({
    inventory()[input$selected_inventory$row, "idinventory"]
  }), coupon = coupon, strainType = reactive(c("hybrid","indica","sativa")[as.logical(c(
    inventory()[input$selected_inventory$row, "hybrid"],
    inventory()[input$selected_inventory$row, "indica"],
    inventory()[input$selected_inventory$row, "sativa"]
  ))]), edit = TRUE)

  observeEvent(input$apply_discount, {
    req(transactionId())
    if (order_info()$status == 6) {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script("$('.modal-content').addClass('table-container');"),
        h1("Order has already been confirmed. Cannot change cart.")
      ))
      req(FALSE)
    }
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script("$('.modal-content').addClass('table-container');"),
        box(tags$form(id = 'discount-frm', CannaPOS:::discountsUI(session$ns("discount"), coupon, subtotal = subtotal, margin_top = 30))),
        footer = parsleyr::submit_form(session$ns("submit_discount"), "Submit", class = "btn-success btn-add", formId = "discount-frm")
      )
    )
  })

  discountM <- callModule(CannaPOS:::discounts, "discount", coupon = coupon)

  observeEvent(input$submit_discount, {
    req(discountM()$discount)
    if (discountM()$unit == "$") {
      req(abs(discountM()$discount) <= subtotal())
    } else {
      req(abs(discountM()$discount) <= 100)
    }
    i_c_discount(pool, transactionId(), discountM()$idcoupon, NA, discountM()$unit, discountM()$reason, abs(discountM()$discount),
                 if (discountM()$unit == "$") abs(discountM()$discount) else round((abs(discountM()$discount)/100) * sum(subtotal()), 2))
    trigger_order_info(trigger_order_info() + 1)
    removeModal()
  })

  output$inventory <- DT::renderDataTable({
    req(transactionId())
    inventory() %>% select_( ~ -idinventory) %>%
      mutate_(
        index = ~ row_number(),
        strainType = ~ if_else(hybrid == 1, "hybrid/", ""),
        strainType = ~ if_else(
          indica == 1,
          paste0(strainType, "indica/"),
          paste0(strainType, "")
        ),
        strainType = ~ if_else(
          sativa == 1,
          paste0(strainType, "sativa"),
          paste0(strainType, "")
        ),
        strainType = ~ if_else(
          substr(strainType, nchar(strainType), nchar(strainType)) == "/",
          substr(strainType, 1, nchar(strainType) - 1),
          strainType
        ),
        photoPath = ~ if_else(
          !is.na(photoPath),
          paste0(
            "https://s3-us-west-2.amazonaws.com/",
            bucket,
            "/",
            photoPath
          ),
          "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/noneLight.svg"
        )
      ) %>%
      select_( ~ index,
               ~ name,
               ~ description,
               ~ strainType,
               ~ type,
               ~ price,
               ~ photoPath,
               ~ thc, ~ cbd)
  }, rownames = FALSE, selection = "none",
  class = "inventory-table table-hover",
  options = list(
    dom = "ft",
    scrollY = "60vh",
    pageLength = 1000000,
    initComplete = JS(
      'function(settings, json) {
      var outer = $(this.api().table().container());
      // clear Search:
      outer.find(".dataTables_filter").children("label")[0].childNodes[0].nodeValue="";
      // update element
      outer.find(".dataTables_filter").removeClass("dataTables_filter").addClass("input-group stylish-input-group").wrapAll("<div class =\\"modal-search-bar\\">").find("input").unwrap("label").addClass("form-control").attr("placeholder", "Search Items").attr("type", "text").parent("div").append("<span class=\\"input-group-addon\\"><button type=\\"submit\\"><span class=\\"glyphicon glyphicon-search\\"></span></button></span>");
      $("#inventory").find(".dataTables_scrollBody").css("height", ($(window).height() - 435) + "px");
}'
),
columnDefs = list(
  list(
    targets = 0,
    width = "5%",
    render = JS(
      'function(data, type, row, meta) {
      return \'<div class = "add-to-cart"><a><i class="fa fa-plus-circle fa-2x" aria-hidden="true" row = "\' + data + \'" onclick = "CannaDistribution.add(this)"></i></a></div>\';
      }'
          )
    ),
  list(
    targets = 1,
    width = "31%",
    render = JS(
      'function(data, type, row, meta) {
      return "<div class = \\"media\\">" +
      "<a class = \\"pull-left\\"><img class = \\"media-photo\\" src=\\"" + row[6] + "\\"></a>" +
      "<div class = \\"media-body\\">" +
      "<h4 class = \\"title\\">" + data + "</h4>" +
      "<div class = \\"potency\\">" +
      (row[7] ?
      "<span><b>THC:</b>" + row[7] + (["flower", "concentrate"].indexOf(row[4]) !== -1 ? " %" : " mg") + "</span>"
      : "") + (row[8] ?
      "<span><b>CBD:</b>" + row[8] + (["flower", "concentrate"].indexOf(row[4]) !== -1 ? " %" : " mg") + "</span>"
      : "") +
      "</div>" +
      (row[2].length > 100 ?
      ("<p class = \\"description readBox-wrapper\\">" + row[2].substring(0, 100) + "<span class = \\"readBox-target hide\\">" + row[2].substring(101) + "</span><a onclick = \\"CannaPOS.description_toggle(this);\\">More</a></p>") :
      ("<p class = \\"description\\">" + row[2] + "</p>")) +
      "</div>" +
      "</div>";
      }'
             )
    ),
  list(
    targets = 3,
    width = "20%",
    className = "dt-center inv-strain-type",
    render = JS(
      'function(data, type, row, meta) {
      var len = data.split("/").length;
      return data ? data.split("/").map(function(value) {
      return "<img class=\\"product-image\\" src = \\"https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/" + value.toLowerCase() + ".svg\\" style = \\"max-width:" + (100/len) + "%\\">";
      }).join("") : data;
      }'
        )
    ),
  list(
    targets = 4,
    className = "dt-center",
    width = "5%",
    render = JS(
      'function(data, type, row, meta) {
      return "<img class=\\"product-image\\" src = \\"https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/" + data.toLowerCase() + ".svg\\">";
      }'
        )
    ),
  list(
    targets = 5, width = "9%",
    render = JS('function(data,type,row,meta) {
                return "$" + data.toFixed(2);
                }')
),
list(targets = c(2, 6:8), visible = FALSE)
    )
  ))

  observeEvent(input$submit_item, {
    req(transactionId())
    req(new_item()$price)
    req(new_item()$quantity)
    if (isTruthy(new_item()$discount)) {
      req(new_item()$unit)
      req(if_else(new_item()$unit == "$", as.numeric(new_item()$discount) <= new_item()$price,
                  new_item()$discount>=0 && new_item()$discount <=100))
    }
    conn <- pool::poolCheckout(pool)

    DBI::dbBegin(conn)
    i_c_add_sale(
      conn,
      transactionId(),
      inventory()$idinventory[input$selected_inventory$row],
      new_item()$price,
      new_item()$quantity
    )
    if (q_c_quantity(conn, inventory()$idinventory[input$selected_inventory$row]) < 0) {
      # should explicitely mention quantity?
      showModal(session = session,
                modalDialog(easyClose = TRUE,
                            tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
                            tags$script("$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"),
                            h1("Warning! Not enough inventory")))
      DBI::dbRollback(conn)
    } else {
      DBI::dbCommit(conn)

      if (isTruthy(new_item()$discount)) {
        id <- last_insert_id(conn)
        # determine if reason is coupon or not
        i_c_discount(conn,
                     transactionId = transactionId(),
                     saleId = id,
                     discount = abs(new_item()$discount),
                     unit = new_item()$unit,
                     couponId = new_item()$idcoupon,
                     reason = new_item()$reason,
                     totalDiscount = if (new_item()$unit == "$") abs(new_item()$discount) else new_item()$price * (abs(new_item()$discount) /100))
      }

      trigger_order_info(trigger_order_info() + 1)
      if (isTruthy(transactionId())) {
        totalDiscounts <- discount() %>% filter_(~is.na(idsale), ~unit == "%") %>%
          mutate_(totalDiscount = ~ subtotal() * (discount/100))

        if (nrow(totalDiscounts) > 0) {
          u_c_discount_total(pool, totalDiscounts$iddiscount, totalDiscount = totalDiscounts$totalDiscount)
        }
      }
      trigger_order_info(trigger_order_info() + 1)
      totalDiscounts <- discount() %>% filter_(~is.na(idsale), ~unit == "%") %>%
        mutate_(totalDiscount = ~ subtotal() * (discount/100))

      if (nrow(totalDiscounts) > 0) {
        u_c_discount_total(pool, totalDiscounts$iddiscount, totalDiscount = totalDiscounts$totalDiscount)
        trigger_order_info(trigger_order_info() + 1)
      }
      removeModal()

    }
    pool::poolReturn(conn)
  })

  observeEvent(input$edit_item, {
    #### deal with discounts ####
    req(input$edit_item$row)
    if (order_info()$status == 6) {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script("$('.modal-content').addClass('table-container');"),
        h1("Order has already been confirmed. Cannot change cart.")
      ))
      req(FALSE)
    }
    info <- sales() %>% slice_(~as.numeric(input$edit_item$row))
    dis <- discount()[isTRUE(discount()$idsale ==info$idsale),]
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal move-icon"), `data-dismiss` = "modal", style = "padding: 15px"),
        tags$script(
          "$('.modal-content').css('background-color', '#061726');$('.modal-body').css('overflow','auto');$('.modal-dialog').css('width', '70%');"
        ), class = "item-edit",
        add_to_cartUI(session$ns("edit_online"), reactive(info$type), reactive(info$name), discount = dis$discount, unit = dis$unit, reason = if (isTruthy(dis$reason)) dis$reason else dis$idcoupon,
                      quantity = info$quantity, price = info$revenue,
                      coupon = coupons(), margin_top = 30, strainType = c("hybrid", "sativa", "indica")[c(
                        as.logical(c(info$hybrid, info$sativa, info$indica))
                      )]),
        footer = tagList(
          actionButton(session$ns("edit"), "Submit", style = "float:left;", class = "btn-info add-queue-btn"),
          actionButton(session$ns("remove"), "Remove", class = "btn-info delete-btn")
        )
      )
    )
  })

  observeEvent(input$print, {
    req(transactionId())
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').addClass('table-container');"
        ),
        tags$form(
        selectizeInput(session$ns("printer"), "Printer", choices = structure(printers$id, names = printers$name))),
        footer = tags$button(id = session$ns("submit_print"), "Print", class = "btn btn-info add-queue-btn action-button")
      )
    )
  })

  observeEvent(input$submit_print, {
    req(input$printer)
    req(transactionId())
    need_labels <- sales() %>% filter_(~type %in% c("flower", "concentrate"))

    if (detectCores() > 1) {
    mcparallel({
      for (i in seq_len(nrow(need_labels))) {
        print_label(
          inventoryId = need_labels$idinventory[i],
          name = paste0(need_labels$name[i], " (", paste0(c("I", "S", "H")[which(c(
            need_labels$indica[i] ==
              1,
            need_labels$sativa[i] == 1,
            need_labels$hybrid[i] == 1
          ))], collapse = "/"), ")"),
          quantity = paste(need_labels$quantity[i], if (need_labels$type[i] %in% c("flower", "concentrate")) "g" else "pkg"),
          template = system.file(package = "CannaInventory", "templates", "label.html"),
          base_url = base_url,
          width = 1100,
          height = 400,
          printer = input$printer,
          key = gsub("\n  ", "\n", getOption("canna_key"))
        )
      }
    })
    } else {
      for (i in seq_len(nrow(need_labels))) {
        print_label(
          inventoryId = need_labels$idinventory[i],
          name = paste0(need_labels$name[i], " (", paste0(c("I", "S", "H")[which(c(
            need_labels$indica[i] ==
              1,
            need_labels$sativa[i] == 1,
            need_labels$hybrid[i] == 1
          ))], collapse = "/"), ")"),
          quantity = paste(need_labels$quantity[i], if (need_labels$type[i] %in% c("flower", "concentrate")) "g" else "pkg"),
          template = system.file(package = "CannaInventory", "templates", "label.html"),
          base_url = base_url,
          width = 1100,
          height = 400,
          printer = input$printer,
          key = gsub("\n  ", "\n", getOption("canna_key"))
        )
      }
    }
    removeModal()
  })

  coupons <- reactive({
    x <- q_c_coupons(pool)
    reactive(structure(x$id, names = x$name))
  })

  edited_item <- callModule(add_to_cart, "edit_online", pool, {
    req(input$edit_item$row)
    reactive(sales() %>% slice_(~as.numeric(input$edit_item$row)) %>% pull("type"))
  },
  {
    req(input$edit_item$row)
    reactive(sales() %>% slice_(~input$edit_item$row) %>% pull("idinventory"))
  },
  {
    NULL
  },
  {
    NULL
  }, edit = TRUE, coupon = coupons(), strainType = reactive(c("hybrid", "sativa", "indica")[c(
    as.logical(c(sales() %>% slice_(~input$edit_item$row) %>% pull("hybrid"),
                 sales() %>% slice_(~input$edit_item$row) %>% pull("sativa"),
                 sales() %>% slice_(~input$edit_item$row) %>% pull("indica")))
  )]))

  observeEvent(input$edit, {
    ### edit sale
    req(edited_item())
    req(input$edit_item$row)
    info <- sales() %>% slice_(~input$edit_item$row)
    conn <- pool::poolCheckout(pool)
    dbBegin(conn)
    u_c_sale(pool, info$idsale, edited_item()$price, edited_item()$quantity - info$quantity)

    if (length(discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)]) > 0 && isTRUE(edited_item()$discount > 0)) {
      u_c_discount(conn, discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)], edited_item()$discount, edited_item()$unit, edited_item()$idcoupon, edited_item()$reason,
                   if (edited_item()$unit == "$") edited_item()$discount else (edited_item()$discount/100) * edited_item()$price)
    } else if (isTRUE(edited_item()$discount > 0)) {
      i_c_discount(conn, transactionId = transactionId(), saleId = info$idsale,
                   discount = edited_item()$discount, unit= edited_item()$unit, reason = edited_item()$reason, couponId = edited_item()$idcoupon,
                   if (edited_item()$unit == "$") edited_item()$discount else (edited_item()$discount/100) * edited_item()$price)
    } else if (length(discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)]) > 0) {
      d_c_discount(conn, discount()$iddiscount[isTRUE(discount()$idsale == info$idsale)])
    }

    if (q_c_quantity(conn, info$idinventory) < 0) {
      # should explicitely mention quantity?
      showModal(session = session,
                modalDialog(easyClose = TRUE, fade = FALSE,
                            tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
                            tags$script("$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"),
                            h1("Warning! Not enough inventory")))
      DBI::dbRollback(conn)
    } else {
      DBI::dbCommit(conn)
    }

    trigger_order_info(trigger_order_info() + 1)
    removeModal()
  })

  observeEvent(input$remove, {
    ### remove sale
    req(input$edit_item$row)
    info <- sales() %>% slice_(~input$edit_item$row)
    d_c_remove_sale(pool, info$idsale, info$idinventory, info$quantity)
    trigger_order_info(trigger_order_info() + 1)
    removeModal()
  })

  observe({
    status <- order_info()$status[1]
    req(status)
    if (status == 5) {
      updateActionButton(session, "submit", label = "Confirm")
    } else if (T) {
      updateActionButton(session, "submit", label = "Receipt")
    }
  })

  observeEvent(input$submit, {
    ### check status then do right thing
    if (order_info()$status == 5) {
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').addClass('table-container');"
        ),
        h1("Confirm Order"),
        tags$form(
        textAreaInput(session$ns("confirm_msg"), NULL, value = sprintf("Your order has been confirmed. The total is $%s. You will receive message with ETA when driver begins your delivery.",
                                                                       total()),
                      rows = 3)),
        footer = actionButton(session$ns("send_confirm"), "Confirm Order", class = "btn-info add-queue-btn")
      )
    )
    } else {
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');"
          ),
          h1("Print Receipt"),
          tags$form(
            selectizeInput(session$ns("printer"), "Printer", choices = structure(printers$id, names = printers$name))
            ),
          footer = actionButton(session$ns("print_receipt"), "Print", class = "btn-info add-queue-btn")
        )
      )
    }
  })

  observeEvent(input$print_receipt, {
    req(input$printer)

    discount <- discount() %>% mutate_(totalDiscount = ~-1 * totalDiscount) %>%
      left_join(sales() %>% select_(~idsale, ~revenue), by = "idsale") %>%
      mutate_(index = ~if_else(is.na(idsale), row_number(), NA_integer_),
              name = ~if_else(is.na(name), reason, name)) %>%
      select_( ~ idsale, ~ iddiscount, ~ name, price = ~ totalDiscount, ~ index)

    cart <- sales() %>%
      mutate_(
        index = ~ row_number()
      ) %>%
      select_(~idsale, ~ name, ~ type, ~ quantity, price =  ~ revenue, ~ index) %>%
      bind_rows(discount) %>% arrange_(~idsale, ~desc(price)) %>% select_(~-idsale)

    out <- cart %>% summarise_(
      Subtotal = ~round(sum(if_else(is.na(iddiscount), price, 0), na.rm = TRUE), 2),
      Discounts = ~round(sum(if_else(!is.na(iddiscount), abs(price), 0), na.rm = TRUE), 2),
      Tax = ~round((settings$tax/100) * (Subtotal - Discounts), 2),
      Total = ~round(Subtotal - Discounts + Tax, 2)
    ) %>% mutate_all(function(x) {
      format(x, digits = 2, nsmall = 2)
    }) %>% gather()

    out_url <- qr_code(base_url = getOption("CannaData_baseUrl"), key = gsub("\n  ", "\n", getOption("canna_key")),
                       idtransaction = transactionId(), path = "survey", encrypt = TRUE)

    barcode <- tempfile(fileext = ".png")
    zintr::barcode_print(out_url, barcode, symbology = 58)

    ### print receipt
    tmp <- tempfile(fileext = ".html")
    fl <- file(tmp, "w")
    writeLines(as.character(
      shiny::htmlTemplate(
        system.file(package = "CannaPOS", "templates", "reciept.html"),
        name = order_info()$name,
        date = format(Sys.time(), "%b %d %Y, %X"),
        transactionId = transactionId(),
        barcode = barcode, len = nrow(cart),
        cart = shiny::HTML(as.character(
          xtable::print.xtable(hline.after = c(),
                               xtable::xtable(
                                 cart %>%
                                   mutate_(
                                     price = ~as.numeric(price), quantity = ~as.numeric(quantity),
                                     price = ~ if_else(price>0, paste0("$", format(price, digits = 2, nsmall = 2, scientific = FALSE)),
                                                       paste0("-$", format(abs(price), digits = 2, nsmall = 2, scientific = FALSE))),
                                     quantity = ~ if_else(is.na(quantity), "", if_else(
                                       tolower(type) %in% c("flower", "concentrate"),
                                       paste(quantity, "(g)"),
                                       paste(quantity, "(pkg)")
                                     ))
                                   ) %>%
                                   select_(item = ~ name, qty = ~
                                             quantity, price = ~ price) %>%
                                   mutate_all(function(x)
                                     paste0(" ", x, " ")) %>%
                                   bind_rows(
                                     data.frame(
                                       item = c(""),
                                       qty = c("Subtotal", "Discount", "Tax", "Total", "Cash", "Change"),
                                       price = paste0(c("$", "-$", "$", "$", "$", "$"), format(c(gsub(" ","",out$value), as.numeric(substr(input$cash, 2, nchar(input$cash))),
                                                                                                 round(as.numeric(substr(input$cash, 2, nchar(input$cash))) - total(), 2)),
                                                                                               digits = 2, nsmall = 2, scientific = FALSE))
                                     )
                                   )  %>%
                                   rename_(` ` = ~qty, Item = ~item, Price = ~price),
                                 auto = TRUE
                               ),
                               type = "html",
                               print.results = FALSE,
                               include.rownames = FALSE
          )
        ))
      )
    ), fl)
    close(fl)

    out <- tempfile(fileext = ".png")
    phantom_code(tmp, out, "null", "null")
    if (detectCores() >1) {
      mcparallel(gcp_submit(input$printer,
                            "Title",
                            content = out,
                            contentType = "image/png"))
    } else {
      gcp_submit(input$printer,
                 "Title",
                 content = out,
                 contentType = "image/png")
    }
    removeModal()
  })

  observeEvent(input$send_confirm, {
    req(input$confirm_msg)
    req(order_info()$status == 5)
    con <- pool::poolCheckout(pool)
    DBI::dbBegin(con)
    budtender <- if (exists("budtenderId") && isTruthy(budtenderId)) budtenderId$idbudtender else NA
    u_d_confirm_order(con, transactionId(), sales()$idinventory, sales()$quantity, budtender, total(), settings$tax)

    quantities <- vapply(sales()$idinventory, function(x) {
      q_c_quantity(con, x)
    }, numeric(1))

    if (any(quantities < 0)) {
      DBI::dbRollback(con)
      showModal(
        modalDialog(
          easyClose = TRUE, fade = FALSE,
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("There was not enough inventory of ", paste0(sales()$name[quantities < 0], collapse = ", "))
        )
      )
    } else {
      DBI::dbCommit(con)
      showModal(
        modalDialog(
          easyClose = TRUE, fade = FALSE,
          tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ),
          h1("Order confirmed! Remember to assign driver.")
        )
      )
      trigger_order_info(trigger_order_info() + 1)
      trigger_transactions(trigger_transactions() + 1)
      tw_send_message(paste0("+1", order_info()$phone), msg_service_id = msg_service_sid, body = input$confirm_msg)
    }
    pool::poolReturn(con)
  })

  observeEvent(input$cancel, {
    ### present cancellation text option
    ### remove transaction
    showModal(
      modalDialog(
        easyClose = TRUE,
        tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ),
        h1("Cancellation Message"),
        if (order_info()$status == 6) h2("Warning! Order was already confirmed."),
        span(class = "text-msg",
             textAreaInput(session$ns("cancel_msg"), NULL, value = "We are sorry we were unable to process your order. Please contact (xxx)-xxx-xxxx ",
                           rows = 3)
        ),
        footer = actionButton(session$ns("send_cancel"), "Cancel Order", class = "btn-info delete-btn")
      )
    )
  })


  observeEvent(input$send_cancel, {
    req(input$cancel_msg)
    if (order_info()$status == 5) {
      d_f_online_sale(pool, transactionId())
    } else if (order_info()$status == 6) {
      # adjust inventory if we cancel after confirming
      d_f_queue(pool, transactionId())
    }
    reload_select(list(id = NULL, time = Sys.time()))
    trigger_transactions(trigger_transactions() + 1)
    removeModal()
    if (detectCores() > 1) {
      mcparallel({
        onfleet_delete_task(order_info()$onfleetId)
        tw_send_message(paste0("+1", order_info()$phone), msg_service_id = msg_service_sid, body = input$cancel_msg)
      })
    } else {
      onfleet_delete_task(order_info()$onfleetId)
      tw_send_message(paste0("+1", order_info()$phone), msg_service_id = msg_service_sid, body = input$cancel_msg)
    }
  })

}


patientInfoUI <- function(id) {
  ns <- shiny::NS(id)

  tagList(div(
    class = "content",
    if (getOption("CannaData_type") == "retail") {
    col(12,
        div(
          class = "row",
          div(class = "countdown-container notexpired",
              uiOutput(ns("expiration")))
        ))},
    col(6,
        div(
          div(class = "name-container",
              uiOutput(ns(
                "name"
              )))),
        box(tableTitle("Basic Info"),
            DT::dataTableOutput(ns("info"))),
        box(tableTitle("Notes"),
            uiOutput(ns("notes"))
        ),
        tagList(
          box(tableTitle("Medical Info"),
              DT::dataTableOutput(ns("recommendation"))),
          box(
            class = "images",
            tableTitle("Images"),
            col(12,
                col(6,
                    h4("Photo ID"),
                    uiOutput(ns("id_image_out"))),
                col(6,
                    h4("Rec"),
                    uiOutput(ns(
                      "recommendation_image_out"
                    )))
            )
          ))
    ),
    col(6,
        div(
          class = "row",
          div(
            class = "add-delete-btn-container",
            style = "width:100%;",
            div(
              tags$button(
                id = ns("remove"),
                "Delete",
                class = "btn btn-info delete-btn action-button",
                style = "width:20%;",
                formnovalidate = NA
              ),
              tags$button(
                id = ns("start"),
                "Start Transaction",
                class = "btn btn-info add-queue-btn action-button",
                style = "width:20%;"
              ),
              uiOutput(ns("textButton"))
            )
          )
        ),
        tagList(
          box(tableTitle("Preferences"),
              DT::dataTableOutput(ns("preference"))),
          box(h1("Past Products", style = "width:100%;text-align:left;"),style = "overflow:hidden",
              div(style = "margin-top:35px",
                  uiOutput(ns("no_type"), TRUE),
                  c3Output(ns("patient_type"))
              )
          )),
        box(h1("Reward Points"),style = "overflow:hidden",
            div(style = "margin-top:8%",
                c3Output(ns("patient_points"))
            )
        ),
        box(h1("Patient History"),
            CannaModules::patientHistoryUI(ns("frontdesk")))
    )
  ))
}

patientInfo <-
  function(input,
           output,
           session,
           pool,
           patientId,
           bucket,
           max_points,
           base_url,
           msg_service_sid,
           settings,
           orders,
           trigger_transactions,
           reload_select,
           trigger_patients) {
    trigger_patient_info_returning <- reactiveVal(0)
    patient_info_returning <- reactive({
      req(patientId())
      trigger_patient_info_returning()
      q_f_patient_info(pool, patientId())
    })

    locations <- reactive({
      req(patientId())
      q_d_locations(pool, patientId())
    })
    # days until expired
    expired <- reactive({
      req(patientId())
      if (isTruthy(patient_info_returning()$expirationDate)) {
        difftime(patient_info_returning()$expirationDate, Sys.Date())
      } else {
        (-1*ceiling(difftime(patient_info_returning()$birthday, Sys.Date())/365)) - 21
      }
    })

    observeEvent(input$start, {
      req(patientId())

      if (patientId() %in% orders()$idpatient) {
        showModal(
          modalDialog(
            easyClose = TRUE,
            tags$script(
              "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
            ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
            h1(
              "Customer already has active transaction. Click below to view info."
            ),
            footer = actionButton(session$ns("view_order"), "View", class = "btn-info add-queue-btn")
          )
        )
      } else {
        showModal(
          modalDialog(
            easyClose = TRUE,
            tags$script(
              "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
            ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
            h1(
              "Delivery Info"
            ),
            if (sum(nchar(locations()$label)) > 0) {
            tagList(selectizeInput(session$ns("location"), "Past Locations", choices = locations()$label, options = list(
              onInitialize = I("function() {this.setValue('');}"),
              onChange= I("function(value) {
                            if (value) {
                              var parts = value.split(',');
                              $('#customerInfo-address')[0].value = parts[0];
                              $('#customerInfo-city')[0].value = parts[1];
                              $('#customerInfo-zip')[0].value = parts[2];
                              $('#customerInfo-address, #customerInfo-city, #customerInfo-zip').trigger('change');
                            }
                          }")
            )),
            tags$script("$('#customerInfo-location').parents('.form-group').css('margin', '0 auto');"))},
            tags$form(id = session$ns("address-form"),
            textInput(session$ns("address"), "Address", placeholder = "Address"),
            textInput(session$ns("city"), "City", placeholder = "City"),
            textInput(session$ns("zip"), "ZIP", placeholder = "ZIP")),
            tags$script(
              "$('#customerInfo-address, #customerInfo-city, #customerInfo-zip').prop('required', true);"
            ),
            footer = parsleyr::submit_form(session$ns("start_order"), "Submit", class = "btn-info add-queue-btn", formId = session$ns("address-form"))
          )
        )
      }
    })

    observeEvent(input$start_order, {
      req(input$address,input$city,input$zip)
      con <- pool::poolCheckout(pool)

      onfleet <- onfleet_post_tasks(destination = list(
        address = list(unparsed = paste0(input$address, input$zip, collapse = ", "))
      ), recipients = data.frame(name = paste(patient_info_returning()$name),
                                 phone = paste0("+1", patient_info_returning()$phone)))

      i_d_transaction(con, patientId(),
                      facilityNumber = if (isTruthy(patient_info_returning()$expirationDate)) settings$medicalFacilityNumber else settings$recreationalFacilityNumber,
                      address = trimws(input$address), city = trimws(input$city), zip = trimws(input$zip),
                      onfleetId = onfleet$id)
      id <- last_insert_id(con)
      pool::poolReturn(con)
      trigger_transactions(trigger_transactions() + 1)
      removeModal()
      reload_select(list(id = paste0("T", id), time = Sys.time()))
    })

    observeEvent(input$remove, {
      req(patientId())
      if (patientId() %in% orders()$idpatient) {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1(
            "Cannot remove customer while customer is in active transaction."
          )
        ))
      } else {
        showModal(modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Are you sure you want to remove customer?"),
          h1("Data cannot be recovered once removed!"),
          footer = tags$button(id = session$ns("delete"), class = "action-button btn btn-info delete-btn", "Remove")
        ))
      }
    })

    observeEvent(input$view_order, {
      removeModal()
      reload_select(list(id = paste0("T", orders()$idtransaction[orders()$idpatient == patientId()]), time = Sys.time()))
    })

    observeEvent(input$delete, {
      d_f_patient(pool, patientId())
      # trigger reload of selectize
      trigger_patients(trigger_patients() + 1)
      removeModal()
      reload_select(list(id = NULL, time = Sys.time()))
    })

    observeEvent(input$edit_basic_info, {
      showModal(
        modalDialog(
          size = "l",
          easyClose = TRUE,
          class = "edit-basic-info",
          tags$script(
            "$('.modal-lg').css('width', '85%');
            $('.modal-content').addClass('form-horizontal col-lg-12');
            $('.modal-body').css('overflow-y', '-webkit-paged-y');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Edit Basic Info"),
          # add parsley
          tags$form(
            id = session$ns("basic_info_form"),
            col(6,
                input(
                  session$ns("name"),
                  placeholder = "First",
                  label_width = 4,
                  value = patient_info_returning()$firstName,
                  input_width = 8
                ),
                input(
                  session$ns("name2"),
                  placeholder = "Last",
                  label_width = 4,
                  value = patient_info_returning()$lastName,
                  input_width = 8
                ),
                input(
                  session$ns("id"),
                  type = "tel",
                  placeholder = "ID #",
                  label = "ID #",
                  label_width = 4,
                  maxlength = 8,
                  `data-parsley-californiaid` = I(""),
                  value = patient_info_returning()$id,
                  input_width = 8
                ),
                input(
                  session$ns("idExpiration"),
                  # type = "date",
                  placeholder = "ID Expiration",
                  label = "ID EXP",
                  label_width = 4,
                  `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
                  value = format(as.Date(patient_info_returning()$idExpiration), "%m/%d/%Y"),
                  input_width = 8,
                  required = FALSE
                ),
                input(
                  session$ns("birthday"),
                  placeholder = "DOB",
                  label = "DOB",
                  label_width = 4,
                  `data-parsley-year` = I(""),
                  `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
                  value = format(as.Date(patient_info_returning()$birthday), "%m/%d/%Y"),
                  input_width = 8
                ),
                input(
                  session$ns("phone"),
                  type = "tel",
                  placeholder = "Phone",
                  required = FALSE,
                  label_width = 4,
                  input_width = 8,
                  `data-parsley-length` = "[12,14]",
                  `data-parsley-pattern` = "/^[\\d\\+\\-\\.\\(\\)\\/\\s]*$/",
                  value = paste(
                    substr(patient_info_returning()$phone, 1, 3),
                    substr(patient_info_returning()$phone, 4, 6),
                    substr(patient_info_returning()$phone, 7, 10)
                  )
                ),
                tags$script(
                  paste0(
                    'var cleave = new Cleave("#',
                    session$ns("phone"),
                    '", {
                    phone: true, phoneRegionCode: "us"
    })'
      )
                ),
      tags$script(
        paste0(
          'var cleave = new Cleave("#',
          session$ns("idExpiration"),
          '", {
          date: true, datePattern: ["m","d", "Y"]
    })'
        )
                ),
      tags$label("Text Deal", class = "control-label control-label-left col-sm-4"),
      col(7, class = "checkbox checkbox-icons",
          tags$li(
            icon("mobile", "deal-type fa-2x"),
            value = tolower(as.character(as.logical(patient_info_returning()$textDeal))),
            class = if (isTRUE(patient_info_returning()$textDeal)) "selected",
            alt = session$ns("textDeal")
          ))
            ),
      col(6,
          input(
            session$ns("address"),
            placeholder = "Address",
            label_width = 4, required = FALSE,
            value = patient_info_returning()$address
          ),
          input(
            session$ns("city"),
            placeholder = "City",
            label_width = 4,required = FALSE,
            value = patient_info_returning()$city
          ),
          input(
            session$ns("state"),
            placeholder = "State",
            label_width = 4,
            maxlength = 2,required = FALSE,
            `data-parsley-length` = "[2,2]",
            value = patient_info_returning()$state
          ),
          input(
            session$ns("zip"),
            type = "tel",required = FALSE,
            placeholder = "ZIP",
            label = "ZIP",
            label_width = 4,
            `data-pasley-type` = "integer",
            maxlength = 5,
            `data-parsley-length` = '[5,5]',
            onkeypress = "return event.charCode >= 48 && event.charCode <= 57",
            value = patient_info_returning()$zip
          ),
          input(
            session$ns("recommender"),
            placeholder = "Referrer",
            label_width = 4,
            value= patient_info_returning()$recommender,
            required = FALSE
          ),
          input(
            session$ns("email"),
            placeholder = "Email",
            label_width = 4, required = FALSE,
            `data-parsley-type` = "email",
            value = patient_info_returning()$email
          ),
          tags$script(
            HTML("CannaDistribution.enable_buttons()")),
          tags$script(
            paste0(
              'var cleaveDOB = new Cleave("#',
              session$ns("birthday"),
              '", {
              date: true, datePattern: ["m","d", "Y"]
    })'
      )
          ),
      tags$label("Email Deal", class = "control-label control-label-left col-sm-4"),
      col(7, class = "checkbox checkbox-icons",
          tags$li(
            icon("envelope-o", "deal-type fa-2x"),
            value = tolower(as.character(as.logical(patient_info_returning()$emailDeal))),
            class = if (isTRUE(patient_info_returning()$emailDeal)) "selected",
            alt = session$ns("emailDeal")
          )
      ))),
      footer = parsleyr::submit_form(
        session$ns("submit_info_edit"),
        "Submit",
        class = "btn btn-info add-queue-btn action-button",
        formId = session$ns("basic_info_form")
      )
      )
      )
          })

    observeEvent(input$submit_info_edit, {
      req(
        patientId(),
        input$name,
        input$name2,
        input$id,
        input$birthday
      )

      # phone and zip are legit
      # zip
      if (isTruthy(input$zip)) req(nchar(input$zip) == 5,!is.na(as.integer(input$zip)))

      # phone
      # convert to number
      if (isTruthy(input$phone)) {
        phone <- as.numeric(gsub("[ ()]", "", input$phone))
        # remove leading 1?
        req(nchar(phone) %in% 10:11,!is.na(phone))

        if (substr(phone, 1, 1) == "1") {
          phone <- substr(phone, 2, nchar(phone))
        }
      } else {
        phone = NA_character_
      }

      # ID # is legit
      req(is_californiaId(input$id))
      if (isTruthy(input$state)) {
        req(nchar(input$state) == 2)
      }
      # make sure date is date
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", input$birthday))
      u_f_edit_info(
        pool,
        patientId(),
        first = input$name,
        last = input$name2,
        address = input$address,
        id = input$id,
        idExpiration = if (isTruthy(input$idExpiration)) input$idExpiration else NA_character_,
        city = input$city,
        zip = input$zip,
        state = input$state,
        email = input$email,
        phone = phone,
        birthday = input$birthday,
        textDeal = input$textDeal,
        emailDeal = input$emailDeal,
        recommender = if (isTruthy(input$recommender)) input$recommender else NA
      )
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)

      trigger_patients(trigger_patients() + 1)

      removeModal()

    })

    observeEvent(input$edit_medical_info, {
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('form-horizontal').css('width','110%').css('font-size','110%');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Edit Medical Info"),
          tags$form(
            id = session$ns("edit_medical_form"),
            input(
              session$ns("physician"),
              placeholder = "Physician",
              value = patient_info_returning()$physician,
              label_width = 4
            ),
            input(
              session$ns("expirationDate"),
              placeholder = "Exp Date",
              `data-parsley-pattern` = "/^(0?[1-9]|1[012])[\\/\\-](0?[1-9]|[12][0-9]|3[01])[\\/\\-]\\d{4}$/",
              value = format(as.Date(patient_info_returning()$expirationDate), "%m/%d/%Y"),
              label_width = 4
            ),
            tags$script(
              paste0(
                'var cleaveExp = new Cleave("#',
                session$ns("expirationDate"),
                '", {
                date: true, datePattern: ["m","d", "Y"]
    })'
      )
            ),
      input(
        session$ns("recId"),
        type = "tel",
        placeholder = "Medicard Card #",
        value = patient_info_returning()$recId,
        label_width = 4
      ),
      input(
        session$ns("plants"),
        type = "tel", value = 6,
        placeholder = "Max Plants",
        value = patient_info_returning()$plants,
        label_width = 4
      ),
      input(
        session$ns("smokable"),
        type = "tel", value = 2,
        placeholder = "Max Smokable",
        value = patient_info_returning()$smokable,
        label_width = 4
      ),
      input(
        session$ns("medicalCondition"),
        placeholder = "Condition",
        required = FALSE,
        value = patient_info_returning()$medicalCondition,
        label_width = 4
      )
              ),
      footer = parsleyr::submit_form(
        session$ns("submit_medical_edit"),
        "Submit",
        class = "btn btn-info add-queue-btn action-button",
        formId = session$ns("edit_medical_form")
      )
        )
    )
    })

    observeEvent(input$submit_medical_edit, {
      # server side form validation
      req(patientId(),
          input$expirationDate,
          input$physician,
          input$recId,
          input$plants,
          input$smokable)

      # validate date
      req(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$",
                input$expirationDate))

      # validate recId
      # req(nchar(input$update_recId) == 15,!is.na(as.numeric(input$update_recId)))

      ####### update patient Metrc ###############
      # if (getOption("CannaData_state") %in% c("CO", "OR", "MD")) {
      #   metrc_post_patients(settings$medicalFacilityNumber, input$recId, input$startDate, input$endDate,
      #                       input$plants, input$smokable, Sys.Date())
      # }

      u_f_med_info(
        pool,
        patientId(),
        expirationDate = input$expirationDate,
        physician = input$physician,
        medicalCondition = input$medicalCondition,
        recId = input$recId, input$plants, input$smokable
      )

      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      trigger_patients(trigger_patients() + 1)

      removeModal()
    })

    observeEvent(input$edit_preferences_info, {
      showModal(
        modalDialog(
          easyClose = TRUE, size = "l",
          class = "edit-pref-info",
          tags$script("$('.modal-content').addClass('form-horizontal');"),
          h1("Edit Preferences"), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          # checkbox inputs
          tags$form(
            id = session$ns("preference_form"),
            class = "preference-edit",
            h3("Strain Type"),
            tags$div(class = "checkbox checkbox-icons",
                     tags$li(
                       tags$img(
                         src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/hybrid.svg",
                         alt = "Hybrid",
                         id = session$ns("hybrid"),
                         class = "strain-type",
                         value = tolower(as.character(as.logical(patient_info_returning()$hybrid)))
                       )
                     ),
                     tags$li(

                       tags$img(
                         src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/sativa.svg",
                         alt = "Sativa",
                         id = session$ns("sativa"),
                         class = "strain-type",
                         value = tolower(as.character(as.logical(patient_info_returning()$sativa)))
                       )

                     ),
                     tags$li(
                       tags$img(
                         src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/indica.svg",
                         alt = "Indica",
                         id = session$ns("indica"),
                         class = "strain-type",
                         value = tolower(as.character(as.logical(patient_info_returning()$indica)))
                       )
                     )),
            h3("Product Type"),
            tags$div(
              class = "checkbox checkbox-icons",
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/flower.svg",
                  alt = "Flower",
                  id = session$ns("flower"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$flower)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/concentrate.svg",
                  alt = "Concentrate",
                  id = session$ns("concentrate"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$concentrate)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/edible.svg",
                  alt = "Edible",
                  id = session$ns("edible"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$edible)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/beverage.svg",
                  alt = "Beverage",
                  id = session$ns("beverage"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$beverage)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/joint.svg",
                  alt = "Joint",
                  id = session$ns("joint"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$joint)))
                )
              )
            ),
            tags$div(
              class = "checkbox checkbox-icons",
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/soap.svg",
                  alt = "Soap",
                  id = session$ns("soap"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$soap)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/vaporizer.svg",
                  alt = "Vaporizer",
                  id = session$ns("vaporizer"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$vaporizer)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/tincture.svg",
                  alt = "Tincture",
                  id = session$ns("tincture"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$tincture)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/ointment.svg",
                  alt = "Ointment",
                  id = session$ns("ointment"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$ointment)))
                )
              ),
              tags$li(
                tags$img(
                  src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/paraphernalia.svg",
                  alt = "Paraphernalia",
                  id = session$ns("paraphernalia"),
                  class = "product-type",
                  value = tolower(as.character(as.logical(patient_info_returning()$paraphernalia)))
                )
              )
            )
          ), tags$script(HTML("CannaDistribution.enable_icons();")),
          footer = parsleyr::submit_form(
            session$ns("submit_preference_edit"),
            "Submit",
            class = "btn btn-info add-queue-btn action-button",
            formId = session$ns("preference_form")
          )
        )
      )
    })

    observeEvent(input$submit_preference_edit, {
      req(patientId())
      u_f_edit_pref(
        pool,
        patientId(),
        input$indica,
        input$sativa,
        input$hybrid,
        input$flower,
        input$concentrate,
        input$edible,
        input$beverage,
        input$joint,
        input$ointment,
        input$tincture,
        input$paraphernalia,
        input$soap,
        input$vaporizer
      )
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })

    observeEvent(input$edit_images, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script("
                    $('.modal-content').addClass('form-horizontal');"),
        h1("Edit Images"), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        div(
          class = "file-input",
          div(
            class = "form-group",
            tags$label(
              `for` = session$ns("photoIdPath"),
              class = "control-label control-label-left col-sm-3",
              "Photo ID",
              span(class = "req", "*")
            ),
            col(9,
                shiny::fileInput(session$ns("photoIdPath"), NULL))
          ),
          div(
            class = "form-group",
            tags$label(
              `for` = session$ns("photoIdPath"),
              class = "control-label control-label-left col-sm-3",
              "Rec",
              span(class = "req", "*")
            ),
            col(9,
                shiny::fileInput(session$ns("medicalPath"), NULL))
          )
        ),
        footer = tags$button(
          id = session$ns("submit_images"),
          "Submit",
          class = "btn btn-info add-queue-btn action-button"
        )
        ))
    })

    observeEvent(input$submit_images, {
      req(isTruthy(input$photoIdPath) | isTruthy(input$medicalPath))

      if (isTruthy(input$photoIdPath)) {
        photoS3 <-
          paste0(
            paste("photo", input$new_patient, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$photoIdPath$datapath)
          )

        tryCatch(
          aws.s3::put_object(input$photoIdPath$datapath, photoS3, bucket),
          warning = function(w) {
            stop("S3 failed", w)
          }
        )

        u_f_photoId(pool, patientId(), photoS3)

      }

      if (isTruthy(input$medicalPath)) {
        medicalS3 <-
          paste0(
            paste("medical", input$new_patient, Sys.Date(), sep = "_"),
            ".",
            tools::file_ext(input$medicalPath$datapath)
          )

        tryCatch(
          aws.s3::put_object(input$medicalPath$datapath, medicalS3, bucket),
          warning = function(w) {
            stop("S3 failed \n", w)
          }
        )

        u_f_medId(pool, patientId(), medicalS3)
      }
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })

    observe({
      req(getOption("CannaData_type") == "retail")
      if (isTRUE(expired() <= 0)) {
        session$sendCustomMessage("toggle_expiration", TRUE)
      } else {
        session$sendCustomMessage("toggle_expiration", FALSE)
      }
    })

    observeEvent(input$text_form, {
      showModal(modalDialog(
        easyClose = TRUE,
        tags$script(
          "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
        ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
        h1("Enter Phone #"),
        div(class = "center",
            shinyCleave::phoneInput(session$ns("text_phone"), NULL, placeholder = "Phone #", value = patient_info_returning()$phone)),
        footer = tags$button(id = session$ns("send_form"), class = "action-button btn btn-info add-queue-btn", "Send")
      ))
    })

    observeEvent(input$send_form, {
      req(input$text_phone)
      url <- httr::modify_url(
        url = paste0(base_url, "reward/"),
        query = list(
          idpatient = patientId(),
          idpatiente = jwt_encode_sig(jwt_claim(idpatient = patientId()), key = gsub("\n  ", "\n", getOption("canna_key")))
        )
      )

      phone <- as.numeric(gsub("[ ()]", "", input$text_phone))
      # remove leading 1?
      req(nchar(phone) %in% 10:11,!is.na(phone))

      if (substr(phone, 1, 1) == "1") {
        phone <- substr(phone, 2, nchar(phone))
      }

      tw_send_message(paste0("+1", phone), msg_service_id = msg_service_sid, body = paste("Please sign-up at the following link: ", url))
      u_f_phone(pool, patientId(), phone)
      removeModal()

    })

    output$name <- renderUI({
      if (isTruthy(patientId())) {
        h1(
         patient_info_returning()$name
        )
      } else {
        h1("Please select patient")
      }
    })

    output$textButton <- renderUI({
      info <- patient_info_returning()
      req(patientId(), !isTruthy(patient_info_returning()$phone) || all((data.frame(
        check.names = FALSE, stringsAsFactors = FALSE,
        Strain = paste0(c("Indica", "Sativa", "Hybrid")[which(c(info$indica ==
                                                                  1, info$sativa == 1, info$hybrid == 1))], collapse = "/"),
        Product = paste0(c(
          "Flower", "Concentrate", "Edible", "Beverage", "Joint", "Ointment", "Tincture", "Paraphernalia", "Soap", "Vaporizer"
        )[which(c(
          info$flower == 1,
          info$concentrate ==
            1,
          info$edible ==
            1,
          info$beverage ==
            1,
          info$joint == 1,
          info$ointment == 1,
          info$tincture == 1,
          info$paraphernalia == 1,
          info$soap == 1,
          info$vaporizer == 1
        ))],
        collapse = "/")
      ) %>% unlist %>% nchar) == 0))
      actionButton(session$ns("text_form"), "Rewards", class = "btn btn-info add-queue-btn", width = "20%")
    })

    # status
    output$expiration <- renderUI({
      if (isTruthy(patient_info_returning()$expirationDate)) {
        if (isTRUE(expired() <= 0)) {
          # bad
          tagList(h2("Expired!!!"),
                  p(paste(
                    "medical card expired", abs(expired()), "days ago"
                  )))
        } else {
          # good
          tagList(h2(expired()),
                  p("days until medical card expires"))
        }
      } else {
        if (isTRUE(expired() <= 0)) {
          tagList(h2("STOP! Customer is under 21! STOP!"))
        } else {
          tagList(h2("Customer is", expired() + 21, "years old"))
        }
      }
    })

    output$info <- DT::renderDataTable(
      patient_info_returning() %>%
        mutate_(
          emailDeal = ~ if_else(emailDeal == 1, "YES", "NO"),
          textDeal = ~ if_else(textDeal == 1, "YES", "NO"),
          birthday = ~ paste0(format(as.Date(birthday), "%m/%d/%Y"), " (", age, " years old)"),
          recommender = ~ if_else(recommender=="", NA_character_, recommender),
          idExpiration = ~format(as.Date(idExpiration), "%m/%d/%Y"),
          phone = ~ paste0("(", substring(phone, 1, 3), ") ", substring(phone, 4, 6), "-", substring(phone, 7))
        ) %>%
        select_(
          # Name = ~ name,
          DOB = ~ birthday,
          Address = ~ address,
          City = ~ city,
          Zip = ~ zip,
          `ID #` = ~ id,
          `ID Exp` = ~ idExpiration,
          Email = ~ email,
          `Deals by Email` = ~ emailDeal,
          Phone = ~ phone,
          `Deals by Text` = ~ textDeal,
          `Referred By` = ~recommender
        ) %>%
        t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                     "N/A")),
      options = list(dom = 't', pageLength = 11, columnDefs = list(
        list(
          targets = 0,
          render = JS(
            "function(data, type, row, meta) {
            return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
    }"
          )
          ),
        list(targets = 1, className = "dt-left", render = JS(
          "function(data, type, row, meta) {
          return row[0] === 'Email' ? '<span style = \"word-break: break-all;\" />' + data + '</span>' : row[0] === 'DOB' ? parseInt(data.substring(12, 15)) < 21 ? '<span style = \"color:red\"/>' + data + '</span>' : data : data;
  }"
        ))
        )),
      rownames = TRUE,
      class = "table dt-row", selection = 'none'
      )

    output$notes <- renderUI({
      tags$textarea(if_else(is.na(patient_info_returning()$comment), "", patient_info_returning()$comment), readonly = TRUE, rows = 3,
                    style = "width: 100%; border-radius: 5px;color:black", placeholder = "No notes recorded")
    })

    observeEvent(input$edit_notes, {
      req(patientId())
      showModal(
        modalDialog(
          easyClose = TRUE,
          tags$script(
            "$('.modal-content').addClass('table-container');$('.modal-body').css('overflow','auto');"
          ), tags$span(icon("times", class = "close-modal"), `data-dismiss` = "modal"),
          h1("Edit Notes"),
          span(class = "text-modal-wrapper",
               textAreaInput(session$ns("edit_note"), NULL, rows = 3, value = if_else(is.na(patient_info_returning()$comment), "", patient_info_returning()$comment))
          ), footer = actionButton(session$ns("submit_note"), "Submit", class = "btn btn-info add-queue-btn", placeholder = "No notes recorded")
        )
      )
    })

    observeEvent(input$submit_note, {
      req(patientId())
      u_f_note(pool, patientId(), input$edit_note)
      trigger_patient_info_returning(trigger_patient_info_returning() + 1)
      removeModal()
    })

    output$preference <- DT::renderDataTable({
      info <- patient_info_returning()
      data.frame(
        check.names = FALSE,
        Strain = paste0(c("Indica", "Sativa", "Hybrid")[which(c(info$indica ==
                                                                  1, info$sativa == 1, info$hybrid == 1))], collapse = "/"),
        Product = paste0(c(
          "Flower", "Concentrate", "Edible", "Beverage", "Joint", "Ointment", "Tincture", "Paraphernalia", "Soap", "Vaporizer"
        )[which(c(
          info$flower == 1,
          info$concentrate ==
            1,
          info$edible ==
            1,
          info$beverage ==
            1,
          info$joint == 1,
          info$ointment == 1,
          info$tincture == 1,
          info$paraphernalia == 1,
          info$soap == 1,
          info$vaporizer == 1
        ))],
        collapse = "/")
      ) %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                         "N/A"))

    }, options = list(dom = 't', columnDefs = list(
      list(
        targets = 0,
        render = JS(
          "function(data, type, row, meta) {
          return '<span class = \\'dt-rowname\\' style = \\'line-height: 8vh\\'>' + data + ':<\\span>';
          }"
        )
        ),
      list(targets = 1, className = "dt-left",
           render = JS(
             'function(data, type, row, meta) {
             return meta.row === 2 ? data : data ? data.split("/").map(function(value) {
             return "<img class=\\"product-image\\" src = \\"https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/" + value.toLowerCase() + ".svg\\">";
             }).join("") : data;
             }'
        )
      )
    )), rownames = TRUE, class = "table dt-row", selection = 'none')

    output$recommendation <- DT::renderDataTable({
      patient_info_returning() %>%
        mutate_(
          expirationDate = ~ format(as.Date(expirationDate), "%m/%d/%Y"),
          medicalCondition = ~if_else(medicalCondition == "", NA_character_, medicalCondition)
        ) %>%
        select_(
          `Exp Date` = ~ expirationDate,
          Physician = ~ physician,
          `Rec ID #` = ~ recId,
          `Medical Condtion` = ~ medicalCondition
        ) %>%
        t() %>% as.data.frame(stringsAsFactors = FALSE) %>% tidyr::replace_na(list(`V1` =
                                                                                     "N/A"))
    }, options = list(dom = 't', columnDefs = list(
      list(
        targets = 0,
        render = JS(
          "function(data, type, row, meta) {
          return '<span class = \\'dt-rowname\\'>' + data + ':<\\span>';
          }"
        )
        ),
      list(targets = 1, className = "dt-left")
        )), rownames = TRUE, class = "table dt-row", selection = 'none')

    # images

    output$id_image_out <- renderUI({
      if (isTruthy(patientId()) &&
          isTruthy(patient_info_returning()$photoPath)) {

        display <- system(sprintf("aws s3 presign s3://%s/%s", bucket, patient_info_returning()$photoPath), intern = TRUE)
        tags$img(
          src = display,
          height = "100%",
          class = "hoverZoomLink",
          width = "100%"
        )
      } else {
        tags$img(src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/noneLight.svg",
                 class = "no-image",
                 height = "100%",
                 width = "100%")
      }
    })

    output$recommendation_image_out <- renderUI({
      if (isTruthy(patientId()) &&
          isTruthy(patient_info_returning()$medicalPath)) {
        tags$img(
          src = system(sprintf("aws s3 presign s3://%s/%s", bucket, patient_info_returning()$medicalPath), intern = TRUE),
          height = "100%",
          class = "hoverZoomLink",
          width = "100%"
        )
      } else {
        tags$img(src = "https://s3-us-west-2.amazonaws.com/cannadatacdn/icons/noneLight.svg",
                 class = "no-image",
                 height = "100%",
                 width = "100%")
      }
    })

    patient_sales <- reactive({
      req(patientId())
      q_f_patient_sales(pool, patientId())
    })

    output$patient_type <- renderC3({
      req(patientId())
      req(patient_sales()$profit)
      req(nrow(patient_sales())>0)
      patient_sales() %>%
        mutate_(type = ~tools::toTitleCase(type)) %>%
        spread_("type", "profit") %>%
        select_(~contains("Flower"), ~contains("Concentrate"),~contains("Edible"),~contains("Beverage"),
                ~contains("Soap"), ~contains("Vaporizer"), ~contains("Tincture"), ~contains("Ointment"),
                ~contains("Joint"), ~contains("Tobacco"), ~contains("Paraphernalia"), ~contains("Misc")) %>%
        summarise_all(function(x) {x[!is.na(x)][1]}) %>% c3() %>%
        c3_pie(format=DT::JS("function(value,ratio,id) {return '$' + value;}"))

    })

    output$patient_points <- renderC3({
      req(patientId())

      patient_info_returning() %>%
        select_(~points) %>%
        mutate_(points=~if_else(is.na(points),0,points)) %>%
        c3() %>%
        c3_gauge(min = 0, max = max_points, label = list(
          format = DT::JS(
            "function(value, ratio) {
            return value;
    }"
          )
        ))
    })

    output$no_type <- renderUI({
      req(!isTruthy(patient_sales()$profit))
      h4("No Data Available")
    })

    patient_history <- callModule(CannaModules::patientHistory,
                                  "frontdesk",
                                  pool,
                                  reactive({
                                    req(patientId())
                                    patientId()
                                  }))

    return(reactive(patient_info_returning()))

    }


tableTitle <- function(title, icon = "pencil") {
  div(class = "table-title-and-icon", h1(title), icon(icon, "fa-2x table-icons"))
}

input <-
  function(id,
           type = "text",
           ...,
           name = id,
           placeholder = tools::toTitleCase(id),
           label = placeholder,
           required = TRUE,
           class = NULL,
           disabled = FALSE,
           label_width = 3,
           input_width = 7) {
    div(
      class = "form-group",
      tags$label(
        class = paste0("control-label control-label-left col-sm-", label_width),
        `for` = id,
        label,
        if (required) {
          span(class = "req", "*")
        }
      ),
      div(
        class = paste0("controls col-sm-", input_width),
        tags$input(
          id = id,
          type = type,
          name = name,
          `data-role` = type,
          placeholder = placeholder,
          required = if (required)
            NA
          else
            NULL,
          disabled = if (disabled)
            NA
          else
            NULL,
          class = paste0(c("form-control k-textbox", class), collapse = " "),
          ...
        )
      )
    )
  }
