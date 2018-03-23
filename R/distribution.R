# Copyright (C) 2017 CannaData Solutions
#
# This file is part of CannaDistribution.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#' Frontdesk Shiny Application
#'
#' @import shiny CannaQueries shinyCleave rintrojs RMariaDB pool DT dplyr CannaModules CannaSelectize hms scales CannaFrontdesk onfleetR CannaPOS
#' @import aws.s3 c3 jsonlite jose openssl httr base64enc twilio googleAuthR googlePrintr DBI parallel docuSignr CannaMetrc leaflet purrr tidyr
#' @importFrom tools file_ext
#' @importFrom tidyr replace_na spread_
#' @inheritParams CannaSignup::signup
#' @param bucket Name of AWS bucket
#' @param gcp_json Google Cloud Print Json
#' @param twilio_sid Twilio secret id
#' @param twilio_token Twilio token
#' @export
#'

distribution <-
  function(pool = pool::dbPool(
    RMariaDB::MariaDB(),
    host = getOption("CannaData_host"),
    port = as.integer(getOption("CannaData_port")),
    user = getOption("CannaData_user"),
    password = getOption("CannaData_password"),
    db = getOption("CannaData_db"),
    `ssl-ca` = "https://s3.amazonaws.com/rds-downloads/rds-combined-ca-bundle.pem",
    `ssl-verify` = "server-cert"
  ), clientName = getOption("CannaData_clientName"),
           # host = getOption("CannaData_host"),
           # port = as.integer(getOption("CannaData_port")),
           # user = getOption("CannaData_user"),
           # password = getOption("CannaData_password"),
           base_url = getOption("CannaData_baseUrl"),
           # db = getOption("CannaData_db"),
           bucket = getOption("CannaData_AWS_bucket"),
  gcp_json = getOption("CannaData_google_json"),
  auth_id = getOption("auth_id"),
  auth_secret = getOption("auth_secret"),
  scope = "openid email", twilio_sid = getOption("TWILIO_SID"),
  twilio_token = getOption("TWILIO_TOKEN"),
  connection_name = getOption("auth_connection"),
  state = getOption("CannaData_state")) {
    Sys.setenv("TWILIO_SID" = getOption("TWILIO_SID"),
               "TWILIO_TOKEN" = getOption("TWILIO_TOKEN"),
               "AWS_ACCESS_KEY_ID" = getOption("AWS_ACCESS_KEY_ID"),
               "AWS_SECRET_ACCESS_KEY" = getOption("AWS_SECRET_ACCESS_KEY"),
               "AWS_DEFAULT_REGION" = getOption("AWS_DEFAULT_REGION")
               )
    APP_URL <- paste0(base_url, "distribution/")

    access_token <- ""
    make_authorization_url <- function(req, APP_URL) {
      url_template <- paste0("https://cannadata.auth0.com/authorize?response_type=code&scope=%s&",
                             "state=%s&client_id=%s&redirect_uri=%s&connection=%s")
      redirect_uri <- APP_URL
      # save state in session global variable
      state <- paste0(sample(c(LETTERS, 0:9), 12, replace =TRUE), collapse = "")

      list(url = sprintf(url_template,
                         utils::URLencode(scope, reserved = TRUE, repeated = TRUE),
                         utils::URLencode(base64enc::base64encode(charToRaw(state))),
                         utils::URLencode(auth_id, reserved = TRUE, repeated = TRUE),
                         utils::URLencode(redirect_uri, reserved = TRUE, repeated = TRUE),
                         utils::URLencode(connection_name, reserved = TRUE, repeated = TRUE)
      ), state = state)
    }

    ui <- function(req) {
      if (length(parseQueryString(req$QUERY_STRING)$code) == 0 && !interactive()) {
        authorization_url <- make_authorization_url(req, APP_URL)
        return(tagList(
          tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                   document.cookie = cname + \"=\" + cvalue + \";path=/\";
      };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
      } else if (!interactive()) {
        params <- parseQueryString(req$QUERY_STRING)

        if (!isTRUE(rawToChar(base64enc::base64decode(params$state)) == parseCookies(req$HTTP_COOKIE)$cannadata_token)) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                     document.cookie = cname + \"=\" + cvalue + \";path=/\";
        };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
        }

        resp <- tryCatch(httr::POST("https://cannadata.auth0.com/oauth/token",
                                    body = list(
                                      grant_type = "authorization_code",
                                      client_id = auth_id,
                                      client_secret = auth_secret,
                                      code = params$code,
                                      redirect_uri = APP_URL
                                    ), httr::accept_json(),
                                    encode = "json"), error = function(e) NULL)

        if (httr::http_error(resp)) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                     document.cookie = cname + \"=\" + cvalue + \";path=/\";
        };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
        }

        respObj <- jsonlite::fromJSON(rawToChar(resp$content))

        # verify access token

        if (!isTruthy(respObj$id_token)) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
                                     document.cookie = cname + \"=\" + cvalue + \";path=/\";
        };setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
        }

        kid <- jsonlite::fromJSON(rawToChar(base64decode(strsplit(respObj$id_token, "\\.")[[1]][1])))$kid

        keys <- jsonlite::fromJSON("https://cannadata.auth0.com/.well-known/jwks.json")$keys

        # 64 characters to a row
        key <- gsub("(.{64})", "\\1\n", keys[keys$kid == kid, ]$x5c)
        publicKey <- openssl::read_cert(paste("-----BEGIN CERTIFICATE-----", key, "-----END CERTIFICATE-----", sep = "\n"))$pubkey

        user <- tryCatch(jose::jwt_decode_sig(respObj$id_token, publicKey), error = function(e) NULL)

        if (is.null(user)) {
          authorization_url <- make_authorization_url(req, APP_URL)
          return(tagList(
            tags$script(HTML(sprintf("function setCookie(cname, cvalue) {
    document.cookie = cname + \"=\" + cvalue + \";path=/\";
};setCookie(\"cannadata_token\",\"%s\");location.replace(\"%s\");", authorization_url$state, authorization_url$url)))))
        }
        return(shiny::htmlTemplate(
          filename = system.file(package = "CannaDistribution", "templates", "template.html"),
          clientName = clientName, state = state, accessToken = respObj$access_token
        ))
      } else {
        return(shiny::htmlTemplate(
          filename = system.file(package = "CannaDistribution", "templates", "template.html"),
          clientName = clientName, state = state, accessToken = ""
        ))
      }
    }

    options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloudprint"))
    options("googleAuthR.ok_content_types" = c(getOption("googleAuthR.ok_content_types"),
                                               "text/plain"))

    msg_service_sid = tw_msg_service_list()[[1]]$sid
    teams <- onfleetR::onfleet_get_teams()
    if (getOption("CannaData_state") %in% c("CO","OR", "MD")) {
      Sys.setenv(
        "metrc_software_key" = getOption("metrc_software_key"),
        "metrc_state" = getOption("metrc_state"),
        "metrc_demo" = getOption("metrc_demo")
      )
    }

    server <- function(input, output, session) {
      params <- parseQueryString(isolate(session$clientData$url_search))
      access_token <- isolate(input$access_token)
      if (length(params$code) == 0 && !interactive()) {
        return()
      }

      googleAuthR::gar_auth_service(gcp_json,
                                    scope = c("https://www.googleapis.com/auth/cloudprint"))
      printers <- gcp_search("")

      if (!interactive()) {
        user <- jsonlite::fromJSON(rawToChar(httr::GET(
          httr::modify_url("https://cannadata.auth0.com/", path = "userinfo/",
                           query = list(access_token = access_token))
        )$content))

        budtenderId <- q_c_budtender(pool, if (isTruthy(user$email)) user$email else NA)

        if (budtenderId$sales == 0) {
          showModal(
            modalDialog(
              tags$script("$('.modal-content').addClass('table-container');"),
              h1("You do not have access to this page."),
              footer = NULL
            )
          )
          return()
        } else {
          Sys.setenv("metrc_user_key" = budtenderId$apiKey)
        }
      } else {
        budtenderId = list(idbudtender = 1, apiKey = getOption("metrc_user_key"))
        Sys.setenv("metrc_user_key" = budtenderId$apiKey)
      }
      settings <- q_s_settings(pool)

      if (interactive()) {
        session$onSessionEnded(stopApp)
      }

      options(shiny.maxRequestSize = 300 * 1024 ^ 2)


      onfleet_orders <- reactive({
        trigger_transactions()
        onfleetR::onfleet_get_tasks(from = Sys.Date()) %>%
          right_join(orders() %>% select_(~onfleetId), by = c(id = "onfleetId"))
      })

      reload_select <- reactiveVal()
      observeEvent(reload_select(), {
        req(reload_select())
        updateSelectizeInput(session, "patient", choices = list(patient = customers() %>% select_(~id, ~name, ~businessName, ~phone, ~idpatient) %>%
                                                                  mutate_(label = ~if_else(!is.na(businessName) & nchar(businessName)>0, businessName, name),
                                                                          valueFld = ~paste0("P", idpatient)),
                                                                orders = orders() %>%
                                                                  select_(~name, ~timeIn, ~status, ~idtransaction) %>%
                                                                  mutate_(timeIn = ~as.character(as.POSIXct(
                                                                    hms::as.hms(timeIn)
                                                                  ), "%I:%M %p"), label = ~name,
                                                                  valueFld = ~paste0("T", idtransaction)))  %>%
                               bind_rows(.id = "selectGrp"), server = TRUE, selected = reload_select()$id)
      })
      trigger_patients <- reactiveVal(0)
      trigger_transactions <- reactiveVal(0)
      orders <- callModule(orders, "orders", pool, onfleet_orders, trigger_transactions)

      customers <- callModule(customers, "customers", pool, trigger_patients)

      drivers <- callModule(drivers, "drivers", pool, onfleet_orders, teams, orders, trigger_transactions)

      patientInfo <- callModule(patientInfo, "customerInfo", pool, reactive({
        req(input$patient)
        patient <- input$patient
        if (substr(patient, 1, 1) == "P") {
          as.numeric(substring(patient, 2))
        } else if (substr(patient, 1, 1) == "T") {
          orders()$idpatient[orders()$idtransaction == as.numeric(substring(patient, 2))]
        }
      }), bucket, settings = settings, msg_service_sid = msg_service_sid, base_url = base_url,
      max_points = settings$points_total, orders = orders, trigger_transactions, reload_select,
      trigger_patients)

      orderInfo <- callModule(orderInfo, "orderInfo", pool, reactive({
        req(input$patient, substr(input$patient,1,1)=="T")
        as.numeric(substring(input$patient, 2))
      }), msg_service_sid, reactive(orders()[as.numeric(substring(input$patient, 2)) == orders()$idtransaction,]),
      reload_select, trigger_transactions, printers, settings, bucket, budtenderId)

      observe({
        selected = isolate(input$patient)
        updateSelectizeInput(session, "patient", choices = list(patient = customers() %>% select_(~id, ~name, ~businessName, ~phone, ~idpatient) %>%
                                                                  mutate_(label = ~if_else(!is.na(businessName) & nchar(businessName)>0, businessName, name),
                                                                          valueFld = ~paste0("P", idpatient)),
                                                       orders = orders() %>%
                                                         select_(~name, ~timeIn, ~status, ~idtransaction) %>%
                                                         mutate_(timeIn = ~as.character(as.POSIXct(
                                                           hms::as.hms(timeIn)
                                                         ), "%I:%M %p"), label = ~name,
                                                         valueFld = ~paste0("T", idtransaction)))  %>%
                               bind_rows(.id = "selectGrp"), server = TRUE, selected = selected)
      })

      observe({
        if (substr(input$patient, 1, 1) == "P") {
          updateNavlistPanel(session, "tabset", "patientInfo")
        } else if (substr(input$patient, 1, 1) == "T") {
          updateNavlistPanel(session, "tabset", "orderInfo")
        }
      })

      observeEvent(input$help, {
        intro <- data.frame(
          element = c(NA),
          intro = c(as.character(h1("This is the distribution application")))
        )

        introjs(session, options = list(steps = intro,
                                        showStepNumbers = FALSE,
                                        showBullets = FALSE))
      })

    }

    shiny::shinyApp(ui = ui, server = server)

  }
