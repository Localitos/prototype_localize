library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(readxl)
library(dplyr)
library(nominatim)

rm(list = ls())

load("app_data.Rda")

col <- nord::nord(palette="frost", 4)

ui <- fluidPage(
  theme = "simplex",
  useShinyjs(),
  tags$head(tags$style(HTML('
      .skin-blue .main-sidebar {
                            background-color: #666666;
                            }
                            .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                            background-color: #444444;
                            }
                            '))),
  
  div(class = "page",
      id = "step1",
      
      h1("Welcome to localize!", align = "center"),
      p("This is your personal profile. The more you tell us about yourself, the better we can tell you what you need.", 
        align = "center"),
      hr(),
      
      fluidRow(
        column(4),
        column(8,
               # Name
               textInput("firstname", label = "First Name*"),
               textInput("lastname", label="Last Name*"),
               
               # Email Adress
               textInput("email", label = "Email adress"),
               
               # Age
               dateInput("age", label="Birthday:",
                         format = "yyyy-mm-dd", startview = "month"),
               # Gender
               selectInput("gender", label="Gender:",
                           choices = c("female","male")),
               
               #Origin
               selectInput("origin", label="Country of Origin:",
                           choices = unique(countries.list))
        )
      ),
      br(),
      actionButton("ShowAdvanced1", "Add Family Status",
                   style = "color: #fff; background-color: #8FBCBB; border-color: #fff"),
      hidden(
        div(
          id = "advanced1",
          fluidRow(
            column(4),
            column(8,
                   selectInput("family", label="Family Status:",
                               choices = c("Single", "Married")),
                   selectInput("children", label="Do you have children?",
                               choices = c("No", "Yes")),
                   sliderInput("childrenNumber", label = "If yes, how many?",
                               min = 0, max = 10, value = 0)
            )
          )
        )
      ),
      hr(),
      actionButton("ShowAdvanced2", "Add Employment Status", 
                   style = "color: #fff; background-color: #8FBCBB; border-color: #fff"),
      br(),
      hidden(
        div(
          id = "advanced2",
          verbatimTextOutput('out4'),
          fluidRow(
            column(4),
            column(8,
                   # Employment
                   selectInput("employment", label="What is your employment status?",
                               choices = c("Employee", "Self-employed",
                                           "Intern", "Volunteer", "Student")),
                   # Income
                   selectInput("income", label="Your annual gross income:",
                               choices = c(1,2,3,4)),
                   # Work-Start
                   dateInput("workstart", "Your first day at work"),
                   verbatimTextOutput('out5'),
                   # Location
                   textInput("employer", label="Name of your employer",
                             placeholder = "My employer"),
                   h5("Based on your input, we found the following employer:"),
                   br(),
                   textOutput("employer"),
                   br(), h6("(If this is not your employer,
                            please try to specify the information, e.g. name of the street)")
            )
            )
          )
        ),
      hr(),
      actionButton("ShowAdvanced3", "Add Text", 
                   style = "color: #fff; background-color: #8FBCBB; border-color: #fff"),
      br(),
      hidden(
        div(
          id = "advanced3",
          fluidRow(
            column(4),
            column(8,
                   HTML(
                     paste0('
                            <div class="form-group">
                            <label class="col-md-4 control-label" for="Overview (max 200 words)">How does your perfect after-work evening look like?</label>
                            <div class="col-md-4">
                            <textarea class="form-control" rows="10"  id="Overview (max 200 words)" name="Overview (max 200 words)">max 200 words</textarea>
                            </div>
                            </div>
                            <div class="form-group">
                            <label class="col-md-4 control-label" ></label> 
                            <div class="col-md-4">
                            <a href="#" class="btn btn-success"><span class="glyphicon glyphicon-thumbs-up"></span> Submit</a>
                            <a href="#" class="btn btn-danger" value=""><span class="glyphicon glyphicon-remove-sign"></span> Clear</a>
                            </div>
                            </div>
                            ')
                     )
                     )
            )
          )
        ),
      hr(),
      column(8),
      column(4,
             actionButton("submit", "Submit", icon("paper-plane"),
                          style="color: #fff; background-color: #81A1C1; border-color: #fff"))
      ),
  hidden(
    div(
      class = "page",
      id = "step2",
      navbarPage(theme = "flatly",
                 tabPanel("start",
                          "Get localized!"
                 ),
                 tabPanel("Home",
                          fluidRow(
                            h4("We provide you with the information you need. If you need any further help, please contact us."),
                            br(),
                            column(4),
                            column(4,
                                   actionButton("home", "Go Back",
                                                style="color: #fff; background-color: #81A1C1; border-color: #fff")
                                   )
                          )
                 ),
                 tabPanel("Visa",
                          fluidRow(
                            column(12,
                                   HTML(paste0("<div class='row'>
                                               <div class='col-sm-14 col-md-12'>
                                               <div class='thumbnail'>
                                               <div class='caption'>
                                               <h3> General Visa Info </h3>
                                               <p>", textOutput("visa_general"), "</p>
                                               </div>
                                               </div>
                                               </div>
                                               </div>"))
                                   ),
                            column(12,
                                   htmlOutput("visa.doc"),
                                   
                                   column(4,
                                          htmlOutput("visa.doc.general")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.passport")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.fotos")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.app_form")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.work_contract")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.cv")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.university")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.zav")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.insurance")
                                   ),
                                   column(4,
                                          htmlOutput("visa.doc.cost")
                                   )
                                   ),
                            column(12,
                                   htmlOutput("process"),
                                   hr(),
                                   htmlOutput("picture")
                            )
                            )
                            ),
                 tabPanel("Insurance",
                          fluidRow(
                            column(12, 
                                   HTML(paste0("<div class='row'>
                                               <div class='col-sm-14 col-md-12'>
                                               <div class='thumbnail'>
                                               <div class='caption'>
                                               <h3> Insurance in Germany </h3>
                                               <p>", "There are different types of insurances in Germany - some are mandatory, others are strongly recommended
                                               and a few are voluntary.", "</p>
                                               </div>
                                               </div>
                                               </div>
                                               </div>"))
                                   ),
                            column(6,
                                   HTML(paste0("
                                               <h4> <b> Mandatory insurances </b> </h4>
                                               ")),
                                   
                                   fluidRow(
                                     column(12, thumbnail(label = 'Health Insurance',
                                                          content = "In general, all health insurance funds will provide you with
                                                          enough coverage for your day-to-day needs - some of the most popular are
                                                          TK, DAK, and Barmer GEK. You can include your spouse and children into
                                                          your family insurance without any extra charge once they are in Germany.
                                                          Other regulations apply if your spouse will be working in Germany. In
                                                          that case an individual consultancy will be required.",
                                                          button_link = 'https://www.check24.de/gesetzliche-krankenversicherung/',
                                                          button_label = 'Find your Insurance')
                                     ),
                                     column(12, thumbnail(label = 'Car insurance (Kfz-Versicherung)',
                                                          content = 'An unlimited third-party liability insurance policy is compulsory for
                                                          all cars/motorized vehicles entering Germany.',
                                                          button_link = 'https://www.check24.de/kfz-versicherung/',
                                                          button_label = 'Find your Insurance')
                                     ),
                                     column(12, thumbnail(label = 'Dog insurance (Tierhalter-Versicherung (Hunde))',
                                                          content = "For dog owners this insurance is mandatory, its needed to register 
                                                          your dog and will cover in case of any damage caused by your dog.",
                                                          button_link = 'https://www.check24.de/hundehaftpflicht/',
                                                          button_label = 'Find your Insurance')
                                     )
                                   )
                                     ),
                            
                            column(6,
                                   HTML(paste0("
                                               <h4> <b> Strongly recommended insurances </b> </h4>")),
                                   
                                   fluidRow(
                                     column(12, thumbnail(label = 'Private liability insurance (Privat-Haftpflichtversicherung)',
                                                          content = "This insurance is optional, but highly recommended. Many landlords will
                                                          demand proof of a liability insurance before signing a lease contract, since it
                                                          covers cases of liability for injury or damage to other persons or their property. If
                                                          you are married or living with your partner, you can include your spouse/partner on
                                                          your insurance as well.",
                                                          button_link = 'https://www.check24.de/privathaftpflicht/',
                                                          button_label = 'Find your Insurance')
                                     ),
                                     column(12, thumbnail(label = 'Pet owner insurance',
                                                          content = 'If you own any big animals, which may cause damage (e.g. horses) a pet
                                                          owner insurance is highly recommended.',
                                                          button_link = 'https://www.check24.de/pferdehaftpflicht/',
                                                          button_label = 'Find your Insurance')
                                     )
                                   )
                                     )
                                     ),
                          column(12,
                                 HTML(paste0("
                                             <h4> <b> Optional, nice-to-have insurances </b> </h3>
                                             ")),
                                 fluidRow(
                                   column(12, thumbnail(label = 'Home Insurance (Hausratversicherung)',
                                                        content = "Household insurance covering the contents of your home against fire, water
                                                        damage, theft, vandalism, etc. is not required by law in Germany, but it is recommend
                                                        ed, and some landlords demand it.",
                                                        button_link = 'https://www.check24.de/hausratversicherung/',
                                                        button_label = 'Find your Insurance')
                                   ),
                                   column(12, thumbnail(label = 'Legal Insurance (Rechtsschutzversicherung)',
                                                        content = 'This insurance covers any legal costs you encounter up to a certain limit.
                                                        And if you want to counter sue, it will cover the payment as long as there is a
                                                        reasonable chance of winning. Legal insurance can be purchased for different purpose
                                                        s, such as for the entire family, the job, traffic infractions, or the lease of your
                                                        home.',
                                                        button_link = 'https://www.check24.de/rechtsschutzversicherung/',
                                                        button_label = 'Find your Insurance')
                                   )
                                 )
                                   )
                                   ),
                 tabPanel("Housing",
                          fluidRow(
                            column(12,
                                   
                                   
                                   HTML(paste0("<div class='row'>
                                               <div class='col-sm-14 col-md-12'>
                                               <div class='thumbnail'>
                                               <div class='caption'>
                                               <h3> Living in Hamburg </h3>
                                               <p>", "Hamburg is made up of 7 boroughs (German: Bezirke, also known as districts or administrative districts) and subdivided into 104 quarters (German: Stadtteile). Most of the quarters were former independent settlements. We matched your input with the characteristics of each quarter to propose the
                                               location that best fits your lifestyle. You can click on the quarters to get more information
                                               about it.", "</p>
                                               </div>
                                               </div>
                                               </div>
                                               </div>"))
                                   ),
                            
                            leafletOutput("mymap"),
                            column(12,
                                   thumbnail(label = 'How to find a new place to live?',
                                             content = "The general housing market in Hamburg is not easy, rent prices have been rising and affordable flats in central areas are scarce.  Apartments are often available on short-notice and go quickly, so there is no use in looking for flats more than three months in advance and it is normal to spend 40% of your net income for your flat.",
                                             button_link = htmlOutput("renting"),
                                             button_label = 'Find your Home!')
                            )
                                   )
                                   )
                 )
      )
    )
  )


server <- function(input, output, session) {
  
  observeEvent(input$submit,{
    shinyjs::show(id ="step2")
    shinyjs::hide(id="step1")
  })
  
  observeEvent(input$home,{
    shinyjs::show(id ="step1")
    shinyjs::hide(id="step2")
  })
  
  # Make First an last name obligatory
  # observe({
  #   if (is.null(input$firstname) || input$firstname == "" &
  #       is.null(input$lastname) || input$lastname == "" 
  #       ) {
  #     shinyjs::disable("submit")
  #   } else {
  #     shinyjs::enable("submit")
  #   }
  # })
  
  
  # Show more Input
  shinyjs::onclick("ShowAdvanced1",
                   shinyjs::toggle(id = "advanced1", anim = T))
  shinyjs::onclick("ShowAdvanced2",
                   shinyjs::toggle(id = "advanced2", anim = T))
  shinyjs::onclick("ShowAdvanced3",
                   shinyjs::toggle(id = "advanced3", anim = T))
  
  ### Visa ###
  output$visa.doc <- renderText({
    "InfoBoxoutput"
  })
  
  #### Visa ####
  
  # Get country-class (EU, Privileged, etc.) of origin country
  cluster_out <- reactive({
    cluster.df$cluster[cluster.df$origin==input$origin]
  })
  
  output$origin <- renderText({
    input$origin
  })
  
  ## General ##
  output$visa_general <- renderText({
    visa.general <- visa.general %>%
      filter(cluster == cluster_out()) 
    paste(visa.general$fyi, visa.general$remarks, sep=" ")
  })
  
  ### Documents ###
  visa_documents_out <- reactive({
    
    if (input$origin %in% visa.documents$origin) {
      visa.documents %>%
        filter(origin == input$origin)
    } else if (cluster_out() %in% visa.documents$origin) {
      visa.documents %>%
        filter(origin == cluster_out())
    } else {
      visa.documents %>%
        filter(origin == "Else")
    }
  })
  
  output$visa.doc <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$application_form)) {
      HTML(paste(""))
    } else {
      HTML(paste0("<h4>Documents</h4>"))
    }
  })
  
  output$visa.doc.general <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$general)) {
      return(NULL)
    } else {
      thumbnail(label = "General",
                content = dat$general,
                button = F)
    }
  })
  
  output$visa.doc.passport <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$passport)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "Passport",
                content = dat$passport,
                button = F)
    }
  })
  
  output$visa.doc.fotos <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$fotos)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "Fotos",
                content = dat$fotos,
                button = F)
    }
  })
  
  output$visa.doc.app_form <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$application_form)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "Application Form",
                content = dat$application_form,
                button = F)
    }
  })
  
  output$visa.doc.work_contract <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$work_contract)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "Work Contract",
                content = dat$work_contract,
                button = F)
    }
  })
  
  output$visa.doc.cv <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$cv)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "CV",
                content = dat$cv,
                button = F)
    }
  })
  
  output$visa.doc.university <-  renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$university_degree)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "University Degree",
                content = dat$university_degree,
                button = F)
    }
  })
  
  output$visa.doc.zav <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$zav_acceptance)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "ZAV Acceptance",
                content = dat$zav_acceptance,
                button = F)
    }
  })
  
  output$visa.doc.insurance <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$insurance)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "Insurance",
                content = dat$insurance,
                button = F)
    }
  })
  
  output$visa.doc.cost <- renderUI({
    dat <- visa_documents_out()
    
    if (is.na(dat$cost)) {
      HTML(paste(""))
    } else {
      thumbnail(label = "Cost",
                content = dat$cost,
                button = F)
    }
  })
  
  ### Process ###
  process_out <- reactive({
    if (cluster_out() == "Priviliged") {
      return("Visa process_priviliged.png")
    } else {
      if (cluster_out() == "Other") {
        return("Visa process_other.png")
      } else {
        return("")
      }
    }
  })
  
  output$process <- renderUI({
    
    if (cluster_out() == "Priviliged") {
      HTML(paste0("<h4>Process</h4>"))
    } else {
      if (cluster_out() == "Other") {
        HTML(paste0("<h4>Process</h4>"))
      } else {
        HTML(paste(""))
      }
    }
    
  })
  
  output$picture <-
    renderText({
      c(
        '<img src="',
        process_out(),
        '">'
      )
    })
  
  #### Housing ####
  age_out <- reactive({
    round(as.numeric(difftime(Sys.time(), input$age, units = "weeks")/52.25))
  })
  
  agerange_out <- reactive({
    if (age_out() %in% c(0:2)) {
      return("share_age0_3")
    } else if (age_out() %in% c(3:5)) {
      return("share_age3_6")
    } else if (age_out() %in% c(6:11)) {
      return("share_age6_12")
    } else if (age_out() %in% c(12:17)) {
      return("share_age12_18")
    } else if (age_out() %in% c(18:19)) {
      return("share_age18_20")
    } else if (age_out() %in% c(20:29)) {
      return("share_age20_30") 
    } else if (age_out() %in% c(30:44)) {
      return("share_age30_45") 
    } else if (age_out() %in% c(45:59)) {
      return("share_age45_60") 
    } else if (age_out() %in% c(60:64)) {
      return("share_age60_65")
    } else {
      return("share_age.65")
    }
  })
  
  
  # Gender Index
  gender_out <- reactive({
    if (input$gender == "female") {
      return("share_f")
    } else {
      return("share_m")
    }
  })
  
  # Family Index
  family_out <- reactive({
    if (input$family == "Single" & input$children == "No") {
      return("share_Einpersonenhaushalte")
    } else if (input$family == "Single" & input$children == "Yes") {
      return("share_Alleinerziehende")
    } else if (input$family == "Married" & input$children == "Yes") {
      return("share_Haushalte.mit.Kindern")
    } else {
      return("share_Bevölkerung")
    }
  })
  
  # Employer location
  employer_out <- reactive({
    empl <- bb_lookup(paste("Hamburg",input$employer, sep=","))
    empl
  })

  output$employer <- renderText({
    # Employer location
    empl <- employer_out()

    print(empl[["display_name"]][1])

  })
  
  
  ## Customize Map ##
  output$mymap <- renderLeaflet({
    
    df$age_value <- df[, as.character(agerange_out())]
    df$gender_value <- df[,as.character(gender_out())]
    df$family_value <- df[,as.character(family_out())]
    df$income_prob <- ifelse(input$income == df$income_group,1,0)
    
    # add employer information
    df$empl_lon <- employer_out()[["lon"]][1]
    df$empl_lat <- employer_out()[["lat"]][1]
    df$empl_name <- employer_out()[["display_name"]][[1]]
    
    # ---
    df %>% mutate(
      value = (age_value+gender_value+family_value+income_prob)/4
    ) -> df
    
    # Calculate percentile
    q1 = quantile(df$value, probs = 0.9, na.rm = TRUE)
    # Keep values above tis threshold
    df <- df %>% filter(value >= q1)
    
    # Customize labels
    df$labels <- paste("<font color=#4B515D", 
                       paste(paste("<h2>", df$hood, "</h2>"),
                             paste('<h4>Location</h4>
                                   <span class="fa fa-star checked"></span>
<span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star"></span>
                                   <span class="fa fa-star"></span>
                                   '),
                             paste('<h4>Family</h4>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star"></span>
                                   <span class="fa fa-star"></span>
                                   '),
                             paste('<h4>Nightlife</h4>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star"></span>
                                   <span class="fa fa-star"></span>
                                   '),
                             paste('<h4>Parks & Nature</h4>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star checked"></span>
                                   <span class="fa fa-star"></span>
                                   <span class="fa fa-star"></span>
                                   '),
                             paste("<br/>General Info:", df$description, sep = "<br/>"),
                             sep = "<br/>"), 
                       "</font>")
    
    leaflet(data = df) %>%
      addCircleMarkers(lng=~long, lat=~lat,
                       popup = ~labels) %>%
      addMarkers(lng=~as.numeric(empl_lon),
                lat=~as.numeric(empl_lat), popup = ~empl_name) %>%
      addProviderTiles(providers$OpenStreetMap)
    
  })
  
  output$age <- reactive({agerange_out()})
  
  ##### Renting ####
  output$renting <- renderUI({
    if (input$renting == "Shared") {
      paste0("http://www.wg-gesucht.de/en/")
    } else {
      paste0("http://www.immobilienscout24.de/")
    }
  })
  
  
}

shinyApp(ui, server)
