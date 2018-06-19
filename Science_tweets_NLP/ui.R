

multiselect_choices <- unique(unpopular_text_date$screen_name)

radio_choices <- sort(unique(na.omit(unpopular_text_date$week_number)))

shinyUI(
  navbarPage(
    "Science News on Twitter",
    tabPanel("Main",
             fluidPage(theme = shinythemes::shinytheme("superhero"),
                       fluidRow(
                         column(12,
                                h1("Examining the Popularity of Science Topics through Twitter Data Analysis"),
                                
                                fluidRow(
                                  
                                  column(4,
                                         selectInput(inputId = "select_visualization",
                                                     label = h3("Select Visualization"),
                                                     choices = list("Wordcloud", "Sentiment", "Word Correlation"),
                                                     selected = "Wordcloud")),
                                  
                                  column(4,
                                         radioButtons(inputId = "select_week",
                                                      label = h3("Select Week"),
                                                      choices = c("All", radio_choices),
                                                      selected = "All",
                                                      inline = T)),
                                  column(4,
                                         selectizeInput(inputId = 'select_users',
                                                        label = list(h3("Select User Handles"),
                                                                     "Click to add, delete to remove"),
                                                        choices = c(multiselect_choices),
                                                        selected = c(multiselect_choices),
                                                        multiple = TRUE,
                                                        width = '100%'))
                                ),
                                fluidRow(
                                  
                                  column(6,
                                         div(style = "height:750px;",
                                         h2("Popular Tweets"),
                                         plotOutput("popular_plot", width = "100%", height = "95%")
                                         )
                                  ),
                                  
                                  column(6,
                                         div(style = "height:750px;",
                                         h2("Unpopular Tweets"),
                                         plotOutput("unpopular_plot", width = "100%", height = "95%")
                                         )
                                  )
                                )))))#,
    # tabPanel("About",
    #          h3("TL;DR"),
    #          h3("Why should I care about science news on Twitter?")
    # ),
    # tabPanel("Methodology",
    #          h3("TL;DR"),
    #          h3("What are the data and where did they come from?"),
    #          h3("How to use:")
    # ),
    # tabPanel("Analysis",
    #          h3("Here are a few observations and further analysis:"))
  
    )
  )