#This is a Shiny web application. 

# libraries ----------------------
library(shiny)
library(shinyjs)
library(shinythemes)
library(promises)
#library(tidyverse)
library(ggplot2)
library(tidytext)
library(stopwords)
library(dplyr)
library(skimr)
library(readr)
library(purrr)
library(epubr)
library(stringr)


# Wrap entire app in function per https://mastering-shiny.org/scaling-packaging.html#converting-an-existing-app
#runApp <- function(...){

# UI ------------------------------
ui <- fluidPage(
  # enable shiny js for resetting reactive values
  useShinyjs(),
  #themeSelector(), # uncomment to enable theme selection
  
  # Application title
  titlePanel("NLP Coεmeta web app 0.5"),

  # Sidebar with upload widget -----
  sidebarLayout(
    sidebarPanel(width=3,
            fileInput("file",
                      #HTML("<a href=www.philly.com>Upload CSV File</a>"),
                      "Upload properly formatted CSV",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            #tags$hr()#,
            #checkboxInput("header", "Has column headers", TRUE)
            #br(),
            h5(em(strong("or"))),
            fileInput("epub_file",
                      #HTML("<a href=www.philly.com>Upload CSV File</a>"),
                      "Upload .epub file",
                      accept = c("epub")
              ),
            # dynamic newline # selector (https://shiny.rstudio.com/articles/dynamic-ui.html)
            uiOutput("newline_ui"),
            br(),
            h5(em(strong("...or"))),
            actionButton("use_demo_data_button", "Use demo dataset"),
            br(),
            br(),
            # dynamic groupby selector (https://shiny.rstudio.com/articles/dynamic-ui.html)
            uiOutput("groupby_ui"),
            br(),
            # dynamic filter selector (https://shiny.rstudio.com/articles/dynamic-ui.html)
            uiOutput("filter_ui")
    ),
        
  # Main panel ----
  mainPanel(
    # Put tabset panel top of page
    tabsetPanel(type = "tabs",
                
        # Uploaded Data panel ---- 
        tabPanel("Uploaded Data",
                 br(),
                 em({"Upload a CSV file as specified on the 'About (How to Use)' tab,
                      or click the 'Use demo dataset' button to the left"}),
                 hr(),
                 h3("Raw Data"),
                 DT::dataTableOutput("table"),
                 br(),
                 hr(),
                 h3("Data Summaries"),
                 verbatimTextOutput("skim"),
                 hr(),
                 verbatimTextOutput("sum"),
                 hr(),
                 verbatimTextOutput("str")
                 ),
        
        # Frequency panel ----
        tabPanel("Word / N-gram Frequency",
                 h3("Count Options"),
                 
                 # selectInput("cats", 
                 #             label = h5("Select column to group by"), 
                 #             choices = list("Choice 1" = 1, 
                 #                            "Choice 2" = 2, 
                 #                            "Choice 3" = 3), 
                 #             selected = 1),
                 fluidRow(
                   column(3, 
                          selectInput("gram",
                                      label = h5("Select token size"), 
                                      choices = list("unigram (1 word)" = 1, 
                                                     "bigram (2 words)" = 2, 
                                                     "trigram (3 words)" = 3,
                                                     "quadgram (4 words)" = 4),
                                      selected = 1)
                   ),
                   
                   column(3, 
                          sliderInput('word_length', 
                                      h5('Min unigram character length'), 
                                      min=1, max=5, value=1, step=1, 
                                      round=T)
                          
                   )
                 ),
                 
                 hr(),
                 h3("Plot of Counts"),
                 fluidRow(
                   
                   column(3, 
                          sliderInput('count_plot_n', 
                                      h5('Plot top __ n-grams'), 
                                      min=5, max=50, value=10, step=1, 
                                      round=T)
                   ),
                   
                   column(3, 
                          sliderInput('count_plot_height', 
                                      h5('Plot Height (pixels)'), 
                                      min=200, max=2000, value=400, step=50, 
                                      round=T)           
                   ),
                   
                   column(3, 
                          sliderInput('count_plot_width', 
                                      h5('Plot Width (pixels)'), 
                                      min=200, max=2000, value=900, step=50, 
                                      round=T)           
                   ),
                   
                   column(3, 
                          sliderInput('count_plot_fontsize', 
                                      h5('Font size (relative)'), 
                                      min=0.5, max=3, value=1, step=.25, 
                                      round=F)           
                   )
                 ),
                 
                 br(),
                 #plotOutput("plot"),
                 uiOutput("plot_ui"),
                 br(),
                 hr(),
                 
                 h3("Table Output"),
                 DT::dataTableOutput("count_table"),
                 br(),
                 downloadButton("count_download", "Download frequency data as CSV")
                 
        ),
        
        # Tf-idf panel ----
        tabPanel("TF-IDF",
                 h3("Note"),
                 em({"For this tab to work, there must be ONE grouping variable selected via the left panel, 
                 AND n-gram counts must first be produced via the 'Word / N-gram Frequency' tab."}),
                 br(),
                 br(),
                 #h3("TF-IDF"),
                 p("TF-IDF (Term Frequency-Inverse Document Frequency) attempts to find terms that are uniquely 
                 important to subsets of a 
                 collection of documents, while supressing terms that are common to the entire collection. 
                 Here the 'collection' (or 'corpus') is the entire uploaded file, & subsets are determined by 
                 the selected grouping variable. For more on tf-idf, see",
                   a("here.", href='https://en.wikipedia.org/wiki/Tf%E2%80%93idf')),
                 
                 hr(),
                 h3("Plot of tf-idf"),
                 fluidRow(
                   
                   column(3, 
                          sliderInput('tfidf_plot_n', 
                                      h5('Plot top __ n-grams'), 
                                      min=5, max=50, value=10, step=1, 
                                      round=T)
                   ),
                   
                   column(3, 
                          sliderInput('tfidf_plot_height', 
                                      h5('Plot Height (pixels)'), 
                                      min=200, max=2000, value=400, step=50, 
                                      round=T)           
                   ),
                   
                   column(3, 
                          sliderInput('tfidf_plot_width', 
                                      h5('Plot Width (pixels)'), 
                                      min=200, max=2000, value=900, step=50, 
                                      round=T)           
                   ),
                   
                   column(3, 
                          sliderInput('tfidf_plot_fontsize', 
                                      h5('Font size (relative)'), 
                                      min=0.5, max=3, value=1, step=.25, 
                                      round=F)           
                   )
                 ),
                 
                 br(),
                 #plotOutput("plot"),
                 uiOutput("tfidf_plot_ui"),
                 br()
                 
                 ),
        
        # Topic model panel ----
        # tabPanel("Topic Modeling",
        #          br(),
        #          em("tktktk")),
        # 
        # # Chatterplot panel ----
        # tabPanel("Chatterplot",
        #          br(),
        #          em("tktktk")),
        # 
        # # Sentiment panel ----
        # tabPanel("Sentiment Analysis",
        #          br(),
        #          em("tktktk")),
        
        # About / Usage panel ----
        tabPanel("About (How to Use)",
                 br(),
                 h2("About"),
                 p("This is a prototype Natural Language Processing (NLP) web app written in ",
                   a("R", href='https://en.wikipedia.org/wiki/R_(programming_language)'),
                   " using the ",
                   a("Shiny", href='https://shiny.rstudio.com/'),
                   " framework. It is in active development to assist with common NLP tasks which would otherwise 
                      require some programming or technical expertise. More features & capabilities are forthcoming."),
                 p("It is being actively developed by ",
                   a("Daniel McNichol", href="https://twitter.com/dnlmc"),
                   "for Coεmeta & will be properly documented & open-sourced asap."),
                 
                 p("Find out more about what me & my company ",
                   a("Coεmeta", href="https://twitter.com/co3meta"),
                   " are up to at ",
                   a("coemeta.xyz", href="https://coemeta.xyz")
                 ),
                 p("For more on NLP, see the ",
                   a("wikipedia entry.", href='https://en.wikipedia.org/wiki/Natural_language_processing')),
                 p("Some similar web apps taking different approaches, with various capabilities:",
                   tags$ul(
                     tags$li(a("https://voyant-tools.org/", href='https://voyant-tools.org/')),
                     tags$li(a("https://languagevariationsuite.shinyapps.io/TextMining/", href='https://languagevariationsuite.shinyapps.io/TextMining/')),
                     tags$li(a("https://infranodus.com/", href='https://infranodus.com/')),
                     )),
                 #br(),
                 hr(),
                 h2("How to Use (input data)"),
                 #br(),
                 h4("Size Limits"),
                 p("This prototype only accepts files up to 500mb."),
                 br(),
                 h4("Processing"),
                 p("Currently, ",a("'stop words'", href='https://en.wikipedia.org/wiki/Stop_word'),
                   " such as 'the', 'a', 'is', etc are automatically removed from uploaded files during processing."),
                 br(),
                 h4("File Format"),
                 p("Input data can uploaded as a CSV, EPUB (ebook) or plain text (.txt) file. CSVs must be ",
                   a("tidy formatted", href='https://r4ds.had.co.nz/tidy-data.html'),
                   ", with text to be analyzed in a column named 'text'.",
                   br(), "(other column names can be anything)"), 
                 p("All this means is that the data should be organized as a single 
                   'rectangular' table / dataframe, where:",
                   tags$ul(
                     tags$li("the first row is a SINGLE row of column headers (names)"),
                     tags$li("each 'variable' / 'feature' of the dataset has its own column"),
                     tags$li("each individual 'observation' or 'entry' of the dataset is its own row."),
                     tags$li("Lastly, the file should be saved as a comma-separated (.csv) text file, 
                             with a column named 'text' which contains the text to be analyzed.")
                   )),
                 p("This can be confusing in words, but is actually quite a
                   simple & natural tabular data format, optimized for both human & machine-readability.
                   See the link above for greater detail, or just mimic the format of the built-in demo
                   dataset:"),
                 #br(),
                 hr(),
                 tableOutput("demo_table"),
                 hr(),
                 downloadButton("demo_download", "Download sample CSV in required format"),
                 br(),
                 br(),
                 p("As seen above, the 'text' column contains the natural language data
                    to be anlayzed, with additional categorical 'id' & 'label' columns, 
                    & a numerical 'score' column. In this case, each row is a single article
                    headline, each with a unique 'id', a section 'label' & a randomly generated
                    'score', for numerical demonstration purposes. (The only required column 
                    name is 'text', the rest are optional)"),
                  
                 p("This is a tidy dataset because each variable is a column (id, text, label, 
                    score) & each observation or entry (unique headline) is a row. 
                    In NLP terms, we might consider each headline a 'document', & the entire 
                    collection of documents (i.e. the whole dataset) a 'corpus' (see the tf-idf tab). 
                    But we could also
                    decide that each 'label' designates a unique document (or corpus), depending
                    on how we want to conduct our analysis. The important thing is that we have 
                    everything in one file, formatted appropriately, with categorial variables
                    designating the different dimensions by which we might want to slice & dice 
                    or aggragate the data. You should structure your input data similarly."),
                    
                 p("You can download the sample CSV file depicted above to see how it looks
                   as a text file, or read into excel or google sheets to replicate the format. 
                   And feel free to contact @dnlmc with any questions or problems.")
        )
        #em("tktktk"))
    )
)
)
)


# server -----------------------------
server <- function(input, output) {

  
  # file initialization stuff ------------
  
      # set file upload size limit to 500mb
    options(shiny.maxRequestSize=10*1024^2)
    
  
    # load demo data for 'How to Use" tab only
    demo_data <- read.csv("nlp_test.csv",nrows = 5)
    output$demo_table <- renderTable(demo_data)
    
    
    # Make demo data downloadable
    output$demo_download <- downloadHandler(
      filename = 'nlpapp_demo_dataformat.csv',
      content = function(file) {
        write.csv(demo_data, file)
      }
    )
  
    # initialize resettable tibble 
    # per: https://stackoverflow.com/questions/49344468/resetting-fileinput-in-shiny-app
    tib <- reactiveValues(data = NULL,
                          clear = FALSE,
                          epub = FALSE)
  
    # make reactive tibble with either uploaded file or demo data
    observe({req(input$file)  #require file or stop
             req(!tib$clear)
                    
             tib$data <- readr::read_csv(input$file$datapath)
            })
    
    # for epub file
    observe({req(input$epub_file)  #require file or stop
      req(!tib$clear)
      
      temp_tib <- epubr::epub(file = input$epub_file$datapath)
      
      tib$data <- temp_tib$data[[1]]
      
      #tib$epub <- temp_tib$data[[1]]
      
      tib$epub <- TRUE
    })
    
    
    # hi-pri observer to allow file upload to override tibble assignment
    observeEvent(input$file,
                {tib$clear <- FALSE
                tib$epub <- FALSE},
                priority = 1000)
    
    observeEvent(input$epub_file,
                 {tib$clear <- FALSE},
                 priority = 1000)
    
    # hi-pri observer to allow demo data to override tibble assignment
    observeEvent(input$use_demo_data_button, 
                 {tib$data <- readr::read_csv("nlp_test.csv") #NULL
                 tib$clear <- TRUE
                 tib$epub <- FALSE
                 reset('file')}, 
                 priority = 1000)
    
    
    # Make dynamic newline selector to create epub section titles
    ## see: https://shiny.rstudio.com/articles/dynamic-ui.html
    
    newline_choices <- reactive({req(tib$epub) 
                        
                          list("none" = 0,"1"=1,"2"=2,"3"=3) })
    
    output$newline_ui <- renderUI({
      selectInput("num_newline", 
                   label = h5(strong("Choose # of newline separators"), 
                              br(), 
                              em("to extract section name column")), 
                   #choiceNames = as.list(c(1,2,3)),
                   #choiceValues = as.list(c(1,2,3)),
                   choices = newline_choices(),  #list("none" = 0,"1"=1,"2"=2,"3"=3),
                   # needed to retain selections
                   selected = 0
      ) 
    })
    
    
    
    #Read columns of tib$data for groupby selector
    #tib_cols <- reactive("NA")
    tib_cols <- reactive({req(tib$data)
                         tib$data %>% 
                          select(-text) %>% # exclude "text" column
                          # include only factor, char & logical columns
                          select_if(function(col) {is.factor(col) || is.character(col) || is.logical(col)}) %>% 
                          colnames()
                      })
    
    # Make dynamic groupby selector using tib_cols() 
    ## see: https://shiny.rstudio.com/articles/dynamic-ui.html
    output$groupby_ui <- renderUI({
                          checkboxGroupInput("check_groupby", 
                                            label = h5(strong("Select variable to group by:"), 
                                                       br(), 
                                                       em("(high cardinality variables will"),
                                                       br(),
                                                       em("cause app to lag or freeze)")), 
                                            choiceNames = as.list(tib_cols()),
                                            choiceValues = as.list(tib_cols()),
                                            # needed to retain selections
                                            selected = colnames(select_if(tib$data, is.factor)) 
                                            )
                          })
    
    # Set reactive group_levels object
    group_level_rct <- reactive({req(tib$group_levels)
      tib$data %>% select(input$check_groupby) %>% unique() %>% as.vector()
    })
    
    
    # Make dynamic select filter of grouped column using input$check_groupby() ?
    # (to drop bs categories causing clutter & issues)
    ## see: https://shiny.rstudio.com/articles/dynamic-ui.html
    output$filter_ui <- renderUI({
      checkboxGroupInput("check_filter", 
                         label = h5(strong("Select values to drop from grouped variable"), 
                                    br(), 
                                    em("(to de-clutter, etc)") ), 
                         choices = group_level_rct()[[1]],
                         #choiceNames = as.list(group_level_rct()),
                         #choiceValues = as.list(group_level_rct()),
                         # needed to retain selections
                         #selected = colnames(select_if(tib$data, is.factor)) 
      )
    })
    
    
    # filter out grouped values as selected
    observeEvent(input$check_filter, {
      
      tib$data <- tib$data %>% filter(!! sym(input$check_groupby) != input$check_filter)
      print(input$check_groupby)
      print(input$check_filter)
      })
    
    
    # create section title column for ebooks
    observeEvent(input$num_newline, {
                 
                if(input$num_newline == 0){
                  if(!is.null(tib$data$section_title)){
                    tib$data <- tib$data %>% select(-section_title)}
                  if(is.null(tib$data$section_title)){
                      tib$data <- tib$data }
                  }
                
      
                 if(input$num_newline == 1){
                   
                   tib$data <- tib$data %>%
                     mutate(section_title = (str_extract(text, "(.*)(\\\n)") %>%
                                               str_replace("\\\n","")) )
                 }
                 
                 if(input$num_newline == 2){

                   tib$data <- tib$data %>%
                     mutate(section_title = (str_extract(text, "(.*)(\\\n)(.*)(\\\n)") %>%
                                               str_replace("\\\n"," ")) )
                 }

                 if(input$num_newline == 3){

                   tib$data <- tib$data %>%
                     mutate(section_title = (str_extract(text, "(.*)(\\\n)(.*)(\\\n)(.*)(\\\n)") %>%
                                               str_replace("\\\n"," ")) )
                 }
                 
                })
    
    
    # Change groupby columns to factors
    observeEvent(input$check_groupby, {
                 tib$data <- tib$data %>% mutate_at(input$check_groupby, as.factor)
                 tib$group_levels <- tib$data %>% select(input$check_groupby) %>% unique() %>% as.vector()
                })
    
    # Make datatable using reactive values tibble 'tib$data'
    output$table <- DT::renderDT({
                        DT::datatable(tib$data,
                                      options = list(filter='top'))
                        })
    
    # Generate summaries of data
    output$sum <- renderPrint(summary(tib$data))
    output$str <- renderPrint(glimpse(tib$data))
    
    # Skimr has trouble with non-ASCII characters so have to clean
    observe({req(tib$data$text)  #require tib$data or stop
            skimtib <- tib$data
            skimtib$text <- iconv(skimtib$text, "UTF-8", "ASCII", sub="")
            output$skim <- renderPrint(skim(skimtib))
    })
    
    
    
    
    
  # Word count tab functions -------
    output$cats_value <- renderPrint({ input$cats })
    
    # tokens with no stop words
    tokens <- reactive({req(tib$data)
                tib$data %>% 
                    unnest_tokens(word, input = text,
                                  collapse = tib_cols() ) %>% 
                    anti_join(get_stopwords()) %>% 
                    rename(ngram = word)
                
                })
    
    # if (is.null(input$check_groupby) ) {
    #   counts <- reactive({req(tokens())
    #                 tokens() %>% unnest_tokens(output="ngram",
    #                                            input="ngram",
    #                                            token = "ngrams",
    #                                            n = as.integer(input$gram) ) %>% 
    #                     count(ngram, sort = T) %>% 
    #                     filter(nchar(ngram) >= input$word_length)
    #                 }) 
    #   }          
      
    #if (!is.null(input$check_groupby) ) {
      counts <- reactive({req(tokens())        
                  tokens() %>% group_by_at(vars(-ngram)) %>% 
                    summarise(full_text = toString(ngram) ) %>% 
                    
                    ungroup() %>% 
                    
                    unnest_tokens(output= ngram,
                                             input= full_text,
                                             token = "ngrams",
                                             n = as.integer(input$gram) ) %>% 
                    count(ngram, !!! syms(input$check_groupby), sort = T) %>% 
                    #group_by_at(vars(one_of(input$check_groupby))) %>% 
                    #summarise(n = n()) %>% 
                    #ungroup() %>% 
                    filter(nchar(ngram) >= input$word_length)
                
                #if (input$gram == 1)
                #    tokens() %>% count(ngram, sort = T)
                })
     # }

    # Make count datatable 
    output$count_table <- DT::renderDT({
                            DT::datatable( counts() )
                            })
    
    # Make count table downloadable
    output$count_download <- downloadHandler(
        filename = 'ngram_counts.csv',
        content = function(file) {
            write.csv(counts(), file)
        }
    )
    
    # Make count plot
    output$plot <- renderPlot({
                   if (length(input$check_groupby) < 1 ) {
                    p <- counts() %>% 
                    #filter(#n > 20 &
                    #    #str_detect(survey_segment, "sun") &
                    #    #survey_segment == 'all_digital_nps_oct2018' &
                    #    detractor_promoter != 'neutral') %>% 
                    arrange(desc(n)) %>%
                    mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>% 
                    #group_by(!!! syms(input$check_groupby)) %>%
                    
                    head(input$count_plot_n) %>%
                    #ungroup %>% 
            
                     ggplot(aes(ngram, n)) + 
                            geom_col(show.legend = F) + 
                            #geom_col(aes(fill = input$check_groupby), show.legend = F) + 

                            labs(y = "# of times n-gram appears", x = "n-gram") +
                            coord_flip() +
                            theme(axis.title = element_text(size = rel(input$count_plot_fontsize)),
                                  axis.text = element_text(size = rel(input$count_plot_fontsize)))
                            #ggtitle("Top Ngrams")
                   }

                  if (length(input$check_groupby) > 0) {
                    
                    p <- counts() %>% 
                      #filter(#n > 20 &
                      #    #str_detect(survey_segment, "sun") &
                      #    #survey_segment == 'all_digital_nps_oct2018' &
                      #    detractor_promoter != 'neutral') %>% 
                      arrange(desc(n)) %>%
                      mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>% 
                      group_by(!!! syms(input$check_groupby)) %>%
                      
                      slice_max(order_by = n, n = input$count_plot_n, with_ties = FALSE) %>%
                      ungroup %>% 
                      
                      # solution to get ordering correct in each facet independently
                      # via https://juliasilge.com/blog/reorder-within/
                      mutate( facet_jawn = as.factor(!!! syms(input$check_groupby)),
                             ngram = reorder_within(ngram, n, !!! syms(input$check_groupby)) ) %>% 
                    
                    
                      #mutate(ngram = reorder_within(ngram, n, !!! syms(input$check_groupby))) %>%
                      
                      ggplot(aes(ngram, n, fill = facet_jawn)) + 
                      #geom_col(show.legend = F) + 
                      geom_col(show.legend = F) + 
                      facet_wrap(vars(!!! syms(input$check_groupby)),
                                 #nrow= length(input$check_groupby),
                                 #ncol = as.integer(round( length(input$check_groupby) / 3 , 0)),
                                 scales = "free") +
                      labs(y = "# of times n-gram appears", x = "n-gram") +
                      coord_flip() +
                      scale_x_reordered() +
                      theme(axis.title = element_text(size = rel(input$count_plot_fontsize)),
                            axis.text = element_text(size = rel(input$count_plot_fontsize)))
                    #ggtitle("Top Ngrams")
                      }
                    p
                    })
    
    output$plot_ui <- renderUI({
        plotOutput("plot", 
                   height = paste0(input$count_plot_height, "px"),
                   width = paste0(input$count_plot_width, "px"))
    })
    


  # tf-idf tab functions ------

    # create tf-idf reactive tibble
    #grams_tfidf <- grams %>% bind_tf_idf(ngram, label, n)
    
    
    
    tfidf <- reactive({req(counts()) 
                    counts() %>% bind_tf_idf(ngram, !!sym(input$check_groupby), n) #label
    })
  
    # Make tf-idf plot
    output$tfidf_plot <- renderPlot({
        
        p <- tfidf() %>%
          
          arrange(desc(n)) %>%
          mutate(ngram = factor(ngram, levels = rev(unique(ngram)))) %>%
          group_by(!!! syms(input$check_groupby)) %>%
          
          slice_max(order_by = tf_idf, n = input$tfidf_plot_n, with_ties = FALSE) %>% 
          #slice_max(tf_idf, n = 15) %>%
          
          ungroup() %>%
          
          # solution to get ordering correct in each facet independently
          # via https://juliasilge.com/blog/reorder-within/
          mutate( facet_jawn = as.factor(!!! syms(input$check_groupby)),
                  ngram = reorder_within(ngram, tf_idf, !!! syms(input$check_groupby)) ) %>% 
          
          ggplot(aes(ngram, tf_idf, fill = facet_jawn)) + 
          #geom_col(show.legend = F) + 
          geom_col(show.legend = F) + 
          facet_wrap(vars(!!! syms(input$check_groupby)),
                     #nrow= length(input$check_groupby),
                     #ncol = as.integer(round( length(input$check_groupby) / 3 , 0)),
                     scales = "free") +
          labs(y = "tf-idf", x = "n-gram") +
          coord_flip() +
          scale_x_reordered() +
          theme(axis.title = element_text(size = rel(input$tfidf_plot_fontsize)),
                axis.text = element_text(size = rel(input$tfidf_plot_fontsize)))
          
          
          
          # ggplot(aes(tf_idf, fct_reorder(ngram, tf_idf), fill = facet_jawn )) +
          #   geom_col(show.legend = FALSE) +
          #   facet_wrap(vars(!!! syms(input$check_groupby)), #ncol = 2, 
          #              scales = "free") +
          #   labs(x = "tf-idf", y = NULL) +
          #   theme(axis.title = element_text(size = rel(input$tfidf_plot_fontsize)),
          #         axis.text = element_text(size = rel(input$tfidf_plot_fontsize)))
      
      p
      })
    
    output$tfidf_plot_ui <- renderUI({
      plotOutput("tfidf_plot", 
                 height = paste0(input$tfidf_plot_height, "px"),
                 width = paste0(input$tfidf_plot_width, "px"))
    })

    
    
} # end of server section    

# Run the application ---------------
shinyApp(ui = ui, server = server)

#}

#runApp()
