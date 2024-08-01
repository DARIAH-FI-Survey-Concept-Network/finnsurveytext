
library(shiny)
library(shinyjs)
ui <- fluidPage(
  useShinyjs(),
  titlePanel(
    "`finnsurveytext` package demo"
  ),
  tabsetPanel(
    tabPanel("Instructions"),
    tabPanel("Prepare Data",
      navlistPanel(
        id = 'tabset1',
        "Load Data",
        tabPanel("Load Data",
          h3("Introduction"),
          p("This app demonstrates functionality available through the ", strong(em("finnsurveytext")),
             "package without the requirement to use R.
            You can either upload a .csv file of your data, or choose to use
            one of the sample datasets."),
          p("(The package also allows as an input", em("svydesign"),
            " objects from the ", strong(em("survey")), "package but this is
            not demonstrated in this app.)"),
          h3("Optional: upload your data"),
          fluidRow(
            column(6,
                   fileInput("upload", "Upload data (Max size is 10 MB)", accept = c(".csv", ".tsv")),
            ),
            column(6,
                   radioButtons("sep", "Separator",
                                c("comma", "semicolon", "tab"),
                                selected = "comma")
            )
          ),
          h3("Choose data"),
          fluidRow(
            selectInput("data", "What data should we use?", c("uploaded data", "child (sample data)", "dev_coop (sample data)"), "child (sample data)"),
            actionButton("showraw", "Press this to show/hide raw data", class = "btn-info"),
            hidden(DT::DTOutput("rawtable"))
          )
        ),
        "Format Data",
        tabPanel("Format Data",
          h3("Instructions"),
          p(strong(em("finnsurveytext")), "functions require data formated into",
            strong("CoNLL-U"), "format. To learn more about the format, see",
            a(href="https://universaldependencies.org/format.html", "the Universal Dependencies Project"), "."),
          p("This panel is used to format the data for later steps."),
          p("Use the dropdowns to choose which columns in your data contain
            your open-ended question, the IDs, and whether to include weights
            or other additional columns. Pick which Finnish language model to use and which list of stopwords to remove from the data"),
          p("When you're ready to format the data, click the button below."),
          p("(The only way to weight your data in this demonstration is via the
            \"weights from column\" since the use of ", em("svydesign"), " objects is not demonstrated in this app.)"),
          h3("Format your data"),
          fluidRow(
            column(6,
                   selectInput('question', "Which question/column containts the open-ended question?", ""),
                   selectInput('id', "Which question/column containts the id?", ""),
                   radioButtons("model", "Which Finnish language model should we use?", c("ftb", "tdt")),
                   radioButtons("swords", "Which stopwords list should we use?", c("nltk", "snowball", "stopwords-iso", "none")),

            ),
            column(6,
                   selectInput('weights', "OPTIONAL: Which question/column contains the weights?", ""),
                   selectInput('addcols', 'OPTIONAL: Which additional columns should we include in the formatted data?', "", multiple = TRUE),
            )
          ),
          fluidRow(
            actionButton("format", "Press this to format your data", class = "btn-success"),
            actionButton("showformat", "Press this to toggle whether to show your formatted data", class="btn-info"),
            DT::DTOutput("formattedtable")
          ),
        ),
      ),
    ),
    tabPanel("Explore Data",
      navlistPanel(
        id = "tabset2",
        "Summary Tables",
        tabPanel("Summary Tables",
                 h3("Instructions"),
                 p("This tab is used to create 3 basic summary tables of you data.
                   Choose which table you would like to create and then press the create table button."),
                 h3("Summary Tables"),
                 radioButtons('summarytable', "Which summary table would you like to see?", c("response", "length", "part-of-speech")),
                 actionButton("makest", "Press this to make the table", class = "btn-success"),
                 actionButton("showsum", "Press this to toggle whether to show your summary table", class="btn-info"),
                 tableOutput("st")
        ),
        "Wordcloud",
        tabPanel("Wordcloud",
                 h3("Wordcloud"),
                 p("The panel creates a wordcloud which visualises the frequency of
                   words in our data (more frequent words are larger in the cloud.
                   You can exclude specific word-types using the checklist on the
                   right-hand side."),
                 p("If you have included weights when formatting your data, you can
                   use these to weight the words in the cloud."),
                 fluidRow(
                   column(6,
                          numericInput('maxwc', 'What is the maximum number of words to show?', 100),
                          radioButtons('weightswc', 'Do you want to weight responses in wordcloud?', c('no weights', 'weights from formatted data'), 'no weights')
                   ),
                   column(6,
                          checkboxGroupInput('poswc',
                                             'Which POS tags should we include?',
                                             c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                             c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X")
                          ),
                   ),
                 ),
                 fluidRow(
                   actionButton("makewc", "Press this to make the wordcloud", class = "btn-success"),
                   actionButton("showwc", "Press this to toggle whether to show your wordcloud", class="btn-info"),
                   plotOutput("wc")
                 ),
        ),
        "Frequent Words/Phrases",
        tabPanel("N-grams",
                 h3("N-grams"),
                 p("A n-gram is a set of N words in order."),
                 p("The tab is used to create a plot of the most common words/phrases in your formatted data."),
                 p("Use the dropdowns to indicate what size n-gram you want to plot and how many n-grams to show.
                   You can also indicate if you want to normalise the data and/or
                   use weights and exclude word types if you want to.
                   Also, you can indicate whether to strictly cut-off at the cut-off number or show equally-occuring words."),
                 fluidRow(
                   column(6,
                          numericInput('ngng', 'What size n-gram should we show? (To show top words, choose 1)', 1, 1, 5),
                          radioButtons('strictng', 'How should we deal with ties?', c('strict cut-off, show first-occurring alphabetically', 'show ties')),
                          numericInput('numberng', 'How many words/phrases should we show?', 10),
                          radioButtons('normng', 'Should we normalise the data?', c("NULL (pick this also if you want to use weights)", "number of words", "number of responses")),
                          radioButtons('weightsng', 'Do you want to weight responses in table?', c('no weights', 'weights from formatted data'), 'no weights'),
                          textInput('nameng', 'Would you like to add a name to plot title?', ''),
                   ),
                   column(6,
                          checkboxGroupInput('posng',
                                             'Which POS tags should we include?',
                                             c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                             c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                          ),
                   ),
                 ),
                 fluidRow(
                   actionButton("makeng", "Press this to make the n-gram plot", class = "btn-success"),
                   actionButton("showng", "Press this to toggle whether to show your n-gram plot", class="btn-info"),
                   plotOutput('ng'),
                 ),

        ),
        "Concept Network",
        tabPanel("Concept Network",
                 h3("Concept Network"),
                 p("Our concept network function uses the TextRank algorithm which
                    is a graph-based ranking model for text processing. Vertices
                    represent words and co-occurrence between words is shown through
                    edges. Word importance is determined recursively (through the
                    unsupervised learning TextRank algorithm) where words get more
                    weight based on how many words co-occur and the weight of these
                    co-occurring words."),
                 p("To utilise the TextRank algorithm in ", strong(em("finnsurveytext")),
                   ", we use the", strong(em("textrank")), " package. For further
                   information on the package, please see ",
                   a(href="https://cran.r-project.org/web/packages/textrank/vignettes/textrank.html", "this documentation"),
                   ". This package implements the TextRank and PageRank algorithms.
                   (PageRank is the algorithm that Google uses to rank webpages.)"),
                 p("You can read about the underlying TextRank algorithm ",
                   a(href="https://web.eecs.umich.edu/~mihalcea/papers/mihalcea.emnlp04.pdf", "here"),
                   "and about the PageRank algorithm ",
                   a(href="https://www.sciencedirect.com/science/article/pii/S016975529800110X", "here"),
                   "."
                   ),
                 fluidRow(
                   column(6,
                     textInput('concepts', "What concept words do you want? Please separate these with commas."),
                     numericInput('thresholdcn', 'What is the minimum number of co-occurences you would like to plot?', 0),
                     radioButtons('normcn', 'Should we normalise the data?', c("NULL", "number of words", "number of responses")),

                     textInput('titlecn', "Optional, a title can be added here"),
                   ),
                   column(6,
                     checkboxGroupInput('poscn',
                                        'Which POS tags should we include?',
                                        c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                        c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                     ),
                   ),
                 ),
                 fluidRow(
                   actionButton("makecn", "Press this to make the concept network plot", class = "btn-success"),
                   actionButton("showcn", "Press this to toggle whether to show your concept network plot", class="btn-info"),
                   plotOutput('cn'),
                 ),
        )
      ),
    ),
    tabPanel("Compare Groups of Responses",
             h3("Comparison Functions"),
             fluidRow(
               column(4,
                      selectInput('ac2', 'Which field would you like to use to split the data for comparison', ''),
                      radioButtons('exnulls', 'Would you like to exclude nulls in the comparison field?', c('Yes', 'No')),
                      ),
               column(8,
                      p("There are counterpart comparison functions for each of the
             functions in the previous \"Explore Data\" tab."),
                      p("Recall that when you preprocessed the data, you were given the
             option to include additional columns. These columns can now be used
             to allow for comparison between respondents based on these values."),
                      p("On the left, you can pick which column to use to split
                        the data, and also indicate what to do with responses which have a null in
                        this splitting column.")
                      )
             ),
             navlistPanel(
               id = "tabset3",
               "Comp. Tables",
               tabPanel("Comparison Summary Tables",
                        h3("Comparison Summary Tables"),
                        p("As previously, you can pick which summary table to show here."),
                        radioButtons('compsummarytable', "Which comparison summary table would you like to see?", c("response", "length", "part-of-speech")),
                        actionButton("makecst", "Press this to make the table", class = "btn-success"),
                        actionButton("showcsum", "Press this to toggle whether to show your summary table", class="btn-info"),
                        tableOutput("cst")
               ),
               "Comp. Cloud",
               tabPanel("Comparison Cloud",
                        h3("Comparison Cloud"),
                        p("The comparison cloud extends the wordcloud concept."),
                        p("A comparison cloud compares the relative frequency with
                        which a term is used in two or more documents. This cloud
                        shows words that occur more regularly in responses from
                        a specific type of respondent."),
                        p("For more information about comparison clouds, you can read ",
                        a(href="https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf", "this documentation"), "."),
                        fluidRow(
                          column(6,
                                 numericInput('maxcc', 'What is the maximum number of words to show?', 100),
                                 radioButtons('weightscc', 'Do you want to weight responses in wordcloud?', c('no weights', 'weights from formatted data'), 'no weights'),
                          ),
                          column(6,
                                 checkboxGroupInput('poscc',
                                                    'Which POS tags should we include?',
                                                    c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                                    c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                 ),
                          ),
                        ),
                        fluidRow(
                          actionButton("makecc", "Press this to make the comparison cloud", class = "btn-success"),
                          actionButton("showcc", "Press this to toggle whether to show your comparison cloud", class="btn-info"),
                          plotOutput("ccloud"),
                        ),
               ),
               "Comp. of Freq. Words",
               tabPanel("Comparison N-grams",
                        h3("Comparison N-grams"),
                        p("The comparison n-grams function creates plots comparing
                          the most frequent n-grams in your data. N-grams which are
                          unique to one group of respondents are highlighted in the
                          tables."),
                        p("Normalising the data, or using weights is a good idea to
                          account for differences in response numbers between groups."),
                        fluidRow(
                          column(6,
                                 numericInput('cngng', 'What size n-gram should we show? (To show top words, choose 1)', 1, 1, 5),
                                 radioButtons('strictcng', 'How should we deal with ties?', c('strict cut-off, show first-occurring alphabetically', 'show ties')),
                                 numericInput('numbercng', 'How many words/phrases should we show?', 10),
                                 radioButtons('normcng', 'Should we normalise the data?', c("NULL (pick this also if you want to use weights)", "number of words", "number of responses")),
                                 radioButtons('weightscng', 'Do you want to weight responses in table?', c('no weights', 'weights from formatted data'), 'no weights'),
                          ),
                          column(6,
                                 checkboxGroupInput('poscng',
                                                    'Which POS tags should we include?',
                                                    c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                                    c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                 ),
                          ),
                        ),
                        fluidRow(
                          actionButton("makecng", "Press this to make the n-gram plot", class = "btn-success"),
                          actionButton("showcng", "Press this to toggle whether to show your n-gram plot", class="btn-info"),
                          plotOutput("cng"),
                        ),

               ),
               "Comp. Concept Network",
               tabPanel("Comparison Concept Network",
                        h3("Comparison Concept Network"),
                        p("The comparison concept network function creates a
                          separate network for each group and highlights any words
                          which are unique to one group."),
                        p("Normalising the data, or using weights is a good idea to
                          account for differences in response numbers between groups."),
                        fluidRow(
                          column(6,
                                 textInput('cconcepts', "What concept words do you want? Please separate these with commas."),
                                 numericInput('thresholdccn', 'What is the minimum number of co-occurences you would like to plot?', 0),
                                 radioButtons('normccn', 'Should we normalise the data?', c("NULL", "number of words", "number of responses")),
                          ),
                          column(6,
                                 checkboxGroupInput('posccn',
                                                    'Which POS tags should we include?',
                                                    c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X"),
                                                    c("ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET", "INTJ", "NOUN", "NUM", "PART", "PRON", "PROPN", "PUNCT", "SCONJ", "SYM", "VERB", "X")
                                 )
                          )
                        ),
                        fluidRow(
                          actionButton("makeccn", "Press this to make the concept network plot", class = "btn-success"),
                          actionButton("showccn", "Press this to toggle whether to show your concept network plot", class="btn-info"),
                          plotOutput('ccn')
                        )
               )
             )
    )
  )
)



server <- function(input, output, session) {
  options(shiny.maxRequestSize=10*1024^2)
  sep2 <- reactive({
    if (input$sep == 'comma') {
      x <- ","
    } else if (input$sep == 'semicolon') {
      x <- ";"
    } else if (input$sep == 'tab') {
      x <- "\t"
    }
  })
  upl <- reactive({
    inFile <- input$upload
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE, sep = sep2())
    return(data)
  })

  df <- reactive({
    if (input$data == "child (sample data)") {
      x <- get("child")
    } else if (input$data == "dev_coop (sample data)") {
      x <- get("dev_coop")
    } else if (input$data == "uploaded data") {
      x <- upl()
    }
    x
  })
  # dset <- reactive({
  #   df()
  # })
  output$rawtable <- DT::renderDT({
    df()
  })
  observeEvent(input$showraw, toggle("rawtable"))
  column_choices <- reactive({
    mydata <- df()
    names(mydata)
  })
  column_choices2 <- reactive({
    mydata <- df()
    x <- list('NO WEIGHTS')
    x <- c(x, names(mydata))
    x
  })
  observe({
    updateSelectInput(session, "question",
                      choices = column_choices()
    )})
  observe({
    updateSelectInput(session, "id",
                      choices = column_choices()
    )})
  observe({
    updateSelectInput(session, "weights",
                      choices = column_choices2()
    )})
  observe({
    updateSelectInput(session, "addcols",
                      choices = column_choices()
    )})
  question2 <- reactive({input$question})
  id2 <- reactive({input$id})
  weights2 <- reactive({
    if (input$weights == 'NO WEIGHTS') {
      NULL
    } else {
      input$weights
    }
  })
  addcols2 <- reactive({
    input$addcols
  })
  mod2 <- reactive({input$model})
  swords2 <- reactive({input$swords})
  qns <- reactive(colnames(df()))
  ft <- eventReactive(input$format, {
    finnsurveytext::fst_prepare(df(),
                                question2(),
                                id2(),
                                mod2(),
                                swords2(),
                                weights2(),
                                add_cols = addcols2(),
                                manual = FALSE,
                                manual_list = "")

  })
  output$formattedtable <- DT::renderDT({
    ft()
  })
  observeEvent(input$showformat, toggle("formattedtable"))
  sum <- reactive({input$summarytable})
  st2 <- eventReactive(input$makest, {
    if (sum() == 'response') {
      sumtable <- finnsurveytext::fst_summarise(ft())
    } else if (sum() == 'length') {
      sumtable <- finnsurveytext::fst_length_summary(ft(),
                                                     incl_sentences = TRUE)
    } else if (sum() == 'part-of-speech') {
      sumtable <- finnsurveytext::fst_pos(ft())
    }
    sumtable
  })
  observeEvent(input$showsum, toggle("st"))
  output$st <- renderTable({
    st2()
  })
  pf <- reactive({input$poswc})
  mx <- reactive({input$maxwc})
  we <- reactive({input$weightswc})
  we_sd <- reactive({
    if (we() == 'weights from svydesign object') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  we_cw <- reactive({
    if (we() == 'weights from formatted data') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  wc2 <- eventReactive(input$makewc, {
    finnsurveytext::fst_wordcloud(ft(),
                                  pos_filter = pf(),
                                  max = mx(),
                                  use_svydesign_weights = we_sd(),
                                  id = "",
                                  svydesign = NULL,
                                  use_column_weights = we_cw()
    )
  })
  output$wc <- renderPlot({
    wc2()
  })
  observeEvent(input$showwc, toggle("wc"))
  pfng <- reactive({input$posng})
  mxng <- reactive({input$numberng})
  ng <- reactive({input$ngng})
  nong <- reactive({
    if (input$normng == "NULL (pick this also if you want to use weights)") {
      y <- NULL
    } else if (input$normng == 'number of words') {
      y <- "number_words"
    } else if (input$normng == 'number of responses') {
      y <- "number_resp"
    } else {
      y <- NULL
    }
    y
  })
  stng <- reactive({
    if (input$strictng == 'strict cut-off, show first-occurring alphabetically') {
      z <- TRUE
    } else if (input$strictng == 'show ties') {
      z <- FALSE
    }
    z
  })
  weng <- reactive({input$weightsng})
  weng_sd <- reactive({
    if (weng() == 'weights from svydesign object') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  weng_cw <- reactive({
    if (weng() == 'weights from formatted data') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  name <- reactive({
    if (!is.null(input$nameng)) {
      x <- input$nameng
    } else {
      x <- NULL
    }
  })
  ng2 <- eventReactive(input$makeng, {
    finnsurveytext::fst_ngrams(ft(),
                               number = mxng(),
                               ngrams = ng(),
                               norm = nong(),
                               pos_filter = pfng(),
                               strict = stng(),
                               name = name(),
                               use_svydesign_weights = weng_sd(),
                               id = "",
                               svydesign = NULL,
                               use_column_weights = weng_cw()
    )
  })
  output$ng <- renderPlot({
    ng2()
  })
  observeEvent(input$showng, toggle("ng"))
  cons <- reactive({input$concepts})
  thres <- reactive({input$thresholdcn})
  nocn <- reactive({
    if (input$normcn == "NULL") {
      y <- NULL
    } else if (input$normcn == 'number of words') {
      y <- "number_words"
    } else if (input$normcn == 'number of responses') {
      y <- "number_resp"
    } else {
      y <- NULL
    }
    y
  })
  pfcn <- reactive({input$poscn})
  ticn <- reactive({input$titlecn})
  cn2 <- eventReactive(input$makecn, {
      finnsurveytext::fst_concept_network(ft(),
                                          concepts = cons(),
                                          threshold = thres(),
                                          norm = nocn(),
                                          pos_filter = pfcn(),
                                          title = ticn())
    })
  output$cn <- renderPlot({
    cn2()
  })
  observeEvent(input$showcn, toggle("cn"))
  observe({
    updateSelectInput(session, "ac2",
                      choices = addcols2()
    )})
  comp_col <- reactive({input$ac2})
  exnu <- reactive({
    if (input$exnulls == 'Yes') {
      x <- TRUE
    } else if (input$exnulls == 'No') {
      x <- FALSE
    }
    x
  })
  csum <- reactive({input$compsummarytable})
  cst2 <- eventReactive(input$makecst, {
    if (csum() == 'response') {
      csumtable <- finnsurveytext::fst_summarise_compare(ft(),
                                                         field = comp_col(),
                                                         exclude_nulls = exnu(),
                                                         rename_nulls = 'null_data'
                                                         )
    } else if (csum() == 'length') {
      csumtable <- finnsurveytext::fst_length_compare(ft(),
                                                     incl_sentences = TRUE,
                                                     field = comp_col(),
                                                     exclude_nulls = exnu(),
                                                     rename_nulls = 'null_data')
    } else if (csum() == 'part-of-speech') {
      csumtable <- finnsurveytext::fst_pos_compare(ft(),
                                                  field = comp_col(),
                                                  exclude_nulls = exnu(),
                                                  rename_nulls = 'null_data')
    }
    csumtable
  })
  observeEvent(input$showcsum, toggle("cst"))
  output$cst <- renderTable({
    cst2()
  })
  pfcc <- reactive({input$poscc})
  mxcc <- reactive({input$maxcc})
  wecc <- reactive({input$weightscc})
  we_sdcc <- reactive({
    if (wecc() == 'weights from svydesign object') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  we_cwcc <- reactive({
    if (wecc() == 'weights from formatted data') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  cc2 <- eventReactive(input$makecc, {
    plot <- NULL
    plot <- finnsurveytext::fst_comparison_cloud(ft(),
                                         field = comp_col(),
                                         pos_filter = pfcc(),
                                         max = mxcc(),
                                         use_svydesign_weights = we_sdcc(),
                                         id = "",
                                         svydesign = NULL,
                                         use_column_weights = we_cwcc(),
                                         exclude_nulls = exnu(),
                                         rename_nulls = "null_data"
    )
    plot
  })
  output$ccloud <- renderPlot({
    cc2()
  })
  observeEvent(input$showcc, toggle("ccloud"))
  pfcng <- reactive({input$poscng})
  mxcng <- reactive({input$numbercng})
  cng <- reactive({input$cngng})
  nocng <- reactive({
    if (input$normcng == "NULL (pick this also if you want to use weights)") {
      y <- NULL
    } else if (input$normcng == 'number of words') {
      y <- "number_words"
    } else if (input$normcng == 'number of responses') {
      y <- "number_resp"
    } else {
      y <- NULL
    }
    y
  })
  stcng <- reactive({
    if (input$strictcng == 'strict cut-off, show first-occurring alphabetically') {
      z <- TRUE
    } else if (input$strictcng == 'show ties') {
      z <- FALSE
    }
    z
  })
  wecng <- reactive({input$weightscng})
  wecng_sd <- reactive({
    if (wecng() == 'weights from svydesign object') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  wecng_cw <- reactive({
    if (wecng() == 'weights from formatted data') {
      x <- TRUE
    } else {
      x <- FALSE
    }
  })
  cng2 <- eventReactive(input$makecng, {
    finnsurveytext::fst_ngrams_compare(ft(),
                                       field = comp_col(),
                                       number = mxcng(),
                                       ngrams = cng(),
                                       norm = nocng(),
                                       pos_filter = pfcng(),
                                       strict = stcng(),
                                       use_svydesign_weights = wecng_sd(),
                                       id = "",
                                       svydesign = NULL,
                                       use_column_weights = wecng_cw(),
                                       exclude_nulls = exnu(),
                                       rename_nulls = 'null_data',
                                       unique_colour = "indianred",
                                       title_size = 20,
                                       subtitle_size = 15)
  })
  output$cng <- renderPlot({
    cng2()
  })
  observeEvent(input$showcng, toggle("cng"))



  ccons <- reactive({input$cconcepts})
  cthres <- reactive({input$thresholdccn})
  noccn <- reactive({
    if (input$normccn == "NULL") {
      y <- NULL
    } else if (input$normccn == 'number of words') {
      y <- "number_words"
    } else if (input$normccn == 'number of responses') {
      y <- "number_resp"
    } else {
      y <- NULL
    }
    y
  })
  pfccn <- reactive({input$posccn})
  ccn2 <- eventReactive(input$makeccn, {
    finnsurveytext::fst_concept_network_compare(ft(),
                                                concepts = ccons(),
                                                field = comp_col(),
                                                norm = noccn(),
                                                threshold = cthres(),
                                                pos_filter = pfccn(),
                                                exclude_nulls = exnu(),
                                                rename_nulls = 'null_data',
                                                title_size = 20,
                                                subtitle_size = 15)
  })
  output$ccn <- renderPlot({
    ccn2()
  })
  observeEvent(input$showccn, toggle("ccn"))
}
shinyApp(ui, server)
