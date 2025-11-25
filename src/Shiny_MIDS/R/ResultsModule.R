ResultsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("start"), "Start MIDS score calculations")
  )
}

ResultsServer <- function(id, parent.session, gbiffile, 
                          tab, disablestart,config,jsonschema) {
  moduleServer(id, function(input, output, module.session) {
    require(RColorBrewer)
    ns <- module.session$ns
    

# Enable/ disable start action button -------------------------------------

    observe(
      if (disablestart() == TRUE){
        shinyjs::disable("start")
      } else {
        shinyjs::enable("start")
      })
    
# Calculations ------------------------------------------------------------
    
    #start spinner when calculations start
    observeEvent(input$start, {
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "cadetblue",
        text = "Calculating MIDS results")
    })
    
    #calculate mids levels and criteria
    gbif_dataset_mids <- eventReactive(input$start, {
      calculate_mids(gbiffile = gbiffile()$datapath, 
                     jsontype = "list", 
                     jsonlist = jsonschema(),
                     config = parent.session$userData$config,
                     session = module.session)
    })
    
    #remove spinner when calculations are finished
    observeEvent(gbif_dataset_mids(), {
      shinybusy::remove_modal_spinner()
    })
    

# Give warning when there were missing columns ----------------------------

    observe({
      if (length(gbif_dataset_mids()$missing) > 0){
        missing_modal(gbif_dataset_mids()$missing)
      }
    })
    
    missing_modal <- function(data) {
      showModal(modalDialog(
        title = "Warning",
        HTML(paste0("The following columns were not found in the dataset. ",
                    "These columns are regarded as NA for all records:",
                    br(),
                    paste(data, collapse= "<br>"))
        ),
        easyClose = T
      ))
    }
    
# Allow multiple results tabs --------------------------------------------
    
    #save all MIDS calculations
    allmidscalc <- reactiveValues(prev_bins = NULL)
    observeEvent(input$start, {
      allmidscalc$prev_bins[[paste0("res", input$start)]] <- gbif_dataset_mids()$results
    })
    
    #save all MIDS unmapped term lists
    allmids_unmapped <- reactiveValues(prev_bins = NULL)
    observeEvent(input$start, {
      allmids_unmapped$prev_bins[[paste0("res", input$start)]] <- gbif_dataset_mids()$missing
    })
    
    #save all MIDS implementations
    allschemas <- reactiveValues(prev_bins = NULL)
    observeEvent(input$start, {
      allschemas$prev_bins[[paste0("res", input$start)]] <- jsonschema()[1:2]
    })
    
    #save names of datasets
    alldatasetnames <- reactiveValues(prev_bins = NULL)
    observeEvent(input$start, {
      alldatasetnames$prev_bins[[paste0("res", input$start)]] <- gbiffile()$name
    })
    
    #get which result tab is active
    resulttabnr <- reactive({
      if (grepl("Results", tab())){
        as.integer(gsub("start-Results", "", tab()))
      }
    })
    
# Filters -----------------------------------------------------------------
    
    #Initialize filters when new analysis is started
    observeEvent(input$start, {
      
      # isolate the results:
      isolated_results = gbif_dataset_mids()$results
      #update rank filter to ranks that are in dataset + reset rank filter when new dataset is provided
      ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Subfamily", "Genus")
      subset_terms = colnames(isolated_results) %>%
        gsub(".*:","",.)
      ranksInDataset <- ranks[tolower(ranks) %in% subset_terms]
      updateSelectInput(parent.session, ns(paste0("rank", input$start)),
                        selected = "None",
                        choices = c("None", ranksInDataset))
      #update country filter with countries from the dataset
      country_pattern = "countryCode" %>%
        paste0(":",.,"$")
      data_colnames = colnames(isolated_results)
      matching_colid = grep(country_pattern,
                            data_colnames)
      matching_colname = data_colnames[matching_colid]
      if (length(matching_colname) > 0) {
        updateSelectInput(parent.session, ns(paste0("country", input$start)), label = "Filter on countrycode",
                          choices = sort(unique(gbif_dataset_mids()$results[[matching_colname]])))
      }
      # #update date filter with dates from the dataset
      # date_pattern = "eventDate" %>%
      #   paste0(":",.,"$")
      # matching_colid = grep(date_pattern,
      #                       data_colnames)
      # matching_colname = data_colnames[matching_colid]
      # if (length(matching_colname) > 0 && 
      #     !all(is.na(isolated_results[[matching_colname]]))){# &&
      #     #! class(isolated_results[[matching_colname]]) == "character"){
      #   updateSliderInput(parent.session, ns(paste0("date", input$start)), label = "Filter on collection date",
      #                   min = min(isolated_results[[matching_colname]], na.rm = TRUE),
      #                   max = max(isolated_results[[matching_colname]], na.rm = TRUE),
      #                   value = c(min(isolated_results[[matching_colname]], na.rm = TRUE),
      #                             max(isolated_results[[matching_colname]], na.rm = TRUE)),
      #                   timeFormat = "%m/%d/%Y")
      # } 
    })
    
    #Don't show filters if the needed values are not in the dataset
    observeEvent(resulttabnr(),{
      # if (all(is.na(allmidscalc$prev_bins[[paste0("res", resulttabnr())]][[get_full_colname("eventDate",resulttabnr(),F)]]))
      #     ){#|| class(allmidscalc$prev_bins[[paste0("res", resulttabnr())]][[get_full_colname("eventDate",resulttabnr(),F)]]) == "character"){
      #   shinyjs::hide(paste0("date", resulttabnr()))
      # } else {
      #   shinyjs::show(paste0("date", resulttabnr()))
      # }
      tabnr = resulttabnr()
      active_result = paste0("res",tabnr)
      if (all(is.na(allmidscalc$prev_bins[[active_result]][[get_full_colname("countryCode",tabnr,F)]]))){
        shinyjs::hide(paste0("country", tabnr))
      } else {
        shinyjs::show(paste0("country", tabnr))
      }
      ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Subfamily", "Genus")
      subset_terms = colnames(allmidscalc$prev_bins[[active_result]]) %>%
        gsub(".*:","",.)
      if (is_empty(ranks[tolower(ranks) %in% subset_terms])){
        shinyjs::hide(paste0("rank",tabnr))
      } else {
        shinyjs::show(paste0("rank",tabnr))
      }
    })
    
    
    #update taxonomy filter when a taxonomic rank is chosen
    observeEvent(input[[paste0("rank", resulttabnr())]], {
      tabnr = resulttabnr()
      if (input[[paste0("rank", tabnr)]] != "None"){
        #update taxonomy filter with values from the dataset
        updateSelectInput(parent.session, ns(paste0("taxonomy", tabnr)), label = "Filter on taxonomy",
                          choices = sort(unique(req(allmidscalc$prev_bins[[paste0("res", tabnr)]])[[get_full_colname("rank",tabnr)]])))
        #only show taxonomy filter when a rank is chosen
        shinyjs::show(paste0("taxonomy", tabnr))
      } else {shinyjs::hide(paste0("taxonomy", tabnr))}
    })
    
    #apply filters if they are set
    gbif_dataset_mids_filtered <- reactive({
      tabnr = resulttabnr()
      req(allmidscalc$prev_bins[[paste0("res", tabnr)]]) %>%
        {if (!is.null(input[[paste0("country", tabnr)]])) {
          filter(., .data[[get_full_colname("countryCode",tabnr,F)]] %in% input[[paste0("country", tabnr)]])} else {.}} %>%
        # {if (req(input[[paste0("date", resulttabnr())]][1]) != 0) {
        #   filter(., .data[[get_full_colname("eventDate",resulttabnr(),F)]] >= input[[paste0("date", resulttabnr())]][1])} else {.}} %>%
        # {if (req(input[[paste0("date", resulttabnr())]][2]) != 100) {
        #   filter(., .data[[get_full_colname("eventDate",resulttabnr(),F)]] <= input[[paste0("date", resulttabnr())]][2])} else {.}} %>%
        {if (input[[paste0("rank", tabnr)]] != "None" && !is.null(input[[paste0("taxonomy", tabnr)]])){
          filter(., .data[[get_full_colname("rank",tabnr)]] %in% input[[paste0("taxonomy", tabnr)]])} else {.}}
    })
    
    # function to add category back to a column
    # will not work if the same column occurs in multiple categories
    get_full_colname <- function(name,tabnr,name_is_input = T) {
      if (name_is_input) {
        name = tolower(input[[paste0(name, tabnr)]])
      }
      chosen_col = name %>%
        paste0(":",.,"$")
      data_colnames = colnames(allmidscalc$prev_bins[[paste0("res",tabnr)]])
      matching_colid = grep(chosen_col,
                            data_colnames)
      return(data_colnames[matching_colid])
    }
    
# Calculate summarized results --------------------------------------------
    
    #create summary of MIDS levels
    midssum <- reactive({
      gbif_dataset_mids_filtered() %>% group_by(MIDS_level) %>% 
        summarise(Number_of_records = n(), Percentage = round(n()/nrow(.)*100))
    })
    
    #create summary of MIDS criteria
    midscrit <- reactive({
      cbind.data.frame(
        gsub("mids:MIDS[0-3]", "", 
             names(gbif_dataset_mids_filtered()[ , grep("mids:MIDS[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE])),
        substr(names(gbif_dataset_mids_filtered()[ , grep("mids:MIDS[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE]), 10, 10),
        gbif_dataset_mids_filtered()[ , grep("mids:MIDS[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE] %>%
          map(~{sum(.x, na.rm = TRUE)}) %>% 
          as.numeric() , 
        gbif_dataset_mids_filtered()[ , grep("mids:MIDS[0-3]", names(gbif_dataset_mids_filtered())), with = FALSE] %>%  
          map(~{round((sum(.x, na.rm = TRUE) / nrow(gbif_dataset_mids_filtered()))*100)}) %>%
          as.numeric())  %>% 
        set_colnames(c("MIDS_elements", "MIDS_level", "Number_of_records","Percentage")) 
    })
    
# Set up plots ------------------------------------------------------------
    
    #define custom color scales
    #MIDS levels
    myColors <- brewer.pal(6, "Blues")[2:6]
    names(myColors) <- c("-1", "0", "1", "2", "3")
    custom_colors <- scale_fill_manual(name = "MIDS level", values = myColors)
    #MIDS criteria
    myColors2 <-  myColors[2:5]
    names(myColors2) <- c("0", "1", "2", "3")
    custom_colors2 <- scale_fill_manual(name = "MIDS level", values = myColors2)
    
    #plot mids levels
    midsplot<-reactive({
      ggplot(midssum(), aes(x=MIDS_level, y=Percentage, fill = factor(MIDS_level))) + 
        geom_bar(stat = "identity") +
        custom_colors +
        coord_cartesian(xlim = c(-1.5, 3.5)) +
        geom_text(data = subset(midssum(), Percentage >= 5), 
                  aes(y = Percentage , label = Percentage),
                  vjust = 1.25, colour = "white") +
        geom_text(data = subset(midssum(), Percentage < 5 & Percentage != 0), 
                  aes(y = Percentage , label = Percentage),
                  vjust = -0.5, colour = "black") +
        labs(x = "MIDS level", y = "Percentage of records at specified MIDS level") +
        ggtitle("MIDS levels") +
        theme(plot.title = element_text(hjust = 0.5, size = 25) , 
              plot.margin = margin(1, 1, 2, 2, "cm"),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15)) 
    })
    
    #plot mids criteria
    midscritsplot<-reactive({
      ggplot(midscrit(), aes(x= MIDS_elements, y=Percentage, fill = MIDS_level)) + 
        geom_bar(stat = "identity") + 
        scale_x_discrete(limits=midscrit()$MIDS_elements) +
        scale_y_continuous(expand=c(0,0)) +
        custom_colors2 +
        coord_flip() + 
        geom_text(data = subset(midscrit(), Percentage >= 5), 
                  aes(y = Percentage, label = Percentage), hjust = 1.25, colour = "white") +
        geom_text(data = subset(midscrit(), Percentage < 5 & Percentage != 0), 
                  aes(y = Percentage, label = Percentage), hjust = -0.5, colour = "black") +
        labs(x = "MIDS elements", y = "Percentage of records that meet the element") +
        ggtitle("MIDS elements") +
        theme(plot.title = element_text(hjust = 0.5, size = 25) , 
              plot.margin = margin(1, 1, 2, 2, "cm"),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 15)) 
    })
    
    
# Create Results tabs -----------------------------------------------------
    
    #add new tab for each analysis
    observeEvent(input$start, {appendTab(
       session = parent.session, "tabs",
       tabPanel(
         title = tags$span(paste0("Results", input$start, "    "),
                          CloseTabUI(ns(paste0("close", input$start)))), 
         value = ns(paste0("Results", input$start)), 
         sidebarLayout(
           sidebarPanel(
             helpText("Filter to view MIDS scores for part of the dataset"),
             actionButton(ns(paste0("missing_terms", 
                                    input$start)),
                          "Show Unused mappings"),
             # sliderInput(ns(paste0("date", input$start)),
             #             label = "Filter on collection date:",
             #             min = 0, max = 100, value = c(0, 100)),
             selectizeInput(ns(paste0("country", input$start)), 
                            label = "Filter on countrycode",  
                            choices = "Nothing yet",
                            multiple = TRUE),
             selectInput(ns(paste0("rank", input$start)), 
                         label = "Filter on the following taxonomic rank",
                         choices = c("None")),
             selectizeInput(ns(paste0("taxonomy", input$start)), 
                            label = "Filter on taxonomy", 
                            choices = "Select a rank first",
                            selected = "Select a rank first",
                            multiple = TRUE)
           ),
           mainPanel(
            tabsetPanel(type = "tabs",
             tabPanel("Plots",
                      plotOutput(ns(paste0("midsplot_prev", input$start))),
                      plotOutput(ns(paste0("midscritsplot", input$start)), 
                                 click = ns(paste0("midscritsplot_click", input$start)), 
                                 height="auto")),
             tabPanel("Summary tables", 
                      DT::dataTableOutput(ns(paste0("summary", input$start))), 
                      br(),
                      DT::dataTableOutput(ns(paste0("summarycrit", input$start)))),
             tabPanel("Record table", 
                      DT::dataTableOutput(ns(paste0("table", input$start)))),
             tabPanel("Export csv",
                      br(), br(),
                      downloadButton(ns(paste0("downloadData", input$start)), "Download all"),
                      br(), br(),
                      downloadButton(ns(paste0("downloadDataFiltered", input$start)), "Download filtered dataset"))
             ),
             br(),
             fluidRow(
                 column(5,
                        helpText("Dataset:"),
                        verbatimTextOutput(ns(paste0("Used_dataset", input$start))),
                        class = "well"
                 ),
                 column(6, offset = 1,
                        helpText("MIDS implementation:"),
                        fluidRow(
                        column(10,
                        verbatimTextOutput(ns(paste0("Used_MIDS_implementation", input$start)))
                        ),
                        column(2,
                               ViewImplementationUI(ns(paste0("showschema", input$start))))
                        ),
                        class = "well"
                  )
               )
           )
         )
         )
    )
      

# Render outputs ----------------------------------------------------------

    #render plots
    output[[paste0("midsplot_prev", input$start)]] <- renderPlot(midsplot())
    output[[paste0("midscritsplot", input$start)]] <- renderPlot(midscritsplot(), height = function(){max(c(nrow(midscrit())*40,400))})
    #render summaries
    output[[paste0("summary", input$start)]] <- 
      DT::renderDataTable(midssum(), rownames = FALSE, options = list(dom = 't'))
    output[[paste0("summarycrit", input$start)]] <- 
      DT::renderDataTable(midscrit(), 
                          rownames = FALSE, 
                          class = "hover",
                          selection = "none",
                          options = list(dom = 't', 
                                         paging = FALSE,
                                         rowCallback = JS(
                                           "function(row, data, displayNum, displayIndex, dataIndex) {",
                                           "  var links  = ", toJSON(paste0("https://tdwg.github.io/mids/information-elements/index.html#",
                                                                            midscrit()$MIDS_elements)),  ";",
                                           "  var idx   = dataIndex;",
                                           
                                           #// Tooltip on hover
                                           "  $('td', row).attr('title', \"Click for more info on this information element on the MIDS website (this will open a new tab).\");",
                                           
                                           #// Make cursor a pointer to signal clickability
                                           "  $(row).css('cursor', 'pointer');",
                                           
                                           #// On click: open external link in new tab
                                           "  $(row).off('click');",  #// avoid duplicate bindings on redraw
                                           "  $(row).on('click', function() {",
                                           "    window.open(links[idx], '_blank');",
                                           "  });",
                                           "}"
                          )))
    #render table
    output[[paste0("table", input$start)]] <- 
      DT::renderDataTable(gbif_dataset_mids_filtered())
    #downloads
    output[[paste0("downloadData", input$start)]] <- 
      downloadHandler(
        filename = function() {
          paste0(tools::file_path_sans_ext(alldatasetnames$prev_bins[[paste0("res", resulttabnr())]]),
                 "_MIDS.csv")
        },
        content = function(file) {
          write.csv(req(allmidscalc$prev_bins[[paste0("res", resulttabnr())]]), file, row.names = FALSE,na="")
        })
    output[[paste0("downloadDataFiltered", input$start)]] <- 
      downloadHandler(
        filename = function() {
          paste0(tools::file_path_sans_ext(alldatasetnames$prev_bins[[paste0("res", resulttabnr())]]), 
                 "_MIDS_filtered.csv")
        },
        content = function(file) {
          write.csv(gbif_dataset_mids_filtered(), file, row.names = FALSE,na="")
        })
    
    })


# Show details of a specific MIDS element ---------------------------------

    observeEvent(input[[paste0("midscritsplot_click", resulttabnr())]], {
      groupId <- round(input[[paste0("midscritsplot_click", resulttabnr())]]$y)
      groupName <- input[[paste0("midscritsplot_click", resulttabnr())]]$domain$discrete_limits$y[[groupId]]
      showModal(modalDialog(
        title = HTML(paste0("<a href=\"https://tdwg.github.io/mids/information-elements/index.html#",
                       groupName,
                       "\" target=\"_blank\">",
                       groupName,
                       "</a> details")),
        tabsetPanel(type = "tabs",
          tabPanel("Plot",
            plotOutput(ns("detail")),
            plotOutput(ns("detail2"))),
          tabPanel("Summary table",
            DT::dataTableOutput(ns("detailTable")),
            DT::dataTableOutput(ns("detailTable2")))),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
      crit <- find_name(jsonschema(), groupName)
      crit_props <- strsplit(substr(crit, 2, nchar(crit)-1), "\\|")
      plot <- c()
      plot2 <- c()
      for (i in 1:length(crit_props[[1]])){
         mapname <- gsub("\\(|\\)|\\!|is.na|`", "", crit_props[[1]][[i]])
         temp <- gbif_dataset_mids_filtered() %>%
          summarise(test = !!rlang::parse_expr(crit_props[[1]][[i]])) %>%
           filter(test == TRUE) %>% 
            summarise(Mappings = mapname, "Number_of_records" = n(),
                      Percentage = round(n()/nrow(gbif_dataset_mids_filtered())*100))
         plot <- rbind(plot, temp)
         #if there are mappings composed of multiple properties, show more detail
         if (grepl( "\\&", crit_props[[1]][[i]]) == TRUE){
           splitprop <- stringr::str_split(crit_props[[1]][[i]], "\\&")[[1]]
           for (j in seq_along(splitprop)){
             propname2 <- gsub("\\(|\\)|\\!|is.na|`", "", splitprop[[j]])
             temp2 <- gbif_dataset_mids_filtered() %>%
               summarise(test = !!rlang::parse_expr(splitprop[[j]])) %>%
               filter(test == TRUE) %>%
               summarise(Properties = propname2, "Number_of_records" = n(),
                         Percentage = round(n()/nrow(gbif_dataset_mids_filtered())*100))
             plot2 <- rbind(plot2, temp2)
            }
         }
      }
      output$detail <- renderPlot({
        ggplot(plot, aes(x= gsub("&", "&\n", Mappings), y=Percentage)) + 
          geom_bar(stat = "identity", fill = "cadetblue") +
          scale_y_continuous(expand=c(0,0)) +
          coord_flip() + 
          ggtitle("Mappings") +
          ylab("Percentage present") +
          geom_text(data = subset(plot, Percentage >= 5),
                    aes(y = Percentage, label = Percentage), hjust = 1.25, colour = "white") +
          geom_text(data = subset(plot, Percentage < 5 & Percentage != 0),
                    aes(y = Percentage, label = Percentage), hjust = -0.5, colour = "black") +
          theme(plot.title = element_text(hjust = 0.5, size = 20) ,
                plot.margin = margin(1, 1, 2, 2, "cm"),
                axis.title = element_text(size = 15),
                axis.text = element_text(size = 15),
                axis.title.y = element_blank())
      })
      output$detail2 <- renderPlot({
        if (!is_empty(plot2)){
          ggplot(plot2, aes(x=Properties, y=Percentage)) +
            geom_bar(stat = "identity", fill = "cadetblue") +
            scale_y_continuous(expand=c(0,0)) +
            coord_flip() +
            ggtitle("Separate properties") +
            ylab("Percentage present") +
            geom_text(data = subset(plot2, Percentage >= 5),
                      aes(y = Percentage, label = Percentage), hjust = 1.25, colour = "white") +
            geom_text(data = subset(plot2, Percentage < 5 & Percentage != 0),
                      aes(y = Percentage, label = Percentage), hjust = -0.5, colour = "black") +
            theme(plot.title = element_text(hjust = 0.5, size = 20) ,
                  plot.margin = margin(1, 1, 2, 2, "cm"),
                  axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15),
                  axis.title.y = element_blank())
        }
      })
      output$detailTable <- 
        DT::renderDataTable(plot, rownames = FALSE, options = list(dom = 't', paging = FALSE))
      output$detailTable2 <- 
        DT::renderDataTable(plot2, rownames = FALSE, options = list(dom = 't', paging = FALSE))
    })
    
    #function to find name in nested list
    find_name <- function(haystack, needle) {
      if (hasName(haystack, needle)) {
        haystack[[needle]]
      } else if (is.list(haystack)) {
        for (obj in haystack) {
          ret <- Recall(obj, needle)
          if (!is.null(ret)) return(ret)
        }
      } else {
        NULL
      }
    }

# Information on dataset and MIDS implementation used ---------------------

    #show which dataset was used
    observe(
      output[[paste0("Used_dataset", input$start)]] <-
        renderText(tools::file_path_sans_ext(isolate(gbiffile()$name)))
    )
    
    #show basic info on which MIDS implementation was used
    observe(
      output[[paste0("Used_MIDS_implementation", input$start)]] <-
        renderText(
            return(paste0("sssom: ",
                          config$app$sssom_id))
        )
    )
    
    observeEvent(input[[paste0("missing_terms", resulttabnr())]], {
      missing_modal(allmids_unmapped$prev_bins[[paste0("res", resulttabnr())]])
    })

    #show complete MIDS implementation schema in modal window
    observe(
      ViewImplementationServer(paste0("showschema", resulttabnr()),session,
         reactive(allschemas$prev_bins[[paste0("res", resulttabnr())]])
      ))
    
# Open results tab automatically when calculations are performed ----------
    
    observeEvent(input$start, {
      updateTabsetPanel(parent.session, "tabs",
                        selected = paste0("start-Results", input$start))
    })
    
# Close results tabs ------------------------------------------------------

    #close tabs
    clear <- reactiveValues()
    observe(
      clear[[paste0("res", resulttabnr())]] <- CloseTabServer(paste0("close", resulttabnr()), resulttabnr(), parent.session)
    )

    #clear associated saved data
    observe(
      if (req(clear[[paste0("res", resulttabnr())]]()$value) == "clear"){
        allmidscalc$prev_bins[[paste0("res", resulttabnr())]] <- NULL
        allschemas$prev_bins[[paste0("res", resulttabnr())]] <- NULL
        alldatasetnames$prev_bins[[paste0("res", resulttabnr())]] <- NULL
      }
    )
    
    
  })
}