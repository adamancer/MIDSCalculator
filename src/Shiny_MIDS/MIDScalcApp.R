#check if all packages are installed and load libraries
source("../packages.R")
pkgLoad()

#Load source files
config = read.ini("../../config.ini")

source(file = "../parse_json_schema.R")
yml_data = parse_sssom_yml_for_config()
supported_standards = names(yml_data)

source(file = "../parse_data_formats.R")
source(file = "../MIDS-calc.R")

sssom_map <- c(
  mapping_set_title     = "Title",
  mapping_set_description = "Description",
  object_type = "Standard and Format",
  subject_type = "Domain",
  mapping_date      = "Mapping Date",
  mapping_set_id = "Mapping ID"
)

# Increase upload limit to 5GB
options(shiny.maxRequestSize = as.numeric(config$app$max_size)*1024^2)

# Define UI ----
ui <- 
  tagList(
  useShinyjs(),
  #change style of modals to fix scroll bar behavior and backdrop of nested of modals
  tags$style(
    HTML("
        body {
          padding-right:0px !important;
        }
        .modal {
          overflow:auto;
        }
        .modal-open {
          overflow:auto;
        }
        .modal-backdrop {
          visibility: hidden !important;
        }
        .modal.in {
            background-color: rgba(0,0,0,0.5);
        }
        .header {
            background-color: #2874A6; color: white; 
            font-size: 20px; text-align: center; padding: 10px;
            box-shadow: 0px 2.5px 5px 0px rgba(0, 0, 0, 0.5);
        }
     ")
  ),
  navbarPage(
   title=div(tags$img(style = "margin: 0px 25px 0px 0px", height = 20, 
                      src = "Logo_klein_BotanicGardenMeise_cmyk.png"),
             tags$span(paste0("Calculate MIDS scores v",config$app$version),
                       actionButton("info",
                          icon("info"),
                          style = "padding:0px 5px 15px 5px; font-size:65%; border-style: none"
                          )), 
             ),
   id = "tabs",
   tabPanel("Submit data",
            br(), br(),
            fluidRow(column(width = 4, offset = 4,
            div("Submit dataset", class = "header"),
            wellPanel(fileInput("gbiffile", NULL,
                      accept = ".zip")))),
            div(
            fluidRow(column(width = 4, offset = 4,
            div(div("Specify MIDS implementation", 
                    div(ViewImplementationUI("viewcurrentschema"), 
                        style = "display: inline-block")), 
                class = "header"), 
            wellPanel(
            #radioButtons("jsonfiletype", label = "Select file", 
             #            choiceNames = list("Use default", 
              #                              "Upload file"),
               #          choiceValues = list("default", "custom")),
            #fileInput("customjsonfile", label = NULL, accept = ".json"),
            hr(style = "border-top: 1px solid #2874A6;"),
            #checkboxInput("editschema", "Edit interactively", value = FALSE),
            selectInput("standard_select", 
                        label = "Select mappings:",
                        choices = supported_standards,
                        selected = config$app$sssom_id),
            htmlOutput("sssom_meta")
            #InteractiveSchemaUI("interactive"),
            ))),
            ResultsUI("start"),
            align="center")
            )
))

# Define server logic ----
server <- function(input, output, session) {
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  
  list_to_html <- function(x,name_map = NULL) {
    stopifnot(is.list(x), !is.null(names(x)))
    
    if (!is.null(name_map)) {
      stopifnot(is.character(name_map), !is.null(names(name_map)))
      
      # Only keep elements that both exist in x and in the map
      mapped_names <- intersect(names(name_map), names(x))
      
      # Reorder x according to the mapping
      x <- x[mapped_names]
      
      # Human-readable labels in the same order
      labels <- unname(name_map[mapped_names])
    } else {
      mapped_names = names(x)
      
      cm_id = grep("curie_map|object_preprocessing|license",mapped_names)
      x = x[-cm_id] 
      mapped_names = mapped_names[-cm_id]
      
      labels = mapped_names
    }
    
    # Build one <p>...</p> per element
    item_html <- mapply(
      FUN = function(nm, val) {
        val_str <- paste(as.character(val), collapse = ", ")
        sprintf("<p align=\"left\"><strong>%s:</strong> %s</p>", nm, val_str)
      },
      nm   = labels,
      val  = x,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    
    # Collapse into a single HTML string
    paste(item_html, collapse = "\n")
  }
  
  observeEvent(input$standard_select, {
    config_live = getConfig()
    session$userData$config = config_live
    
    formatted_metadata = yml_data[[input$standard_select]] %>%
      list_to_html(name_map = sssom_map)
      
    output$sssom_meta <- renderUI({
      HTML(formatted_metadata)
    })
  })
  
# Define reactive and update format type
  getConfig <- reactive({
    
    config_live = parse_sssom_id(input$standard_select,
                                 config)
    return(config_live)
  })

# Show information about the app ------------------------------------------

  observeEvent(input$info,{ 
    #show modal
    showModal(modalDialog(
      title = "About",
      HTML(paste0(h4('Submit data'), '
        On this page a zipped GBIF annotated Darwin Core Archive or a comma or tab separated occurrence file can be uploaded (max 5GB). In addition, the MIDS implementation can be specified and viewed. To specify the MIDS implementation you can either choose the default schema (included in the app) or upload a schema from file. It is also possible to choose to edit this schema interactively. The interactive editing opens in a pop-up window, where in a first tab, MIDS elements can be added, removed, or moved to another MIDS level. In addition, mappings can be removed or added by clicking the "edit" icon of a MIDS element. In a second tab, the Unknown or Missing section of the schema can be edited, i.e. new properties and new values can be added. This interactively edited schema can be saved to file (JSON). The schema (be it default, custom or interactive) can be viewed by clicking the eye icon, which opens a human-friendly visualization of the MIDS schema, so that it is not necessary to read the JSON file to be able to understand the specifics of the MIDS schema used. Once a dataset and a MIDS implementation have been chosen, calculations can be started by clicking "Start MIDS score calculations".
        ', br(), h4('Results'), '
        The results of each analysis are visualized on a new page, where it is possible to explore summaries of the results of both MIDS levels and MIDS elements, either as plots or as tables. The MIDS element plot can be clicked to get more details on the results of the mappings of that element. It is also possible to explore the complete records table with the MIDS results for each record, and to download it as a csv file. In addition, the data can be filtered to see how MIDS results change when filtering on properties such as country code /taxonomic group/ collection date. The filename of the dataset is shown, as well as the used MIDS implementation, to make the provenance of the calculations clear.'
      )),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })
  

# Check dataset file ------------------------------------------------------

  disablesInvalidFile <- reactiveVal(FALSE)
  observeEvent(input$gbiffile, {
    if(!tools::file_ext(input$gbiffile$datapath) %in% c("zip", "txt", "csv")) {
      showModal(modalDialog(
        title = "Invalid input",
        "File must be a zip, txt or csv file"
      ))
      disablesInvalidFile(TRUE)
    } #else if (tools::file_ext(input$gbiffile$datapath) == "zip" && !"occurrence.txt" %in% unzip(zipfile = input$gbiffile$datapath, list = TRUE)$Name) {
    #   showModal(modalDialog(
    #     title = "Invalid input",
    #     "Zip file must contain occurence.txt file"
    #   ))
    #   disablesInvalidFile(TRUE)
    # } else {disablesInvalidFile(FALSE)}
  })
  
  
# Enable / disable action buttons -----------------------------------------

  #hide schema upload when schema is default
  # observe({
  #   if (input$jsonfiletype == "default"|input$jsonfiletype == "sssom"){
  #     shinyjs::hide("customjsonfile")} else {shinyjs::show("customjsonfile")}
  # })
  
  #hide edit interactively radiobutton when sssom option is selected
  # observe({
  #   if (input$jsonfiletype == "sssom"){
  #     shinyjs::hide("editschema")} else {shinyjs::show("editschema")}
  # })
  
  #check if file is uploading
  hold <- reactiveVal(FALSE)
  onclick("gbiffile", {hold(TRUE)})
  observeEvent(input$gbiffile, {hold(FALSE)})
  
  #disable start when there is no input file, when interactive is chosen but not visited, 
  #or when custom upload is chosen but empty, or when file is uploading
  #or when invalid dataset file is uploaded
  disablestart <- reactiveVal(FALSE)
  observe({
    if (is.null(input$gbiffile) |
        hold() == TRUE|
        disablesInvalidFile() == TRUE){
      disablestart(TRUE)} else {disablestart(FALSE)}
  })
    
  #disable "View MIDS implementation" when custom upload is chosen but empty, and when edit schema is chosen but not visited
  # disableviewschema <- reactiveVal(FALSE)
  # observe({
  #   if (input$jsonfiletype == "custom" & is.null(input$customjsonfile) | 
  #       (input$editschema == TRUE && interactiveschema$visited() == FALSE & input$jsonfiletype != "sssom")) {
  #     disableviewschema(TRUE)} else {disableviewschema(FALSE)}
  # })
  
  #disable "Edit MIDS implementation" if schema doesn't need to be edited and when custom upload is chosen but empty 
  # disableinteractive <- reactiveVal(FALSE)
  # observe({
  #   if ((input$jsonfiletype == "custom" & is.null(input$customjsonfile)) |
  #       input$editschema == FALSE | input$jsonfiletype == "sssom"){
  #     disableinteractive(TRUE)} else {disableinteractive(FALSE)}
  # })
  
# Initialize MIDS implementation ------------------------------------------

  #get path to json schema
  # jsonpath <- reactive({
  #   if (input$jsonfiletype == "default" | is.null(input$customjsonfile$datapath)){
  #     return(paste0("../../", default_schema))}
  #   if (input$jsonfiletype == "custom"){
  #     return(input$customjsonfile$datapath)}
  # })
  
  #update MIDS implementation radiobuttons to show schema info
  # observe(
  # updateRadioButtons(session, "jsonfiletype", 
  #                    choiceNames = list(paste("Use default:", 
  #                                             paste0(read_json(jsonpath())$schemaName, 
  #                                                    " v", 
  #                                                    read_json(jsonpath())$schemaVersion)),
  #                                       "Upload file",
  #                                       "Use default SSSOM mapping"),
  #                    choiceValues = list("default", "custom","sssom"),
  #                    selected = input$jsonfiletype)
  # )

  #read json schema from file
  # jsonschemafile <- reactive({ 
  #   read_json_mids_criteria(schema = jsonpath(), outtype = "criteria")
  # })
  # 
  # #read json UoM from file
  # jsonUoMfile <- reactive({ 
  #   read_json_unknownOrMissing(schema = jsonpath())
  # })
  

# Get final MIDS implementation schema (either from file or from interactive editing) --------

  jsonschemafinal <- reactive({
    config_live = getConfig()

    resp = c(list("criteria" = read_json_mids_criteria(outtype = "criteria",
                                                      type = "sssom",
                                                      config = config_live)),
            list("UoM" = read_json_unknownOrMissing(type = "sssom",
                                                    config = config_live)),
            list("properties" = read_json_mids_criteria(outtype = "properties",
                                                        type = "sssom",
                                                        config = config_live))) 
    return(resp)
  })
  
# Show current MIDS implementation ----------------------------------------
  
  #view MIDS implementation in modal window
  ViewImplementationServer("viewcurrentschema",session,jsonschemafinal)
    

# Edit MIDS implementation interactively ----------------------------------
  
  #interactiveschema <- InteractiveSchemaServer("interactive",session,jsonschemafinal)
  

# Calculate and show results ----------------------------------------------

  ResultsServer("start", session, reactive(input$gbiffile),
                reactive(input$tabs), disablestart,getConfig(),jsonschemafinal)
 
}
# Run the app ----
shinyApp(ui = ui, server = server)