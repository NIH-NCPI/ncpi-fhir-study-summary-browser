library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)
library(googleAuthR)

# Configure the OAuth request
# GCP API scope
scopes="https://www.googleapis.com/auth/cloud-platform"
# This is a test web application OAuth registration, requiring special access 
# Setup here: https://console.developers.google.com/apis/credentials
gar_set_client(web_json = "AnVIL FHIR Test Web.json",
               scopes = scopes,
               activate="web")


ui <- fluidPage(


    # Application title
    titlePanel("NCPI Study Summary FHIR Browser"),

    # Main page that sets up a workflow of tabs
    navbarPage("=>",
        tabPanel("ResearchStudy Browser",
             fluidPage(
                 fluidRow(
                     column(4,
                            helpText("Select a study:"),
                            DTOutput("study_table")
                            ),
                     column(8,
                                       fluidPage(
                                                fluidRow(htmlOutput("study_detail_header")),
                                                fluidRow(column(6,plotOutput("study_detail_race")),
                                                         column(6,plotOutput("study_detail_eth"))),
                                                fluidRow(column(4,tableOutput("study_detail_race_eth_tab")),
                                                         column(2,tableOutput("study_detail_gender_tab")),
                                                         column(6,plotOutput("study_detail_gender")))
                                       )
                            )
                     )
             )
        ),
       tabPanel("Study Phenotypes Browser",fluidPage(
           fluidRow(
               column(4,
                      htmlOutput("study_detail_header_part"),
                      helpText("List of groups in this study:"),
                      DTOutput("study_group_table")
               ),
               column(8,
                      fluidPage(
                                  helpText("Phenotype Summary"),
                                  DTOutput("participant_phenotype_summary")
                      )
               )
           )
       )
       ),
       tabPanel("Configuration",
                helpText("Please indicate the FHIR base URL"),
                textInput("server",
                          "Server Loc:",
                          value = "https://healthcare.googleapis.com/v1/projects/anvil-fhir-vumc/locations/us-central1/datasets/anvil-public-test/fhirStores/anvil-public-2021Q4/fhir/")
                
                
       )
    )
)

server <- function(input, output, session) {
    # create a non-reactive access_token as we should never get past this if not authenticated
    gar_shiny_auth(session)
    
    ## Load helper functions
    source("support_functions.R", local=TRUE)
    
    
    ## Create some reactive expressions for each step so the plots don't cause too many calls
    
    ###
    #Block for ResearchStudy Tab
    ###
    
    # Get the list of studies
    studies <- reactive({
        req(input$server)
        get_all("ResearchStudy")
    })

    # Study group lookup
    groups_by_study_id <- reactive({
        tibble(studies=studies()) %>% unnest_wider(studies) %>% 
            unnest_longer(enrollment) %>% unnest_wider(enrollment) %>% 
            transmute(study_id=id, group_reference=reference, group_id=substring(reference,7))
    })
    
    # Get all groups referenced
    all_groups <- reactive({
        get_resource_list(groups_by_study_id()[["group_reference"]])
    })
    
    # Parse groups
    groups <- reactive({
        parse_groups(all_groups())
    })
    
    # Complete groups
    complete_group <- reactive({
        groups() %>% filter(complete_group)
    })
    
    #Get demographics
    complete_group_demographics <- reactive({
        bind_rows(extract_stats(get_all(paste("Observation?code:text=Gender%20Variable%20Summary&subject=",
                                              paste0("Group/",complete_group()[["group_id"]],collapse = ","),sep = ""))),
                  extract_stats(get_all(paste("Observation?code:text=Race%20Variable%20Summary&subject=",
                                              paste0("Group/",complete_group()[["group_id"]],collapse = ","),sep = ""))),
                  extract_stats(get_all(paste("Observation?code:text=69490-1%20Variable%20Summary&subject=",
                                              paste0("Group/",complete_group()[["group_id"]],collapse = ","),sep = ""))))
        
    })
    
        
    # Summarize in a table
    studyTable <- reactive({
        bind_rows(lapply(studies(), function(x){
            data.frame(study_id=x$id,
                       study_title=x$title)
        })) %>% 
            inner_join(groups_by_study_id()) %>%  
            inner_join(complete_group()) %>% 
            select(study_id, study_title, n_participants) %>% 
        arrange(study_title)
    })
    

        
    # Create output
    output$study_table <- renderDT( 
        datatable(studyTable() %>% transmute(study_id, `Study Title`=study_title, `Participants`=n_participants), 
                  selection = "single", rownames=F,
                  options=list(columnDefs = list(list(visible=FALSE, targets=c(0)))))
    )
    
    ###
    #Block for research study detail tab
    ###
    
    selected_study_row <- reactive({
        req(input$study_table_rows_selected)
        studyTable()[input$study_table_rows_selected,]
    })
    
    
    selected_study_id <- reactive({
        selected_study_row()[["study_id"]]
    })
    
    selected_study_groups <- reactive({
        req(input$study_table_rows_selected)
        groups_by_study_id() %>% filter(study_id==selected_study_id())
    })
    
    #Get details for that ID
    study_groups <- reactive({
        groups() %>% inner_join(selected_study_groups())
    })
    
    studyGroupTable <- reactive({
        study_groups() %>% 
            arrange(n_participants) %>% 
            transmute(group_id, `Group Name`=group_name, `Participants`=n_participants)
    })

    # Create output
    output$study_group_table <- renderDT( 
        datatable(studyGroupTable(),selection = "single", rownames=F,
                  options=list(columnDefs = list(list(visible=FALSE, targets=c(0)))))
    )
    
    selected_group_id <- reactive({
        req(input$study_group_table_rows_selected)
        studyGroupTable()[input$study_group_table_rows_selected,][["group_id"]]
    })

    ##Study Summary tabs
    # Create the study summary
    output$study_detail_header_part <- output$study_detail_header <- renderText({
        sprintf("<h2>%s</h2><br/>Study ID: %s",
                studyTable()[input$study_table_rows_selected,"study_title"],
                studyTable()[input$study_table_rows_selected,"study_id"])
    })
    
    studyDemographicTable <- reactive({
        complete_group_demographics() %>% 
            inner_join(selected_study_groups())
    })

    output$study_detail_gender <- renderPlot({
        gender_table = studyDemographicTable() %>% 
            filter(term_name=="Gender", !grepl("^Total",component_text))
        
        ggplot(gender_table, aes(x="",y=component_value,fill=component_text)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            theme_void() +
            ggtitle("Gender")
    })

    output$study_detail_race <- renderPlot({
        race_table = studyDemographicTable() %>% 
            filter(term_name=="Race", !grepl("^Total",component_text)) %>% 
            mutate(component_text=str_replace(component_text,"Demographics .*:",""))
        ggplot(race_table, 
               aes(x=component_text, y=component_value,fill=component_text)) +
            geom_bar(stat="identity") +
            ggtitle("Race") +
            theme_light() +
            theme(legend.position="bottom",
                  axis.text.x = element_text(angle = -15)) 
    })

    output$study_detail_eth <- renderPlot({
        eth_table = studyDemographicTable() %>% 
            filter(term_name=="Ethnicity", !grepl("^Total",component_text)) %>% 
            mutate(component_text=str_replace(component_text,"Demographics .*:",""))
        ggplot(eth_table, 
               aes(x=component_text, y=component_value,fill=component_text)) +
            geom_bar(stat="identity") +
            theme_light() +
            ggtitle("Ethnicity") +
            theme(legend.position="bottom",
                  axis.text.x = element_text(angle = -15))
    })
    
    output$study_detail_gender_tab <- renderTable({
        studyDemographicTable() %>% 
            filter(term_name=="Gender", !grepl("^Total",component_text)) %>% 
            transmute(`Category`=component_text, `Participants`=component_value)
    })
    
    output$study_detail_race_eth_tab <- renderTable({
        studyDemographicTable() %>% 
            filter(term_name=="Race", !grepl("^Total",component_text)) %>% 
            transmute(`Category`=component_text, `Participants`=component_value)
    })



    ##Participant Summary tabs
    
    # Create the participant summary
    
    phenotypeTable <- reactive({
        extract_stats(get_all(paste("Observation?code:text=Variable%20Summary&subject=Group/",
                                    selected_group_id(),sep = ""))) %>% 
            filter(grepl("Phenotype",component_text)) %>% 
        mutate(term_name=str_replace(term_name,"Present: ","")) %>% 
        pivot_wider(id_cols=term_name,names_from=component_text,values_from=component_value, 
                    values_fill=0, values_fn=max) %>% 
        rename(Phenotype=term_name) %>% arrange(-`Phenotype Present`)
    })
    
    
    output$participant_phenotype_summary <- renderDT({
        
        datatable(phenotypeTable(),rownames=F)
    })

    
    

    
}

# Run the application 
shinyApp(ui = gar_shiny_ui(ui), server = server, options = list(port=1221))
