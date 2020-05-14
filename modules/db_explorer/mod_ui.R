db_explorer_ui <- function(id) {
    
    ns <- NS(id)
    
    div(
        box(
            title = "Database Explorer",
            width = NULL,
            collapsible  = T,
            collapsed = T,
            tabBox(
                width = NULL,
                tabPanel(
                    title = "Query Tool",
                    div(
                        textAreaInput(
                            inputId = ns("db_query"),
                            label = NULL,
                            placeholder = "Enter your query here...",
                            width = "100%",
                            height = "246px",
                            resize = "vertical"
                        ),
                        div(
                            class = "centered",
                            actionButton(
                                inputId = ns("db_execute"),
                                label = "Execute",
                                icon = icon("play")
                            ),
                            div(style = "margin: 0 5px 0 5px"),
                            actionButton(
                                inputId = ns("db_clear"),
                                label = "Clear",
                                icon = icon("eraser")
                            )
                        ),
                        DT::dataTableOutput(outputId = ns("db_output"))
                    )
                ),
                tabPanel(title = "Flight Plan", DT::dataTableOutput(outputId = ns("db_fp_table"))),
                tabPanel(title = "Landing Pair", DT::dataTableOutput(outputId = ns("db_lp_table"))),
                tabPanel(title = "Volumes", DT::dataTableOutput(outputId = ns("db_volumes"))),
                tabPanel(title = "Legs", DT::dataTableOutput(outputId = ns("db_legs")))
            )
        ),
        box(
            title = "Flight Plan Statistics",
            width = NULL,
            collapsible = T,
            collapsed = T,
            tabBox(
                width = NULL,
                tabPanel(title = "General", DT::dataTableOutput(outputId = ns("db_fp_general_table"))),
                tabPanel(title = "Aircraft Type", DT::dataTableOutput(outputId = ns("db_fp_type_table"))),
                tabPanel(title = "Wake", DT::dataTableOutput(outputId = ns("db_fp_wake_table"))),
                tabPanel(title = "Runway", DT::dataTableOutput(outputId = ns("db_fp_lrwy_table"))),
                tabPanel(title = "Runway Hourly", DT::dataTableOutput(outputId = ns("db_fp_lrwyt_table")))
            )
        ),
        box(
            title = "Landing Pair Statistics",
            width = NULL,
            collapsible = T,
            collapsed = T,
            pickerInput_customised(inputId = ns("db_lp_type"), label = "Landing Pair Type"),
            tabBox(
                width = NULL,
                tabPanel(title = "Wake", DT::dataTableOutput(outputId = ns("db_lp_wake_table"))),
                tabPanel(title = "Runway", DT::dataTableOutput(outputId = ns("db_lp_lrwy_table"))),
                tabPanel(title = "Wake By Runway", DT::dataTableOutput(outputId = ns("db_lp_wakerwy_table")))
            )
        ),
        box(
            title = "Adaptation Data",
            width = NULL,
            collapsible = T,
            collapsed = T,
            tabBox(
                width = NULL,
                tabPanel(title = "Aircraft", DT::dataTableOutput(outputId = ns("db_aircraft_adaptation_table"))),
                tabPanel(title = "Wake", DT::dataTableOutput(outputId = ns("db_wake_adaptation_table"))),
                tabPanel(title = "DBS", DT::dataTableOutput(outputId = ns("db_dbs_adaptation_table"))),
                tabPanel(title = "Runway", DT::dataTableOutput(outputId = ns("db_runway_adaptation_table")))
            )
        )
    )
    
}
