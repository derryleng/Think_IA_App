example_2_ui <- function(id) {
	ns <- NS(id)
	div(
	    box(
	        title = "Box 1",
	        width = NULL,
	        collapsible  = T,
	        collapsed = F,
	        pickerInput_customised(ns("dropdown1"), "Numbers", seq(1, 10, 1)),
	        textOutput(ns("out"))
	    ),
	    box(
	        title = "Box 2",
	        width = NULL,
	        collapsible  = T,
	        collapsed = T,
	        "Nothing interesting happens."
	    )
	)
}
