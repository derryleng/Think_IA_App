This is an example layout of a Think IA App module.

Your module files must be contained within a folder with no spaces in its name, this folder must be placed within Think_IA_App/modules/

Your module must contain mod_ui.R and optionally mod_server.R, these should contain one function each corresponding to a modularised UI and server element. For details see https://shiny.rstudio.com/articles/modules.html



mod_ui.R

This should contain one function with the same name as the module folder plus "_ui",
e.g. for the module named super_cool_mod, the UI function must be named super_cool_mod_ui.

The UI function must at the very least have "id" as an argument, this corresponds to the namespace of the module. Defining a namespace for each module prevents accidents caused by different modules specifying the same ID for input or output elements.

Individual UI elements (e.g. box() or textInput()) can be placed directly within the UI function, when defining multiple UI elements they must be contained within div() or tagList()

Note: don't forget to wrap your inputID and outputID in ns()!



mod_server.R

This should contain one function with the same name as the module folder plus "_server",
e.g. for the module named super_cool_mod, the UI function must be named super_cool_mod_server.

The server function must have "input", "output", "session" and "con" as arguments.
