# Think IA App

## Introduction

The Think IA App is created by incorporating all current non-database IA data processing and visualisation scripts into the existing eTBS Visualiser. In addition to the functionality of the eTBS Visualiser, the app provides a user-friendly GUI from which data processing scripts can be easily adjusted and ran.

## Description

The Think IA App is a continuation of the eTBS Visualiser written in the R language, re-designed to allow modular addition of analysis scripts such as those for PLT, ORD, GWCS, ROT and flying-time analysis to be incorporated. The existing functionality within eTBS Visualiser (database explorer and PLT track visualiser) is also modularised. This design is intended to enable collaboration of developers working independently and concurrently on IA tasks, simplify the IA workflow, centralise data, and improve traceability.

Since the IA Capability internal project, further modules such as data loader (migrated from the legacy data loader written in VB) and PLT tools have been added.

# Developers Guide

## Getting Started

Development of the Think IA App and other code-based work uses Git as a version control system, managed centrally via the Bonobo Git Server. This section describes the details for connecting to the Bonobo Git Server on Maverick where the code repositories are located, as well as setting up your GitHub Desktop on your computer – a user-friendly GUI for managing Git repos.

-	Download Git command line tools here: https://git-scm.com/downloads (Portable edition will not require administrator to install/use).
-	Go to https://desktop.github.com/, download and install for GitHub Desktop for Windows (64-bit).
-	Git repos can be cloned locally via URL: http://192.168.1.23/Bonobo.Git.Server/name_of_repo.git
-	RStudio is recommended for working with Shiny apps: https://rstudio.com/products/rstudio/download/
-	If you want to access the Bonobo Git Server directly, go to http://192.168.1.23/Bonobo.Git.Server/Repository/Index

## Creating New Modules in the Think IA App

The Think IA App takes advantage of the new Shiny module functionality which alleviates loading times and name collisions within a large Shiny app. In this case, each entry in the app’s sidebar corresponds to an individual module.

Each module is contained within its own folder inside the /modules/ directory, the module must be made up of at minimum two files: mod_ui.R and mod_server.R, for code containing the ui and server logic respectively.

The file mod_ui.R must contain a function with the name “X_ui”, where X is the same as the name of the folder containing the module. Similarly, the file mod_server.R must contain a function with the name “X_server”.

After the module has been created, for it to be displayed within the app it must be added to the sidebarSettings variable in settings.R, where the name of the new list element must correspond to the module’s folder name, the vector contains the sidebar display name and the display icon for the module.

## Creating New Log Types in the Think IA App – Data Loader module

The log loading logic for each log type is contained within its own function. Currently, these functions are contained within several files (following convention from VB loader) – TBS_Log_Loader.R, NavCan_Log_Loader.R, LVNL_Log_Loader.R and Asterix_Log_Loader.R. Feel free to use the existing functions as a guide for writing new ones. Here are the concise steps:

1.	Create a new log loading function by referring to the example in Figure 3 for arguments and layout convention. This example can also be found in /modules/data_loader/example_Log_Loader.R
(if you create the function in a new R file, make sure to link it using the source function in mod_server.R)

2.	Add a new entry in the choices of “logs_type” to the mod_ui.R and a new if condition in the read_logs function in mod_server.R files (see Figure 4 and Figure 5).
After restarting the app, the new log type should now be displayed in the Log Type dropdown.
