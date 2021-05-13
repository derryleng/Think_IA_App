

#### Config Control

# Operation/Airfield Directories
# Operation_Dir <- file.path(Base_Dir, Operation)
# Airfield_Dir <- file.path(Operation_Dir, Airfield)
# 
# Create_Directory(Operation_Dir)
# Create_Directory(Airfield_Dir)

# Directories
Airfield_Dir <- Base_Dir # OUTPUT DIRECTORY!
#Adap_Version_Dir <- file.path(Base_Dir, Operation, Airfield, paste0("v", Adap_Version))

Adap_Version_Dir <- file.path(Base_Dir, paste0("v", Adap_Version))
Ref_Dir <- file.path(Adap_Version_Dir, "Reference Data")
Adap_Iteration_Dir <- file.path(Adap_Version_Dir, paste0("v", Adap_Iteration_Version))
Input_Data_Dir <- file.path(Adap_Iteration_Dir, "Input Data")
Local_Iteration_Dir <- file.path(Adap_Iteration_Dir, paste0("v", Local_Iteration_Version))

Create_Directory(Adap_Version_Dir)
Create_Directory(Ref_Dir)
Create_Directory(Adap_Iteration_Dir)
Create_Directory(Input_Data_Dir)
Create_Directory(Local_Iteration_Dir)

# Directories - Local
Initial_Compare_Dir <- file.path(Local_Iteration_Dir, "Initial Comparisons")
Adaptation_Dir <- file.path(Local_Iteration_Dir, "Config File Output")
Inter_Dir <- file.path(Local_Iteration_Dir, "Intermediary Data")
Plot_Dir <- file.path(Local_Iteration_Dir, "Plots")

Create_Directory(Initial_Compare_Dir)
Create_Directory(Adaptation_Dir)
Create_Directory(Inter_Dir)
Create_Directory(Plot_Dir)

# Output Directories - Local - Adaptation
Adaptation_Full_Dir_Name <- file.path("Config File Output", "Full Files")
Adaptation_Compare_Dir <- file.path(Adaptation_Dir, "Comparisons")
Adaptation_Full_Dir <- file.path(Adaptation_Dir, "Full Files")

Create_Directory(Adaptation_Compare_Dir)
Create_Directory(Adaptation_Full_Dir)


