#run and deploy the thingy
library(taskscheduleR)

taskscheduler_create(taskname = "test_run", rscript = "knitit.R", 
                     schedule = "DAILY", starttime = format(Sys.time() + 50, "%H:%M"))
taskschedulerAddin()
