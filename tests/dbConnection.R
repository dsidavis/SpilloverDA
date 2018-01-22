library(SpilloverDA)
Sys.setenv("ANNOTATOR_DB_PATH" = "")

SpilloverDA:::checkEpitatorDB()
Sys.setenv("ANNOTATOR_DB_PATH" = file.path(getwd(), ".epitator.sqlitedb"))
SpilloverDA:::getEpitatorDBFilename()


tt = try(SpilloverDA:::checkEpitatorDB())
if(!is(tt, "try-error"))
    stop("checkEpitatorDB found database in installed dir")
