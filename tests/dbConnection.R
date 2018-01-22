library(SpilloverDA)
Sys.setenv("ANNOTATOR_DB_PATH" = "")

SpilloverDA:::checkEpitatorDB(error = FALSE)

Sys.setenv("ANNOTATOR_DB_PATH" = file.path(tempdir(), ".epitator.sqlitedb"))
SpilloverDA:::getEpitatorDBFilename()

status = SpilloverDA:::checkEpitatorDB(error = FALSE)
stopifnot(!status)

tt = try(SpilloverDA:::checkEpitatorDB())
stopifnot(is(tt, "try-error"))
