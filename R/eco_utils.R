getEcoExtractPyScript =
function()
{
  getOption("EcoExtractScript", system.file("python", "ecoextract.py", package = "SpilloverDA"))
}


getEpitatorDBFilename =
function()
{
    val = Sys.getenv("ANNOTATOR_DB_PATH")
    if(is.na(val) || val == "")
        val = path.expand("~/.epitator.sqlitedb")
    val
}

checkEpitatorDB =
function(db = getEpitatorDBFilename(), error = TRUE)
{
    status = file.exists(db)
    if(error && !status)
        stop("Epitator SQLite database doesn't exist: ", db)

    status
}
