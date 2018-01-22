# Function to go from a PDF file to JSON


xml2eco =
    # Handle a PDF file as well.
function(XML, ecoextract = getEcoExtractPyScript(),
         results_dir = character(), cache.dir = character())
{
    if(length(cache.dir) && !is.na(cache.dir))  {
       if(!dir.exists(cache.dir))
            dir.create(cache.dir)

       cache.file = file.path(cache.dir, gsub("xml$", "rds", basename(XML)))
       if(exists(cache.file))
           ll = readRDS(cache.file)
       else {
           ll = readXMLSections(XML)
           saveRDS(ll, file = cache.file)
       }
     } else {
           ll = readXMLSections(XML) # repeated from above. Fix!
     }
       
    
    # Clean up special characters
    ll = lapply(ll, function(x) gsub('Â|"', "", x))

    # Drop references, etc.
    i = grep("references?|cited|acknowledgements?|funding|interests|disclosure", names(ll),
             ignore.case = TRUE, invert = TRUE)
    ll = ll[i]
    ans = lapply(ll, function(x) try(sect2eco(x, ecoextract)))

    if(length(results_dir) && !is.na(results_dir)) {
        if(!dir.exists(results_dir))
            dir.create(results_dir)
        
        saveRDS(ans, file.path(results_dir, gsub("xml$", "rds", basename(XML))))
    }
    ans
}

sect2eco = function(section, ecoextract = getEcoExtractPyScript())
{
    f = tempfile()
    f2 = tempfile()

    cat(section, file = f)
    #    system2(ecoextract, args = c(f, f2))
    system2("python3", args = c(ecoextract, f, f2))
    res = fromJSON(f2)
    res$txt = section
    res
}

fixSectionNames = function(sect_names)
{
    tmp = sect_names

    tmp = XML:::trim(tmp)
    
    # Find and relabel the "authors" - might need to be fixed later
    auth_regex = "^[A-Z][[:alpha:]]+[ -*]([A-z]\\.?[[:alpha:]]*[ -*]?)?([A-z][[:alpha:]]+[ -*]?)?,"
    i = grep(auth_regex, tmp)
    if(length(i) > 0)
        warning("Replacing: ", paste(tmp[i], collapse = ":"))
    
    tmp[i] = "authors"

    # remove numbers
    tmp = gsub("^([0-9]\\.)+ ?", "", tmp)

    tmp = tolower(tmp)
    return(tmp)
}


readXMLSections =
function(XML)
{    
    doc = readPDFXML(XML)

    title = getDocTitleString(doc)
    abst = findAbstract(doc)
        
    abst = paste(names(abst), collapse = " ")
        
    allsect = try(lapply(getSectionText(doc), function(x)
                                                  paste(unlist(x), collapse = " ")))
       
    if(is(allsect, "try-error") || length(allsect) > 20)
        allsect = list(body = getDocText(doc))
        
    list(title = title, abstract = abst, allsect)
}



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
