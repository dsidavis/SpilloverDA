# Function to go from a PDF file to JSON


xml2eco =
    # Handle a PDF file as well.
    function(doc.file, ecoextract = getEcoExtractPyScript(),
             results.dir = character(),
             cache.dir = character(), cache.file = file.path(cache.dir, gsub("xml$", "rds", basename(doc.file))),
             section.text = load_text(doc.file, cache.file, cache.dir))

{    
    # Clean up special characters
    section.text = lapply(section.text, function(x) gsub('Â|"', "", x))

    # Drop references, etc.
    i = grep("references?|cited|acknowledgements?|funding|interests|disclosure", names(section.text),
             ignore.case = TRUE, invert = TRUE)
    section.text = section.text[i]
    ans = lapply(section.text, function(x) try(sect2eco(x, ecoextract)))

    if(length(results.dir) && !is.na(results.dir)) {
        if(!dir.exists(results.dir))
            dir.create(results.dir)
        
        saveRDS(ans, file.path(results.dir, gsub("xml$", "rds", basename(doc.file))))
    }
    ans
}

load_text = function(doc.file, cache.file,
                     cache.dir = dirname(cache.file))
{
    if(exists(cache.file)) {
        section.text = readRDS(cache.file)
    } else {
        section.text = readXMLSections(doc.file)
        if(length(cache.file) && !is.na(cache.file)){
            if(!dir.exists(cache.dir))
                dir.create(cache.dir)
            
            saveRDS(section.text, file = cache.file)
        }
    }
    
    section.text
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
        
    c(title = title, abstract = abst, allsect)
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
