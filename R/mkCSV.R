getX = function(f, ext = "xml", pdfs =  list.files("../LatestDocs/PDF", recursive = TRUE, full = TRUE, pattern = "\\.pdf$"))
          gsub("pdf$", ext, grep(gsub("\\.rds", "", basename(f)), pdfs, val = TRUE))

o = function(f) opdf(getX(f, "pdf"))

if(FALSE) {
dir = "ecoJSON-body"
ff = list.files(dir, pattern = "rds$", full = TRUE)

sp = readRDS("../matchSpecies.rds")
}

# For a given rds file, we create a data frame then CSV file
# with truth and results for


if(FALSE) {
 z = mkCSV(ff[1], write = TRUE, jvar = getDiagTest, spVar = "most_specific_diagnostic_Test")
}

mkCSV =
    #
    #
    # obs - the observations in the species
    # eco - the R object from the eco health JSON output
    # spVar -
    #
    # jdata - 
    #
function(f, obs = grep(gsub("rds", "", basename(f)), species$PDF, fixed = TRUE), species = sp,
         eco = readRDS(f),
         spVar = c("Country", "State", "City", "Location", "Region"),
         spFixed = c("reference_ID", "PDF"),
         jvar = getLocation,
         write = TRUE,
         jdata = jvar(eco),
         xlsxFile = outFilename(f))
{    
    fx = sp[obs, c(spFixed, spVar)]
    # Only grab the unique rows
    fx = fx[!duplicated(fx),]
    ans = jdata
    n = nrow(ans)

    # Get the fixed part from the manually generated table, i.e. truth
    # extend the rows with empty cells
    fx = as.data.frame(lapply(fx, function(x) c(x, rep("", n-nrow(fx)))), stringsAsFactors = FALSE)
    fx$PDF = gsub("internal-pdf://", "", fx$PDF)
    ans = cbind(fx, section = rep("", n), correct = rep(FALSE, n), ans)
        
      # Clean up a character that causes Excel to barf.
    w = sapply(ans, is.character)
    ans[w] = lapply(ans[w], function(x) gsub("\031", "'", x))


  
    if(write) {
        library(openxlsx)
        createXLSX(ans, xlsxFile,
                   sprintf("file:///Users/duncan/DSIProjects/Zoonotics-shared/EcoResults/bob.html#%d", 1:nrow(ans)))
    }

    ans
}

outFilename =
function(f, ext = "xlsx", dir = "CSV")    
  sprintf("%s/%s.%s", dir, gsub("\\.rds", "", basename(f)), ext)    


createXLSX =
function(df, filename)    
    write.xlsx(df, filename)

createXLSX =
    # Second version that adds hyperlinks to the matches.
function(df, filename, links, addLinks = FALSE)
{
    if(addLinks) {
        df$links = links
        class(df$links) <- "hyperlink"
    }

    wb <- createWorkbook()
    addWorksheet(wb, "A")
    writeData(wb, 1, df, startRow = 1, startCol = 1)

    ## to change the display text for a hyperlink column just write over those cells
    if(addLinks)
       writeData(wb, sheet = 1, x = df$links, startRow = 2, startCol = match("links", names(df)))

    saveWorkbook(wb, filename, overwrite = TRUE)
    filename
}

getLocation  =
function(x, full = TRUE)
{
    if(full) {
        tmp = lapply(x, getLocation, full = FALSE)
        # browser()
        ans = do.call(rbind, tmp)
        ans$sectionName = rep(names(x), sapply(tmp, function(x) if(is.null(x)) 0 else nrow(x)))        
        ans
    } else {
        # try-errors break this
        if(is(x, "try-error"))
            return(data.frame(label = "try-error",
                              countryCode = NA, start = NA, end = NA, score = NA,
                              namesUsed = NA, parent = NA, parentNamesUsed = NA,
                              stringsAsFactors = FALSE))
        # browser()
        do.call(rbind, lapply(x$location, getLocationFun))
    }
}

getLocationFun =
function(x)
             data.frame(label = x$label,
               countryCode = x$geoname$country_code,
               start = x$textOffsets[[1]][1],
               end = x$textOffsets[[1]][2],
               score = x$geoname$score,
               namesUsed = x$geoname$names_used,
               parent = paste(sapply(x$geoname$parents, `[[`, "name"), collapse = ";"),
               parentNamesUsed = paste(sapply(x$geoname$parents, `[[`, "names_used"), collapse = ";"),
               stringsAsFactors = FALSE)    


getVirus =
function(res, full = TRUE)
{
  getMisc(res, full, type = "disease")
}

getSpecies =
function(res, full = TRUE)
{
  getMisc(res, full, type = "species")
}

getDiagTest =
function(res, full = TRUE)
{
  getMisc(res, full, type = "diagnostic_test")
}



getMisc =
    # toplevel
    # For now assume res is by section
    #
    #  section named (rep), span, diseaseName(s - but as ;-separated string)
    #
function(res, full = TRUE, type = "disease")
{
    tmp = lapply(names(res), function(nm) getVirusSection(res[[nm]], nm, type = type))
    do.call(rbind, tmp)
}

getVirusSection =
function(sec, nm, type = "disease")
{
    bad = data.frame(start = integer(), end = integer(), disease = character())
    if(!("resolved_keyword" %in% names(sec)))
        return(bad)
    
    w = sapply(sec$resolved_keyword, function(x) all(sapply(x$resolutions, function(x) x$entity["type"]) == type))
    if(!any(w))
        return(bad)
    ans = do.call(rbind, lapply(sec$resolved_keyword[w], getVirusInfo, type = type))
    ans$sectionName = nm
    ans
}

getVirusInfo =
    # span and the disease name
function(kw, type = "disease")    
{
    d = getVirusInfoFromResolution(kw$resolutions, type)
    ans = data.frame(start = kw$textOffsets[[1]][1],
                     end = kw$textOffsets[[1]][2],
                     disease = d,
                     label = kw$label)
    names(ans)[3] = type
    ans
}

getVirusInfoFromResolution =
function(res, type = "disease")
{
    w = sapply(res, function(x) x$entity["type"]) == type
    if(any(w)) 
       paste(sapply(res[w], function(x) x$entity["label"]), collapse = ";")
    else
       character()
}
