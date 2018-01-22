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
function(f, eco = readRDS(f),
         obs = grep(gsub("rds", "", basename(f)), species$PDF, fixed = TRUE),
         species = NULL, # the data.frame from Species.csv
         spVar = c("Country", "State", "City", "Location", "Region"),
         spFixed = c("reference_ID", "PDF"),
         jvar = getLocation,
         write = TRUE,
         jdata = jvar(eco),
         xlsxFile = outFilename(f))
{
    if(!is.null(species)) {
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
     } else
       ans = obs
        
       # Clean up a character that causes Excel to barf.
    ans = fixCharacters(ans)

    if(write) {
        createXLSX(ans, xlsxFile)
                   # sprintf("file:///Users/duncan/DSIProjects/Zoonotics-shared/EcoResults/bob.html#%d", 1:nrow(ans)))
    }

    ans
}

fixCharacters =
function(df)
{
    w = sapply(df, is.character)
    df[w] = lapply(df[w], function(x) gsub("\031", "'", stringi::stri_trans_general(x, id = "latin-ascii")))
    df
}            
    
outFilename =
function(f, ext = "xlsx", dir = "CSV")    
  sprintf("%s/%s.%s", dir, gsub("\\.rds", "", basename(f)), ext)    


createXLSX.simple =
function(df, filename)    
    write.xlsx(df, filename)

createXLSX =
    # Second version that adds hyperlinks to the matches.
function(df, filename, links, addLinks = !missing(links))
{
    if(!addLinks)
       return(createXLSX.simple(df, filename))
    
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

