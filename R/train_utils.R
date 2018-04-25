compTopSect = function(x)
{   
    x$sectionName = tolower(x$sectionName)
    tt = sort(table(unlist(tapply(x$sectionName, x$pdf, unique))), decreasing = TRUE)
    # tt = sort(table(x$sectionName), decreasing = TRUE)
    
    i = x$sectionName %in% names(tt)[1:20]
    table(x$correct[i], x$sectionName[i])
}

mkTrainingSet = function(sp, extractResults, extractVar, spVar, resultNames, test = FALSE)
{
    tmp2 = mkTestSet(extractResults, extractVar, resultNames)
    tmp2 = split(tmp2, tmp2$pdf)
    
    tmp3 = lapply(tmp2, function(x){
        if(!test){
            i = grep(basename(unique(x$pdf)), sp$fixedPDF, fixed = TRUE)
            if(length(i) == 0)
                browser()
            spCorrect = tolower(unlist(strsplit(sp[i, spVar], ";")))
            correct = unlist(lapply(tolower(x[["var"]]), function(term) {
                if(spVar == "virus")
                    term = gsub(" virus$|encephalitis$", "", term)
                term = paste0("\\b", term, "\\b")
                any(grepl(term, x = spCorrect, ignore.case = TRUE))
            }))
            if(length(correct) > nrow(x))
                browser()
            x$correct = correct
        }
    })
    
    tmp3 = do.call(rbind, tmp3)
    rownames(tmp3) = NULL

    tmp3
}

mkTestSet = function(extractResults, extractVar, resultNames)
{
    tmp = lapply(seq(along = extractResults), function(i) {
        invisible(try({
            x = extractResults[[i]][,c(extractVar, "sectionName")]
            colnames(x) = c("var", "sectionName")
            x$pdf = resultNames[i]
            x$pdfFreq = freqBy(x, extractVar = "var")
            x = freqBySect(x, extractVar = "var")
            x
        }))
    })
    i = !sapply(tmp, is, "try-error")
    if(all(!i))
        browser()
    
    tmp2 = do.call(rbind, tmp[i])

    # Sep out the combined variables
    tmp2 = sepVars(tmp2)
    rownames(tmp2) = NULL

    tmp2
}

sepVars = function(df)
{
    tmp = strsplit(as.character(df$var), ";")
    data.frame(var = XML:::trim(unlist(tmp)),
               sectionName = rep(df$sectionName, sapply(tmp, length)),
               pdf = rep(df$pdf, sapply(tmp, length)),
               stringsAsFactors = FALSE)
}

freqBy = function(x, extractVar)
{
    tt = table(x[[extractVar]]) / nrow(x)
    i = match(x[[extractVar]], names(tt))
    as.numeric(tt)[i]
}

freqBySect = function(x, extractVar)
{
    tmp = split(x, x$sectionName)
    ans = lapply(tmp, freqBy, extractVar = extractVar)
    tmp = do.call(rbind, tmp)
    tmp$sectFreq = unlist(ans)
    tmp
}

reduceSects = function(sectionNames, pdf, n = 30,
                       commonSections = c("<other>", "abstract", "ackowledgements",
                                          "author contributions","author ref",
                                          "author summary", "background", "body",
                                          "conclusions", "discussion",
                                          "future directions", "impacts", 
                                          "introduction", "methods", 
                                          "natural cycles of infection", 
                                          "results", "results and discussion",
                                          "sources and manufacturers", 
                                          "summary", "tables", "the disease",
                                          "the study", "title")
                       )
{
    sectionNames = standardizeSectionNames(sectionNames)
    i = grep("^table", sectionNames)
    
    sectionNames[i] = "tables"
    tt = sort(table(unlist(tapply(sectionNames, pdf, unique))), decreasing = TRUE)
    
    topSect = names(tt)[1:(n-1)]
    
    sectionNames[!sectionNames %in% commonSections] = "misc"
    factor(sectionNames)
}

standardizeSectionNames = function(sects)
{
    sects = tolower(sects)
    sects = XML:::trim(gsub("^[0-9]\\.", "", sects))
    sects[sects == "di scus sio n"] = "discussion"
    sects[grep("(materials? and )?methods?", sects)] = "methods"
    sects[grep("author(s')? contributions?", sects)] = "author contributions"
    sects[grep("conclusions?", sects)] = "conclusions"
    sects[sects %in% c("a cknowled gements",
                       "ac knowledgements",
                       "acknowledgment",
                       "acknowledgments")] = "ackowledgements"
    sects[grep("et al\\.?", sects)] = "author ref"
    sects[sects == "summar y"] = "summary"
    # sects[grep("and [a-z]+ (([a-z]\\.)+)? [a-z]+", sects)] = "author list"
    sects
}

commonSections = c("", "<other>", "abstract", "ackowledgements", "author contributions", 
                   "author ref", "author summary", "background", "body", "conclusions", 
                   "control of vsv", "discussion", "future directions", "impacts", 
                   "introduction", "methods", "misc", "natural cycles of infection", 
                   "results", "results and discussion", "sources and manufacturers", 
                   "summary", "tables", "the disease", "the study", "title")

collapseTestSets = function(tar, test_sets)
{

    ans = lapply(names(test_sets), function(nm) {
        x = test_sets[[nm]]
        x = x[x$pdf == tar,]
        if(nm == "location")
            x$var = countrycode::countrycode(x$var,  "iso2c", "country.name")
        
        x$dataType = nm
        x$correct = FALSE
        x$missing = NA
        x = x[!duplicated(x),]
        x[order(x$score, decreasing = TRUE),]
    })
    do.call(rbind, ans)
}

collectMissing = function(missing, sp, spVar)
{
    ans = lapply(names(missing), function(nm){
        i = grep(nm, sp$fixedPDF, fixed = TRUE)
        tmp = unique(sp[i, spVar])
        tmp[!tmp %in% c("","?", "unknown","Unknown")]
    })
    data.frame(spVar = unlist(ans),
               pdf = rep(names(missing), sapply(ans, length)),
               stringsAsFactors = FALSE)
}


