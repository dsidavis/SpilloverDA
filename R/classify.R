trainTreeClassifier = function(trainSet)
{
    trainSet$sectionName = reduceSects(trainSet$sectionName, trainSet$pdf) 
    ctree(as.integer(correct) ~ sectionName + pdfFreq + sectFreq, data = trainSet)
}

trimExtractResults = function(testSet, mod, threshold = numeric())
{
    testSet = testSet[complete.cases(testSet),]
    testSet$sectionName = reduceSects(testSet$sectionName, testSet$pdf)
    testSet$score = predict(mod, newdata = testSet[,c("sectionName","pdfFreq", "sectFreq")])
    if(length(threshold) > 0)
        testSet = testSet[testSet$score >= threshold,]
    
    collapseResults(testSet)
}

collapseResults = function(testSet)
{
    tmp = split(testSet, paste(testSet$var, testSet$pdf))
    ans = lapply(tmp, function(x) {
        data.frame(var = unique(x$var),
                   sections = paste(unique(x$sectionName), collapse = ";"),
                   pdf = unique(x$pdf),
                   avg_score = round(mean(x$score, na.rm = TRUE), 3),
                   stringsAsFactors = FALSE)
    })
    ans = do.call(rbind, ans)
    row.names(ans) = NULL
    ans
}


resultsByPDF = function(resultsList, pdfNames, saveDir = character(),
                        outfiles = file.path(saveDir,
                                             gsub("\\.pdf$|\\.xml$", "_extract.json",
                                                  basename(pdfNames))))
{
    ans = lapply(seq(along = pdfNames), function(i){
        ans = getExtractByPDF(resultsList, pdfNames[i])
        ans = lapply(ans, function(x) x[order(x$avg_score, decreasing = TRUE),])
        if( length(saveDir) != 0 ){
            if(!dir.exists(saveDir))
                dir.create(saveDir)
            saveResultsJSON(ans, outfiles[i])
        }
    })
    ans
}

getMetaInfo = function(extractResults, saveDir = character(),
                       outfiles = file.path(saveDir,
                                 gsub("\\.pdf$|\\.xml$", "_meta.json",
                                      basename(names(extractResults)))))
{
    lapply(seq(along = extractResults), function(i, ee, saveDir, outfiles) { 

        res = data.frame(title = NA,
                   abstract = NA,
                   pdf = names(ee)[i],
                   year = NA,
                   stringsAsFactors = FALSE)       

        if(!is(ee[[i]]$title, "try-error"))
            res$title = ee[[i]]$title$txt

        if(!is(ee[[i]]$abstract, "try-error"))
            res$abstract = ee[[i]]$abstract$txt

        if(length(saveDir) != 0){
            if(!dir.exists(saveDir))
                dir.create(saveDir)
            saveResultsJSON(res, outfiles[i])
        }

        res
        }, ee = extractResults, saveDir = saveDir, outfiles = outfiles)
}

getExtractByPDF = function(resultsList, pdfName)
{
    lapply(resultsList, function(x) {
        x[x$pdf == pdfName,]
    })
}

saveResultsJSON = function(ans, outfile)
{
    write(toJSON(ans), file = outfile)
    return(outfile)
}
    
