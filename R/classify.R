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

mkJSON = function(resultsList, pdfNames, saveDir = character())
{
    ans = resultsByPDF(resultsList, pdfNames, saveDir)
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

getMetaInfo = function(extractResults)
{
    lapply(seq(along = extractResults), function() {
        data.frame(title = extractResults[[i]]$title$txt,
                   abstract = extractResults[[i]]$abstract$txt,
                   pdf = names(extractResults)[i],
                   year = integer(),
                   stringsAsFactors = FALSE)
        })
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
    
################################################################################
# Plotting code - maybe move elsewhere?

plotErrorRates = function(mod, trainSetCorrect)
{
    tt = getErrorTab(mod, trainSetCorrect)
    # browser()
    ggplot2::ggplot(tt, aes(x = Var1, y = cumsum, color = trainSetCorrect)) +
        geom_smooth(se = FALSE) +
        geom_point() +
        xlab("Confidence Threshold") +
        scale_x_reverse() +
        theme_bw()
}
getErrorTab = function(mod, trainSetCorrect)
{
    tt = table(predict(mod), trainSetCorrect)
    tt = tt[nrow(tt):1,]
    n_true = cumsum(tt[,"TRUE"]) 
    n_false = cumsum(tt[,"FALSE"]) 
    tt = as.data.frame(tt)
    tt$Var1 = as.numeric(as.character(tt$Var1))
    tt$cumsum = NA
    tt$cumsum[tt$trainSetCorrect == "FALSE"] = n_false
    tt$cumsum[tt$trainSetCorrect == "TRUE"] = n_true
    levels(tt$trainSetCorrect) = c("False Positive", "Correctly included")

    tt
    
}
