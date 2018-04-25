trainTreeClassifier = function(trainSet)
{
    trainSet$sectionName = reduceSects(trainSet$sectionName, trainSet$pdf) 
    ctree(as.integer(correct) ~ sectionName + pdfFreq + sectFreq, data = trainSet)
}

trimExtractResults = function(testSet, mod, threshold = numeric())
{
    testSet = testSet[complete.cases(testSet),]
    testSet$sectionName = reduceSects(testSet$sectionName, testSet$pdf)
    testSet$score = predict(mods[[i]], newdata = testSet[,c("sectionName","pdfFreq", "sectFreq")])
    if(length(threshold) > 0)
        testSet = testSet[testSet$score >= threshold,]
    
    testSet
}
