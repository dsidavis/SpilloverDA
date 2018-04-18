# library(SpilloverDA)
# Functions to create a dashboard of Type I and II from Species.csv and EpiResults
summarizeErrors = function(errors)
{
    
}


getErrors = function(extractResults = readRDS("OldPDF_ecoResults.rds"),
                       sp = readRDS("SpeciesWithDatesForPlots.rds"),
                       fun = NULL,
                       spVar = character(), extractVar = character(),
                       resultNames = names(extractResults))
{       

    
    vars = lapply(extractResults, function(x) try(fun(x)))
    i = sapply(vars, is, "try-error")
    vars = vars[!i]
    
    trainset = mkTrainingSet(sp, vars,
                             extractVar, spVar, resultNames)

    # browser()
    type2 = getType2(sp$fixedPDF, sp[[spVar]], trainset)
    rownames(type2) = NULL
    type1 = getType1(trainset)
    rownames(type1) = NULL
    list(type1 = type1,
         type2 = type2)
}

getType2 = function(spPDFs, spVarCol, trainset)
{
    tmp = split(trainset, trainset$pdf)
    ans = lapply(tmp, function(x){
        i = grep(unique(basename(x$pdf)), spPDFs, fixed = TRUE)
        # browser()

        if(length(i) == 0)
            return(data.frame(type2 = NA, pdf = unique(x$pdf), stringsAsFactors = FALSE))
        
        ll = spVarCol[i]
        ll = ll[ !ll %in% x$var]

        if(length(ll) == 0)
            return(data.frame(type2 = NA, pdf = unique(x$pdf), stringsAsFactors = FALSE))
        
        data.frame(type2 = ll,
                   pdf = unique(x$pdf),
                   stringsAsFactors = FALSE)
    })
    ans = do.call(rbind, ans)
    
}

getType1 = function(trainset)
{
    ans = by(trainset, trainset$pdf, function(x){
        tt = tapply(x$correct, x$var, sum)
        type1 = names(tt)[tt == 0]
        if(length(type1) == 0)
            type1 = NA

        x[x$var %in% type1,]
        # data.frame(type1 = type1,
                  # pdf = unique(x$pdf),
                  # stringsAsFactors = FALSE)
    })
    do.call(rbind, ans)
}
