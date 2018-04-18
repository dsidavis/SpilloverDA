if(FALSE){
    library(SpilloverDA)
    sp = readRDS("~/DSI/Projects/Zoonotics-shared/SpeciesWithDatesForPlots.rds")
    ecoResults = readRDS("~/DSI/Projects/Zoonotics-shared/OldPDF_ecoResults.rds")
    ans = getErrors(extractResults=ecoResults,
                    sp = sp,
                    spVar = "virus", extractVar = "virus", fun = getVirus2)
    

    
}
