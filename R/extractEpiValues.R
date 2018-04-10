getLocation  =
    #
    # conflicts in name with another in Zoonotics-shared/R. So change the name in one of them.
    #
function(x, full = TRUE)
{
    if(full) {
        tmp = lapply(x, getLocation, full = FALSE)
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
        do.call(rbind, lapply(x$location, getLocationFun))
    }
}

getDate =
function(x)
{
    tmp = lapply(x, "[[", "date")
    browser()
    i = sapply(tmp, length)
    ans = lapply(tmp[i > 0 ], function(x) {
        ll = do.call(rbind, strsplit(x, ":"))
        
        data.frame(span = ll[,1],
                   text = ll[,2],
                   range = ll[,3],
                   stringsAsFactors = FALSE)
        
        })
    ans = do.call(rbind, ans)
    ans$section = rep(names(x), i)  
    ans
}

getLocationFun =
function(x)
{    
   data.frame(label = x$label,
               countryCode = x$geoname$country_code,
               start = x$textOffsets[[1]][1],
               end = x$textOffsets[[1]][2],
               score = x$geoname$score,
               namesUsed = x$geoname$names_used,
               parent = paste(sapply(x$geoname$parents, `[[`, "name"), collapse = ";"),
               parentNamesUsed = paste(sapply(x$geoname$parents, `[[`, "names_used"), collapse = ";"),
               stringsAsFactors = FALSE)    
}

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

getSpeciesAbb =
function(res, full = TRUE)
{
  getMisc(res, full, type = "species_abb")
}

getCountry =
function(res, full = TRUE)
{
  getMisc(res, full, type = "country")
}


# Maybe add one for dates.


getMisc =
    # toplevel
    # For now assume res is by section
    #
    #  section named (rep), span, diseaseName(s - but as ;-separated string)
    #
function(res, full = TRUE, type = "disease")
{
    tmp = lapply(names(res), function(nm) getMiscSection(res[[nm]], nm, type = type))
    do.call(rbind, tmp)
}

getMiscSection =
function(sec, nm, type = "disease")
{
    bad = data.frame(start = integer(), end = integer(), disease = character())
    if(!("resolved_keyword" %in% names(sec)))
        return(bad)
    
    w = sapply(sec$resolved_keyword, function(x) all(sapply(x$resolutions, function(x) x$entity["type"]) == type))
    if(!any(w))
        return(bad)
    ans = do.call(rbind, lapply(sec$resolved_keyword[w], getMiscInfo, type = type))
    ans$sectionName = nm
    ans
}

getMiscInfo =
    # span and the disease name
function(kw, type = "disease")    
{
    d = getMiscInfoFromResolution(kw$resolutions, type)
    ans = data.frame(start = kw$textOffsets[[1]][1],
                     end = kw$textOffsets[[1]][2],
                     disease = d,
                     label = kw$label)
    names(ans)[3] = type
    ans
}

getMiscInfoFromResolution =
function(res, type = "disease")
{
    w = sapply(res, function(x) x$entity["type"]) == type
    if(any(w)) 
       paste(sapply(res[w], function(x) x$entity["label"]), collapse = ";")
    else
       character()
}
