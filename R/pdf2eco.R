# Function to go from a PDF file to JSON

xml2eco = function(XML, ecoextract = "EcoHealth/ecoextract.py",
                   results.dir = "ecoJSON", cache.dir = "sections",
                   cache.file = file.path(cache.dir, gsub("xml$", "rds", basename(XML))),
                   section.text = getXMLText(cache.file),
                   save = FALSE, save_file = file.path(results.dir, gsub("xml$", "rds", basename(XML))))
{
    # Clean up special characters
    section.text = lapply(section.text, function(x) gsub('Â|"', "", x))

    # Drop references, etc.
    i = grep("references?|cited|acknowledgements?|funding|interests|disclosure", names(section.text),
             ignore.case = TRUE, invert = TRUE)
    section.text = section.text[i]
    ans = lapply(section.text, function(x) try(sect2eco(x, ecoextract = ecoextract)))

    if(save)
        saveRDS(ans, save_file)
    ans
}

getXMLText = function(cache.file)
{
    if(!exists(cache.file)){
        doc = readPDFXML(XML)
        f = tempfile()
        
        title = getDocTitleString(doc)
        abst = findAbstract(doc)
        
        abst = paste(names(abst), collapse = " ")
        
        allsect = try(lapply(getSectionText(doc), function(x)
            paste(unlist(x), collapse = " ")))
        
        # browser()
    
        if(is(allsect, "try-error") | length(allsect) > 20)
            allsect = list(body = getDocText(doc))
        
        ll = c(title = title, abstract = abst, allsect)
        saveRDS(ll, file = cache.file)
    } else {
        ll = readRDS(cache.file)
    }
    return(ll)
}

sect2eco = function(section, ecoextract = "EcoHealth/ecoextract.py")
{
    f = tempfile()
    f2 = tempfile()

    cat(section, file = f)
    system2(ecoextract, args = c(f, f2))
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


expand_abbrevs = function(txt)
{
    # txt = "This is a Genus species example of an abbreviation G. species, but this is M. Espe's name."
    txt = strsplit(txt, " +")[[1]]
    pos = grep("[A-Z]\\.", txt)
    next_word = txt[pos + 1]
    next_word = gsub("\\(|\\)|,|;|\\.", "", next_word)
    other_pos = lapply(next_word, function(x) {
        possible_pos = grep(x, txt)
        setdiff(possible_pos, pos + 1)
    })
    
    replace_term = lapply(seq(along = other_pos), function(i){
        x = txt[other_pos[[i]] - 1]
        ans = txt[pos[[i]]]
        if(length(x) != 0){
            if(substr(ans, 1, 1) == substr(x, 1, 1))
                ans = x
        }
        x             
    })

    mod_txt = txt
    for(i in seq(along = pos)){
        if(length(replace_term[[i]]) != 0){
            mod_txt[pos[i]] = replace_term[[i]]
        }
    }

    return(paste(mod_txt, collapse = " "))
}
