# This is not quite working

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

map_abbrevs = function(fulls, abbrevs)
{
    if(!is.character(abbrevs$species_abb))
        abbrevs$species_abb = as.character(abbrevs$species_abb)
    if(!is.character(fulls$species))
        fulls$species = as.character(fulls$species)
    
    u_abb = unique(abbrevs$species_abb)
    u_full = unique(unlist(strsplit(fulls$species, ";")))
    # browser()
    s_abbrevs = strsplit(u_abb, " +")
    poss_full = sapply(s_abbrevs, function(x) {
        i = grep(x[2], u_full)
        if(length(i) != 1)
            return(character())
        if(substring(x[1], 1, 1) == substring(u_full[i], 1, 1))
            return(u_full[i])
        return(character())
    })
    idx = match(abbrevs$species_abb, u_abb)
    ans = abbrevs
    ans$species = poss_full[idx]
    ans
}

combine_species = function(extractSpecies, extractSpAbb)
{
    if(nrow(extractSpecies) == 0 || nrow(extractSpAbb) == 0)
        return(extractSpecies)
    
    expandAbb = map_abbrevs(extractSpecies, extractSpAbb)
    expandAbb = expandAbb[expandAbb$species != "", colnames(extractSpecies)]
    if(!is.character(extractSpecies$species))
        extractSpecies$species = as.character(extractSpecies$species)
    rbind(extractSpecies, expandAbb)
}
