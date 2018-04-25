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
