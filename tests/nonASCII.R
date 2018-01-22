library(SpilloverDA)
if(FALSE){
# Assumes you are sitting in Zoonotic-shared
    f = "LatestDocs/PDF/0013939837/Huang-2012-Simian foamy virus prevalence in Ma.xml"

    tmp = xml2eco(f, cache.dir = NA, results_dir = NA)
    tmp_file = tempfile()
    saveRDS(tmp, tmp_file)
    # Gets killed by non-ASCII char
    tmp2 = mkCSV(tmp_file)

    # this corrects it
    tmp3 = mkCSV(tmp_file, write = FALSE)
    tmp3 = sapply(tmp3, stringi::stri_trans_general, id = "latin-ascii")
    openxlsx::write.xlsx(tmp3, tempfile())
}
    
