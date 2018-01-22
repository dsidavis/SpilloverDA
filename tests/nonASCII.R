library(SpilloverDA)
if(FALSE)
{
# Assumes you are sitting in Zoonotic-shared
    f = "LatestDocs/PDF/0013939837/Huang-2012-Simian foamy virus prevalence in Ma.xml"

    if(file.exists('bob.rds'))
       tmp = readRDS('bob.rds')
    else
       tmp = xml2eco(f, cache.dir = NA, results_dir = NA)

    loc = getLocation(tmp)        
    # Gets killed by non-ASCII char
    tmp2 = mkCSV(eco = loc, write = TRUE, xlsxFile = "test.xlsx")

    # this corrects it
#    tmp3 = mkCSV(tmp_file, write = FALSE)
#    tmp3 = sapply(tmp3, stringi::stri_trans_general, id = "latin-ascii")
#    openxlsx::write.xlsx(tmp3, tempfile())
}
    
