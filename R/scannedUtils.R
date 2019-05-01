getPDFconvertUtil = function()
{
    getOption("ImageConvert", Sys.getenv("ImageConvert", 'convert'))
}

PDFtoImage = function(pdf_file, out_file = character(),
                      convertUtil = getPDFconvertUtil(),
                      out_dir = tempdir(),
                      out_format = "png",
                      args = c("-density 300", "-background white",
                               "-alpha remove", "-verbose"))
{
    if(length(out_file) == 0 & grepl("\\.pdf$", pdf_file))
        out_file = file.path(out_dir,
                             gsub("\\.pdf$", paste0(".", out_format), basename(pdf_file)))
    
    cmd = sprintf("%s %s '%s' '%s'", convertUtil,
                  paste(args, collapse = " "),
                  path.expand(pdf_file),
                  path.expand(out_file))
    out = system(cmd, intern = TRUE)
    list.files(path = dirname(out_file),
               pattern = gsub(paste0("(\\.)", out_format, "$"),
                              paste0(".*\\\\.", out_format),
                              basename(out_file)),
               full.name = TRUE)
}

ocrPDF = function(pdf_file,
                  images = PDFtoImage(pdf_file),
                  titlePage = 1L,
                  titleThreshold = 0.9,
                  ocrEngine = "OEM_LSTM_ONLY",
                  pageSeg = "PSM_AUTO",
                  ocrCacheDir = "ocr_cache",
                  ...)
{
    apis = lapply(images, Rtesseract::tesseract,
                  engineMode = ocrEngine, pageSegMode = pageSeg, ...)
    title = getTitleWords(Rtesseract::GetBoxes(apis[[titlePage]], level = "textline"), titleThreshold)
    # Currently not trying to recover sections
    text = paste(unlist(lapply(apis, Rtesseract::GetText, level = "para")), collapse = " ")
    ans = list(title = title, text = text)
    names(ans) = basename(pdf_file)

    if(!is.null(ocrCacheDir)){
        if(!dir.exists(ocrCacheDir))
            dir.create(ocrCacheDir)
        saveRDS(ans, file = file.path(ocrCacheDir, gsub("pdf$", "rds", basename(pdf_file))))
    }
    
    return(ans)
}
                  
findTitle = function(api, pages = 1)
{
    bb = do.call(rbind, lapply(api[pages], GetBoxes))
    titleWords = getTitleWords(bb, level = "textline")
}

getTitleWords = function(boxes, threshold)
    # Assumptions:
    # 1. Title is on top half of first page
    # 2. Title is larger than <threshold>% of main text
    # 2a. Title is min X characters, and max Y characters
    # 3. Does not "look" like the authors
    # 4. Does not contain words common in journal titles
    # 5. Does not contain words associated with affiliations
{
    textSize = boxes$top - boxes$bottom
    titleSize = quantile(textSize, threshold)
    topHalf = boxes$bottom <= median(boxes$bottom)

    notAuthors = !grepl("([A-Z].*, ){2,}", boxes$text)
    notJournalTitle = !grepl("Journal|Research|Proceedings", boxes$text)
    # notUni = !grepl("University|Department|")
    title = paste(boxes$text[textSize >= titleSize &
                             topHalf &
                             notAuthors &
                             notJournalTitle],
                  collapse = " ")
    return(gsub("^ +|\\n| +$", "", title))
}

trainTitle = function(bbox)
{
    textSize = bbox$top - bbox$bottom
    textWords = sapply(strsplit(bbox$text, " "), length)
    browser()
    paste(bbox$text[i], collapse = " ")
}

findTitleSize = function(bbox,
                         minWords = 3L,
                         maxWords = 25L)
{
    textSize = bbox$top - bbox$bottom
    textWords = sapply(strsplit(bbox$text, " "), length)

    
}

