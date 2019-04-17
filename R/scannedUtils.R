getPDFconvertUtil = function()
{
    getOption("ImageConvert", Sys.getenv("ImageConvert", 'convert'))
}

PDFtoImage = function(pdf_file, out_file = character(),
                      convertUtil = getPDFconvertUtil(),
                      out_dir = tempdir(),
                      out_format = "png",
                      args = c("-density 300", "-background white", "-alpha remove", "-verbose"))
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
                  ocr_engine = "OEM_LSTM_ONLY",
                  pageSeg = "PSM_AUTO",
                  ...)
{
    apis = lapply(images, Rtesseract::tesseract,
                  engineMode = ocr_engine, pageSegMode = pageSeg, ...)
    title = getTitleWords(GetBoxes(apis[[titlePage]]), titleThreshold)
    text = paste(unlist(lapply(apis, GetText, level = "para")), collapse = " ")
    return(list(title = title, text = text))
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
    # 3. Does not "look" like the authors
    # 4. Does not contain words common in journal titles
{
    textSize = boxes$top - boxes$bottom
    titleSize = quantile(textSize, threshold)
    topHalf = boxes$bottom <= median(boxes$bottom)

    notAuthors = !grepl("([A-Z].*, ){2,}", boxes$text)
    notJournalTitle = !grepl("Journal|Research|Proceedings", boxes$text)
    title = paste(boxes$text[textSize >= titleSize &
                             topHalf &
                             notAuthors &
                             notJournalTitle],
                  collapse = " ")
    return(gsub("^ +|\\n| +$", "", title))
}

