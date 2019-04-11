getPDFconvertUtil = function()
{
    getOption("ImageConvert", Sys.getenv("ImageConvert", 'convert'))
}

PDFtoImage = function(pdf_file, out_file = character(),
                      convertUtil = getPDFconvertUtil(),
                      out_format = "png",
                      args = c("-density 300", "-background white", "-alpha remove", "-verbose"))
{
    if(length(out_file) == 0 & grepl("\\.pdf$", pdf_file))
        out_file = gsub("\\.pdf$", paste0(".", out_format), pdf_file)
    
    cmd = sprintf("%s '%s' %s '%s'", convertUtil, path.expand(pdf_file),
                  paste(args, collapse = " "), path.expand(out_file))
    out = system(cmd, intern = TRUE)
    list.files(path = dirname(out_file),
               pattern = gsub(paste0("(\\.)", out_format, "$"),
                              paste0(".*\\\\.", out_format),
                              basename(out_file)),
               full.name = TRUE)
}

ocrPDF = function(pdf_file,
                  images = PDFtoImage(pdf_file),
                  ...)
{
    ans = lapply(images, Rtesseract::GetBoxes, ...)
    do.call(rbind, ans)
}
                  
                 
