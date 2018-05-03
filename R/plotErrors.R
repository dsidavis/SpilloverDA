################################################################################
# Plotting code - maybe move elsewhere?

plotErrorRates = function(mod, trainSetCorrect)
{
    tt = getErrorTab(mod, trainSetCorrect)
    # browser()
    ggplot2::ggplot(tt, aes(x = Var1, y = cumsum, color = trainSetCorrect)) +
        geom_smooth(se = FALSE) +
        geom_point() +
        xlab("Confidence Threshold") +
        scale_x_reverse() +
        theme_bw()
}
getErrorTab = function(mod, trainSetCorrect)
{
    tt = table(predict(mod), trainSetCorrect)
    tt = tt[nrow(tt):1,]
    n_true = cumsum(tt[,"TRUE"]) 
    n_false = cumsum(tt[,"FALSE"]) 
    tt = as.data.frame(tt)
    tt$Var1 = as.numeric(as.character(tt$Var1))
    tt$cumsum = NA
    tt$cumsum[tt$trainSetCorrect == "FALSE"] = n_false
    tt$cumsum[tt$trainSetCorrect == "TRUE"] = n_true
    levels(tt$trainSetCorrect) = c("False Positive", "Correctly included")

    tt
    
}
