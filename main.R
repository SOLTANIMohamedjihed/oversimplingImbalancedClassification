############################################################################################
######################## OverSampling for imbalanced classification ########################
############################################################################################
library(imbalance)
data(newthyroid1)              ###################################################################
table(newthyroid1$Class)       #  numPositive <- length(which(newthyroid1$Class == "positive")) #
## negative positive           #  numNegative <- length(which(newthyroid1$Class == "negative")) #
#     180       35             #  nInstances <- numNegative - numPositive                       #
imbalanceRatio(newthyroid1)    ###################################################################
# [1] 0.1944444
##################  Random walk oversampling #########################
NewSample <- rwo(newthyroid1,numInstances = 100)
#imbalanceRatio(NewSample)
plotComparison(newthyroid1, rbind(newthyroid1,NewSample),
               attrs = names(newthyroid1)[1:4])
##################  Rapidly converging Gibbs #########################
NewSample1 <- racog(newthyroid1,numInstances = 100)
plotComparison(newthyroid1, rbind(newthyroid1,NewSample1),attrs = names(newthyroid1)[1:4])
table(NewSample1$Class)
# negative positive
#      0      100
##################  Coperative game theorie #########################
# pdfos probability density function estimation baesd over sampling
PDFOS <- pdfos(newthyroid1,numInstances = 100)
NewSample2 <- neater(newthyroid1,newSamples = PDFOS)
table(NewSample2$Class)
#  negative positive
#      0       75
plotComparison(newthyroid1, rbind(newthyroid1,NewSample2),attrs = names(newthyroid1)[1:4])
#imbalanceRatio(NewSample2)
OvrSampl <- oversample(newthyroid1,ratio = 1,method = "PDFOS",filtering = TRUE,iteration=500)
table(OvrSampl$Class)
# negative positive
#      180      128
imbalanceRatio(OvrSampl)
# [1] 0.7111111