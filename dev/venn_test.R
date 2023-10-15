library(ggVennDiagram)
library(ggplot2)
library(gplots)


#venn <- Venn(list(A=1:3,B=2:5,C=4:8))
#data <- process_data(venn)


AW.DL <- c("a","b","c","d")

AW.FL <- c("a","b", "e", "f")

AW.UL <- c("a","c", "e", "g")

lst <- list(
  First=AW.DL,
  Second=AW.FL,
  Third=AW.UL
  )

venn(lst)

ItemsList <- venn(lst, show.plot = FALSE)

listaAtt <- attributes(ItemsList)$intersections

compareList <- names(listaAtt)

compare <- "First:Third"

listaAtt[names(listaAtt) == compare] 
