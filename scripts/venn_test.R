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

test <- ggVennDiagram(lst, label_alpha=0.25, category.names = c("one","two","three")) 
test

venn <- Venn(lst)
data <- process_data(venn)
# create venn diagram
vennSets <- ggplot() +
  # change mapping of color filling
  geom_sf(aes(fill = id), data = venn_region(data), show.legend = FALSE) +  
  # adjust edge size and color
  geom_sf(color="grey", size = 3, data = venn_setedge(data), show.legend = FALSE) +  
  # show set label in bold
  geom_sf_text(aes(label = c("one","two","three")), fontface = "bold", data = venn_setlabel(data)) +  
  # add a alternative region name
  geom_sf_label(aes(label = name), data = venn_region(data), alpha = 0.5) +  
  theme_void()
#return plot
vennSets
