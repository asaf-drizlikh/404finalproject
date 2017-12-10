dta <- read.csv('https://raw.githubusercontent.com/asafgibor/404finalproject/master/FinalReport/hof_all_cp.csv')
library(reshape2)##for melt
library(ggplot2)
dta_cor <- dta[,c("OBP", "AB", "SF","SLG","SB","SH")]
cor_mat <- cor(dta_cor)
###Extract lower triangle
cor_mat[lower.tri(cor_mat)] <- NA
###must melt matrix for ggplot
melt_mat <- melt(cor_mat, na.rm = TRUE)
##plot
ggplot(data = melt_mat, aes(Var2, Var1, fill = value)) +
  geom_raster(color = "white") + 	##faster than geom_tile()
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name = "Baseball Stats Correlation") +		## create gradient, darker approaching +- 1, provides legend 
  theme_minimal()+		##white background
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+ ##rotate and shrink x axis labels
  theme(axis.text.y = element_text(size = 8))+
  coord_fixed()
