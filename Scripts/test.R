library(plotly)
library(tidyverse)
library(ggrepel)
library(cowplot)
library(patchwork)

setwd("C:/Users/Mariano/Documents/Cami/SocialResearch")

files <- list.files("Data", recursive = T)
files <- grep(".csv", files, value = T)
files <- grep("./", files, value = T)

tables <- lapply(paste0("Data/", files), read_csv)

tables <- lapply(tables, mutate,  Period = paste0(
  floor((Year - 1950) / 5) * 5 + 1950, 
  "-", 
  floor((Year - 1950) / 5) * 5 + 1954
))

tables <- lapply(tables, select, -c(Year, Code))

LAM <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Republica Dominicana", "Uruguay", "Venezuela")

LAM_urb <- paste0(LAM, " (urban)")
LAM <- c(LAM, LAM_urb)

Code <- rep(c("ARG", "BOL", "BRA", "CHI", "COL", "CR", "CU", "ECU", "SAL", "GUA", "HON", "MEX", "NIC", "PAN", "PAR", "PE", "DOM", "URU", "VEN"), 2)

years <- 1950:2022

# Stratify years into periods of 5 years
# Create a data frame with years and corresponding periods
stratified_years <- data.frame(
  Year = years,
  Period = paste0(
    floor((years - 1950) / 5) * 5 + 1950, 
    "-", 
    floor((years - 1950) / 5) * 5 + 1954
  )
)

Periods <- unique(stratified_years$Period)
big.table <- data.frame(
  Entity = rep(LAM, each = length(Periods)), 
  Code = rep(Code, each = length(Periods)), 
  Period = rep(Periods, time = length(LAM)))

for (i in 1:length(tables)){
  big.table <-  left_join(big.table, tables[[i]], by = c("Entity", "Period")) 
}
  
Literacy.dt <- group_by(big.table, Code, Period) %>% summarize(Literacy = mean(`Literacy rate`, na.rm = T)) %>%  ungroup()
Literacy.dt <- mutate(Literacy.dt, Period = as.numeric(str_split_i(Literacy.dt$Period, "-", 2)))

ggplot(Literacy.dt, aes(x= Period, color = Code)) +
  geom_line(aes(y = Literacy))

ggplot(filter(Literacy.dt, !is.nan(Literacy)), aes(x= Period, color = Code)) +
  geom_line(aes(y = Literacy))


## Correlation Literacy and homosexual couples good parents...
LiteracyTolerance.dt <- filter(big.table, Period %in% c("2020-2024")) %>% group_by(Code) %>% summarize(Literacy = mean(`Literacy rate`, na.rm = T), Tolerance = mean(`Homosexual couples are as good parents as other couples: Agree (aggregate)`, na.rm = T), Education_GDP = mean(`Public spending on education as a share of GDP`, na.rm = T)) %>%  ungroup() %>% na.omit()

LiteracyTolerance.dt$log_Tolerance <- log(LiteracyTolerance.dt$Tolerance)

model <- lm(log_Tolerance ~ Literacy, data = LiteracyTolerance.dt)
summary(model)
LiteracyTolerance.dt$predicted <- exp(predict(model, newdata = LiteracyTolerance.dt))
LiteracyTolerance.dt$residuals_upper <- LiteracyTolerance.dt$predicted * exp((0.03303*nrow(LiteracyTolerance.dt))^2)
LiteracyTolerance.dt$residuals_lower <- LiteracyTolerance.dt$predicted * exp(-(0.03303*nrow(LiteracyTolerance.dt))^2)


ggplot(LiteracyTolerance.dt, aes(x= Literacy, y = Tolerance, size = Education_GDP, color = Code)) +
  geom_ribbon(mapping = aes(x= Literacy, ymin = residuals_lower, ymax = residuals_upper), size = 1, fill = "lightgray", color = "lightgray") +
  geom_line(mapping = aes(x= Literacy, y = predicted), size = 1, color = "black") +
  geom_point() +
  theme_classic()

big.table.cumsum <- select(big.table, -Entity, -`990179-annotations`, -`935623-annotations`, -Period) %>% mutate(`LGBT+ employment discrimination (historical)` = as.numeric(factor(`LGBT+ employment discrimination (historical)`))) %>% group_by(Code) %>% summarise_all(sum, na.rm =  T)

big.table.m <- select(big.table.cumsum, -Code) %>%  as.matrix()
rownames(big.table.m) <- big.table.cumsum$Code

#big.table.m <- apply(big.table.m, 2, function(x) ifelse(class(x) == "factor", as.numeric(factor(x)), as.numeric(x)))

pca_result <- prcomp(big.table.m, scale. = TRUE) #na.action = na.omit by default

pca_df <- data.frame(
  PC1 = pca_result$x[, 1],  # First principal component
  PC2 = pca_result$x[, 2],  # Labels for coloring
  Code = big.table.cumsum$Code
)

p1 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Code, label = Code)) +
  geom_point(size = 3) +  # Points with a size of 3
  labs(title = "PCA: PC1 vs PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  geom_text_repel() +
  theme_minimal()  # Optional: for cleaner look

# Get the loadings (contributions of original variables)
loadings <- as.data.frame(pca_result$rotation[, 1:2])  # Only first two PCs
loadings$Variable <- rownames(loadings)  # Add variable names for plotting


# Plot PCA scores and loadings (biplot)
p2 <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Code)) +
  geom_point(size = 3) +  # Plot points for each sample
  geom_segment(data = loadings, 
               aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3), 
               arrow = arrow(type = "closed", length = unit(0.02, "inches")), 
               color = "black", size = 0.5) +  # Add arrows for loadings
  geom_text(data = loadings, 
            aes(x = PC1 * 3, y = PC2 * 3, label = Variable), 
            size = 3, vjust = 0.5, hjust = 0.5, color = "black") +  # Add variable labels
  labs(title = "PCA Biplot: PC1 vs PC2",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "none")

p2 + p1

# Algun grafico de barras en periodo definido que se pueda ir cambiando el periodo con la barra
# Algun mapa de LAM coloreado por alguna variable modificable
# alguna distribucion on ridge plot o algo asi, de por ejemplo las variables cuanti (cambiables tipo input)
# algun clustering sobre el grafico de PCs.
# alg'un grafico tipo venn paises en comun que comparten ... y ahi variables cuali.
