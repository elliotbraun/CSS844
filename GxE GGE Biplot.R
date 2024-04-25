#GGE Biplot

#packages and functions
library(ggplot2)
library(tidyverse)
library(GGEBiplots)

#didn't use the below libraries or the mean_ function
#mean_   <- function(...) mean(..., na.rm = T) #from Falcon et al
#library(dplyr)
#library(plyr)
#library(data.table)
#library(reshape2)

setwd("E:/")
getwd()

phenos <- read.csv("PHENO.csv", row.names = )
#ger <- lm(e_cov$TT_GerEme~e_cov$biomass_GerEme)
#anova(ger)
#e_cov_no_locs <- e_cov %>% 
#  select(-X)
#pr_ger <- prcomp(e_cov)
#make some factors (optional)

#phenos$year_loc <- factor(phenos$year_loc)
#phenos$irrigated <- factor(phenos$irrigated)
#phenos$region <- factor(phenos$region)

#average_yield_df <- phenos %>%
#  group_by(year_loc, genotype) %>%
#  summarise(average_yield = mean(yield, na.rm = TRUE), .groups = 'drop')
#gge_v1 <- average_yield_df %>%
#  pivot_wider(names_from = year_loc, values_from = average_yield)

phenosv2 <- phenos %>% 
  filter(year==2020 | year==2021)
  
average_yield_v4 <- phenosv2 %>%
  group_by(year_loc, genotype) %>%
  summarise(average_yield = mean(yield, na.rm = TRUE), .groups = 'drop')

gge_v4 <- average_yield_v4 %>%
  pivot_wider(names_from = year_loc, values_from = average_yield)

#genotypes <- gge_v4$genotype

#From Falcon et al paper: The below removes rows with more than 90% missing data
#gge_v2 <- gge_v1[,colMeans(is.na(gge_v1)) < 0.9]
#gge_v3 <- gge_v2[rowMeans(is.na(gge_v2)) < 0.9,]
#na_count <- 0

#for (r in 1:nrow(gge_v2)) {
#  for (c in 1:ncol(gge_v2)) {
#    if (is.na(gge_v2[r,c])) {
#      na_count <- na_count + 1
#    }
#  }
#}

#optionally remove genotype column
#gge_v4 <- gge_v4 %>% select(-genotype)

#From Falcon et al paper: The below removes rows with more than X% missing data
na_proportions_v4_1 <- colMeans(is.na(gge_v4[, 2:ncol(gge_v4)]))
gge_v4_1 <- gge_v4[, c(TRUE, na_proportions_v4_1 < 0.5)]
na_proportions_v4_2 <- rowMeans(is.na(gge_v4_1[, -1]))
gge_v4_2 <- gge_v4_1[na_proportions_v4_2 < 0.5, ]
genotype <- gge_v4_2$genotype
#optionally remove genotype column
gge_v4_2 <- gge_v4_2 %>% select(-genotype)
#### From Falcon et al paper: estimate missing data as row/column mean ####
row_means <- rowMeans(gge_v4_2, na.rm = TRUE)
col_means <- colMeans(gge_v4_2, na.rm = TRUE)
impute_count = 0
for (r in 1:nrow(gge_v4_2)) {
  for (c in 1:ncol(gge_v4_2)) {
    if (is.na(gge_v4_2[r,c])) {
      impute_count <- impute_count + 1
      gge_v4_2[r,c] <- mean(row_means[r], col_means[c])
    }
  }
}
gge_v5 <- gge_v4_2
gge_v5 <- gge_v5 %>% mutate(genotype = genotype, .before = 1)
#write.csv(gge_v5, "gge_v6.csv")
#old discriminability file from Eleanor
discrims2 <- read.csv("discriminability_ranks2.csv")
discrims2 <- discrims2 %>% 
  select(-X) %>% 
  arrange(rank)
high_discrims2 <- discrims2 %>% 
  filter(rank<18)
low_discrims2 <- discrims2 %>% 
  filter(rank>=18)

#use Eleanor's discriminibility ranks csv
discrims <- read.csv("discriminability_ranks3.csv")
#discrims <- discrims %>%)
colnames(discrims)[1] <- "year_loc"
discrim_names <- discrims$year_loc
new_discrim_names <- gsub("^X(\\d{4})\\.(.*)$", "\\1-\\2", discrim_names)
even_newer_discrim_names <- new_discrim_names <- gsub("\\.(?=Dry|Early|Late)", "-", new_discrim_names, perl = TRUE)
even_newer_discrim_names <- even_newer_discrim_names
discrims$year_loc <- even_newer_discrim_names
discrims <- discrims %>% 
  arrange(vectorLength) %>% 
  filter(year_loc != 'X')
#hist(discrims$vectorLength)
high_discrims <- discrims %>% 
  filter(vectorLength>4.92)
low_discrims <- discrims %>% 
  filter(vectorLength<4.92)

e_cov <- read.csv("ECOV.csv")
colnames(e_cov)[1] <- "year_loc"
### mock-up code for t-test
# make group_df1 as example before plugging into for loop
group_covs1 <- e_cov %>% 
  filter(year_loc %in% high_discrims$year_loc)
  #vector that is given by subsetting e_cov into
  #covariate X and corresponding groups Y
group_covs2 <- e_cov %>% 
  filter(year_loc %in% low_discrims$year_loc)#vector that is given by subsetting e_cov into
  #covariate X and corresponding groups Z

cov_df <- data.frame()
#cov_df <- data.frame(col.names = colnames(group_covs1))

for (i in 1:length(colnames(group_covs1 %>% select(-year_loc)))) {
  col_name <- colnames(group_covs1 %>% select(-year_loc))[i]
  covector1 <- group_covs1 %>% 
    select(-year_loc) %>% 
    select(all_of(col_name))
  covector2 <- group_covs2 %>% 
    select(-year_loc) %>% 
    select(all_of(col_name))
  t_test <- t.test(covector1, covector2)
  p_value <- t_test$p.value
  new_row <- c(col_name, p_value)
  cov_df <- rbind(cov_df, new_row)
  #cov_df$col_name[1] =  #make an entry for covariate, p-value
}
colnames(cov_df) <- c("covariate", "p_value")

### working on this function, should make a group for each cov
### and then we can use bonferroni correction for each group so
### we can say each group is independent of the others. Hint use screenshots for env covariate descriptions
### groups will be temp (cov 1 & 2 on table), water (3-16), plant growth (17-21)

grouper <- function(string_list) {
  list_df <- list()
  
  for (starter in string_list) {
    cov_subsetted <- cov_df %>% 
      filter(str_starts(covariate, starter))
    
    list_df[[starter]] <- cov_subsetted
  }
  
  cov_full_group <- bind_rows(list_df)
  return(cov_full_group)
}
temp_group <- grouper(c("HI30", "CumHI30", "TT"))
water_group <- grouper(c("Eo", "Eos", "Es", "ESW", "Flow", "Flux", "Infiltration", "Pot", "Runoff", "SW", "T_", "Water"))
plant_growth_group <- grouper(c( "Cover", "LAI", "biomass", "yield"))

p_adjusted_temp <- p.adjust(temp_group$p_value, method = "BH")
temp_group$p_adjusted_FDR <- p_adjusted_temp

p_adjusted_water <- p.adjust(water_group$p_value, method = "BH")
water_group$p_adjusted_FDR <- p_adjusted_water

p_adjusted_growth <- p.adjust(plant_growth_group$p_value, method = "BH")
plant_growth_group$p_adjusted_FDR <- p_adjusted_growth

water_df_low_p_vals <- water_group %>% 
  filter(p_adjusted_FDR <.05)
#write.csv(water_df_low_p_vals, "fdr_adjusted_water_covariates.csv", row.names = F)

sw_highs <- group_covs1 %>% 
  select(year_loc, starts_with("SW"))
sw_lows <- group_covs2 %>%
  select(year_loc, starts_with("SW"))
row_means_highs <- rowMeans(sw_highs[,-1])
row_means_lows <- rowMeans(sw_lows[,-1])
row_sds_highs <- apply(sw_highs[,-1], 1, sd)
row_sds_lows <- apply(sw_lows[,-1], 1, sd)

sw_highs$mean <- row_means_highs
sw_lows$mean <- row_means_lows
sw_highs$sd <- row_sds_highs
sw_lows$sd <- row_sds_lows
#write.csv(sw_highs, "sw_highs.csv")
#write.csv(sw_lows, "sw_lows.csv")

sw_all <- read.csv("csv_SW_covariate_vals_per_year_loc.csv")
sw_all <- sw_all %>%
  arrange(Discriminibility.Group, year_loc) %>%
  mutate(year_loc = factor(year_loc, levels = unique(year_loc)))

plot <- ggplot(sw_all, aes(x = year_loc, y = mean, fill = Discriminibility.Group)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = c("High" = "red", "Low" = "blue")) +
  labs(x = "Year Location", y = "Mean Soil Water Covariate Value (mm)", fill = "Discriminability Group") +
  ggtitle("Soil Water Values at Each Year-Location") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1),  # Rotate and shrink x-axis labels
    axis.text.y = element_text(size = rel(.5))  # Set y-axis text to two-thirds the default size
  )

one_cov_plot <- ggplot(sw_all, aes(x = year_loc, y = mean, fill = Discriminibility.Group)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  #scale_fill_manual(values = c("High" = "red", "Low" = "blue")) +
  labs(x = "Year Location", y = "Mean Soil Water Covariate Value", fill = "Discriminability Group") +
  ggtitle("Soil Water Values at Each Year-Location") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1),  # Rotate and shrink x-axis labels
    axis.text.y = element_text(size = rel(.5))  # Set y-axis text to two-thirds the default size
  )

# Print the plot
print(plot)

print(colnames(sw_all))


#playing around to make for loop work
covector1 <- group_covs1 %>% 
  select(-year_loc) %>% 
  select(all_of("HI30_GerEme"))
covector2 <- group_covs2 %>% 
  select(-year_loc) %>% 
  select(all_of("HI30_GerEme"))
t_test <- t.test(covector1, covector2)
p_value <- t_test$p.value
col_name <- colnames(group_covs1 %>% select(-year_loc))[1]
new_row <- c(col_name, p_value)
cov_df <- rbind(cov_df, new_row)

col_test <- colnames(group_covs1)[2]
test_df <- group_covs1 %>% select(-year_loc)
test_df2 <- group_covs2 %>% select(-year_loc)
t_test <- t.test(test_df$HI30_GerEme, test_df2$HI30_GerEme)
add <- t_test$p.value
print('hello')









######  test/scratch code   #######

#generate the model
GGE1 <- GGEModel(gge_v4_2, centering = "tester", scaling = "none", SVP = "column")

# generate the GGE Biplot using GGEPlot()
discrimVsRepresent <- GGEPlot(GGE1, type = 7)
# if you want to see the physical plot, uncomment and run the below code
GGEPlot(GGE1, type = 7)
# calculate the environment vector lengths
data <- discrimVsRepresent$data
nenv <- ncol(data)
ngen <- nrow(data)
vectorLengths <- matrix(nrow = nenv, ncol = 1, dimnames = list(colnames(data), "vectorLength"))
for (v in (ngen + 1):(ngen + nenv)) {
  vectorLengths[(v - ngen),1] <- sqrt(discrimVsRepresent$data[v,1]^2 + (discrimVsRepresent$data[v,2])^2)
}

write.csv(gge_v4, "gge_v4.csv")
gge_v4$genotypes <- genotypes
row.names(gge_v3_no_genos) <- gge_v3$genotype
write.csv(gge_v3_no_genos, "gge_v3_no_genos.csv")
#### create models #### from Falcon et al paper
GGE1 <- GGEModel(gge_v3_no_genos, centering = "tester", SVP = "column") # Model for which won where plot & discriminability vs representativeness plot
GGE2 <- GGEModel(gge_v3_no_genos, centering = "tester", SVP = "row") # Model for mean vs stability plot

whichWonWhere <- GGEPlot(GGE1, type = 6) # which won where
ggsave(plot = whichWonWhere, filename = paste0("./GGEbiplots_standardized/WWW-", ".tif"),
       device = "tiff", width = 4, height = 4, units = "in")


total_year_loc <- length(unique(phenos$year_loc))
year_loc_counts <- table(phenos$year_loc)
barplot(year_loc_counts)
max(year_loc_counts)
#maximum is 1763
year_loc_counts[year_loc_counts==1763]
#2018-MNH1
min(year_loc_counts)
#minimum is 38
mean(year_loc_counts)
#make a table with only the year
#year_loc_counts_unique_genos <- year_loc_counts[which(year_loc_counts)]

#chatgpt says only one genotype with the following?
genotypes_in_all_year_loc <- phenos %>%
  group_by(genotype) %>%
  summarise(count = n_distinct(year_loc)) %>%
  ungroup() %>% # Ensure we're operating on the entire dataframe
  filter(count == total_year_loc) %>%
  nrow()


#make script to go iteratively through each genotype. For each genotype,
#if it appears in a year_loc, count += 1 
#next, only use genotypes that have x_count / 4372 unique genotypes = something
length(unique(phenos$year_loc))
genos_unique <- as.data.frame(unique(phenos$genotype))
genos_unique$count <- ""
# there's 136 unique year_loc and 4372 unique genotypes

year_loc <- phenos$year_loc
genotypes <- phenos$genotype
geno_counts_df <- genos_unique

geno_counts <- function(genos_unique) {
  for (i in 1:length(genos_counts_df)){
    for (j in 1:length(genotypes)){
      if (geno_counts_df[i] %in% genotypes[j]){
      geno_counts_df$count[i] = geno_counts_df$count[i] + 1
      }
    }
  }
  return(geno_counts_df)
}

geno_counts(genos_unique)

length(unique(phenos$genotype))
geno_counts <- table(phenos$genotype)
barplot(geno_counts)

#eleanor's plot below
plot.default(phenos$year_loc, phenos$yield, xlab = "Year_Location", ylab = "Yield", col = phenos$region)

phenos <- ordered(phenos)
filtered_phenos <- phenos[(phenos$genotype/>.7,]
locations_to_keep <- phenos %>%
  group_by(phenos$year_loc) %>%
  summarise(UniqueGenotypes = n_distinct(genotype)) %>%
  filter(UniqueGenotypes >= 0.7 * length(unique(phenos$genotype))) %>%
  pull(phenos$year_loc)


countsdf <- as.data.frame(year_date_counts)
colnames(countsdf) = c("year_loc", "Freq")
summarydf <- phenos %>%
  group_by(phenos$year_loc, phenos$location) %>%
  summarise(Count = n(), .groups = 'drop')
ggplot(summarydf)

standardize <- function(column) {
  mean <- mean(column)
  sd <- sd(column)
  for i in 1:length(column):
    (column[i] - mean) / (sd)
  }

###### useful R stuff from Addie demo ######
asi <- aggregate(pheno$ASI, by=list(pheno$year_loc), FUN="mean")
# this is very handy dandy


