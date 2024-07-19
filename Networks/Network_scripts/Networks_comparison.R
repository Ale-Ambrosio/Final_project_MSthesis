#group differences with mnet
library(here)
library(dplyr)
library(qgraph)
library(ggplot2)
library(mnet)
library(tibble)
library(mlVAR)

Low_EE_group = read.csv2 (here ("Networks","Network_data","Input_data","All_Participants", "Filtered_data_whole", "lifestyle_filtered_lowEE.csv"),sep = ",")
High_EE_group = read.csv2 (here ("Networks","Network_data","Input_data","All_Participants", "Filtered_data_whole", "lifestyle_filtered_highEE.csv"),sep = ",")

Vars<- c("Stressed", "Happy", "Sad", "Bored", "Craving")

#we cut from the dataframes the columns that we don't use anymore 
Low_EE_group <- Low_EE_group[, -c(4, 5, 10, 11, 12, 13)]
High_EE_group <- High_EE_group[, -c(4, 5, 10, 11, 12, 13)]

# Convert variables to double to ensure compatibility
Low_EE_group <- Low_EE_group %>%
  mutate(across(all_of(Vars), as.double))

High_EE_group <- High_EE_group %>%
  mutate(across(all_of(Vars), as.double))

mlVAR_LowEE <- mlVAR(Low_EE_group,
                     vars = Vars,
                     idvar = "Participant.Label",
                     dayvar = "day",
                     beepvar = "beep",
                     temporal = "orthogonal",
                     contemporaneous = "orthogonal",
                     lags=1)

mlVAR_HighEE <- mlVAR(High_EE_group,
                      vars = Vars,
                      idvar = "Participant.Label",
                      dayvar = "day",
                      beepvar = "beep",
                      temporal = "orthogonal",
                      contemporaneous = "orthogonal",
                      lags=1)

#Contemporaneous
cont1 <- getNet(mlVAR_LowEE, "contemporaneous", nonsig = "hide", rule = "and")
cont2 <- getNet(mlVAR_HighEE, "contemporaneous", nonsig = "hide", rule = "and")

# Temporal:
temp1 <- getNet(mlVAR_LowEE, "temporal", nonsig = "hide")
temp2 <- getNet(mlVAR_HighEE, "temporal", nonsig = "hide")

# BETWEEN
bet1 <- getNet(mlVAR_LowEE, "between", nonsig = "hide", rule = "and")
bet2 <- getNet(mlVAR_HighEE, "between", nonsig = "hide", rule = "and")

#first we merge the two dataframes of the two groups

High_EE_group <- High_EE_group %>% mutate(group = "2")
Low_EE_group <- Low_EE_group %>% mutate(group = "1")

complete_df <- bind_rows(Low_EE_group, High_EE_group)

mlVAR_comparison <- mlVAR_GC(complete_df, 
                             vars = Vars, 
                             idvar = "Participant.Label", 
                             dayvar = "day",
                             beepvar = "beep",
                             groups = "group",
                             test = "parametric",
                             temporal = "orthogonal",
                             contemporaneous = "orthogonal",
                             nP = 1000,
                             pbar = TRUE)

#for the figures
l_outlist <- list()
output = mlVAR_comparison
l_outlist$GC <- output
out_g1 = mlVAR_LowEE
out_g2 = mlVAR_HighEE
l_outlist$g1 <- out_g1
l_outlist$g2 <- out_g2

#for temporal 
l_nets1 <- list()
l_nets1$phi_group0 <- temp1
l_nets1$phi_group1 <- temp2
l_nets1$phi_diff <- output$EmpDiffs$Lagged_fixed[, , 1]
l_nets1$phi_diffs_sig <- l_nets1$phi_diff
l_nets1$phi_diffs_sig[output$Pval$Lagged_fixed>0.05] <- 0

output$Pval$Lagged_fixed

titles <- c("Low EE", "High EE",
            "Differences: Group Low - Group High",
            "Significant Differences")

pdf(here("Networks","Networks_output","Comparison","PDF","Temporal_comparison.pdf"), width = 7, height = 7)
par(mfrow=c(2,2))
for(i in 1:4) qgraph(t(l_nets1[[i]]), # needs transpose, because qgraph plots X[2,1] as 2->1
                     layout = "circle",
                     edge.labels = TRUE,
                     title = titles[i],
                     theme = "colorblind",
                     maximum=0.2,
                     mar=rep(5,4),
                     vsize=13, esize=15, asize=9, edge.label.cex=1.5)

dev.off()

#for contemporaneous
l_nets2 <- list()
l_nets2$phi_group0 <- cont1
l_nets2$phi_group1 <- cont2
l_nets2$phi_diff <- output$EmpDiffs$Contemp_fixed
l_nets2$phi_diffs_sig <- l_nets2$phi_diff

condition <- output$Pval$Contemp_fixed > 0.05 | is.na(output$Pval$Contemp_fixed)
l_nets2$phi_diffs_sig[condition] <- 0


output$Pval$Contemp_fixed

titles <- c("Low EE", "High EE",
            "Differences: Group Low - Group High",
            "Significant Differences")

pdf(here("Networks","Networks_output","Comparison","PDF","Contemporaneous_comparison.pdf"), width = 7, height = 7)
par(mfrow=c(2,2))
for(i in 1:4) qgraph(t(l_nets2[[i]]), # needs transpose, because qgraph plots X[2,1] as 2->1
                     layout = "circle",
                     edge.labels = TRUE,
                     title = titles[i],
                     theme = "colorblind",
                     maximum=0.2,
                     mar=rep(5,4),
                     vsize=13, esize=15, asize=9, edge.label.cex=1.5)

dev.off()

#for between 
l_nets3 <- list()
l_nets3$phi_group0 <- bet1
l_nets3$phi_group1 <- bet2
l_nets3$phi_diff <- output$EmpDiffs$Between
l_nets3$phi_diffs_sig <- l_nets3$phi_diff

condition <- output$Pval$Between > 0.05 | is.na(output$Pval$Between)
l_nets3$phi_diffs_sig[condition] <- 0

output$Pval$Between

titles <- c("Low EE", "High EE",
            "Differences: Group Low - Group High",
            "Significant Differences")

pdf(here("Networks","Networks_output","Comparison","PDF","Between_comparison.pdf"), width = 7, height = 7)
par(mfrow=c(2,2))
for(i in 1:4) qgraph(t(l_nets3[[i]]), # needs transpose, because qgraph plots X[2,1] as 2->1
                     layout = "circle",
                     edge.labels = TRUE,
                     title = titles[i],
                     theme = "colorblind",
                     maximum=0.2,
                     mar=rep(5,4),
                     vsize=13, esize=15, asize=9, edge.label.cex=1.5)

dev.off()

#making matrices for results 
pvalues_temporal <- mlVAR_comparison[["Pval"]][["Lagged_fixed"]]
pvalues_temporal <- as.data.frame(pvalues_temporal)
pvalues_temporal$Variable <- rownames(pvalues_temporal)
pvalues_temporal <- pvalues_temporal[, c(ncol(pvalues_temporal), 1:(ncol(pvalues_temporal) - 1))]

temp_comp_path <- file.path (here("Networks","Networks_output","Comparison","Matrices","Temporal_comparison.csv"))
write.csv(pvalues_temporal, temp_comp_path, row.names = FALSE)

pvalues_contemporaneous <- mlVAR_comparison[["Pval"]][["Contemp_fixed"]]
pvalues_contemporaneous <- as.data.frame(pvalues_contemporaneous)
pvalues_contemporaneous$Variable <- rownames(pvalues_contemporaneous)
pvalues_contemporaneous <- pvalues_contemporaneous[, c(ncol(pvalues_contemporaneous), 1:(ncol(pvalues_contemporaneous) - 1))]

cont_comp_path <- file.path (here("Networks","Networks_output","Comparison","Matrices","Contemporaneous_comparison.csv"))
write.csv(pvalues_contemporaneous, cont_comp_path, row.names = FALSE)

pvalues_between <- mlVAR_comparison[["Pval"]][["Between"]]
pvalues_between <- as.data.frame(pvalues_between)
pvalues_between$Variable <- rownames(pvalues_between)
pvalues_between <- pvalues_between[, c(ncol(pvalues_between), 1:(ncol(pvalues_between) - 1))]


bet_comp_path <- file.path (here("Networks","Networks_output","Comparison","Matrices","Between_comparison.csv"))
write.csv(pvalues_between, bet_comp_path, row.names = FALSE)

#Introducing all the references 
library(report)

References = report(sessionInfo())
ref_path = here("Networks","Networks_output","Comparison","SessionReferences.txt")
writeLines(References, con = ref_path)
