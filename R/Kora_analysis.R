library(tidyverse)
library(patchwork)
library(splines)
library(broom)
library(openxlsx)
library(tableone)

df1 <- readRDS("Data/KORA_data_formatted.RDS")


# plotting functions
theme_apply <- theme_minimal() +
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.title = element_text(size=16, face="bold"),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18),
          axis.text.x = element_text(size=15, face="bold"),
          axis.text.y = element_text(size=15, face="bold"))

theme_apply_sm <- theme_minimal() +
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.title = element_text(size=14, face="bold"),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.x = element_text(size=12, face="bold"),
          axis.text.y = element_text(size=12, face="bold"))

theme_apply_aspect1 <- theme_minimal() +
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.title = element_text(size=16, face="bold"),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18),
          axis.text.x = element_text(size=15, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          aspect.ratio = 1)

theme_apply_change <- theme_minimal() +
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.position = "none",
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18),
          axis.text.x = element_text(size=15, face="bold"),
          axis.text.y = element_text(size=15, face="bold"),
          strip.text.x = element_text(size = 14, face = "bold"),
          panel.spacing.y=unit(0, "lines"))

theme_apply_box <- theme_minimal() +
    theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
          legend.title = element_text(size=16, face="bold"),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=18),
          axis.text.x = element_text(size=12, face="bold"),
          axis.text.y = element_text(size=15, face="bold"))



# Function to calculate geometric mean of vector
gm_mean = function(x, na.rm=FALSE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


# How many study participants were included?
nrow(df1)
# How many had lead levels measurements ?
nrow(df1[ !is.na(df1$pb2020), ])

##### Summary table of cohort characteristics
catvars_tab1 <- c("sex", "age_cat", "bmi_whocat",
                  "smok_cat5", "etoh_cat", "physact_cat",
                  "educ_level", "prof_cat", "currentwork_status",
                  "income_helmert", "soc_class_helmert_cat",
                  "health_ins")
df1$leadmeasured2020 <- ifelse(!is.na(df1$pb2020), "Yes", "No")
df1$leadmeasured1988 <- ifelse(!is.na(df1$pb1988), "Yes", "No")

vars_cohort_describe <- c(catvars_tab1[catvars_tab1!="sex" ], "leadmeasured2020", "leadmeasured1988")
cohort_descriptives <- CreateTableOne(vars = vars_cohort_describe,
                                      factorVars = catvars_tab1,
                                      includeNA = TRUE, data = df1, strata = "sex",
                                      test = FALSE)
write.csv(print(cohort_descriptives, quote = FALSE),
          "Results/tableS1_cohort_descriptives.csv", row.names = TRUE)










##### summary tables of lead by demographic variables #####
# define quantiles for summary data
quants <- c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95)



data_quantiles <- lapply(1:length(catvars_tab1 ), function(i){
    df1 %>% 
        group_by(!!sym(catvars_tab1 [i])) %>%  
        summarise(enframe(quantile(pb2020, quants,
                                   na.rm=TRUE), "quantile", "pb")) %>% 
        rename(strata=1) %>% 
        pivot_wider(names_from = quantile, values_from = pb, names_prefix = "P") %>% 
        mutate(var = !!(catvars_tab1 [i]))
})
data_quantiles <- do.call("rbind", data_quantiles)

data_means <- lapply(1:length(catvars_tab1 ), function(i){
    df1 %>% 
        group_by(!!sym(catvars_tab1 [i])) %>%  
        summarise(n = n(),
                  armean = mean(pb2020, na.rm=TRUE),
              geomean = gm_mean(pb2020, na.rm=TRUE)) %>% 
        rename(strata=1) %>% 
        mutate(var = !!(catvars_tab1 [i]))
})
data_means <- do.call("rbind", data_means)


#Overall summary
alldata_quantiles <- df1 %>%
    summarise(enframe(quantile(pb2020, quants,
                                   na.rm=TRUE), "quantile", "pb")) %>% 
    pivot_wider(names_from = quantile, values_from = pb, names_prefix = "P")
alldata_means <- df1 %>% 
    summarise(n = n(),
                  armean = mean(pb2020, na.rm=TRUE),
                  geomean = gm_mean(pb2020, na.rm=TRUE))
all_data_summ <- cbind(alldata_means, alldata_quantiles)
all_data_summ$strata <- "All particpants"


table1 <- left_join (data_means, data_quantiles )
table1 <- bind_rows(all_data_summ, table1) %>% select(var, strata, everything())
write.csv(table1, "Results/table1.csv", row.names = FALSE)

# Ratio of geometric means in men compared to women
table1[ table1$strata== "Male" & !is.na(table1$strata), ]$geomean /
    table1[ table1$strata== "Female" & !is.na(table1$strata), ]$geomean 

# How many individuals had BLLs over reference levels ?
#  For women 30 is German Federal Environmental Agency proposed limit
table(df1$pb2020 > 30, df1$sex, useNA = "ifany")
#  For men 40 is German Federal Environmental Agency proposed limit
table(df1$pb2020 > 40, df1$sex, useNA = "ifany")
# For CDC 50 is limit regardless of gender
table(df1$pb2020 > 50, useNA = "ifany")


##### Sex stratified tables ####
sex_quantiles <- lapply(1:length(catvars_tab1 ), function(i){
    if( catvars_tab1[i] != "sex"){ 
        df1 %>% 
            group_by(!!sym(catvars_tab1 [i]), sex) %>%  
            summarise(enframe(quantile(pb2020, quants,
                                       na.rm=TRUE), "quantile", "pb")) %>% 
            rename(strata=1) %>% 
            pivot_wider(names_from = quantile, values_from = pb, names_prefix = "P") %>% 
            mutate(var = !!(catvars_tab1 [i]))
    }
})
sex_quantiles <- do.call("rbind", sex_quantiles)

sex_means <- lapply(1:length(catvars_tab1 ), function(i){
    if( catvars_tab1[i] != "sex"){ 
        df1 %>% 
            group_by(!!sym(catvars_tab1 [i]), sex) %>%  
            summarise(n = n(),
                      armean = mean(pb2020, na.rm=TRUE),
                      geomean = gm_mean(pb2020, na.rm=TRUE)) %>% 
            rename(strata=1) %>% 
            mutate(var = !!(catvars_tab1 [i]))
    }
})
sex_means <- do.call("rbind", sex_means)

table1_by_sex <- left_join (sex_means, sex_quantiles ) %>% 
    arrange(var, sex)
write.csv(table1_by_sex, "Results/table1_by_sex.csv", row.names = FALSE)

# Note that summary data in table1 here is used in tables 1 and 2 of paper

# Anova tests for key variables
summary(aov(pb2020 ~ sex, df1))
summary(aov(pb2020 ~ age_cat, df1))
summary(aov(pb2020 ~ smok_cat5, df1))
summary(aov(pb2020 ~ physact_cat, df1))
summary(aov(pb2020 ~ educ_level, df1))
summary(aov(pb2020 ~ prof_cat, df1))
summary(aov(pb2020 ~ income_helmert, df1))
summary(aov(pb2020 ~ bmi, df1))
summary(aov(pb2020 ~ etoh_g_day, df1))



### Box plots for paper table 2 variables - education level, professional category, household income
boxplot1 <- ggplot(df1 %>% filter(!is.na(educ_level)), aes(x = educ_level, y = pb2020)) +
    geom_boxplot() + coord_flip() + 
    ylab("Blood lead level μg/L") + xlab("") +
    ggtitle("Blood lead levels by education level") +
    theme_apply_sm
boxplot2 <- ggplot(df1 %>% filter(!is.na(prof_cat)), aes(x = prof_cat, y = pb2020)) +
    geom_boxplot() + coord_flip() + 
    ylab("Blood lead level μg/L") + xlab("") +
    ggtitle("Blood lead levels by professional category") +
    theme_apply_sm
boxplot3 <- ggplot(df1 %>% filter(!is.na(income_helmert)), aes(x = income_helmert, y = pb2020)) +
    geom_boxplot() + coord_flip() + 
    ylab("Blood lead level μg/L") + xlab("") +
    ggtitle("Blood lead levels by Helmert income category") +
    theme_apply_sm

tiff("Graphs/FigS2_boxplot_education.tiff")
    boxplot1
dev.off()

tiff("Graphs/FigS3_boxplot_profcat.tiff")
    boxplot2
dev.off()

tiff("Graphs/FigS4_boxplot_helmert.tiff")
    boxplot3
dev.off()



# Quantile plots
g1 <- ggplot(df1, aes(x = bmi, y=log(pb2020))) + geom_point() +
    geom_quantile(quantiles = 0.1, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    geom_quantile(quantiles = 0.25, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.5, formula=y ~ ns(x, 2), aes(col="50%"), size=3) +
    geom_quantile(quantiles = 0.75, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.9, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    guides(colour=guide_legend(title="Quantiles")) + 
    scale_colour_manual(values=c("gold", "darkorange2", "red")) + 
    xlab("BMI (kg/m2)") + ylab("Log( blood lead level)") +
    ggtitle("Log blood lead levels vs BMI") + 
    coord_cartesian(xlim = c(15, 40)) + theme_apply

g2 <- ggplot(df1, aes(x = log(etoh_g_day), y=log(pb2020))) + geom_point() +
    geom_quantile(quantiles = 0.1, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    geom_quantile(quantiles = 0.25, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.5, formula=y ~ ns(x, 2), aes(col="50%"), size=3) +
    geom_quantile(quantiles = 0.75, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.9, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    guides(colour=guide_legend(title="Quantiles")) + 
    scale_colour_manual(values=c("gold", "darkorange2", "red")) + 
    xlab("Log( g of alcohol per day)") + ylab("Log( blood lead level)") +
    ggtitle("Log blood lead levels vs log alcohol consumption") +
    theme_apply

tiff("Graphs/Fig3_BLLvsEtoH.tiff", width=1200, height=900)
    print(g1 + g2 + plot_layout(guides="collect"))
dev.off()

# Draw similar graphs for alcohol subtypes
g_spirits <-  ggplot(df1, aes(x = log(spirits_g_day), y=log(pb2020))) + geom_point() +
    geom_quantile(quantiles = 0.1, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    geom_quantile(quantiles = 0.25, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.5, formula=y ~ ns(x, 2), aes(col="50%"), size=3) +
    geom_quantile(quantiles = 0.75, formula=y ~ ns(x, 2), aes(col="25%, 75%"), , size=1.25) +
    geom_quantile(quantiles = 0.9, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    guides(colour=guide_legend(title="Quantiles")) + 
    scale_colour_manual(values=c("gold", "darkorange2", "red")) + 
    xlab("Log( g of alcohol per day)") + ylab("Log( blood lead level)") +
    ggtitle("Spirits") +
    theme_apply_sm

g_wine <-  ggplot(df1, aes(x = log(wine_g_day), y=log(pb2020))) + geom_point() +
    geom_quantile(quantiles = 0.1, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    geom_quantile(quantiles = 0.25, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.5, formula=y ~ ns(x, 2), aes(col="50%"), size=3) +
    geom_quantile(quantiles = 0.75, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.9, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    guides(colour=guide_legend(title="Quantiles")) + 
    scale_colour_manual(values=c("gold", "darkorange2", "red")) + 
    xlab("Log( g of alcohol per day)") + ylab("Log( blood lead level)") +
    ggtitle("Wine") +
    theme_apply_sm

g_beer <-  ggplot(df1, aes(x = log(beer_g_day), y=log(pb2020))) + geom_point() +
    geom_quantile(quantiles = 0.1, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    geom_quantile(quantiles = 0.25, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.5, formula=y ~ ns(x, 2), aes(col="50%"), size=3) +
    geom_quantile(quantiles = 0.75, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.9, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    guides(colour=guide_legend(title="Quantiles")) + 
    scale_colour_manual(values=c("gold", "darkorange2", "red")) + 
    xlab("Log( g of alcohol per day)") + ylab("Log( blood lead level)") +
    ggtitle("Beer", ) +
    theme_apply_sm

tiff("Graphs/Fig4_BLLvs_typeEToH.tiff", width=1200, height=900)
    g_spirits + g_wine + g_beer + plot_layout(guides="collect")
dev.off()

# Plot sex stratified graph of wine consumption
g_wine_sex <-  ggplot(df1, aes(x = log(wine_g_day), y=log(pb2020))) + geom_point() +
    geom_quantile(quantiles = 0.1, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    geom_quantile(quantiles = 0.25, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.5, formula=y ~ ns(x, 2), aes(col="50%"), size=3) +
    geom_quantile(quantiles = 0.75, formula=y ~ ns(x, 2), aes(col="25%, 75%"), size=1.25) +
    geom_quantile(quantiles = 0.9, formula=y ~ ns(x, 2), aes(col="10%, 90%"), size=1.25) +
    guides(colour=guide_legend(title="Quantiles")) + 
    scale_colour_manual(values=c("gold", "darkorange2", "red")) + 
    xlab("Log( g of alcohol per day)") + ylab("Log( blood lead level)") +
    ggtitle("Wine") +
    theme_apply_sm + facet_wrap(~sex)

tiff("Graphs/FigS1_BLL_vs_wineXsex.tiff", width=1200, height=900)
    g_wine_sex
dev.off()




# How many drank each type of alcohol
table(df1$spirits_g_day >0 )
table(df1$spirits_g_day >0 ) / nrow(df1)
table(df1$wine_g_day >0 )
table(df1$wine_g_day >0 ) / nrow(df1)
table(df1$beer_g_day >0 )
table(df1$beer_g_day >0 ) / nrow(df1)


##### Make graphs for social variables
# Define a function to set whisker and box levels
f <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
}


g_educ <- ggplot(df1 %>% filter(!is.na(df1$educ_level)),
                 aes(x = educ_level, y = pb2020)) +
    stat_summary(geom = "boxplot", fun.data = f) + 
    stat_summary(geom = "errorbar", fun.data = f) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("") + ylab("Blood lead level") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme_apply_box

g_prof <- ggplot(df1 %>% filter(!is.na(df1$prof_cat)),
                 aes(x = prof_cat, y = pb2020)) +
    stat_summary(geom = "boxplot", fun.data = f) + 
    stat_summary(geom = "errorbar", fun.data = f) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("") + ylab("Blood lead level") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme_apply_box

g_helcat <- ggplot(df1 %>% filter(!is.na(df1$soc_class_helmert_cat)),
                 aes(x = soc_class_helmert_cat, y = pb2020)) +
    stat_summary(geom = "boxplot", fun.data = f) + 
    stat_summary(geom = "errorbar", fun.data = f) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("") + ylab("Blood lead level") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme_apply_box

g_helinc <- ggplot(df1 %>% filter(!is.na(df1$income_helmert)),
                   aes(x = income_helmert, y = pb2020)) +
    stat_summary(geom = "boxplot", fun.data = f) + 
    stat_summary(geom = "errorbar", fun.data = f) +
    coord_cartesian(ylim = c(0, 80)) +
    xlab("") + ylab("Blood lead level") +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) + 
    theme_apply_box

(g_educ + g_prof) / (g_helcat + g_helinc)


##### Compare blood lead between 1988 and 2018/19

# How many were measured in both years ?

table(!is.na(df1$pb1988), !is.na(df1$pb2020))
# both TRUE = measured in both years.


# Plot changes for individuals with measurements in both years
# Note this excludes those below l.o.d. in 1988
df2 <- df1[ !is.na(df1$pb1988) & !is.na(df1$pb2020), ] 


# overall GM mean in 1988
gm_mean(df2$pb1988, na.rm=TRUE)

# summary data for comparing data by year
quantiles2020 <- df2 %>% 
        group_by(sex) %>%  
        summarise(enframe(quantile(pb2020, quants,
                                   na.rm=TRUE), "quantile", "pb")) %>% 
        rename(strata=1) %>% 
        pivot_wider(names_from = quantile, values_from = pb, names_prefix = "P") %>% 
        mutate(year = "2018/19") %>%
    ungroup() %>% 
    select( year, everything())

quantiles1988 <- df2 %>% 
    group_by(sex) %>%  
    summarise(enframe(quantile(pb1988, quants,
                               na.rm=TRUE), "quantile", "pb")) %>% 
    rename(strata=1) %>% 
    pivot_wider(names_from = quantile, values_from = pb, names_prefix = "P") %>% 
    mutate(year = "1988") %>%
    ungroup() %>% 
    select( year, everything())

data_means2020 <- df2 %>% 
        group_by(sex) %>%  
        summarise(n = n(),
                  armean = mean(pb2020, na.rm=TRUE),
                  geomean = gm_mean(pb2020, na.rm=TRUE)) %>% 
        rename(strata=1) %>% 
    mutate(year = "2018/19") %>%
    ungroup() %>% 
    select( year, everything())

data_means1988 <- df2 %>% 
    group_by(sex) %>%  
    summarise(n = n(),
              armean = mean(pb1988, na.rm=TRUE),
              geomean = gm_mean(pb1988, na.rm=TRUE)) %>% 
    rename(strata=1) %>% 
    mutate(year = "1988") %>%
    ungroup() %>% 
    select( year, everything())


table3 <- bind_cols( bind_rows(data_means1988, data_means2020), bind_rows(quantiles1988, quantiles2020) )
table3$year...6 <- NULL
table3$strata...7 <- NULL

table3 <- table3 %>% arrange(strata...2, year...1)
write.csv(table3, "Results/table3.csv", row.names = FALSE)

# difference of GM means between periods
data_means1988$geomean - data_means2020$geomean

# The limit of detection in 1988 < lod in 2020
# How many were below lod in 1988 (these have value 0 in the data)
table(df2$pb1988 ==0)

# For individuals - how many experienced increased levels vs decreased ?
# Once not at the lod in 1988
table(df2[df2$pb1988 > 0, ]$pb2020 >= df2[df2$pb1988 > 0, ]$pb1988)

# Who are the individuals with increased BLL ?

df2[ df2$pb1988 > 0 & (df2$pb2020 >= df2$pb1988), ]



# Pivot to long format to facilitate plot of change in pb levels between years
dflong <- pivot_longer(df2, cols = c("pb2020", "pb1988"), values_to = "BLL" )
dflong$BLL_year <- ifelse(dflong$name == "pb2020", "2018/19", "1988")
    
g_change <- ggplot( dflong %>% filter(name == "pb2020" |
                              (name == "pb1988" & BLL> 0)), aes(x = BLL_year, y = BLL, group = zz_nr )) +
    geom_line(aes(col=sex), alpha = 0.1, size = 1) +
    geom_point(aes(col=sex), alpha = 0.5, size = 1) + facet_wrap(~sex) +
    xlab("Year of measurement") + ylab("Blood Pb ng/L") +
    theme_apply_change

tiff("Graphs/Fig6_individualchange.tiff", width=800, height=600)
    g_change
dev.off()


# Boxplot by gender over time periods
g_box_delta <- ggplot(dflong, aes(x = BLL, col = sex)) + geom_boxplot() +
    facet_wrap(~BLL_year) + coord_flip() + theme_apply

tiff("Graphs/Fig5_change_by_sex_boxplot.tiff", width=800, height=600)
    g_box_delta
dev.off()





#### Linear regression models for exogenous variables using pb2020 as outcome ####
#mod_etoh_smok <- lm(pb2020 ~ etoh_g_day + smok_cat5 + sex + age_at_survey + bmi +
#                        educ_level + smok_cat5, df1)

mod_etoh_smok_male <- lm(pb2020 ~ etoh_g_day + smok_cat5 + age_at_survey + bmi +
       educ_level + smok_cat5, df1 %>% filter(sex == "Male"))
mod_etoh_smok_female <- lm(pb2020 ~ etoh_g_day + smok_cat5 + age_at_survey + bmi +
       educ_level + smok_cat5, df1 %>% filter(sex == "Female"))

# gather model statistics
modstats <- data.frame(rbind(
    cbind(model = "Males", glance(mod_etoh_smok_male)),
    cbind(model = "Females", glance(mod_etoh_smok_female))))

modresults <- data.frame( rbind(
    cbind(model = "Males", tidy(mod_etoh_smok_male, conf.int = TRUE)),
    cbind(model = "Females", tidy(mod_etoh_smok_female, conf.int = TRUE))))

# write to file
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Model Summaries")
writeData(wb, "Model Summaries", modstats)

addWorksheet(wb, sheetName = "Parameter Estimates")
writeData(wb, "Parameter Estimates", modresults)

saveWorkbook(wb, "Results/model_results.xlsx", overwrite = TRUE)








