#!/usr/bin/env Rscript

#install packages
#install.packages(c("argparse", "dplyr", "Hmisc", "ggpubr"))
library("argparse", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("Hmisc", quietly = TRUE)
library("ggpubr", quietly = TRUE)
library("outliers", quietly = TRUE)
library("stats", quietly = TRUE)
library("car", quietly = TRUE)
library("dunn.test", quietly = TRUE)
library("FSA", quietly = TRUE)
library("purrr", quietly = TRUE)

script_desc <- "Not much for now"

#parse arguments for command
#parse <- argparse::ArgumentParser(description = script_desc,
#                                  formatter_class = 'argparse.RawTextHelpFormatter')
#parse$add_argument('-o', dest = 'output_file', help = 
#                     'Output file with report', required = FALSE)
#parse$add_argument('-f', dest = 'file', help = 
#                     'Source file with data', required = TRUE)
#parse$add_argument('-s', dest = 'sep', help = 
#                     'Data separator from csv file', required = TRUE)
#args <- parse$parse_args()

#FUNCTIONS=============================================
#standard function to get mode value; thanks tutorialspoint!
getModeValue <- function(data_vector)
{
  unique_values <- unique(data_vector)
  unique_values[which.max(tabulate(match(data_vector, unique_values)))]
}

#imputes values by inserting mean if the value is numeric
#or by inserting mode if the value is not numeric
imputing <- function(data, missing_val)
{
  for(col in colnames(data))
  {
    if(is.numeric(data[[col]]))
    {
      data[[col]] <- impute(data[[col]], mean)
    }
    else
    {
      data[[col]] <- impute(data[[col]], fun=getModeValue)
    }
  }
  return(data)
}

#full summary with median, mean, min, max, var, sd, shapiro_test, outliers
allDataSummary <- function(data)
{
  group <- as.name(colnames(data)[1])
  summary <- group_by(data, !!group) %>% summarise_if(.predicate = is.numeric,
                                           .funs = list(
                                             median = median,
                                             average = mean,
                                             min_value = min,
                                             max_value = max,
                                             variance = var,
                                             s_deviation = sd,
                                             p_value = function(data) return(shapiro.test(data)$p.value)
                                           ))
  return(summary)
}

#print outliers
returnOutliers <- function(out)
{
  for(name in names(out))
  {
    cat("In column", name, length(out[[name]]), "outliers with value of",  out[[name]], "\n")
  }
}

#print distribution
returnDataDistribution <- function(attributes, summaryData)
{
  groups <- unique(summaryData[[1]])
  for(group in groups)
  {
    cat("For group", group, "\n")
    group_num <- which(summaryData[[1]] == group)
    for(attribute in attributes) 
    {
      attribute_name <- paste(attribute, '_p_value', sep = '')
      result <- as.numeric(summaryData[group_num,attribute_name])
      if(result < 0.05)
      {
        cat("Attribute", attribute_name, "is NOT distributed normally with p-value", result, "\n")
      }
      else
      {
        cat("Attribute", attribute_name, "is distributed normally with p-value", result, "\n")
        
      }
    }
  }
}

# plot distribution
density_group_plot <- function(data)
{
  graphics.off()
  
  group <- as.name(colnames(data)[1])
  groupedData <- data %>% group_by(!!group)
  gr_char <- as.character(unique(groupedData[[1]]))
  namesCol <- colnames(data %>% select_if(is.numeric))
  for(gr in gr_char)
  {
    group_num <- which(groupedData[[1]] == gr)
    selectedData <- groupedData[group_num,namesCol]
    applyToCol <- lapply(colnames(selectedData), function(col) 
      {
        ggdensity(
        data = selectedData,
        x = col,
        title = paste(col, 'density plot'),
        xlab = paste(col),
        alpha = 0
        )
      }
    )
    doPlot <- ggarrange(plotlist = applyToCol)
    png(filename = paste(gr, 'density.png', sep = '_'), width = 3840, height = 2160, bg = "transparent")
    annotate_figure(doPlot, top = paste(gr, 'attributes desnsity'))
    print(doPlot)
    dev.off()
  }
}

#plot outliers
box_group_plot <- function(data)
{
  graphics.off()
  
  group <- as.name(colnames(data)[1])
  groupedData <- group_by(data, !!group)
  gr_char <- as.character(unique(groupedData[[1]]))
  for(gr in gr_char)
  {
    group_num <- which(groupedData[[1]] == gr)
    selectedData <- as.data.frame(select_if(groupedData[group_num,], is.numeric))
    png(filename = paste(gr, 'boxplot.png', sep = '_'), width = 1980, height = 1080)
    for(col in ncol(selectedData))
    {
      selectedData[,col] <- as.numeric(selectedData[,col])
    }
    boxplot.default(selectedData,
            main = paste("Boxplot of group", gr, "of all numeric values"),
            ylab = "Values",
            col = sample(colors(), ncol(selectedData))
            )
    dev.off()
  }
}

#generate output file with report
createReport <- function(rData)
{
  #init
  sink(file = rData$args$output_file, append = TRUE)
    cat("=====Automatically generated report of analysis=====\n")
    cat("Analysis time: ", paste(now()), "\n")
    cat("=====================================================\n")
  sink()
  
  #report missing data
  sink(file = rData$args$output_file, append = TRUE)
    cat("===MISSING VALUES===\n")
    cat("There is ", rData$missing_values, "record/s with NA value.\n")
  sink()
  
  #report summary
  sink(file = rData$args$output_file, append = TRUE)
    cat("===SUMMARY===\n")
    cat("===For visualisation of outliers in every group check generated boxplots (groupname_boxplot.png)===")
    cat(as.data.frame(rData$summary))
  sink()
  
  #report summary
  sink(file = rData$args$output_file, append = TRUE)
    cat("===DISTRIBUTION===\n")
    cat("===For visualisation of distribution check generatred density plots (groupname_density.png)===")
    cat(as.data.frame(rData$summary))
  sink()
  
  cat("Report saved to", rData$args$args$output_file, "\n")
}

isEqualDistribution <- function(gr1, gr2, attribute, summaryData)
{
  gr1_num <- which(summaryData[[1]] == gr1)
  gr2_num <- which(summaryData[[1]] == gr2)
  attribute_name <- paste(attribute, '_p_value', sep = '')
  return(as.numeric(summaryData[gr1_num, attribute_name]) > 0.05 &
           as.numeric(summaryData[gr2_num, attribute_name]) > 0.05)
}

isEqualDistributionMany <- function(groups, attribute, summaryData)
{
  logical <- list()
  for(gr in groups)
  {
    gr_num <- which(summaryData[[1]] == gr)
    attribute_name <- paste(attribute, '_p_value', sep = '')
    logical <- append(logical, as.numeric(summaryData[gr_num, attribute_name]) > 0.05)
  }
  return(all(logical))
}

analiseTwoGroups <- function(data, summaryData)
{
  attributes <- colnames(data %>% select_if(is.numeric))
  groups <- unique(data[[1]])
  #initialize results dataframe
  results <- data.frame(setNames(rep(list(rep(NA, 2)), length(attributes) + 1), c('result', attributes)))
  results[['result']] <- c('p-value', 'test')
  
  cat("For attribute:\n")
  for(attribute in attributes)
  {
    cat(">", attribute, "\n")
    distribution_euqality <- isEqualDistribution(groups[[1]], groups[[2]], attribute, summaryData)
    variance_equality <- leveneTest(data[[attribute]] ~ data[[1]])$`Pr(>F)`[1] > 0.05
    if(distribution_euqality & variance_equality)
    {
      test <- t.test(data[[attribute]] ~ data[[1]], var.equal = TRUE)$p.value
      if(test < 0.05)
      {
        cat("\t[t.test]There are differences between groups\n")
      }
      else
      {
        cat("\t[t.test]There are NO differences between groups\n")
      }
      results[[attribute]] <- c(test, 't.test')
    }
    else if(distribution_euqality & !variance_equality)
    {
      test <- t.test(data[[attribute]] ~ data[[1]], var.equal = FALSE)$p.value
      if(test < 0.05)
      {
        cat("\t[Welch test]There are differences between groups\n")
      }
      else
      {
        cat("\t[Welch test]There are NO differences between groups\n")
      }
      results[[attribute]] <- c(test, 'Welch test')
    }
    else(distribution_euqality & variance_equality)
    {
      test <- wilcox.test(data[[attribute]] ~ data[[1]])$p.value
      if(test < 0.05)
      {
        cat("\t[Wilcoxon test]There are differences between groups\n")
      }
      else
      {
        cat("\t[Wilcoxon test]There are NO differences between groups\n")
      }
      results[[attribute]] <- c(test, 'Wilcoxon test')
    }
  }
  return(results)
}


analiseMultipleGroups <- function(data, summaryData)
{
  attributes <- colnames(data %>% select_if(is.numeric))
  groups <- unique(data[[1]])
  #initialize results dataframe
  results <- data.frame(setNames(rep(list(rep(NA, 2)), length(attributes) + 1), c('result', attributes)))
  results[['result']] <- c('p-value', 'test')
  
  cat("For attribute:\n")
  for(attribute in attributes)
  {
    cat(">", attribute, "================================\n")
    distribution_euqality <- isEqualDistributionMany(groups, attribute, summaryData)
    variance_equality <- leveneTest(data[[attribute]] ~ data[[1]])$`Pr(>F)`[1] > 0.05
    if(distribution_euqality & variance_equality)
    {
      test <- summary(aov(data[[attribute]] ~ data[[1]]))[[1]][["Pr(>F)"]][[1]]
      if(test < 0.05)
      {
        cat("\t[ANOVA test]There are differences between groups\n")
        cat("\tPost hoc test:\n")
        cat("\t")
        print(TukeyHSD(aov(data[[attribute]] ~ data[[1]])))
        cat("\n")
      }
      else
      {
        cat("\t[ANOVA test]There are NO differences between groups\n")
      }
      results[[attribute]] <- c(test, 'ANOVA test')
    }
    else if(distribution_euqality & !variance_equality)
    {
      test <- kruskal.test(data[[attribute]] ~ data[[1]])$p.value
      if(test < 0.05)
      {
        cat("\t[Kruskal test]There are differences between groups\n")
        cat("\tPost hoc test:\n")
        cat("\t")
        print(dunnTest(as.numeric(data[[attribute]]), data[[1]]))
        cat("\n")
      }
      else
      {
        cat("\t[Kruskal test]There are NO differences between groups\n")
      }
      results[[attribute]] <- c(test, 'Kruskal test')
    }
    else
    {
      test <- kruskal.test(data[[attribute]] ~ data[[1]])$p.value
      if(test < 0.05)
      {
        cat("\t[Kruskal test]There are differences between groups\n")
        cat("\tPost hoc test:\n")
        cat("\t")
        print(dunnTest(as.numeric(data[[attribute]]), data[[1]]))
        cat("\n")
      }
      else
      {
        cat("\t[Kruskal test]There are NO differences between groups\n")
      }
      results[[attribute]] <- c(test, 'Kruskal test')
    }
  }
  return(results)
}

analiseNonNumeric <- function(data)
{
  selectedData <- as.data.frame(data) %>% select_if(negate(is.numeric))
  cols <- colnames(selectedData)[-1]
  result <- list()
  
  cat("For attribute:\n")
  for(col in cols)
  {
    cat(">", col, "================================\n")
    test <- chisq.test(selectedData[[1]], selectedData[[col]])$p.value
    if(test < 0.05)
    {
      cat("\t[Chi-squared test]There are differences between groups\n")
    }
    else
    {
      cat("\t[Chi-squared test]There are NO differences between groups\n")
    }
    png(filename = paste(col, '_barplot.png', sep = ''), width = 1920, height = 1080)
    assembly <- table(selectedData[[col]], selectedData[[1]])
    barplot(assembly,
            legend = rownames(assembly),
            col = sample(colors(), length(unique(selectedData[[col]]))),
            beside = TRUE,
            xlab = 'group',
            ylab = col)
    dev.off()
    result[[col]] <- assembly
  }
  return(result)
}
#======================================================

#initialize report
dataReport <- list()

#load csv
#loadedData <- read.csv2(file = args$file, sep = args$sep)
loadedData <- read.csv2(file = "przykladoweDaneZBrakami.csv", sep = ';')

#detect and impute missing values
missing_values <- sum(!complete.cases(loadedData))
cat("There is ", missing_values, "record/s with NA value.\n")
dataReport$missingValues <- missing_values

loadedData <- imputing(loadedData, missing_values)
cat("Data corrected with average and mode values.\n")
dataReport$correctedData <- loadedData

#print report
#group <- colnames(loadedData)[1]
dataReport$summary <- allDataSummary(data=loadedData)
cat("===Numeric data summary===\n")
print(as.data.frame(dataReport$summary))
cat("======End of summary======\n")


#outliers
cat("===Outlying values in gropus===\n")
outliers <- lapply(loadedData %>% select_if(is.numeric), function(x){boxplot(x, plot = FALSE)$out})

#print outliers
cat(returnOutliers(outliers), sep='\n')

#put outliers to report
dataReport$outliersStr <- capture.output(returnOutliers(outliers))


#print distribution
cat("===Normal distribution===\n")
returnDataDistribution(colnames(loadedData %>% select_if(is.numeric)), dataReport$summary)
dataReport$dataDistribution <- capture.output(
  returnDataDistribution(colnames(loadedData %>% select_if(is.numeric)), dataReport$summary))

density_group_plot(loadedData)

box_group_plot(loadedData)

#if(!is.null(args$output_file))
#{
#  dataReport$args <- args
#  createReport(dataReport)
#}

#numeric statistical analysis
if(length(unique(loadedData[[1]])) < 2)
{
  warning('Loaded data contains only 1 group. Statistical comparsion is not possible - skipping.\n')
}else if(length(unique(loadedData[[1]])) == 2)
{
  cat("Group pairwise analysis:\n")
  dataReport$analysis <- analiseTwoGroups(loadedData, dataReport$summary)
  dataReport$analysisInterpretation <- capture.output(analiseTwoGroups(loadedData, dataReport$summary)) 
}else
{
  cat("Multiple group analysis:\n")
  dataReport$analysis <- analiseMultipleGroups(loadedData, dataReport$summary)
  dataReport$analysisInterpretation <- capture.output(analiseMultipleGroups(loadedData, dataReport$summary))
}
#non-numeric analysis
dataReport$nonNumericAnalysisInterpretation <- capture.output(analiseNonNumeric(loadedData))
cat(dataReport$nonNumericAnalysisInterpretation, sep = '\n')


#attributes <- names(unique(dataReport$summary %>% select_if(is.numeric)))
#groups <- unique(dataReport$summary[[1]])
#for(gr in groups)
#{
#  cat("GROUP:", gr)
#  gr_num <- which(dataReport$summary[[1]] == gr)
#  for(attr in attributes)
#  {
#    stargazer(as.data.frame(dataReport$summary[gr_num, attr]), summary = F, type = 'text')
#  }
#}

