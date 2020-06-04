#!/usr/bin/env Rscript

options(warn = -1)

cat("Checking libraries...\n")


#install.packages(c("argparse", "dplyr", "Hmisc",
#                   "ggpubr", "stats", "car", "dunn.test", "FSA",
#                   "purrr", "reshape2", "stargazer"), repos = "http://cran.r-project.org")
suppressMessages(library("argparse", quietly = TRUE))
suppressMessages(library("dplyr", quietly = TRUE))
suppressMessages(library("Hmisc", quietly = TRUE))
suppressMessages(library("ggpubr", quietly = TRUE))
#suppressMessages(library("outliers", quietly = TRUE))
suppressMessages(library("stats", quietly = TRUE))
suppressMessages(library("car", quietly = TRUE))
suppressMessages(library("dunn.test", quietly = TRUE))
suppressMessages(library("FSA", quietly = TRUE))
suppressMessages(library("purrr", quietly = TRUE))
suppressMessages(library("reshape2", quietly = TRUE))
suppressMessages(library("stargazer", quietly = TRUE))
suppressMessages(library("gdata", quietly = TRUE))


script_desc <- "Statistical analiser by Maciej Michalczyk"

#parse arguments for command
parser <- argparse::ArgumentParser(description = script_desc,
                                  formatter_class = 'argparse.RawTextHelpFormatter')
parser$add_argument('-o', dest = 'output_file', help = 
                     'Output file with report', required = FALSE)
parser$add_argument('-f', dest = 'file', help = 
                     'Source file with data', required = TRUE)
parser$add_argument('-s', dest = 'sep', help = 
                     'Data separator from csv file', required = TRUE)
args <- parser$parse_args()

#FUNCTIONS=============================================
#standard function to get mode value; thanks tutorialspoint!
getModeValue <- function(data_vector)
{
  unique_values <- unique(data_vector)
  return(unique_values[which.max(tabulate(match(data_vector, unique_values)))])
}

#imputes values by inserting mean if the value is numeric
#or by inserting mode if the value is not numeric
imputing <- function(data, missing_val)
{
  if(missing_val)
  {
    cat("There are", as.numeric(missing_values), "records with NA value.\n")
  }
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

#full summary with median, mean, min, max, var, sd, shapiro_test
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
    cat("In column", name, length(out[[name]]), "outliers with value of",  paste(out[[name]], sep = ","), "\n")
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
    doPlot <- suppressMessages(ggarrange(plotlist = applyToCol))
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
  groupedData <- data %>% group_by(!!group)
  gr_char <- as.character(unique(groupedData[[1]]))
  namesCol <- colnames(data %>% select_if(is.numeric))
  for(gr in gr_char)
  {
    group_num <- which(groupedData[[1]] == gr)
    selectedData <- as.data.frame(groupedData[group_num,namesCol])
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
    cat("Analysis time: ", paste(lubridate::now()), "\n")
    cat("=====================================================\n")
  sink()
  
  #report missing data
  sink(file = rData$args$output_file, append = TRUE)
    cat("===MISSING VALUES===\n")
    cat("There are", rData$missingValues, "records with NA value.\n")
  sink()
  
  #report summary
  sink(file = rData$args$output_file, append = TRUE)
    cat("===SUMMARY===\n")
    suppressMessages(melted <- melt(rData$summary, variable.name = "attribute"))
    melted %>% format(scientific = FALSE, digits = 2) %>% stargazer::stargazer(type = "text", summary = FALSE, rownames = FALSE)
    cat("\n")
  sink()
  
  #report outliers
  sink(file = rData$args$output_file, append = TRUE)
  cat("===OUTLIERS===\n")
  cat("===For visualisation of outliers in every group check generated boxplots (groupname_boxplot.png)===\n")
  cat(rData$outliersStr, sep = "\n")
  sink()
  
  #report distribution
  sink(file = rData$args$output_file, append = TRUE)
    cat("===DISTRIBUTION===\n")
    cat("===For visualisation of distribution check generatred density plots (groupname_density.png)===\n")
    cat(rData$dataDistribution, sep = "\n")
  sink()
  
  #report analysis
  sink(file = rData$args$output_file, append = TRUE)
  cat("===STATISTICAL ANALYSIS===\n")
  if(length(unique(rData$correctedData[[1]])) < 2)
  {
    sink(file = rData$args$output_file, append = TRUE)
    cat('[WARNING] Loaded data contains only 1 group. Statistical comparsions are not possible - skipping group analysis and correlation analysis.\n')
    sink()
  }else if(length(unique(rData$correctedData[[1]])) == 2)
  {
    sink(file = rData$args$output_file, append = TRUE)
    cat(rData$analysisInterpretation, sep = "\n") 
    sink()
    
    sink(file = rData$args$output_file, append = TRUE)
    cat(rData$nonNumericAnalysisInterpretation, sep = '\n')
    cat("\n")
    sink()
    
    #report correlation
    sink(file = rData$args$output_file, append = TRUE)
    cat("===CORRELATION ANALYSIS===\n")
    cat(rData$correlationAnalysisInterpretation, sep = "\n")
    cat("\n")
    sink()
  }else
  {
    sink(file = rData$args$output_file, append = TRUE)
    cat(rData$analysisInterpretation, sep = "\n")
    sink()
    
    sink(file = rData$args$output_file, append = TRUE)
    cat(rData$nonNumericAnalysisInterpretation, sep = '\n')
    cat("\n")
    sink()
    
    #report correlation
    sink(file = rData$args$output_file, append = TRUE)
    cat("===CORRELATION ANALYSIS===\n")
    cat(rData$correlationAnalysisInterpretation, sep = "\n")
    cat("\n")
    sink()
  }

  cat("END OF REPORT=====================================================\n")
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
  #checks if there are only TRUE values in the list and returns T or F
  return(all(logical))
}

isEqualDistributionAttribute <- function(attr1, attr2, group, summaryData)
{
  gr_num <- which(summaryData[[1]] == group)
  return(as.numeric(summaryData[gr_num, attr1]) > 0.05 &
           as.numeric(summaryData[gr_num, attr2]) > 0.05)
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
    else
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
        cat("\t>Post hoc test:\n\t")
        postTukey_text <- TukeyHSD(aov(data[[attribute]] ~ data[[1]]))[1] %>%
        stargazer::stargazer(., type = "text", summary = FALSE)
        #cat("\t",postTukey_text,"\n")
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
        cat("\t>Post hoc test:\n\t")
        postDunn_text <- dunn.test(as.numeric(data[[attribute]]), data[[1]]) %>% as.data.frame %>%
        stargazer::stargazer(., type = "text", summary = FALSE)
        #cat("\t", postDunn_text,"\n")
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
        cat("\t>Post hoc test:\n\t")
        postDunn_text <- dunn.test(as.numeric(data[[attribute]]), data[[1]]) %>% as.data.frame %>%
        stargazer::stargazer(., type = "text", summary = FALSE)
        #cat("\t", postDunn_text,"\n")
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


typeOfCorrelation <- function(test_result)
{
  if(test_result > -1 & test_result < -0.7)
  {
    paste("are very strongly, negatively correlated")
  }
  else if(test_result >= -0.7 & test_result < -0.5)
  {
    paste("are strongly, negatively correlated")
  }
  else if(test_result >= -0.5 & test_result < -0.3)
  {
    paste("are moderately, negatively correlated")
  }
  else if(test_result >= -0.3 & test_result < -0.2)
  {
    paste("are weakly, negatively correlated")
  }
  else if(test_result >= 0.2 & test_result < 0.3)
  {
    paste("are weakly, positively correlated")
  }
  else if(test_result >= 0.3 & test_result < 0.5)
  {
    paste("are moderately, positively correlated")
  }
  else if(test_result >= 0.5 & test_result < 0.7)
  {
    paste("are strongly, positively correlated")
  }
  else if(test_result >= 0.7 & test_result < 1)
  {
    paste("are very strongly, positively correlated")
  }
}

analiseCorrelation <- function(data, summaryData)
{
  attributes <- colnames(data %>% select_if(is.numeric))
  groups <- unique(data[[1]])
  #set to false returns list
  attr_tuples <- combn(attributes, 2, simplify = FALSE)
  grCol <- colnames(data)[[1]]
  
  for(group in groups)
  {
    gr_num <- which(data[[1]] == group)
    selectedData <- data[gr_num,] %>% select_if(is.numeric)
    cat("For group", group, '==============================\n')
    #list of plots to put into single file
    results <- list()
    
    for(attribute in attr_tuples)
    {
      pair_name <- sprintf('%s-%s', attribute[[1]], attribute[[2]])
      attribute_name1 <- paste(attribute[[1]], '_p_value', sep = '')
      attribute_name2 <- paste(attribute[[2]], '_p_value', sep = '')
      distribution_equality <- isEqualDistributionAttribute(attribute_name1,
                                                            attribute_name2, group, summaryData)
      variance_equality <- leveneTest(data[[attribute[[1]]]] ~ data[[1]])$`Pr(>F)`[1] > 0.05 &
        leveneTest(data[[attribute[[2]]]] ~ data[[1]])$`Pr(>F)`[1] > 0.05
      if(variance_equality & distribution_equality)
      {
        test <- cor.test(selectedData[[attribute[[1]]]], selectedData[[attribute[[2]]]], method = "pearson")
        if(test$p.value < 0.05)
        {
          cat("\t[Pearson test] Attribute", as.character(attribute[[1]]), "and", as.character(attribute[[2]]),
          as.character(typeOfCorrelation(test$estimate)), "with correlation value of", test$estimate, '\n')
        }
        else
        {
          cat("\t[Pearson test] Attribute", as.character(attribute[[1]]), "and", as.character(attribute[[2]]), "are probably NOT correlated\n")
        }
        plot <- ggscatter(selectedData,
                          x = attribute[[1]],
                          y = attribute[[2]],
                          add = "reg.line",
                          conf.int = TRUE,
                          cor.coef = TRUE,
                          cor.method = "prearson",
                          color = "black",
                          fill = "lightgray",
                          palette = "green",
                          ylab = attribute[[2]],
                          xlab = attribute[[1]])
        results[[pair_name]] <- plot 
      }
      else
      {
        test <- cor.test(selectedData[[attribute[[1]]]], selectedData[[attribute[[2]]]], method = "spearman")
        if(test$p.value < 0.05)
        {
          cat("\t[Spearman test] Attribute", as.character(attribute[[1]]), "and", as.character(attribute[[2]]),
          as.character(typeOfCorrelation(test$estimate)), "with correlation value of", test$estimate, '\n')
        }
        else
        {
          cat("\t[Spearman test] Attribute", as.character(attribute[[1]]), "and", as.character(attribute[[2]]), "are probably NOT correlated\n")
        }
        plot <- ggscatter(selectedData,
                          x = attribute[[1]],
                          y = attribute[[2]],
                          add = "reg.line",
                          conf.int = TRUE,
                          cor.coef = TRUE,
                          cor.method = "spearman",
                          color = "black",
                          fill = "lightgray",
                          palette = "green",
                          ylab = attribute[[2]],
                          xlab = attribute[[1]])
        results[[pair_name]] <- plot
      }
    }
    final_plot <- suppressMessages(ggarrange(plotlist = results))
    png(filename = paste(group, 'correlation.png', sep = '_'), width = 3840, height = 2160, bg = "transparent")
    print(final_plot)
    dev.off()
  }
}
#======================================================

#initialize report
dataReport <- list()

#load csv
loadedData <- read.csv2(file = args$file, sep = args$sep) %>% as.data.frame %>% mutate_all(na_if,"")

#detect and impute missing values
cat("===Missing values===\n")
missing_values <- sum(is.na(loadedData))
dataReport$missingValues <- missing_values

loadedData <- imputing(loadedData, missing_values)
cat("Data corrected with average and mode values.\n")
dataReport$correctedData <- loadedData

#remove redundant levels
loadedData <- droplevels(loadedData)

#print report
#group <- colnames(loadedData)[1]
dataReport$summary <- allDataSummary(data=loadedData)
cat("===Numeric data summary===\n")
suppressMessages(melt(dataReport$summary, variable.name = "attribute", scientific = FALSE)) %>% format(., scientific = FALSE, digits = 2) %>%
  as.data.frame(.) %>% stargazer::stargazer(., summary = FALSE, type = "text", rownames = FALSE)


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

#plot density
cat("===Plotting density===\n")
density_group_plot(loadedData)

#plot boxplots
cat("===Plotting boxplots===\n")
box_group_plot(loadedData)

#numeric statistical analysis
if(length(unique(loadedData[[1]])) < 2)
{
  cat('Loaded data contains only 1 group. Statistical comparsions are not possible - skipping group analysis and correlation analysis.\n')
}else if(length(unique(loadedData[[1]])) == 2)
{
  cat("===Pairwise group analysis===\n")
  dataReport$analysis <- analiseTwoGroups(loadedData, dataReport$summary)
  dataReport$analysisInterpretation <- capture.output(analiseTwoGroups(loadedData, dataReport$summary)) 
  
  #non-numeric analysis
  cat("===Non-numeric group analysis===\n")
  dataReport$nonNumericAnalysisInterpretation <- capture.output(analiseNonNumeric(loadedData))
  cat(dataReport$nonNumericAnalysisInterpretation, sep = '\n')
  
  #correlation analysis
  cat("===Correlation analysis===\nPlotting might take a while with significant amount of attributes.\n")
  dataReport$correlationAnalysisInterpretation <- capture.output(analiseCorrelation(loadedData, dataReport$summary))
  cat(dataReport$correlationAnalysisInterpretation, sep = "\n")
}else
{
  cat("===Multiple group analysis===\n")
  dataReport$analysis <- analiseMultipleGroups(loadedData, dataReport$summary)
  dataReport$analysisInterpretation <- capture.output(analiseMultipleGroups(loadedData, dataReport$summary))
  
  #non-numeric analysis
  cat("===Non-numeric group analysis===\n")
  dataReport$nonNumericAnalysisInterpretation <- capture.output(analiseNonNumeric(loadedData))
  cat(dataReport$nonNumericAnalysisInterpretation, sep = '\n')
  
  #correlation analysis
  cat("===Correlation analysis===\nPlotting might take a while with significant amount of attributes.\n")
  dataReport$correlationAnalysisInterpretation <- capture.output(analiseCorrelation(loadedData, dataReport$summary))
  cat(dataReport$correlationAnalysisInterpretation, sep = "\n")
}

#save report to output file
if(!is.null(args$output_file))
{
  dataReport$args <- args
  createReport(dataReport)
  closeAllConnections()
  #end this please
  cat("[SUCCESS] Report saved to", dataReport$args$output_file, "\n")
}

