Assignment 4 - Heart rate, respiration and interpersonal coordination
=====================================================================

Physiological data (here heart rate \[variability\], and respiration)
are increasingly popular. Historically treated as pernicious noise to be
regressed out of neuro-imaging data, there is now increasing research on
how these signals tell us something important about cognition and beyond
being just a signal of cognitive processes also impact them in
interesting ways. Advanced sport science, and the quantified self
movement (closely followed by marketing and communication) have hailed
continuous physiological tracking as a powerful way to access and modify
attitudes, habits, and performance. Further, as team coordination (in
the military, in decision processes and organizational contexts) is more
and more in focus, research has attempted to measure how interpersonal
coordination between physiological systems might tell us something
important about e.g. emotional and cognitive coordination. See
references in the reading list for more on this.

In this assignment, you will learn to: - collect physiological data -
pre-process physiological data (and grow further your mad R skills) -
model the continuous interdependence between two signals (using a
multilevel model as proxy for a dynamical system approach) -
conservatively assess the presence of coordination between to signals in
a controlled context

This assignment has two parts. The first part familiarizes you with
heart rate, and respiration data and their preprocessing. The second
part explores how to analyze interpersonal coordination of these
signals.

These are the questions you need to be able to answer at the end of the
assignment (aka that you need to submit as part of the portfolio)

1.  How do you preprocess heart rate and respiration data? Describe the
    process. If any data needs to be excluded, list the excluded data
    and motivate the exclusion.

2.  Do you observe interpersonal coordination in heart rate and
    respiration? Describe your control baseline, the method used to
    quantify coordination, and the statistical models used to infer
    whether coordination was higher than in the baseline. Report the
    results of the models.

3.  Do you observe differences in coordination between conditions?
    Report the models and results.

4.  Is respiration coordination a likely driver of heart rate
    coordination? Describe how you would test for it. Bonus points if
    you actually run the tests and report methods and results.

N.B. to give you a bit more data I included data from previous years
(Study1, Study2 and Study 3). Note that synchronouns and turn-taking are
the same across both studies, but the third condition is different: in
the first year it was self-paced joint reading; in the second year it
was the tv-series conversation.

Let’s get started
-----------------

### Exploring physiological signals

-   Choose one pair (one pair, three conditions)
-   Load the logs
-   Produce a plot of the participants’ respiration signal and a
    different one of the participants’ HR signal. N.B: remember the
    slides: artifacts, downsampling, scaling. N.B. The
    gridExtra::grid.arrange() function allows you to display the plots
    side by side. E.g. grid.arrange(plot1, plot2, plot3, ncol=3). There
    are also smarter packages, like cowplot and ggpubr.
-   Can you eye-ball which condition if any displays more physiological
    coordination?

### First we read one data file and identify the procedure

-   Load the file
-   correctly identify all columns
-   plot the data
-   deal with the artifacts
-   downsample the dat
-   Add a column for study, group, trial and condition

<!-- -->

    # Load the libraries
    library(pacman, tidyverse)
    p_load(ggplot2, gridExtra, tidyverse, groupdata2, purr, stats, lmer4, lmerTest)

    ## Warning: package 'purr' is not available (for R version 3.6.1)

    ## Warning: 'BiocManager' not available.  Could not check Bioconductor.
    ## 
    ## Please use `install.packages('BiocManager')` and then retry.

    ## Warning in p_install(package, character.only = TRUE, ...):

    ## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
    ## logical.return = TRUE, : there is no package called 'purr'

    ## Warning: package 'lmer4' is not available (for R version 3.6.1)

    ## Warning: 'BiocManager' not available.  Could not check Bioconductor.
    ## 
    ## Please use `install.packages('BiocManager')` and then retry.

    ## Warning in p_install(package, character.only = TRUE, ...):

    ## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
    ## logical.return = TRUE, : there is no package called 'lmer4'

    ## Warning in p_load(ggplot2, gridExtra, tidyverse, groupdata2, purr, stats, : Failed to install/load:
    ## purr, lmer4

    #Citation for report
    citation("gridExtra")

    ## 
    ## To cite package 'gridExtra' in publications use:
    ## 
    ##   Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for
    ##   "Grid" Graphics. R package version 2.3.
    ##   https://CRAN.R-project.org/package=gridExtra
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {gridExtra: Miscellaneous Functions for "Grid" Graphics},
    ##     author = {Baptiste Auguie},
    ##     year = {2017},
    ##     note = {R package version 2.3},
    ##     url = {https://CRAN.R-project.org/package=gridExtra},
    ##   }

    citation("ggplot2")

    ## 
    ## To cite ggplot2 in publications, please use:
    ## 
    ##   H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
    ##   Springer-Verlag New York, 2016.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Book{,
    ##     author = {Hadley Wickham},
    ##     title = {ggplot2: Elegant Graphics for Data Analysis},
    ##     publisher = {Springer-Verlag New York},
    ##     year = {2016},
    ##     isbn = {978-3-319-24277-4},
    ##     url = {https://ggplot2.tidyverse.org},
    ##   }

    citation("groupdata2")

    ## 
    ## To cite package 'groupdata2' in publications use:
    ## 
    ##   Ludvig Renbo Olsen (2019). groupdata2: Creating Groups from
    ##   Data. R package version 1.1.2.
    ##   https://CRAN.R-project.org/package=groupdata2
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Manual{,
    ##     title = {groupdata2: Creating Groups from Data},
    ##     author = {Ludvig Renbo Olsen},
    ##     year = {2019},
    ##     note = {R package version 1.1.2},
    ##     url = {https://CRAN.R-project.org/package=groupdata2},
    ##   }

    citation("lmerTest")

    ## 
    ## To cite lmerTest in publications use:
    ## 
    ## Kuznetsova A, Brockhoff PB, Christensen RHB (2017). "lmerTest
    ## Package: Tests in Linear Mixed Effects Models." _Journal of
    ## Statistical Software_, *82*(13), 1-26. doi: 10.18637/jss.v082.i13
    ## (URL: https://doi.org/10.18637/jss.v082.i13).
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     title = {{lmerTest} Package: Tests in Linear Mixed Effects Models},
    ##     author = {Alexandra Kuznetsova and Per B. Brockhoff and Rune H. B. Christensen},
    ##     journal = {Journal of Statistical Software},
    ##     year = {2017},
    ##     volume = {82},
    ##     number = {13},
    ##     pages = {1--26},
    ##     doi = {10.18637/jss.v082.i13},
    ##   }

    # Load the file
    s1_t1 <- read.csv("Data/Study1_G1_T1_Synchronous.csv")

    # Plot
    p1_resp <- ggplot(data = s1_t1) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")
    p1_resp

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    p1_hr <- ggplot(data = s1_t1) +
      geom_path(aes(time, HR1, color = "P1")) +
      geom_path(aes(time, HR2, color = "P2")) +
      labs(x = "time", y = "HR") +
      theme(legend.position="bottom")
    p1_hr

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    ## Remove outliers

    ### Tip, check the function below
    removeOuts <- function(ts,threshold){
      ts[ts > (mean(ts,na.rm=T) +
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T)+threshold*sd(ts,na.rm=T)
      ts[ts < (mean(ts,na.rm=T) -
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T)-threshold*sd(ts,na.rm=T)
      return(ts)
    }
    threshold=3 # Default value at 3 sds from the mean
    #Removing outliers for respiration
    s1_t1$Resp1_cleaned <- removeOuts(s1_t1$Resp1, threshold)
    s1_t1$Resp2_cleaned <- removeOuts(s1_t1$Resp2, threshold)
    #Removing outliers for HR
    s1_t1$HR1_cleaned <- removeOuts(s1_t1$HR1, threshold)
    s1_t1$HR2_cleaned <- removeOuts(s1_t1$HR2, threshold)

    #Plotting for cleaned data respiration
    p1_resp_cleaned <- ggplot(data = s1_t1) +
      geom_path(aes(time, Resp1_cleaned, color = "P1")) +
      geom_path(aes(time, Resp2_cleaned, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")
    p1_resp_cleaned

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-3.png)

    #Plotting for cleaned data heart rate
    p1_hr_cleaned <- ggplot(data = s1_t1) +
      geom_path(aes(time, HR1_cleaned, color = "P1")) +
      geom_path(aes(time, HR2_cleaned, color = "P2")) +
      labs(x = "time", y = "HR") +
      theme(legend.position="bottom")
    p1_hr_cleaned

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-4.png)

    # Plot raw data againt those with the artiacts removed
    s1_t1_plots <- gridExtra::grid.arrange(p1_resp, p1_resp_cleaned,p1_hr,p1_hr_cleaned, nrow = 2, ncol = 2)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-5.png)

    ## Scale
    ### Tip: if scale() gives some issues, try the one below
    z_scale <- function(column){
      column_c <- (column - mean(column)) / sd(column)
    }
    s1_t1_scaled <-s1_t1 %>% dplyr::mutate_at(c("Resp1_cleaned", "Resp2_cleaned", "HR1_cleaned", "HR2_cleaned"),z_scale)

    # Plot again to check how scaled data look like
    #Plotting for scaled data respiration
    p1_resp_scaled <- ggplot(data = s1_t1_scaled) +
      geom_path(aes(time, Resp1_cleaned, color = "P1")) +
      geom_path(aes(time, Resp2_cleaned, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")
    p1_resp_scaled

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-6.png)

    #Plotting for scaled data heart rate
    p1_hr_scaled <- ggplot(data = s1_t1_scaled) +
      geom_path(aes(time, HR1_cleaned, color = "P1")) +
      geom_path(aes(time, HR2_cleaned, color = "P2")) +
      labs(x = "time", y = "HR") +
      theme(legend.position="bottom")
    p1_hr_scaled

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-7.png)

    s1_t1_plots_scaled <- gridExtra::grid.arrange(p1_resp_cleaned,p1_resp_scaled, p1_hr_cleaned, p1_hr_scaled, nrow = 2, ncol = 2)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-8.png)

    ## Downsample
    ### This is tricky, so you can have a look at my code  (relying on Ludvig's groupdata2) if you get stuck
    s1_t1_scaled$row <- seq.int(nrow(s1_t1_scaled))
    s1_t1_down = s1_t1_scaled %>%
      group(n = 100, method = 'greedy') %>%
      dplyr::summarise(
        time = mean(time,na.rm=T),
        HR1 = mean(HR1_cleaned,na.rm=T),
        HR2 = mean(HR2_cleaned,na.rm=T),
        Resp1 = mean(Resp1_cleaned,na.rm=T),
        Resp2 = mean(Resp2_cleaned,na.rm=T),
        rowname = row[1]) #the index we use to put them back together 

    ## Plot the downsampled data
    p4 <- ggplot(data = s1_t1_down) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")
    p4

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-1-9.png)

    ## Now add the group, trial, condition to the cleaned up, scaled, downsampled data
    match <- str_match('Study1_G1_T1_Synchronous.csv', "Study([\\d]+)_G([\\d]+)_T([\\d]+)_([:alpha:]+)")
    x <- c("file", "study", "group", "trial", "condition")
    colnames(match) <- x
    # Binding downsampled dataframe with info extracted from the filename
    s1_t1_down <- cbind(s1_t1_down, match)

Now we are ready to go to load and pre-process all files
--------------------------------------------------------

Go through all the files (with a function passed onto map\_df), check
which files should be excluded, if any, and save the pre-processed
time-series

A couple of tips: - looping is oh so slow. Making a function and using
Map/Map\_df is your salvation. - each study restarts the group
numbering, so you should make sure to change that (e.g. 100 \* Study +
Group) - you need to make sure all the data are meaningful or something
has to be removed. Plotting is your friend. E.g.
“Study1\_G1\_T1\_Synchronous” has one bad respiration signal. We could
replace it with NAs

    ## Remove outliers
    removeOuts <- function(ts,threshold){
      ts[ts > (mean(ts,na.rm=T) +
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T)+threshold*sd(ts,na.rm=T)
      ts[ts < (mean(ts,na.rm=T) -
                 (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T)-threshold*sd(ts,na.rm=T)
      return(ts)
    }

    ## Scale
    z_scale <- function(column){
      column_c <- (column - mean(column)) / sd(column)
    }

    # Define a function running the loading, artifact removal, scaling, downsampling, info adding.

    data_preprocess <- function(filename, threshold = 3){
      
      # To be filled in
      # load data
      file <- read.csv(filename)
      # change colnames
      colnames(file)[which(names(file) == "min")] <- "time"
      # list of HR and Respiration
      resp_hr <- c("Resp1", "Resp2", "HR1", "HR2")
      # change 
      file <- file %>% mutate_at(resp_hr, as.numeric)
      # remove outliers
      file$Resp1 <- removeOuts(file$Resp1, threshold)
      file$Resp2 <- removeOuts(file$Resp2, threshold)
      file$HR1 <- removeOuts(file$HR1, threshold)
      file$HR2 <- removeOuts(file$HR2, threshold)
      # scale
      file <- file %>% dplyr::mutate_at(resp_hr,z_scale)
      # downsample
      file$row <- seq.int(nrow(file))
      file <- file %>%
      group(n = 1000, method = 'greedy') %>%
      dplyr::summarise(
        time = mean(time,na.rm=T),
        HR1 = mean(HR1,na.rm=T),
        HR2 = mean(HR2,na.rm=T),
        Resp1 = mean(Resp1,na.rm=T),
        Resp2 = mean(Resp2,na.rm=T),
        rowname = row[1])
      # parse filename to extract study, diagnosis, subject and trial
      match <- str_match(filename, "Study([\\d]+)_G([\\d]+)_T([\\d]+)_([:alpha:]+)")
      x <- c("file", "study", "group", "trial", "condition")
      colnames(match) <- x
      # bind the matched and the downsampled data
      data <- cbind(match,file)
      # identifying and removing trials below a certain treshold
      dup_detector1 <- ifelse(duplicated(data$Resp1)==T,"duplicate","not-duplicate")
      dup_detector2 <- ifelse(duplicated(data$Resp2)==T,"duplicate","not-duplicate")
       if(sum(dup_detector1=="duplicate") >= 8 | sum(dup_detector2=="duplicate") >= 8) {
        file$Resp1 <- NA
        file$Resp2 <- NA
       } 
         
      return(data)

    }

    # test it on just one file while writing the function
    test_data <- data_preprocess("Data/Study1_G2_T1_TurnTaking.csv")

    #  Identify all files to be read
    # Run the function on the whole dataset using map_df
    phys_data <- list.files(path = "/Users/matilde/Desktop/AU/Experimental Methods III/Assignment 4 - heartrate_respiration/assignment-4/data",pattern = ".csv", full.names = T) %>% ## NB replace with your path to the files
        purrr::map_df(data_preprocess)

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

After preprocess we need to assess the data and change some variables

    # Making a unique pair ID
    phys_data$study <- as.numeric(phys_data$study)
    phys_data$group <- as.numeric(phys_data$group)

    phys_data$uPairID <- (100 * phys_data$study + phys_data$group)

    #Normalizing the time values
    #Assuming that the large values are millisecond
    #choosing 400 arbitrarily because it is above a reasonable minute count.
    phys_data[which(phys_data$time > 400),]$time <- phys_data[which(phys_data$time > 400),]$time / 1000 / 60
     
    #time since 0
    phys_data <- phys_data %>% group_by(uPairID, trial) %>% mutate(actual_time_min = time - min(time))

    # change timename
    colnames(phys_data)[7] <- "time_min"


    # Now we need to make sure all the data are meaningful or something has to be removed
    # E.g. "Study1_G1_T1_Synchronous" has one bad respiration signal. We could replace it with NAs

    # plots plots plots
    phys_data %>% subset(study==4) %>%
      ggplot() + 
      geom_line(aes(actual_time_min,HR1),color="red") + 
      geom_line(aes(actual_time_min,HR2),color="blue") + 
      facet_grid(group ~ trial)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    phys_data %>% subset(study==4) %>%
      ggplot() + 
      geom_line(aes(actual_time_min,Resp1),color="red") + 
      geom_line(aes(actual_time_min,Resp2),color="blue") + 
      facet_grid(group ~ trial)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    # Romove bad heart rate data
    # Removing after 3 min.
    phys_data$HR1 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$HR1)
    phys_data$HR2 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$HR2)
    phys_data$Resp1 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$Resp1)
    phys_data$Resp2 <- ifelse(phys_data$actual_time_min > 3 & phys_data$study == 3, NA, phys_data$Resp2)

    #Removing single bad trials
    #Study 2: G5T1-3, G6T1-3, G8T1-3, G9T1
    #Study 3: G9T1-3, G1T3        
    #Study 4: G3T1-3, G5T1-3, G6T1-3, G7T1-3, G8T
    bad_files <- c("Study2_G5_T1_Synchronous","Study2_G5_T2_TurnTaking","Study2_G5_T3_Conversation","Study2_G6_T1_TurnTaking","Study2_G6_T2_Conversation","Study2_G6_T3_Synchronous","Study2_G8_T1_TurnTaking","Study2_G8_T2_Synchronous","Study2_G8_T3_Conversation","Study2_G9_T1_Synchronous","Study3_G9_T1_Conversation","Study3_G9_T2_Synchronous","Study3_G9_T3_TurnTaking","Study3_G1_T3_Conversation","Study4_G3_T1_MovementGuided","Study4_G3_T2_MovementCoop","Study4_G3_T3_Synchronous","Study4_G5_T1_Synchronous","Study4_G5_T2_TurnTaking","Study4_G5_T3_Conversation","Study4_G6_T3_Conversation","Study4_G6_T1_TurnTaking","Study4_G6_T2_Synchronous","Study4_G7_T1_MovementGuided","Study4_G7_T2_MovementCoop","Study4_G7_T3_Synchronous","Study4_G8_T4_MovementCoop")

    phys_data$HR1 <- ifelse(phys_data$file %in% bad_files , NA, phys_data$HR1)
    phys_data$HR2 <- ifelse(phys_data$file %in% bad_files , NA, phys_data$HR2)



    # Save the data
    write.csv(phys_data, file = "physiological_data.csv")
    phys_data <- read.csv("physiological_data.csv")    

This is an extra chunk we have put in to show how we decided to set the
threshhold for the function removing replications in the respiration
data. Note that it is downsampled by 1000

    # Argumentation for why we use 8 as threashold
    overview <- phys_data %>% 
      group_by(file) %>% 
      summarise(
        sum(duplicated(Resp1)),
        sum(duplicated(Resp2)))

    #We plot some of the files from the overview table to assess if it is a resonable threshhold to put

    bad1 <- data_preprocess("Data/Study2_G7_T3_TurnTaking.csv")
    bad2 <- data_preprocess("Data/Study1_G1_T2_TurnTaking.csv")
    bad3 <- data_preprocess("Data/Study1_G1_T1_Synchronous.csv")
    bad4 <- data_preprocess("Data/Study3_G9_T2_Synchronous.csv")
    bad5 <- data_preprocess("Data/Study4_G4_T5_TurnTaking.csv")
    bad6 <- data_preprocess("Data/Study4_G1_T1_Synchronous.csv")
    bad7 <- data_preprocess("Data/Study4_G4_T1_MovementGuided.csv")
    good <- data_preprocess("Data/Study3_G2_T2_Synchronous.csv")
    # Plot
    p1 <- ggplot(data = bad1) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p2 <- ggplot(data = bad2) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p3 <- ggplot(data = bad3) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p4 <- ggplot(data = bad4) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p5 <- ggplot(data = bad5) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p6 <- ggplot(data = bad6) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p7 <- ggplot(data = bad7) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    p_good <- ggplot(data = good) +
      geom_path(aes(time, Resp1, color = "P1")) +
      geom_path(aes(time, Resp2, color = "P2")) +
      labs(x = "time", y = "Resp") +
      theme(legend.position="bottom")

    plot_collection <- gridExtra::grid.arrange(p2, p3, p4, p5, p6, p7)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    gridExtra::grid.arrange(p2,p_good)

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-4-2.png)

    grid.arrange(arrangeGrob(p2, top="Bad plot"), arrangeGrob(p_good, top="Good plot"))

![](A4_P1_PhysiologicalCoordination_instructions_files/figure-markdown_strict/unnamed-chunk-4-3.png)

Now we need to run some analysis
--------------------------------

Let’s start with a multilevel model that accounts for - stability (how
each signal is autocorrelated) - interpersonal dependence (each signal
is dependent from the previous state of the other signal)

The data needs to be further prepared, so we can analyze both
participants in the same model. We need to turn the data into a long
format: - a column indicating own hr and one own respiration - a column
indicating other hr and one other respiration - a column indicating
change in hr from previous round and one in respiration

    #lme4 can do a trick to invert betas by adding - 1  
    # Genearate a column for each: previous HR1, HR2, Resp1, Resp2
    phys_data$study <- as.numeric(phys_data$study)
    phys_data$group <- as.numeric(phys_data$group)
    phys_data$trial <- as.numeric(phys_data$trial)

    phys_data <- phys_data %>% 
      group_by(group, study, trial) %>%
      mutate(
        HR1_future = lead(HR1, 1),
        HR2_future = lead(HR2, 1),
        Resp1_future = lead(Resp1, 1),
        Resp2_future = lead(Resp2, 1))

    # Genearate a column for each: change in HR1, HR2, Resp1, Resp2
    ## create variable for future HR1 and HR2 (HR1_future, HR2_future), using lag(x-1), group by study/trial/condition, use mutate to put it in the same function

    phys_data <- phys_data %>% 
      mutate(
        HR1_change = (HR1_future - HR1),
        HR2_change = (HR2_future - HR2),
        Resp1_change = (Resp1_future - Resp1),
        Resp2_change = (Resp2_future - Resp1)
      )

    # Make the data long, so we can analyze both participants at the same time 
    ## N.B. This is a bit tricky and you might have to do it in several steps
    d_hr_future <- 
      gather(phys_data, # data        
             participant, HR_future, # new vars
             HR1_future, HR2_future) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_future, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_resp_future <- 
      gather(phys_data, # data        
             participant, Resp_future, # new vars
             Resp1_future, Resp2_future) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_future, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_hr_change_self <- 
      gather(phys_data, # data        
             participant, HR_change_self, # new vars
             HR1_change, HR2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_change_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_resp_change_self <- 
      gather(phys_data, # data        
             participant, Resp_change_self, # new vars
             Resp1_change, Resp2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_change_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_hr_self <- 
      gather(phys_data, # data        
             participant, HR_self, # new vars
             HR1, HR2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_resp_self <- 
      gather(phys_data, # data        
             participant, Resp_self, # new vars
             Resp1, Resp2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    phys_data_other <- phys_data %>% 
      rename(
        HR1 = "HR2",
        HR2 = "HR1",
        Resp1 = "Resp2",
        Resp2 = "Resp1",
        HR1_change = "HR2_change",
        HR2_change = "HR1_change",
        Resp1_change = "Resp2_change",
        Resp2_change = "Resp1_change"
      )

    d_hr_change_other <- 
      gather(phys_data_other, # data        
             participant, HR_change_other, # new vars
             HR1_change, HR2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_change_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_resp_change_other <- 
      gather(phys_data_other, # data        
             participant, Resp_change_other, # new vars
             Resp1_change, Resp2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_change_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_hr_other <- 
      gather(phys_data_other, # data        
             participant, HR_other, # new vars
             HR1, HR2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    d_resp_other <- 
      gather(phys_data_other, # data        
             participant, Resp_other, # new vars
             Resp1, Resp2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    #Binding together all the dataframes created above
    dd <- cbind(d_hr_change_self,d_hr_change_other,d_resp_change_self,d_resp_change_other,d_hr_future,d_resp_future,d_hr_self,d_hr_other,d_resp_self,d_resp_other)
    #Selecting only the relevant columns
    dd <- dd %>%  select(actual_time_min,participant,group,condition,trial,study,HR_self,Resp_self,HR_other,Resp_other,HR_future,Resp_future,HR_change_self,HR_change_other,Resp_change_self,Resp_change_other)
    #Making participant unique for study
    dd$participant <- (dd$study * 1000)+dd$participant

We can then run an analysis where change is a function of one’s previous
state (stability, see slides), and the other’s previous state
(coupling). Make sure to: - set up the most interesting contrasts: how
do these parameters vary by condition? which condition should be
baseline? - set up the right random effects. - N.B. the model will be
slow. Make sure it works on a subset of the data first!

Bonus question: what if we include an additional layer? Is my heart rate
just adjusting to yours, or also to how much you are adjusting to mine?
- to start answering this we can add a column indicating the previous
change in hr in the other and one in respiration - we can then build on
the previous models by also adding the previous change in the other

    # Model change as a function of own and other previous state 
      #stability = beta HR_self
      #coupling = beta HR_other
      #baseline = surrogate pairs

    # We only want to analyse study 4 - making a subset of the data
    dd4 <- subset(dd,study == 4)

    ##Trying out a model of a single participant...because of slides.
    #Subset single participant 101
    dd_single <- subset(dd4, participant == 4101)

    #Single participant
    single_model1 <- lm(HR_change_self ~ 1 + HR_self + HR_other, data = dd_single)
    single_model1

    ## 
    ## Call:
    ## lm(formula = HR_change_self ~ 1 + HR_self + HR_other, data = dd_single)
    ## 
    ## Coefficients:
    ## (Intercept)      HR_self     HR_other  
    ##    0.006826    -0.258938     0.052084

    ##And then for a single group
    #Subset single group and run model
    dd_group1 <- subset(dd4, group == 1)
    model_group1 <- lmer(HR_change_self ~ 0 + condition + (HR_self + HR_other) : condition + 
                              (0 + condition | participant), data = dd_group1)

    ## boundary (singular) fit: see ?isSingular

    model_group1

    ## Linear mixed model fit by REML ['lmerModLmerTest']
    ## Formula: 
    ## HR_change_self ~ 0 + condition + (HR_self + HR_other):condition +  
    ##     (0 + condition | participant)
    ##    Data: dd_group1
    ## REML criterion at convergence: 2738.621
    ## Random effects:
    ##  Groups      Name                    Std.Dev.  Corr                   
    ##  participant conditionConversation   0.000e+00                        
    ##              conditionMovementCoop   1.042e-04   NaN                  
    ##              conditionMovementGuided 8.001e-05   NaN -0.35            
    ##              conditionSynchronous    7.428e-05   NaN  0.07 -0.33      
    ##              conditionTurnTaking     7.817e-05   NaN -0.78  0.21 -0.60
    ##  Residual                            6.049e-01                        
    ## Number of obs: 1464, groups:  participant, 2
    ## Fixed Effects:
    ##            conditionConversation             conditionMovementCoop  
    ##                         0.003989                          0.012757  
    ##          conditionMovementGuided              conditionSynchronous  
    ##                         0.012757                         -0.006247  
    ##              conditionTurnTaking     conditionConversation:HR_self  
    ##                        -0.010178                         -0.312400  
    ##    conditionMovementCoop:HR_self   conditionMovementGuided:HR_self  
    ##                        -0.178388                         -0.178388  
    ##     conditionSynchronous:HR_self       conditionTurnTaking:HR_self  
    ##                        -0.234550                         -0.268479  
    ##   conditionConversation:HR_other    conditionMovementCoop:HR_other  
    ##                         0.028054                          0.033436  
    ## conditionMovementGuided:HR_other     conditionSynchronous:HR_other  
    ##                         0.033436                         -0.005088  
    ##     conditionTurnTaking:HR_other  
    ##                        -0.033281  
    ## convergence code 0; 1 optimizer warnings; 0 lme4 warnings

    ##And now for the real models
    # Set the most interesting contrast e.g. by defining synchronous or conversation as the baseline
    # Releveling with conversation as baseline
    dd4$condition <- as.factor(dd4$condition)
    dd4$condition <- relevel(dd4$condition, ref = "Conversation")

    ###Model for study 4 only

    #Creating a +0 model for assessing beta values for quantification of coordination
    model_study4 <- lmer(HR_change_self ~ 0 + condition + (HR_self + HR_other) : condition + 
                              #we tell the model that we are not all the same. Accouting for independance of the                           data. Plus we are doing partial pooling.
                              (0 + condition | group) +        
                              (0 + condition | participant), data = dd4)

    ## boundary (singular) fit: see ?isSingular

    summary(model_study4)

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## HR_change_self ~ 0 + condition + (HR_self + HR_other):condition +  
    ##     (0 + condition | group) + (0 + condition | participant)
    ##    Data: dd4
    ## 
    ## REML criterion at convergence: 15004.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.6542 -0.4877  0.0601  0.5585  6.6767 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant conditionConversation   1.068e-10 1.033e-05                  
    ##              conditionMovementCoop   2.362e-10 1.537e-05 -1.00            
    ##              conditionMovementGuided 1.804e-09 4.247e-05  0.87 -0.87      
    ##              conditionSynchronous    3.912e-10 1.978e-05  0.58 -0.58  0.52
    ##              conditionTurnTaking     4.812e-10 2.194e-05  0.72 -0.72  0.85
    ##  group       conditionConversation   0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   7.609e-11 8.723e-06   NaN            
    ##              conditionMovementGuided 7.699e-10 2.775e-05   NaN -0.67      
    ##              conditionSynchronous    1.607e-10 1.268e-05   NaN -0.02 -0.28
    ##              conditionTurnTaking     8.922e-10 2.987e-05   NaN  0.14 -0.06
    ##  Residual                            4.005e-01 6.329e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##   0.04
    ##       
    ##       
    ##       
    ##       
    ##  -0.24
    ##       
    ## Number of obs: 7762, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## conditionConversation             4.674e-03  1.362e-02  7.746e+03   0.343
    ## conditionMovementCoop             9.639e-03  2.011e-02  7.740e+03   0.479
    ## conditionMovementGuided          -6.056e-04  1.722e-02  7.712e+03  -0.035
    ## conditionSynchronous              5.373e-03  1.779e-02  7.734e+03   0.302
    ## conditionTurnTaking              -5.801e-04  1.417e-02  7.723e+03  -0.041
    ## conditionConversation:HR_self    -3.210e-01  1.518e-02  7.747e+03 -21.141
    ## conditionMovementCoop:HR_self    -2.230e-01  2.245e-02  7.747e+03  -9.931
    ## conditionMovementGuided:HR_self  -2.854e-01  1.898e-02  7.747e+03 -15.041
    ## conditionSynchronous:HR_self     -2.710e-01  1.937e-02  7.747e+03 -13.991
    ## conditionTurnTaking:HR_self      -2.906e-01  1.549e-02  7.747e+03 -18.759
    ## conditionConversation:HR_other    1.241e-02  1.518e-02  7.747e+03   0.817
    ## conditionMovementCoop:HR_other    4.600e-02  2.245e-02  7.747e+03   2.049
    ## conditionMovementGuided:HR_other  3.931e-02  1.898e-02  7.747e+03   2.071
    ## conditionSynchronous:HR_other    -1.023e-02  1.937e-02  7.747e+03  -0.528
    ## conditionTurnTaking:HR_other     -5.162e-04  1.549e-02  7.747e+03  -0.033
    ##                                  Pr(>|t|)    
    ## conditionConversation              0.7314    
    ## conditionMovementCoop              0.6318    
    ## conditionMovementGuided            0.9720    
    ## conditionSynchronous               0.7626    
    ## conditionTurnTaking                0.9673    
    ## conditionConversation:HR_self      <2e-16 ***
    ## conditionMovementCoop:HR_self      <2e-16 ***
    ## conditionMovementGuided:HR_self    <2e-16 ***
    ## conditionSynchronous:HR_self       <2e-16 ***
    ## conditionTurnTaking:HR_self        <2e-16 ***
    ## conditionConversation:HR_other     0.4137    
    ## conditionMovementCoop:HR_other     0.0405 *  
    ## conditionMovementGuided:HR_other   0.0384 *  
    ## conditionSynchronous:HR_other      0.5973    
    ## conditionTurnTaking:HR_other       0.9734    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    #Creating a +1 model to assess whether coordination is higher than baseline (here being the condition conversation)
    model_study4_interaction <- lmer(HR_change_self ~ 1 + condition + (HR_self + HR_other) * condition + 
                              #we tell the model that we are not all the same. Accouting for independance of the                           data. Plus we are doing partial pooling.
                              (1 + condition | group) +        
                              (1 + condition | participant), data = dd4)

    ## boundary (singular) fit: see ?isSingular

    summary(model_study4_interaction)

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## HR_change_self ~ 1 + condition + (HR_self + HR_other) * condition +  
    ##     (1 + condition | group) + (1 + condition | participant)
    ##    Data: dd4
    ## 
    ## REML criterion at convergence: 15004.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.6542 -0.4877  0.0601  0.5585  6.6767 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant (Intercept)             0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   2.526e-12 1.589e-06   NaN            
    ##              conditionMovementGuided 2.867e-10 1.693e-05   NaN  0.49      
    ##              conditionSynchronous    1.768e-10 1.330e-05   NaN -0.09  0.45
    ##              conditionTurnTaking     7.042e-11 8.391e-06   NaN  0.26  0.05
    ##  group       (Intercept)             0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   3.654e-10 1.912e-05   NaN            
    ##              conditionMovementGuided 8.327e-11 9.125e-06   NaN -0.23      
    ##              conditionSynchronous    4.824e-10 2.196e-05   NaN  0.42 -0.52
    ##              conditionTurnTaking     2.410e-10 1.552e-05   NaN -0.32 -0.37
    ##  Residual                            4.005e-01 6.329e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##  -0.79
    ##       
    ##       
    ##       
    ##       
    ##  -0.47
    ##       
    ## Number of obs: 7762, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## (Intercept)                       4.674e-03  1.362e-02  7.747e+03   0.343
    ## conditionMovementCoop             4.965e-03  2.429e-02  7.745e+03   0.204
    ## conditionMovementGuided          -5.280e-03  2.196e-02  7.746e+03  -0.240
    ## conditionSynchronous              6.987e-04  2.240e-02  7.735e+03   0.031
    ## conditionTurnTaking              -5.254e-03  1.965e-02  7.745e+03  -0.267
    ## HR_self                          -3.210e-01  1.518e-02  7.747e+03 -21.141
    ## HR_other                          1.241e-02  1.518e-02  7.747e+03   0.817
    ## conditionMovementCoop:HR_self     9.806e-02  2.710e-02  7.747e+03   3.618
    ## conditionMovementGuided:HR_self   3.560e-02  2.430e-02  7.747e+03   1.465
    ## conditionSynchronous:HR_self      5.006e-02  2.461e-02  7.747e+03   2.034
    ## conditionTurnTaking:HR_self       3.040e-02  2.169e-02  7.747e+03   1.402
    ## conditionMovementCoop:HR_other    3.358e-02  2.710e-02  7.747e+03   1.239
    ## conditionMovementGuided:HR_other  2.689e-02  2.430e-02  7.747e+03   1.107
    ## conditionSynchronous:HR_other    -2.264e-02  2.461e-02  7.747e+03  -0.920
    ## conditionTurnTaking:HR_other     -1.293e-02  2.169e-02  7.747e+03  -0.596
    ##                                  Pr(>|t|)    
    ## (Intercept)                      0.731425    
    ## conditionMovementCoop            0.838056    
    ## conditionMovementGuided          0.809988    
    ## conditionSynchronous             0.975117    
    ## conditionTurnTaking              0.789173    
    ## HR_self                           < 2e-16 ***
    ## HR_other                         0.413703    
    ## conditionMovementCoop:HR_self    0.000299 ***
    ## conditionMovementGuided:HR_self  0.143012    
    ## conditionSynchronous:HR_self     0.041951 *  
    ## conditionTurnTaking:HR_self      0.161081    
    ## conditionMovementCoop:HR_other   0.215330    
    ## conditionMovementGuided:HR_other 0.268526    
    ## conditionSynchronous:HR_other    0.357541    
    ## conditionTurnTaking:HR_other     0.551191    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # Bonus points: Add to the previous model also change in the other to see whether my adaptation is influenced by the other's adaptation.

Now we need to create control baselines.
----------------------------------------

First shuffled controls, then surrogate pairs.

### Creating controls: shuffled controls

Shuffled controls break the temporal dependencies of time-series by
shuffling the value within one time-series. This ensures the
“coordination” observed is not due to the actual values in the series
and not their sequence. Tip: sample() is your friend, but make sure to
shuffle things within participant/condition and not throughout the whole
dataset

    ## Create a shuffled dataset
    # Shuffled dataframe
    shuffled <- dd
    # Sample the relevant variables
    shuffled$HR_other <- sample(shuffled$HR_other, replace = T)
    shuffled$Resp_other <- sample(shuffled$Resp_other, replace = T)
    # Create variable type
    dd$type <- 'real'
    shuffled$type <- 'shuffled'
    dd_shuf_type <- rbind(dd,shuffled)

    ## Create the same models as in the previous chunk, but adding an interaction by shuffled vs. real
    # We only want to analyse study 4 - making a subset of the data
    dd_shuf_4 <- subset(dd_shuf_type,study == 4)
    # Set the most interesting contrast e.g. by defining synchronous or conversation as the baseline
    dd_shuf_4$condition <- relevel(dd_shuf_4$condition, ref = "Conversation")
    # Relevel type
    dd_shuf_4$type <- as.factor(dd_shuf_4$type)
    dd_shuf_4$type <- relevel(dd_shuf_4$type, ref = "shuffled")
    # Models
    model_1_control <- lmer(HR_change_self ~ 
      1 + ((HR_self + HR_other) * condition) * type + 
      (1 + condition | participant) + 
      (1 + condition | group),
      data = dd_shuf_4,
      REML = F)

    ## boundary (singular) fit: see ?isSingular

    ## Warning: Model failed to converge with 1 negative eigenvalue: -3.8e+03

    summary(model_1_control)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: HR_change_self ~ 1 + ((HR_self + HR_other) * condition) * type +  
    ##     (1 + condition | participant) + (1 + condition | group)
    ##    Data: dd_shuf_4
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  25023.3  25479.4 -12450.6  24901.3    13000 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.7090 -0.4917  0.0577  0.5622  6.7316 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant (Intercept)             0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   1.238e-10 1.113e-05   NaN            
    ##              conditionMovementGuided 9.584e-10 3.096e-05   NaN  0.71      
    ##              conditionSynchronous    1.069e-09 3.269e-05   NaN  0.54  0.94
    ##              conditionTurnTaking     3.304e-10 1.818e-05   NaN -0.72 -0.08
    ##  group       (Intercept)             3.484e-10 1.867e-05                  
    ##              conditionMovementCoop   1.081e-14 1.040e-07  1.00            
    ##              conditionMovementGuided 1.732e-09 4.162e-05 -0.67 -0.67      
    ##              conditionSynchronous    6.504e-10 2.550e-05  0.45  0.45  0.33
    ##              conditionTurnTaking     4.541e-10 2.131e-05 -0.31 -0.31  0.14
    ##  Residual                            3.940e-01 6.277e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##   0.11
    ##       
    ##       
    ##       
    ##       
    ##  -0.27
    ##       
    ## Number of obs: 13061, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                             Estimate Std. Error         df
    ## (Intercept)                               -1.767e-02  1.631e-02  1.306e+04
    ## HR_self                                   -3.132e-01  1.825e-02  1.306e+04
    ## HR_other                                  -1.501e-03  1.786e-02  1.306e+04
    ## conditionMovementCoop                      4.454e-02  2.898e-02  1.306e+04
    ## conditionMovementGuided                    2.542e-02  2.645e-02  1.305e+04
    ## conditionSynchronous                       1.490e-02  2.683e-02  1.305e+04
    ## conditionTurnTaking                        9.950e-03  2.363e-02  1.306e+04
    ## typereal                                   2.234e-02  2.117e-02  1.306e+04
    ## HR_self:conditionMovementCoop              1.048e-01  3.159e-02  1.306e+04
    ## HR_self:conditionMovementGuided            4.880e-02  2.911e-02  1.306e+04
    ## HR_self:conditionSynchronous               1.908e-02  2.968e-02  1.306e+04
    ## HR_self:conditionTurnTaking                3.141e-02  2.615e-02  1.306e+04
    ## HR_other:conditionMovementCoop            -1.751e-02  3.187e-02  1.306e+04
    ## HR_other:conditionMovementGuided          -1.074e-02  2.893e-02  1.306e+04
    ## HR_other:conditionSynchronous              3.284e-02  2.881e-02  1.306e+04
    ## HR_other:conditionTurnTaking              -4.657e-02  2.523e-02  1.306e+04
    ## HR_self:typereal                          -7.841e-03  2.366e-02  1.306e+04
    ## HR_other:typereal                          1.391e-02  2.336e-02  1.306e+04
    ## conditionMovementCoop:typereal            -3.957e-02  3.769e-02  1.306e+04
    ## conditionMovementGuided:typereal          -3.070e-02  3.426e-02  1.306e+04
    ## conditionSynchronous:typereal             -1.420e-02  3.483e-02  1.306e+04
    ## conditionTurnTaking:typereal              -1.520e-02  3.063e-02  1.306e+04
    ## HR_self:conditionMovementCoop:typereal    -6.726e-03  4.148e-02  1.306e+04
    ## HR_self:conditionMovementGuided:typereal  -1.320e-02  3.780e-02  1.306e+04
    ## HR_self:conditionSynchronous:typereal      3.098e-02  3.843e-02  1.306e+04
    ## HR_self:conditionTurnTaking:typereal      -1.002e-03  3.386e-02  1.306e+04
    ## HR_other:conditionMovementCoop:typereal    5.109e-02  4.169e-02  1.306e+04
    ## HR_other:conditionMovementGuided:typereal  3.764e-02  3.766e-02  1.306e+04
    ## HR_other:conditionSynchronous:typereal    -5.549e-02  3.776e-02  1.306e+04
    ## HR_other:conditionTurnTaking:typereal      3.364e-02  3.315e-02  1.306e+04
    ##                                           t value Pr(>|t|)    
    ## (Intercept)                                -1.084 0.278587    
    ## HR_self                                   -17.160  < 2e-16 ***
    ## HR_other                                   -0.084 0.933013    
    ## conditionMovementCoop                       1.537 0.124314    
    ## conditionMovementGuided                     0.961 0.336587    
    ## conditionSynchronous                        0.555 0.578678    
    ## conditionTurnTaking                         0.421 0.673651    
    ## typereal                                    1.055 0.291352    
    ## HR_self:conditionMovementCoop               3.317 0.000914 ***
    ## HR_self:conditionMovementGuided             1.676 0.093717 .  
    ## HR_self:conditionSynchronous                0.643 0.520318    
    ## HR_self:conditionTurnTaking                 1.201 0.229823    
    ## HR_other:conditionMovementCoop             -0.549 0.582735    
    ## HR_other:conditionMovementGuided           -0.371 0.710361    
    ## HR_other:conditionSynchronous               1.140 0.254316    
    ## HR_other:conditionTurnTaking               -1.846 0.064909 .  
    ## HR_self:typereal                           -0.331 0.740369    
    ## HR_other:typereal                           0.596 0.551446    
    ## conditionMovementCoop:typereal             -1.050 0.293667    
    ## conditionMovementGuided:typereal           -0.896 0.370281    
    ## conditionSynchronous:typereal              -0.408 0.683535    
    ## conditionTurnTaking:typereal               -0.496 0.619596    
    ## HR_self:conditionMovementCoop:typereal     -0.162 0.871197    
    ## HR_self:conditionMovementGuided:typereal   -0.349 0.726975    
    ## HR_self:conditionSynchronous:typereal       0.806 0.420146    
    ## HR_self:conditionTurnTaking:typereal       -0.030 0.976401    
    ## HR_other:conditionMovementCoop:typereal     1.226 0.220405    
    ## HR_other:conditionMovementGuided:typereal   0.999 0.317579    
    ## HR_other:conditionSynchronous:typereal     -1.469 0.141732    
    ## HR_other:conditionTurnTaking:typereal       1.015 0.310328    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 30 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    model_0_control <- lmer(HR_change_self ~ 
      0 + ((HR_self + HR_other) : condition) : type + 
      (0 + condition | participant) +
      (0 + condition | group),
      data = dd_shuf_4,
      REML = F)

    ## boundary (singular) fit: see ?isSingular

    ## Warning: Model failed to converge with 1 negative eigenvalue: -6.1e+02

    summary(model_0_control)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: HR_change_self ~ 0 + ((HR_self + HR_other):condition):type +  
    ##     (0 + condition | participant) + (0 + condition | group)
    ##    Data: dd_shuf_4
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  25006.5  25387.9 -12452.3  24904.5    13010 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.7007 -0.4917  0.0612  0.5628  6.7298 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant conditionConversation   2.328e-10 1.526e-05                  
    ##              conditionMovementCoop   4.141e-10 2.035e-05  1.00            
    ##              conditionMovementGuided 7.977e-10 2.824e-05  0.13  0.13      
    ##              conditionSynchronous    1.464e-10 1.210e-05  0.78  0.78 -0.34
    ##              conditionTurnTaking     5.862e-10 2.421e-05  0.38  0.38 -0.07
    ##  group       conditionConversation   0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   2.469e-09 4.969e-05   NaN            
    ##              conditionMovementGuided 5.443e-10 2.333e-05   NaN  0.96      
    ##              conditionSynchronous    4.284e-10 2.070e-05   NaN  0.41  0.63
    ##              conditionTurnTaking     8.827e-12 2.971e-06   NaN -0.36 -0.60
    ##  Residual                            3.941e-01 6.278e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##   0.41
    ##       
    ##       
    ##       
    ##       
    ##  -0.92
    ##       
    ## Number of obs: 13061, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                                 Estimate Std. Error
    ## HR_self:conditionConversation:typeshuffled    -3.128e-01  1.825e-02
    ## HR_self:conditionMovementCoop:typeshuffled    -2.084e-01  2.579e-02
    ## HR_self:conditionMovementGuided:typeshuffled  -2.646e-01  2.267e-02
    ## HR_self:conditionSynchronous:typeshuffled     -2.941e-01  2.341e-02
    ## HR_self:conditionTurnTaking:typeshuffled      -2.819e-01  1.873e-02
    ## HR_self:conditionConversation:typereal        -3.210e-01  1.506e-02
    ## HR_self:conditionMovementCoop:typereal        -2.230e-01  2.227e-02
    ## HR_self:conditionMovementGuided:typereal      -2.854e-01  1.882e-02
    ## HR_self:conditionSynchronous:typereal         -2.710e-01  1.921e-02
    ## HR_self:conditionTurnTaking:typereal          -2.906e-01  1.537e-02
    ## HR_other:conditionConversation:typeshuffled   -1.273e-03  1.786e-02
    ## HR_other:conditionMovementCoop:typeshuffled   -1.972e-02  2.639e-02
    ## HR_other:conditionMovementGuided:typeshuffled -1.253e-02  2.275e-02
    ## HR_other:conditionSynchronous:typeshuffled     3.133e-02  2.261e-02
    ## HR_other:conditionTurnTaking:typeshuffled     -4.841e-02  1.780e-02
    ## HR_other:conditionConversation:typereal        1.241e-02  1.506e-02
    ## HR_other:conditionMovementCoop:typereal        4.599e-02  2.227e-02
    ## HR_other:conditionMovementGuided:typereal      3.931e-02  1.882e-02
    ## HR_other:conditionSynchronous:typereal        -1.024e-02  1.921e-02
    ## HR_other:conditionTurnTaking:typereal         -5.166e-04  1.537e-02
    ##                                                       df t value Pr(>|t|)
    ## HR_self:conditionConversation:typeshuffled     1.306e+04 -17.139  < 2e-16
    ## HR_self:conditionMovementCoop:typeshuffled     1.306e+04  -8.079 7.08e-16
    ## HR_self:conditionMovementGuided:typeshuffled   1.306e+04 -11.673  < 2e-16
    ## HR_self:conditionSynchronous:typeshuffled      1.306e+04 -12.564  < 2e-16
    ## HR_self:conditionTurnTaking:typeshuffled       1.306e+04 -15.053  < 2e-16
    ## HR_self:conditionConversation:typereal         1.306e+04 -21.312  < 2e-16
    ## HR_self:conditionMovementCoop:typereal         1.306e+04 -10.012  < 2e-16
    ## HR_self:conditionMovementGuided:typereal       1.306e+04 -15.163  < 2e-16
    ## HR_self:conditionSynchronous:typereal          1.306e+04 -14.105  < 2e-16
    ## HR_self:conditionTurnTaking:typereal           1.306e+04 -18.912  < 2e-16
    ## HR_other:conditionConversation:typeshuffled    1.306e+04  -0.071  0.94316
    ## HR_other:conditionMovementCoop:typeshuffled    1.306e+04  -0.747  0.45492
    ## HR_other:conditionMovementGuided:typeshuffled  1.306e+04  -0.551  0.58195
    ## HR_other:conditionSynchronous:typeshuffled     1.306e+04   1.386  0.16591
    ## HR_other:conditionTurnTaking:typeshuffled      1.306e+04  -2.719  0.00655
    ## HR_other:conditionConversation:typereal        1.306e+04   0.824  0.40995
    ## HR_other:conditionMovementCoop:typereal        1.306e+04   2.065  0.03891
    ## HR_other:conditionMovementGuided:typereal      1.306e+04   2.088  0.03681
    ## HR_other:conditionSynchronous:typereal         1.306e+04  -0.533  0.59408
    ## HR_other:conditionTurnTaking:typereal          1.306e+04  -0.034  0.97318
    ##                                                  
    ## HR_self:conditionConversation:typeshuffled    ***
    ## HR_self:conditionMovementCoop:typeshuffled    ***
    ## HR_self:conditionMovementGuided:typeshuffled  ***
    ## HR_self:conditionSynchronous:typeshuffled     ***
    ## HR_self:conditionTurnTaking:typeshuffled      ***
    ## HR_self:conditionConversation:typereal        ***
    ## HR_self:conditionMovementCoop:typereal        ***
    ## HR_self:conditionMovementGuided:typereal      ***
    ## HR_self:conditionSynchronous:typereal         ***
    ## HR_self:conditionTurnTaking:typereal          ***
    ## HR_other:conditionConversation:typeshuffled      
    ## HR_other:conditionMovementCoop:typeshuffled      
    ## HR_other:conditionMovementGuided:typeshuffled    
    ## HR_other:conditionSynchronous:typeshuffled       
    ## HR_other:conditionTurnTaking:typeshuffled     ** 
    ## HR_other:conditionConversation:typereal          
    ## HR_other:conditionMovementCoop:typereal       *  
    ## HR_other:conditionMovementGuided:typereal     *  
    ## HR_other:conditionSynchronous:typereal           
    ## HR_other:conditionTurnTaking:typereal            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 20 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

    # Concatenate it to the original dataset (and remember to have a column telling you which is which)

### TRICKY! Creating controls: surrogate pair controls

-   Per each real pair, identify at least one surrogate pair (matching
    one of the participants, with somebody doing the same task, but in a
    different pair)

<!-- -->

    # Identify unique pairs within a given study (to keep things manageable) and create list of possible surrogate pairs (e.g. individual 1 from pair 1 and individual 2 from pair 2)

    # Starting from the wide format, create "surrogate" dataset with the data from surrogate pairs
    groups <- as.numeric(as.character(unique(dd$group[dd$study==4]))) # List all pairs
    SurrogateList <- expand.grid(a = groups, b = groups) # Identify all possible combinations of 2 pairs
    SurrogateList = subset(SurrogateList, a != b)  # exclude combinations with identical pairs

    phys_data4 <- subset(phys_data, study == 4)
    #Create new dataframe
    surrogate_data <- phys_data4[0,]

    for (i in 1:nrow(SurrogateList)){  # loop through all combinations
      x <- subset(phys_data4, group==SurrogateList$a[i]) # subset data from the first pair    
      y <- subset(phys_data4, group!=SurrogateList$a[i]) # subset data from the second pair   
      newPairID <- c(800 + ((1:4)*i))                            
      # create new pair id
        for (co in c("Synchronous","TurnTaking","Conversation", "MovementCoop","MovementGuided")){ # loop through conditions
        if (co %in% unique(x$condition) & co %in% unique(y$condition)){ # check that both pairs have the data for that condition
          z1 <- subset(x, condition==co) # subset only that condtion from first pair
          z2 <- subset(y, condition==co) # subset only that condtion from second pair
          if (nrow(z1) > nrow(z2)) {    # make sure data have same length in both pairs
            z1<-z1[1:nrow(z2),]
            }
          if (nrow(z2) > nrow(z1)) { 
            z2<-z2[1:nrow(z1),]
            }
          w1 <- z1 %>% mutate(  # assemble new pair combining the 2 pairs
            HR2 = z2$HR2,
            Resp2 = z2$Resp2,
            HR2_future = z2$HR2_future, 
            Resp2_future = z2$Resp2_future, 
            HR2_change = z2$HR2_change, 
            Resp2_change = z2$Resp2_change)
          # Saving data
          if (nrow(surrogate_data) == 0) {
            surrogate_data <- w1
            }
            else {
              surrogate_data <- rbind(surrogate_data,w1)
            } # make sure that you do this!
    }}}
    # Make it into long format
    hr_future <- 
      gather(surrogate_data, # data        
             participant, HR_future, # new vars
             HR1_future, HR2_future) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_future, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    resp_future <- 
      gather(surrogate_data, # data        
             participant, Resp_future, # new vars
             Resp1_future, Resp2_future) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_future, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    hr_change_self <- 
      gather(surrogate_data, # data        
             participant, HR_change_self, # new vars
             HR1_change, HR2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_change_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    resp_change_self <- 
      gather(surrogate_data, # data        
             participant, Resp_change_self, # new vars
             Resp1_change, Resp2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_change_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    hr_self <- 
      gather(surrogate_data, # data        
             participant, HR_self, # new vars
             HR1, HR2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    resp_self <- 
      gather(surrogate_data, # data        
             participant, Resp_self, # new vars
             Resp1, Resp2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_self, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    surrogate_data_other <- surrogate_data %>% 
      rename(
        HR1 = "HR2",
        HR2 = "HR1",
        Resp1 = "Resp2",
        Resp2 = "Resp1",
        HR1_change = "HR2_change",
        HR2_change = "HR1_change",
        Resp1_change = "Resp2_change",
        Resp2_change = "Resp1_change"
      )

    hr_change_other <- 
      gather(surrogate_data_other, # data        
             participant, HR_change_other, # new vars
             HR1_change, HR2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_change_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    resp_change_other <- 
      gather(surrogate_data_other, # data        
             participant, Resp_change_other, # new vars
             Resp1_change, Resp2_change) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_change_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    hr_other <- 
      gather(surrogate_data_other, # data        
             participant, HR_other, # new vars
             HR1, HR2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, HR_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    resp_other <- 
      gather(surrogate_data_other, # data        
             participant, Resp_other, # new vars
             Resp1, Resp2) %>% #old vars
      select( # drop irrelevant vars
        actual_time_min, Resp_other, participant, study, group, condition, trial) %>%
      mutate( # create unique participant ID
        participant = parse_number(as.character(group)) * 100 + 
          parse_number(participant))

    dd2 <- cbind(hr_change_self,hr_change_other,resp_change_self,resp_change_other,hr_future,resp_future,hr_self,hr_other,resp_self,resp_other) 
    dd2 <- dd2 %>%  select(actual_time_min,participant,group,condition,trial,study,HR_self,Resp_self,HR_other,Resp_other,HR_future,Resp_future,HR_change_self,HR_change_other,Resp_change_self,Resp_change_other)

    #Making participant unique for study
    dd2$participant <- (dd2$study * 1000)+dd2$participant

    #Create type variable
    dd2$type <- "surrogate"
    dd$type <- "real"

    #Bind the dataframes together
    dd_all <- rbind(dd,dd2)


    #Creating new main effect for other - self
    dd_all$other_self <- (dd_all$HR_other) - (dd_all$HR_self)
    #Subset study 4
    dd_all_4 <- subset(dd_all, study == 4)

    # Releveling with conversation as baseline
    dd_all_4$type <- as.factor(dd_all_4$type)
    dd_all_4$type <- relevel(dd_all_4$type, ref = "surrogate")

    # Create models as in chunks above, but adding an interaction with the Real vs. Surrogate variable (exclude shuffled ones for simplicity)
    model_surrogate <- lmer(HR_change_self ~ 0 + (condition + (HR_self + HR_other):condition):type + 
                              (1 + condition | group) +        
                              (1 + condition | participant), 
                            data = dd_all_4, 
                            REML = F, 
                            control = lmerControl(
                            optimizer = "nloptwrap",
                            calc.derivs = FALSE,
                            optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxfun = 1000)))

    ## Warning: Model failed to converge with 6 negative eigenvalues: -3.5e+02
    ## -4.2e+03 -6.9e+03 -1.4e+04 -1.9e+04 -3.6e+04

    summary(model_surrogate)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: 
    ## HR_change_self ~ 0 + (condition + (HR_self + HR_other):condition):type +  
    ##     (1 + condition | group) + (1 + condition | participant)
    ##    Data: dd_all_4
    ## Control: 
    ## lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(xtol_abs = 1e-08,  
    ##     ftol_abs = 1e-08, maxfun = 1000))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 116459.1 117010.3 -58168.6 116337.1    61986 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.8643 -0.5275  0.0601  0.5812  6.8358 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant (Intercept)             6.187e-10 2.487e-05                  
    ##              conditionMovementCoop   1.715e-08 1.310e-04 -0.38            
    ##              conditionMovementGuided 1.329e-08 1.153e-04 -0.55  0.98      
    ##              conditionSynchronous    6.045e-09 7.775e-05 -0.58  0.97  1.00
    ##              conditionTurnTaking     4.343e-09 6.590e-05 -0.10  0.74  0.69
    ##  group       (Intercept)             3.605e-06 1.899e-03                  
    ##              conditionMovementCoop   2.419e-05 4.918e-03  1.00            
    ##              conditionMovementGuided 2.375e-05 4.873e-03  1.00  1.00      
    ##              conditionSynchronous    1.814e-05 4.260e-03 -1.00 -1.00 -1.00
    ##              conditionTurnTaking     7.064e-07 8.405e-04 -1.00 -1.00 -1.00
    ##  Residual                            3.818e-01 6.179e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##   0.64
    ##       
    ##       
    ##       
    ##       
    ##   1.00
    ##       
    ## Number of obs: 62047, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                                  Estimate Std. Error
    ## conditionConversation:typesurrogate             1.926e-03  5.081e-03
    ## conditionMovementCoop:typesurrogate             9.975e-03  7.995e-03
    ## conditionMovementGuided:typesurrogate          -6.603e-04  6.956e-03
    ## conditionSynchronous:typesurrogate              8.321e-04  6.685e-03
    ## conditionTurnTaking:typesurrogate              -4.952e-03  5.242e-03
    ## conditionConversation:typereal                  4.486e-03  1.332e-02
    ## conditionMovementCoop:typereal                  8.988e-03  1.986e-02
    ## conditionMovementGuided:typereal               -3.692e-04  1.705e-02
    ## conditionSynchronous:typereal                   5.926e-03  1.740e-02
    ## conditionTurnTaking:typereal                   -7.273e-04  1.384e-02
    ## conditionConversation:HR_self:typesurrogate    -2.663e-01  5.444e-03
    ## conditionMovementCoop:HR_self:typesurrogate    -1.731e-01  7.776e-03
    ## conditionMovementGuided:HR_self:typesurrogate  -2.234e-01  6.774e-03
    ## conditionSynchronous:HR_self:typesurrogate     -3.088e-01  7.129e-03
    ## conditionTurnTaking:HR_self:typesurrogate      -2.821e-01  5.706e-03
    ## conditionConversation:HR_self:typereal         -3.210e-01  1.482e-02
    ## conditionMovementCoop:HR_self:typereal         -2.230e-01  2.192e-02
    ## conditionMovementGuided:HR_self:typereal       -2.854e-01  1.853e-02
    ## conditionSynchronous:HR_self:typereal          -2.710e-01  1.891e-02
    ## conditionTurnTaking:HR_self:typereal           -2.906e-01  1.512e-02
    ## conditionConversation:HR_other:typesurrogate   -6.203e-03  5.444e-03
    ## conditionMovementCoop:HR_other:typesurrogate   -9.026e-03  7.776e-03
    ## conditionMovementGuided:HR_other:typesurrogate  1.207e-02  6.775e-03
    ## conditionSynchronous:HR_other:typesurrogate    -4.151e-03  7.133e-03
    ## conditionTurnTaking:HR_other:typesurrogate     -1.061e-02  5.701e-03
    ## conditionConversation:HR_other:typereal         1.241e-02  1.482e-02
    ## conditionMovementCoop:HR_other:typereal         4.600e-02  2.192e-02
    ## conditionMovementGuided:HR_other:typereal       3.931e-02  1.853e-02
    ## conditionSynchronous:HR_other:typereal         -1.023e-02  1.891e-02
    ## conditionTurnTaking:HR_other:typereal          -5.162e-04  1.512e-02
    ##                                                        df t value Pr(>|t|)
    ## conditionConversation:typesurrogate             1.311e+02   0.379   0.7053
    ## conditionMovementCoop:typesurrogate             1.643e+01   1.248   0.2297
    ## conditionMovementGuided:typesurrogate           1.261e+01  -0.095   0.9259
    ## conditionSynchronous:typesurrogate              6.147e+01   0.124   0.9013
    ## conditionTurnTaking:typesurrogate               3.612e+02  -0.945   0.3454
    ## conditionConversation:typereal                  5.647e+03   0.337   0.7362
    ## conditionMovementCoop:typereal                  6.203e+02   0.453   0.6510
    ## conditionMovementGuided:typereal                4.524e+02  -0.022   0.9827
    ## conditionSynchronous:typereal                   2.699e+03   0.341   0.7335
    ## conditionTurnTaking:typereal                    1.374e+04  -0.053   0.9581
    ## conditionConversation:HR_self:typesurrogate     6.204e+04 -48.910   <2e-16
    ## conditionMovementCoop:HR_self:typesurrogate     6.204e+04 -22.267   <2e-16
    ## conditionMovementGuided:HR_self:typesurrogate   6.204e+04 -32.984   <2e-16
    ## conditionSynchronous:HR_self:typesurrogate      6.204e+04 -43.314   <2e-16
    ## conditionTurnTaking:HR_self:typesurrogate       6.204e+04 -49.433   <2e-16
    ## conditionConversation:HR_self:typereal          6.204e+04 -21.654   <2e-16
    ## conditionMovementCoop:HR_self:typereal          6.204e+04 -10.173   <2e-16
    ## conditionMovementGuided:HR_self:typereal        6.204e+04 -15.406   <2e-16
    ## conditionSynchronous:HR_self:typereal           6.204e+04 -14.331   <2e-16
    ## conditionTurnTaking:HR_self:typereal            6.204e+04 -19.215   <2e-16
    ## conditionConversation:HR_other:typesurrogate    6.204e+04  -1.139   0.2546
    ## conditionMovementCoop:HR_other:typesurrogate    6.204e+04  -1.161   0.2457
    ## conditionMovementGuided:HR_other:typesurrogate  6.204e+04   1.781   0.0749
    ## conditionSynchronous:HR_other:typesurrogate     6.204e+04  -0.582   0.5606
    ## conditionTurnTaking:HR_other:typesurrogate      6.204e+04  -1.861   0.0627
    ## conditionConversation:HR_other:typereal         6.204e+04   0.837   0.4024
    ## conditionMovementCoop:HR_other:typereal         6.204e+04   2.099   0.0359
    ## conditionMovementGuided:HR_other:typereal       6.204e+04   2.122   0.0339
    ## conditionSynchronous:HR_other:typereal          6.204e+04  -0.541   0.5884
    ## conditionTurnTaking:HR_other:typereal           6.204e+04  -0.034   0.9728
    ##                                                   
    ## conditionConversation:typesurrogate               
    ## conditionMovementCoop:typesurrogate               
    ## conditionMovementGuided:typesurrogate             
    ## conditionSynchronous:typesurrogate                
    ## conditionTurnTaking:typesurrogate                 
    ## conditionConversation:typereal                    
    ## conditionMovementCoop:typereal                    
    ## conditionMovementGuided:typereal                  
    ## conditionSynchronous:typereal                     
    ## conditionTurnTaking:typereal                      
    ## conditionConversation:HR_self:typesurrogate    ***
    ## conditionMovementCoop:HR_self:typesurrogate    ***
    ## conditionMovementGuided:HR_self:typesurrogate  ***
    ## conditionSynchronous:HR_self:typesurrogate     ***
    ## conditionTurnTaking:HR_self:typesurrogate      ***
    ## conditionConversation:HR_self:typereal         ***
    ## conditionMovementCoop:HR_self:typereal         ***
    ## conditionMovementGuided:HR_self:typereal       ***
    ## conditionSynchronous:HR_self:typereal          ***
    ## conditionTurnTaking:HR_self:typereal           ***
    ## conditionConversation:HR_other:typesurrogate      
    ## conditionMovementCoop:HR_other:typesurrogate      
    ## conditionMovementGuided:HR_other:typesurrogate .  
    ## conditionSynchronous:HR_other:typesurrogate       
    ## conditionTurnTaking:HR_other:typesurrogate     .  
    ## conditionConversation:HR_other:typereal           
    ## conditionMovementCoop:HR_other:typereal        *  
    ## conditionMovementGuided:HR_other:typereal      *  
    ## conditionSynchronous:HR_other:typereal            
    ## conditionTurnTaking:HR_other:typereal             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 30 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    # Model with other-self as main effect
    model_control2 <- lmer(HR_change_self ~ 0 + (condition + (HR_self + other_self):condition):type + 
                              (1 + condition | group) +        
                              (1 + condition | participant), 
                            data = dd_all_4, 
                            REML = F, 
                            control = lmerControl(
                            optimizer = "nloptwrap",
                            calc.derivs = FALSE,
                            optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8, maxfun = 1000)))

    ## Warning: Model failed to converge with 1 negative eigenvalue: -6.2e+02

    summary(model_control2)

    ## Linear mixed model fit by maximum likelihood . t-tests use
    ##   Satterthwaite's method [lmerModLmerTest]
    ## Formula: 
    ## HR_change_self ~ 0 + (condition + (HR_self + other_self):condition):type +  
    ##     (1 + condition | group) + (1 + condition | participant)
    ##    Data: dd_all_4
    ## Control: 
    ## lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE, optCtrl = list(xtol_abs = 1e-08,  
    ##     ftol_abs = 1e-08, maxfun = 1000))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 116459.1 117010.3 -58168.6 116337.1    61986 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.8643 -0.5275  0.0601  0.5812  6.8358 
    ## 
    ## Random effects:
    ##  Groups      Name                    Variance  Std.Dev.  Corr             
    ##  participant (Intercept)             0.000e+00 0.000e+00                  
    ##              conditionMovementCoop   1.089e-10 1.043e-05   NaN            
    ##              conditionMovementGuided 4.205e-10 2.051e-05   NaN  0.96      
    ##              conditionSynchronous    6.136e-11 7.834e-06   NaN -0.67 -0.43
    ##              conditionTurnTaking     5.058e-11 7.112e-06   NaN -0.10  0.02
    ##  group       (Intercept)             3.598e-06 1.897e-03                  
    ##              conditionMovementCoop   2.402e-05 4.901e-03  1.00            
    ##              conditionMovementGuided 2.364e-05 4.862e-03  1.00  1.00      
    ##              conditionSynchronous    1.820e-05 4.266e-03 -1.00 -1.00 -1.00
    ##              conditionTurnTaking     7.023e-07 8.380e-04 -1.00 -1.00 -1.00
    ##  Residual                            3.818e-01 6.179e-01                  
    ##       
    ##       
    ##       
    ##       
    ##       
    ##   0.46
    ##       
    ##       
    ##       
    ##       
    ##   1.00
    ##       
    ## Number of obs: 62047, groups:  participant, 16; group, 8
    ## 
    ## Fixed effects:
    ##                                                    Estimate Std. Error
    ## conditionConversation:typesurrogate               1.926e-03  5.081e-03
    ## conditionMovementCoop:typesurrogate               9.976e-03  7.992e-03
    ## conditionMovementGuided:typesurrogate            -6.606e-04  6.954e-03
    ## conditionSynchronous:typesurrogate                8.339e-04  6.686e-03
    ## conditionTurnTaking:typesurrogate                -4.952e-03  5.242e-03
    ## conditionConversation:typereal                    4.486e-03  1.332e-02
    ## conditionMovementCoop:typereal                    8.990e-03  1.986e-02
    ## conditionMovementGuided:typereal                 -3.696e-04  1.705e-02
    ## conditionSynchronous:typereal                     5.928e-03  1.740e-02
    ## conditionTurnTaking:typereal                     -7.273e-04  1.384e-02
    ## conditionConversation:HR_self:typesurrogate      -2.725e-01  7.786e-03
    ## conditionMovementCoop:HR_self:typesurrogate      -1.822e-01  1.081e-02
    ## conditionMovementGuided:HR_self:typesurrogate    -2.114e-01  9.338e-03
    ## conditionSynchronous:HR_self:typesurrogate       -3.129e-01  9.920e-03
    ## conditionTurnTaking:HR_self:typesurrogate        -2.927e-01  8.619e-03
    ## conditionConversation:HR_self:typereal           -3.086e-01  2.012e-02
    ## conditionMovementCoop:HR_self:typereal           -1.770e-01  2.718e-02
    ## conditionMovementGuided:HR_self:typereal         -2.461e-01  2.548e-02
    ## conditionSynchronous:HR_self:typereal            -2.812e-01  2.560e-02
    ## conditionTurnTaking:HR_self:typereal             -2.911e-01  2.038e-02
    ## conditionConversation:other_self:typesurrogate   -6.203e-03  5.444e-03
    ## conditionMovementCoop:other_self:typesurrogate   -9.026e-03  7.776e-03
    ## conditionMovementGuided:other_self:typesurrogate  1.207e-02  6.775e-03
    ## conditionSynchronous:other_self:typesurrogate    -4.151e-03  7.133e-03
    ## conditionTurnTaking:other_self:typesurrogate     -1.061e-02  5.701e-03
    ## conditionConversation:other_self:typereal         1.241e-02  1.482e-02
    ## conditionMovementCoop:other_self:typereal         4.600e-02  2.192e-02
    ## conditionMovementGuided:other_self:typereal       3.931e-02  1.853e-02
    ## conditionSynchronous:other_self:typereal         -1.023e-02  1.891e-02
    ## conditionTurnTaking:other_self:typereal          -5.162e-04  1.512e-02
    ##                                                          df t value
    ## conditionConversation:typesurrogate               1.317e+02   0.379
    ## conditionMovementCoop:typesurrogate               1.649e+01   1.248
    ## conditionMovementGuided:typesurrogate             1.263e+01  -0.095
    ## conditionSynchronous:typesurrogate                6.107e+01   0.125
    ## conditionTurnTaking:typesurrogate                 3.622e+02  -0.945
    ## conditionConversation:typereal                    5.671e+03   0.337
    ## conditionMovementCoop:typereal                    6.233e+02   0.453
    ## conditionMovementGuided:typereal                  4.539e+02  -0.022
    ## conditionSynchronous:typereal                     2.681e+03   0.341
    ## conditionTurnTaking:typereal                      1.378e+04  -0.053
    ## conditionConversation:HR_self:typesurrogate       6.204e+04 -34.994
    ## conditionMovementCoop:HR_self:typesurrogate       6.204e+04 -16.858
    ## conditionMovementGuided:HR_self:typesurrogate     6.202e+04 -22.636
    ## conditionSynchronous:HR_self:typesurrogate        6.204e+04 -31.543
    ## conditionTurnTaking:HR_self:typesurrogate         6.204e+04 -33.956
    ## conditionConversation:HR_self:typereal            6.204e+04 -15.335
    ## conditionMovementCoop:HR_self:typereal            6.204e+04  -6.510
    ## conditionMovementGuided:HR_self:typereal          6.204e+04  -9.658
    ## conditionSynchronous:HR_self:typereal             6.204e+04 -10.985
    ## conditionTurnTaking:HR_self:typereal              6.204e+04 -14.283
    ## conditionConversation:other_self:typesurrogate    6.204e+04  -1.139
    ## conditionMovementCoop:other_self:typesurrogate    6.204e+04  -1.161
    ## conditionMovementGuided:other_self:typesurrogate  6.204e+04   1.781
    ## conditionSynchronous:other_self:typesurrogate     6.204e+04  -0.582
    ## conditionTurnTaking:other_self:typesurrogate      6.204e+04  -1.861
    ## conditionConversation:other_self:typereal         6.204e+04   0.837
    ## conditionMovementCoop:other_self:typereal         6.204e+04   2.099
    ## conditionMovementGuided:other_self:typereal       6.204e+04   2.122
    ## conditionSynchronous:other_self:typereal          6.204e+04  -0.541
    ## conditionTurnTaking:other_self:typereal           6.204e+04  -0.034
    ##                                                  Pr(>|t|)    
    ## conditionConversation:typesurrogate                0.7052    
    ## conditionMovementCoop:typesurrogate                0.2294    
    ## conditionMovementGuided:typesurrogate              0.9258    
    ## conditionSynchronous:typesurrogate                 0.9011    
    ## conditionTurnTaking:typesurrogate                  0.3454    
    ## conditionConversation:typereal                     0.7362    
    ## conditionMovementCoop:typereal                     0.6510    
    ## conditionMovementGuided:typereal                   0.9827    
    ## conditionSynchronous:typereal                      0.7334    
    ## conditionTurnTaking:typereal                       0.9581    
    ## conditionConversation:HR_self:typesurrogate       < 2e-16 ***
    ## conditionMovementCoop:HR_self:typesurrogate       < 2e-16 ***
    ## conditionMovementGuided:HR_self:typesurrogate     < 2e-16 ***
    ## conditionSynchronous:HR_self:typesurrogate        < 2e-16 ***
    ## conditionTurnTaking:HR_self:typesurrogate         < 2e-16 ***
    ## conditionConversation:HR_self:typereal            < 2e-16 ***
    ## conditionMovementCoop:HR_self:typereal           7.56e-11 ***
    ## conditionMovementGuided:HR_self:typereal          < 2e-16 ***
    ## conditionSynchronous:HR_self:typereal             < 2e-16 ***
    ## conditionTurnTaking:HR_self:typereal              < 2e-16 ***
    ## conditionConversation:other_self:typesurrogate     0.2546    
    ## conditionMovementCoop:other_self:typesurrogate     0.2457    
    ## conditionMovementGuided:other_self:typesurrogate   0.0749 .  
    ## conditionSynchronous:other_self:typesurrogate      0.5606    
    ## conditionTurnTaking:other_self:typesurrogate       0.0627 .  
    ## conditionConversation:other_self:typereal          0.4024    
    ## conditionMovementCoop:other_self:typereal          0.0359 *  
    ## conditionMovementGuided:other_self:typereal        0.0339 *  
    ## conditionSynchronous:other_self:typereal           0.5884    
    ## conditionTurnTaking:other_self:typereal            0.9728    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 30 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

### Effects of respiration coordination on heart rate coordination

-   describe how you would test those.
-   Optional: run the models and report them
