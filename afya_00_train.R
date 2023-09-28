#...............................................................................
### +++++++++ AFYA CONSORTIUM - YEAR 2 MEETING, NAIROBI, SEP 2023 ++++++++++ ###
#...............................................................................

#...............................................................................
## -------------- GENERIC R SCRIPT SUGGESTED TO PARTICIPANTS  --------------- ##
#...............................................................................


#...............................................................................
### 01 - Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  
  pacman::p_load(
    flextable,   # To write tables in .docx format
    ggplot2,     # Data visualization
    ggpubr,      # Arranging multiple plots into a single plot
    lubridate,   # Makes it easier to work with dates and times
    MASS,        # For various statistical functions
    parameters,  # Visualise model output
    readxl,      # Read Excel database
    reshape2,    # For converting between wide and long data structure
    scales,      # Scaling and formatting data for visualizations
    tidyverse)   # Tidyverse suite of packages

  #...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
      , "/", sep = "")
    setwd(dir_path)
    print( getwd() )
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
    palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
      "#0072B2", "#D55E00", "#CC79A7")
    show_col(palette_cb)

    
#...............................................................................  
### 02 - Reading in required data and setting any needed parameters
#...............................................................................

  #.........................................
  ## Read in required datasets
    # IDP PRMN dataset
    prmn <- read_excel("UNHCR-PRMN-Displacement-Dataset.xlsx")
    prmn <- as.data.frame(prmn)
  
    # Composite Drought Index
    cdi <- read.csv("som_cdi_data.csv")
    
    # Population
    pop <- read.csv("som_pop.csv")

  #.........................................      
  ## Set any needed parameters  
    
    # Proportion of children under 5 years
    prop_u5 <- 0.17 # 17% - rough estimate

#...............................................................................  
### 03 - Preparing the data for analysis
#...............................................................................

  #.........................................      
  ## Prepare the PRMN displacement dataset
    
    # Explore the data structure
    head(prmn) # see the first few rows
    colnames(prmn) # column / variable names
    nrow(prmn) # how many rows
    ncol(prmn) # how many columns
    str(prmn) # column names and formats
                
    # Rename columns
    colnames(prmn) <- c("date", "year_week", "region_arr", "district_arr",
      "region_dep", "district_dep", "reason", "priority_need", "n_idp")
  
    # Create a month and year variable
      # make sure the date variable actually is of type date
      prmn$date_ok <- as.Date(prmn$date)
      str(prmn$date_ok) # that didn't work
      prmn$date_ok <- lubridate::dmy(prmn$date)
      str(prmn$date_ok) # OK!
      
      # extract year and month
      prmn$year <- year(prmn$date_ok)
      prmn$month <- month(prmn$date_ok)
    
    # Only keep variables needed
    prmn <- prmn[, c("region_dep", "district_dep", "date_ok", "year", "month", 
      "reason", "n_idp")]  
    
    # Check for any missingness using the complete.cases function
    table(complete.cases(prmn)) # no missing values
      
    # Aggregate (sum) dataset by region, district, year, month, reason
    prmn_aggr <- aggregate(prmn$n_idp, by = prmn[, c("region", "district", 
      "year", "month", "reason")], FUN = sum) # ouch - wrong variable names
    prmn_aggr <- aggregate(prmn$n_idp, by = prmn[, c("region_dep", "district_dep", 
      "year", "month", "reason")], FUN = sum) # ok
    head(prmn_aggr)
    
    # Compare the number of rows in the unaggregated and aggregated databases
    nrow(prmn)
    nrow(prmn_aggr)
    
    # Rename the columns
    colnames(prmn_aggr) <- c("region", "district", "year", "month", "reason", 
      "n_idp")
    
  #.........................................      
  ## Prepare the CDI dataset
    
    # Inspect
    str(cdi)
    
    # Select the required columns
    cdi <- subset(cdi, select = -pcode)
    
    # Check for missingness
    table(complete.cases(cdi)) # quite a bit of missingness
    table(is.na(cdi$cdi), cdi$year) # a-ha: 2013, 2014 and some of 2023 missing
    x <- subset(cdi, year == 2023)
    table(is.na(x$cdi), x$month) # OK, so July 2023 is missing

  #.........................................      
  ## Prepare the population dataset
    
    # Inspect
    str(pop)
    
    # Rename the columns
    colnames(pop) <- c("district", "year", "month", "pop") 
    
    
  #.........................................      
  ## Merge the datasets together
    
    # First merge cdi into prmn
    merged <- merge(prmn_aggr, cdi, by = c("district", "year", "month"))
    View(merged) # any problems?
    merged <- merge(prmn_aggr, cdi, 
      by = c("region", "district", "year", "month"), all.x = TRUE)
    nrow(prmn_aggr)
    nrow(merged)
    
    # Then merge pop into prmn
    merged <- merge(merged, pop, by = c("district", "year", "month"), 
      all.x = TRUE)
    nrow(merged)
        

  #.........................................      
  ## Create any additional / secondary variables needed for analysis
    
    # IDP departure rate (per 1000 population per district-month)
    merged$dep_rate <- merged$n_idp * 1000 / merged$pop
    

  #.........................................      
  ## Save the cleaned dataset (optional)
  write.csv(merged, "som_idp_merged.csv", row.names = FALSE)    
    
            
#...............................................................................  
### 04 - Performing statistical analyses
#...............................................................................
                
  # #.........................................      
  # ## Demonstration of base versus tidyverse syntax
  # 
  #   # Sum n of IDPs and tabulate cause of displacement - in base
  #   sum(prmn$n_idp)
  #   table(prmn$reason)
  # 
  #   # Sum n of IDPs and tabulate cause of displacement - in tidyverse
  #   prmn %>% 
  #     group_by(reason) %>%
  #       summarize(
  #         sum(n_idp),
  #         n()
  #       ) 
  # 

  #.........................................      
  ## Restrict the dataset as desired
    
    # Only non-missing records
      # one way
      merged_sub <- merged[complete.cases(merged), ]
      
      # another way
      merged_sub <- na.omit(merged)
    
    # Only IDPs who left because of drought
    table(merged_sub$reason)
    merged_sub <- subset(merged_sub, reason == "Drought related")  
    nrow(merged_sub)
          
  #.........................................      
  ## Linear regression: outcome = IDP departure rate; exposure = CDI
    
    # First let's look at the distributions of the outcome
    range(merged_sub$dep_rate, na.rm = TRUE)
    quantile(merged_sub$dep_rate, c(0.025, 0.25, 0.50, 0.75, 0.975) )
    ggplot(data = merged_sub, aes(x = dep_rate)) +
      geom_histogram(colour = palette_cb[4])
    
      # what if we take the natural log of departure rate?
      merged_sub$dep_rate_log <- log(merged_sub$dep_rate)
      ggplot(data = merged_sub, aes(x = dep_rate_log)) +
        geom_histogram(colour = palette_cb[4], fill = palette_cb[4], 
          alpha = 0.5) +
        theme_bw()
    
    # What about the distribution of the exposure?
    range(merged_sub$cdi, na.rm = TRUE)
    quantile(merged_sub$cdi, c(0.025, 0.25, 0.50, 0.75, 0.975), na.rm = TRUE)
    ggplot(data = merged_sub, aes(x = cdi)) +
      geom_histogram(colour = palette_cb[6], fill = palette_cb[6], 
        alpha = 0.5) +
      theme_bw()
    
    # Fit a linear regression model
    mod1 <- lm(dep_rate_log ~ cdi, data = merged_sub)
    summary(mod1)
    parameters(mod1)
    
        
#...............................................................................  
### 05 - Generating tables and figures
#...............................................................................
                
  #.........................................      
  ## Plot of number of IDP departures per month
    
    # Prepare a separate dataframe for the plot
    merged_plot <- merged
    merged_plot <- na.omit(merged_plot)
    
    # Add a date variable again
    merged_plot$date <- paste(merged_plot$year, merged_plot$month, 
      "15", sep = "-")
    head(merged_plot$date)
    str(merged_plot$date) # not yet a date!
    merged_plot$date <- ymd(merged_plot$date) # now a date
    
    # Aggregate the data by date, n_idp
    merged_plot <- aggregate(merged_plot$n_idp, by = list(merged_plot$date),
      FUN = sum)
    colnames(merged_plot) <- c("date", "n_idp")
      
    # Plot the countrywide trend
      # basic plot
      ggplot(data = merged_plot, mapping = aes(x = date, y = n_idp) ) +
        geom_bar(stat = "identity")
    
      # make it prettier
      ggplot(data = merged_plot, mapping = aes(x = date, y = n_idp) ) +
        geom_bar(stat = "identity", colour = palette_cb[6], 
          fill = palette_cb[6], alpha = 0.5) +
        theme_bw() +
        scale_y_continuous(name = "number of IDPs departing", 
          labels = scales::label_number_auto(), 
          breaks = seq(0, 400000, by = 50000)) +
        scale_x_date(name = "", labels = date_format("%Y"), breaks = "1 year")
        
      # if we like it, let's save it
      ggsave("trends_idp_departures.png", dpi = "print", units = "cm", 
        height = 13, width = 20)  
    

    # Plot a scatter plot of departure rate versus CDI
      # basic plot
      ggplot(data = merged_plot, mapping = aes(x = date, y = n_idp) ) +
        geom_bar(stat = "identity")
    
      # make it prettier
      ggplot(data = merged_plot, mapping = aes(x = date, y = n_idp) ) +
        geom_bar(stat = "identity", colour = palette_cb[6], 
          fill = palette_cb[6], alpha = 0.5) +
        theme_bw() +
        scale_y_continuous(name = "number of IDPs departing", 
          labels = scales::label_number_auto(), 
          breaks = seq(0, 400000, by = 50000)) +
        scale_x_date(name = "", labels = date_format("%Y"), breaks = "1 year")
        
      # if we like it, let's save it
      ggsave("trends_idp_departures.png", dpi = "print", units = "cm", 
        height = 13, width = 20)  
    
      
          
    
                        
#...............................................................................  
### ENDS
#...............................................................................
