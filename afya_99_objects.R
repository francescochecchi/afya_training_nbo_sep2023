#...............................................................................
### +++++++++ AFYA CONSORTIUM - YEAR 2 MEETING, NAIROBI, SEP 2023 ++++++++++ ###
#...............................................................................


...............................................................................  
### 99 - Practice with R objects
#...............................................................................
    
  #.........................................      
  ## Vectors
    
    # Initialise vectors
      # Afya consortium partners
      partners <- c("UCB", "SIMAD", "BNO", "SRG", "LSHTM")
      partners
  
      # year of founding of each partner institution
      year_founded <- c(1989, 1999, 2017, 2020, 1899)
  
      # whether the partner is a university
      university <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
  
      # first name (in A-Z order) of female partner team members
      fem_member <- c(NA, "Fartun", NA, "Israa", "Fiona")

    # Properties of the vectors
      # Type
      typeof(university)
      str(partners)
      
      # Number of observations
      length(partners)
    
      # Name each position in the university vector
      names(university) <- c("UCB", "SIMAD", "BNO", "SRG", "LSHTM") # or
      names(university) <- partners
      university

      # Remove NA values from fem_member vector
      fem_member_na_omit <- na.omit(fem_member)
      length(fem_member_na_omit)
      
      # Access SIMAD position within university vector
      university["SIMAD"]
      university[2]
      

  #.........................................      
  ## Data frames
      
    # Generate a data frame - one way
    partner_data <- data.frame(
      partner = c("UCB", "SIMAD", "BNO", "SRG", "LSHTM"),
      year_founded = c(1989, 1999, 2017, 2020, 1899),
      university = c(TRUE, TRUE, FALSE, FALSE, TRUE),
      fem_member = c(NA, "Fartun", NA, "Israa", "Fiona")
    )
      
    # Generate a data frame - another way, given we have the columns already
    partner_data <- data.frame(partners, year_founded, university, fem_member)
    colnames(partner_data) <- c("partner", "year_founded", "university", 
      "fem_member")    
    partner_data

    # Access different elements of the data frame
      # a whole column
      partner_data$year_founded # or
      partner_data[ , "year_founded"] # or
      partner_data[ , 2]
    
      # a whole row
      partner_data[3, ]
    
      # a specific cell
      partner_data[2, 4] # or
      partner_data[2, "fem_member"]
    
    # Add a new column
    partner_data$n_students <- c(3419, 3478, NA, NA, 1307) # made-up numbers
        
    # Describe some characteristics
      # mean year of foundation
      mean(partner_data$year_founded)
      
      # proportions of partners that are universities
      table(partner_data$university) # numbers
      prop.table(table(partner_data$university)) # proportions
      
      # situation with NA values
      mean(partner_data$n_students) # this doesn't work
      mean(partner_data$n_students, na.rm = TRUE) # this works
  
      
  #.........................................      
  ## Functions
    
    # Example
    f_degrees <- function(degree_f) {
      if (degree_f == "BSc") {return("maybe a good idea")}
      if (degree_f == "MSc") {return("did you really have to?")}
      if (degree_f == "PhD") {return("why did you do it?")}
      if (! degree_f %in% c("Bsc", "MSc", "PhD")) {return("unclear")}
    }
     
    f_degrees("PhD")
   
  
    
    f_square <- function(x) {x^2}
    f_square(16)    
          