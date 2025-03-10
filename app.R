library(shiny)
library(tuneR)
library(openxlsx)
library(shinyWidgets)
library(suwo)
library(stringr)
library(bslib) # Load bslib
library(dplyr)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "sketchy"), # Apply sketchy theme using bslib
  titlePanel("Bird Identification Quiz"),
  tags$head(
    tags$style(HTML("
      #audioQuiz {
        margin-top: 20px;
      }
      .question-row {
        border-bottom: 1px solid #ddd;
        margin-bottom: 5px;
        padding-bottom: 5px;
      }
      .playing-button {
        background-color: lightblue;
      }
    "))
  ),
  tabsetPanel(
    tabPanel("Quiz",
             uiOutput("audioQuiz"),
             textInput("pseudo", "Enter Your Name/Pseudo:"),
             actionButton("submitButton", "Submit Choices"),
             actionButton("resetButton", "Reset Quiz"),
             tableOutput("resultsTable"),
             htmlOutput("score"),
             uiOutput("resetMessage")
    ),
    tabPanel("Classement",
             tableOutput("classementTable")
    )
  )
)

server <- function(input, output, session) {
  audioFiles <- reactiveVal(NULL)
  responses <- reactiveVal(NULL)
  file_info_df <- reactiveVal(data.frame())
  rights <- reactiveVal(data.frame())
  score <- reactiveVal(0)
  classement_df <- reactiveVal(data.frame())
  submitted <- reactiveVal(FALSE)
  playing_button <- reactiveVal(NULL) # Reactive value to track playing butto
  
  loadSounds <- function(){ 
    folder_path <- "sounds/" # Replace with the actual folder path 
    
    ######### Replacement of sounds files in sounds folder and reset of file_info_df when app starts ###### 
    # List all files in the folder 
    files_to_delete <- list.files(folder_path, full.names = TRUE) 
    
    # Check if there are any files to delete 
    if (length(files_to_delete) > 0) { 
      attempt <- 1 
      max_attempts <- 3 # Set a maximum number of attempts to avoid infinite loops 
      
      while (attempt <= max_attempts) { 
        result <- tryCatch({ 
          # Attempt to delete each file 
          unlink(files_to_delete) 
          #cat"All files in", folder_path, "have been deleted.\n") 
          "success" # Signal successful deletion 
        }, error = function(e) { 
          #cat"Attempt", attempt, "failed:", e$message, "\n") 
          Sys.sleep(1) # Wait for 2 seconds before retrying 
          "failure" # Signal deletion failure 
        }) 
        
        if (result == "success") { 
          break # Exit the loop if deletion was successful 
        } 
        
        attempt <- attempt + 1 
      } 
      
      if (attempt > max_attempts) { 
        #cat"Failed to delete files after", max_attempts, "attempts. Please ensure files are not open in other applications.\n") 
      } 
    } else { 
      #cat"No files found in", folder_path, "to delete.\n") 
    } 
    
    #read the list 
    species_list<-openxlsx2::read_xlsx("bird_list.xlsx") 
    
    #selcet ten randomly and extract them to put them ito a vector 
    sampled_rows <- species_list[sample(nrow(species_list), 10), ] 
    sampled_rows <- sampled_rows[order(sampled_rows$species), ] 
    ten_species <- c(sampled_rows$species) 
    
    #create df with infos that will permit to rename the file in the folder sounds 
    infos<-data.frame() 
    
    withProgress(message = "Downloading bird sounds...", value = 0, { 
      for (i in 1:length(ten_species)) { 
        species <- ten_species[i] 
        tryCatch({ 
          # Clean species name (replace underscores with spaces) 
          clean_species <- gsub("_", " ", species) 
          
          # Construct the query string (Quality A first) 
          query_string_A <- paste(clean_species, "cnt:France q:A") 
          query_string_any <- paste(clean_species, "cnt:France") #Query to use if A quality is not found. 
          
          # #print the query string (for debugging) 
          ##print(paste("Querying (Quality A):", query_string_A)) 
          
          # Query xeno-canto (Quality A) 
          metadata <- suwo::query_xenocanto(query_string_A, all_data = getOption("all_data", TRUE)) 
          
          # Check for NULL or empty data frame 
          if (is.null(metadata) || nrow(metadata) == 0) { 
            #cat"No Quality A recordings found for", species, ". Trying any quality.\n") 
            #print(paste("Querying (Any Quality):", query_string_any)) 
            metadata <- suwo::query_xenocanto(query_string_any, all_data = getOption("all_data", TRUE)) 
            
            if (is.null(metadata) || nrow(metadata) == 0) { 
              #cat"No recordings found for", species, "of any quality.\n") 
            } else { 
              # proceed with any quality sound 
              rows_with_any <- metadata # no need to filter by A. 
              random_row_with_any <- rows_with_any[sample(nrow(rows_with_any), 1), , drop = FALSE] 
              
              name_ofthe_file <- random_row_with_any[, c("file.name", "genus", "specific.epithet", "recordist", "license"), drop = FALSE] 
              name_ofthe_file$latin_name <- paste0(name_ofthe_file$genus, " ", name_ofthe_file$specific.epithet) 
              name_ofthe_file <- name_ofthe_file[, c("file.name", "latin_name","recordist","license"), drop = FALSE] 
              
              suwo::download_media(random_row_with_any, path = "sounds/") 
              #cat"Downloaded media for", species, "(Any Quality)\n") 
              
              # Store file information 
              infos <- rbind(infos, name_ofthe_file) 
            } 
            
          } else { #Quality A recordings found 
            # Filter for quality "A" recordings 
            rows_with_A <- metadata[metadata$quality == "A", , drop = FALSE] 
            
            # Check if any quality "A" recordings were found 
            if (nrow(rows_with_A) > 0) { 
              
              # Select a random row with quality "A" 
              random_row_with_A <- rows_with_A[sample(nrow(rows_with_A), 1), , drop = FALSE] 
              
              # Extract relevant columns 
              name_ofthe_file <- random_row_with_A[, c("file.name", "genus", "specific.epithet", "recordist", "license"), drop = FALSE] 
              
              # Create latin name 
              name_ofthe_file$latin_name <- paste0(name_ofthe_file$genus, " ", name_ofthe_file$specific.epithet) 
              
              # Keep only file.name and latin_name 
              name_ofthe_file <- name_ofthe_file[, c("file.name", "latin_name", "recordist", "license"), drop = FALSE] 
              
              # Download the media 
              suwo::download_media(random_row_with_A, path = "sounds/") 
              
              # #print success message 
              #cat"Downloaded media for", species, "(Quality A)\n") 
              
              # Store file information 
              infos <- rbind(infos, name_ofthe_file) 
              
            } else { 
              #cat"No quality A recordings found for", species, ". Trying any quality.\n") 
              #print(paste("Querying (Any Quality):", query_string_any)) 
              metadata <- suwo::query_xenocanto(query_string_any, all_data = getOption("all_data", TRUE)) 
              
              if (is.null(metadata) || nrow(metadata) == 0) { 
                #cat"No recordings found for", species, "of any quality.\n") 
              } else { 
                # proceed with any quality sound 
                rows_with_any <- metadata # no need to filter by A. 
                random_row_with_any <- rows_with_any[sample(nrow(rows_with_any), 1), , drop = FALSE] 
                
                name_ofthe_file <- random_row_with_any[, c("file.name", "genus", "specific.epithet", "recordist", "license"), drop = FALSE] 
                name_ofthe_file$latin_name <- paste0(name_ofthe_file$genus, " ", name_ofthe_file$specific.epithet) 
                name_ofthe_file <- name_ofthe_file[, c("file.name", "latin_name", "recordist", "license"), drop = FALSE] 
                
                suwo::download_media(random_row_with_any, path = "sounds/") 
                #cat"Downloaded media for", species, "(Any Quality)\n") 
                
                # Store file information 
                infos <- rbind(infos, name_ofthe_file) 
              } 
            } 
          } 
          
          # Update progress bar 
          setProgress(value = i / length(ten_species), detail = paste("Question", i, "of", length(ten_species))) 
          # Introduce a delay (0.1 seconds) to avoid rate limiting 
          Sys.sleep(0.1) 
          
        }, error = function(e) { 
          # #print error message 
          #cat"Error processing", species, ":", conditionMessage(e), "\n") 
        }) 
      } 
      
    })#end of progress bar 
    selected_cols <- infos[, c("recordist", "license")] # Extract cols with authors and licenses 
    rights(selected_cols) # Update the reactive df for rights (authors and licenses) 
    infos <- infos[, !(names(infos) %in% c("recordist", "license"))] # Remove cols with authors and licenses 
    infos<-cbind(infos,sampled_rows$species2) #adding of the column with the common names 
    infos$file.name <- str_replace_all(infos$file.name, "-", " ") 
    colnames(infos)[3] <- "language_name" 
    file_info_df(infos) 
    infos<-file_info_df() 
    #print("#############") 
    #print(file_info_df()) 
    #print(infos) 
    #### end of reset of sounds and file_info_df at the start 
    
    #loading of sounds files to the app 
    if (dir.exists(folder_path)) { 
      files <- list.files(folder_path, pattern = "\\.(wav|mp3)$", full.names = TRUE, ignore.case = TRUE) 
      if (length(files) > 0) { 
        audioFiles(files) 
      } else { 
        showNotification("No audio files found in the 'sounds' folder.", type = "warning") 
      } 
    } else { 
      showNotification("The 'sounds' folder does not exist.", type = "error") 
    } 
    
    if (file.exists("possible_responses.xlsx")) { 
      tryCatch({ 
        responses_data <- read.xlsx("possible_responses.xlsx") 
        if (ncol(responses_data) > 0) { 
          response_choices <- as.character(responses_data[[1]]) 
          responses(response_choices) 
        } else { 
          showNotification("possible_responses.xlsx has no columns.", type = "warning") 
        } 
      }, error = function(e) { 
        showNotification(paste("Error reading possible_responses.xlsx:", e$message), type = "error") 
      }) 
    } else { 
      showNotification("possible_responses.xlsx not found.", type = "warning") 
    } 
    
    if (file.exists("classement.xlsx")) { 
      tryCatch({ 
        df <- read.xlsx("classement.xlsx") 
        classement_df(df) 
      }, error = function(e) { 
        showNotification(paste("Error reading classement.xlsx:", e$message), type = "error") 
      }) 
    } else { 
      classement_df(data.frame(Name = character(), MeanScore = numeric())) 
    }
  } #function that clears sounds folder, reset file_info_df and upload new sounds/fill file_info_df
  
  observe({
    loadSounds()
  }) #END OF Observe(...) FUNCTION THAT WORKS AT SOON AS THE APP STARTS
  
  output$audioQuiz <- renderUI({
    files <- audioFiles()
    response_choices <- responses()
    df_rights <- rights()
    playing <- playing_button() # Get the currently playing button
    
    if (!is.null(files) && !is.null(response_choices) && !is.null(df_rights) && nrow(df_rights) > 0) {
      lapply(1:length(files), function(i) {
        file <- files[i]
        buttonId <- paste0("play_", gsub("\\.[^.]*$", "", basename(file)))
        selectInputId <- paste0("response_", gsub("\\.[^.]*$", "", basename(file)))
        recordist <- df_rights[i, "recordist"]
        license_url <- df_rights[i, "license"]
        
        div(
          class = "question-row",
          fluidRow(
            column(
              width = 2,
              actionButton(buttonId, paste("Question", i), 
                           class = if (!is.null(playing) && buttonId == playing) "playing-button" else NULL
                           ),
              style = "padding-right: 2px;"
            ),
            column(
              width = 3,
              div(
                style = "font-size: 12px; white-space: nowrap;",
                paste("Author:", recordist, " "),
                tags$a("License", href = license_url, target = "_blank")
              )
            ),
            column(
              width = 7,
              shinyWidgets::pickerInput(
                inputId = selectInputId,
                label = NULL,
                choices = response_choices,
                options = list(`live-search` = TRUE)
              )
            ),
            style = "margin-bottom: 3px;"
          ),
          style = "margin-bottom: 8px;"
        )
      })
    }
  })
  
  observe({
    files <- audioFiles()
    if (!is.null(files)) {
      lapply(files, function(file) {
        buttonId <- paste0("play_", gsub("\\.[^.]*$", "", basename(file)))
        observeEvent(input[[buttonId]], {
          
          
          tryCatch({
            if (grepl("\\.wav$", file, ignore.case = TRUE)) {
              audio <- readWave(file)
            } else if (grepl("\\.mp3$", file, ignore.case = TRUE)) {
              audio <- readMP3(file)
            } else {
              stop("Unsupported file type")
            }
            play(audio)
            
          }, error = function(e) {
            showNotification(paste("Error playing audio file:", e$message), type = "error")
            
          })
        })
      })
    }
  }) #audioplayers code
  
  observeEvent(input$submitButton, {
    if (submitted()) {
      showNotification("You have already submitted your answers.", type = "warning")
      return()
    }
    submitted(TRUE)
    
    files <- audioFiles()
    df <- file_info_df()
    current_score <- 0
    
    if (!is.null(files) && !is.null(df) && nrow(df) > 0) {
      results_df <- lapply(1:length(files), function(i) {
        file <- files[i]
        response_id <- paste0("response_", gsub("\\.[^.]*$", "", basename(file)))
        user_choice <- input[[response_id]]
        correct_choice <- df[i, "language_name"]
        latin_name <- df[i, "latin_name"]
        file_name <- df[i, "file.name"]
        
        result_message <- if (user_choice == correct_choice) {
          current_score <- current_score + 1
          "<strong style='color: green;'>Correct</strong>"
        } else {
          paste("<strong style='color: red;'>Incorrect. Correct:", correct_choice)
        }
        
        data.frame(
          Question = paste("Question", i),
          Result = result_message,
          UserChoice = user_choice,
          CorrectChoice = correct_choice,
          LatinName = latin_name,
          FileName = file_name,
          stringsAsFactors = FALSE
        )
      }) %>% dplyr::bind_rows()
      
      output$resultsTable <- renderTable({
        results_df
      }, sanitize.text = function(x) x)
      
      score(current_score)
      output$score <- renderUI({
        if (score() == 10) {
          HTML(paste0("<div style='font-size: 24px; color: green;'><strong>SCORE: ", score(), "/10</strong></div>"))
        } else {
          HTML(paste0("<div style='font-size: 18px;'><strong>SCORE: ", score(), "/10</strong></div>"))
        }
      })
      
      if (input$pseudo != "") {
        classement <- classement_df()
        if (input$pseudo %in% classement$Name) {
          classement$MeanScore[classement$Name == input$pseudo] <- ((classement$MeanScore[classement$Name == input$pseudo] * classement$Tries[classement$Name == input$pseudo] + current_score) / (classement$Tries[classement$Name == input$pseudo] + 1))
          classement$Tries[classement$Name == input$pseudo] <- classement$Tries[classement$Name == input$pseudo] + 1
        } else {
          classement <- rbind(classement, data.frame(Name = input$pseudo, Tries = 1, MeanScore = current_score))
        }
        classement_df(classement)
        write.xlsx(classement, "classement.xlsx")
      }
    } else if (is.null(df) || nrow(df) == 0) {
      output$resultsTable <- renderTable({
        data.frame(Message = "file_info_df is empty, please fill it with data.")
      })
    }
    
    output$results <- renderUI(NULL)
  })
  
  observeEvent(input$resetButton, {
    
    showModal(modalDialog(
      title = "Reset Confirmation",
      strong("Please close any external audio players before resetting.", style = "color: red;"),
      footer = tagList(
        actionButton("confirmReset", "OK")
      )
    ))
    
    observeEvent(input$confirmReset, {
      removeModal() # Close the modal
      
      output$resultsTable <- renderTable({
        NULL
      })
      
      audioFiles(NULL) 
      responses(NULL) 
      file_info_df(data.frame())
      rights(data.frame())
      score(0)
      classement_df(data.frame())
      submitted(FALSE)
      playing_button <- reactiveVal(NULL)
      
    }) #end of confirmReset button
    
  }) #END OF RESET BUTTON
  
  output$classementTable <- renderTable({
    classement_df()
  })
  
  session$onSessionEnded(function(){
    session$close()# Close the shiny session.
    stopApp()# Stops the Shiny app and closes the R process
  })#Close the R console and the R work once the user closes the ui app window
}

  
  
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE)) #launch.browser = FALSE, port = 3838
