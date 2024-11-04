# Cargar librerías
library(shiny)
library(DT)           # Para una tabla interactiva
library(writexl)      # Para exportar a XLSX
library(rio)          # Para importar archivos XLSX de forma sencilla
library(dplyr)        # Para manipulación de datos
library(shinythemes)  # Para aplicar un tema
library(shinyjs)      # Para controlar la interfaz
library(blockrand)    # Para aleatorizar grupos

################################################################################

# Interfaz de la aplicación
ui <- fluidPage(
  
  # Usar shinyjs
  useShinyjs(),  
  
  # Tema de la aplicación
  theme = shinytheme("flatly"),
  
  # Título de la aplicación en la pestaña del navegador
  tags$head(
    tags$title("RandomTeamsApp©"),
    tags$style(
      HTML("
        .custom-title {
          font-family: 'Fredericka the Great', serif;
          text-align: left;
          font-size: 48px;
          font-weight: normal;
          color: black;
          padding: 10px;
          background-color: white;
        }
        
        .custom-subtitle {
          font-family: 'Fredericka the Great', serif;
          text-align: left;
          font-size: 24px;
          font-weight: normal;
          color: black;
          padding: 0px;
          background-color: transparent !important;
          margin-top: 5px;
          margin-bottom: 15px;
        }
          
        .header {
          width: 100%;
          background-color: #f8f9fa;
          padding: 10px;
          text-align: center;
          border-bottom: 1px solid #e7e7e7;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        
        .custom-line-height {
          line-height: 2.5;
        }
        
        .small-math {
          font-size: 0.9em;
        }
        
      ")
    ),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Fredericka+the+Great&display=swap", rel = "stylesheet")
  ),
  
  # Encabezado en la parte superior de la app
  div(
    class = "header",
    div(
      style = "display: flex; align-items: center; margin: 0;",
      actionButton("reset_app", "Restart App", class = "btn btn-danger", style = "margin: 0;"),
      p(style = "margin: 0; margin-left: 20px;", 'Developer:', a("Raúl Hileno, PhD", href = "https://orcid.org/0000-0003-3447-395X", target = "_blank")),
      p(style = "margin: 0; margin-left: 20px;", 'Date: 26/10/2024'),
      p(style = "margin: 0; margin-left: 20px;", 'License:', a("CC BY-NC-ND 4.0", href = "https://creativecommons.org/licenses/by-nc-nd/4.0/", target = "_blank"))
    )
  ),
  
  # Título de la aplicación en la interfaz
  titlePanel(
    div(class = "custom-title", 
        "RandomTeamsApp©")
  ),
  
  # Espacio para mostrar mensajes de estado
  uiOutput("status_message"),
  
  tabsetPanel(
    # Pestaña para cargar jugadores desde un archivo XLSX, XLS o CSV
    tabPanel("Load players",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          fileInput("file", "Open file", accept = c(".xlsx", ".xls", ".csv")),
                          actionButton("load_players_btn", "Load players", class = "btn btn-success")  # Botón para cargar jugadores
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can open a data file from your device in XLSX, XLS, or CSV format."),
                          helpText("After selecting your file, click 'Load players' to add the player information to the data table."),
                          helpText("This step is optional and is useful if you have previously created a data file with the following columns:"),
                          helpText("- ID: the unique identification number assigned to each player."),
                          helpText("- Player: the full name of the player."),
                          helpText("- Sport: the team sport in which the player participates."),
                          helpText("- Team: the name of the team the player is a part of."),
                          helpText("- Category: the age category of the player."),
                          helpText("- Position: the specific position the player holds within the team.")
                 )
               ),
               mainPanel(
                 DTOutput("loaded_player_table")
               )
             )
    ),
    
    # Pestaña para crear jugadores y exportar el registro
    tabPanel("Add players",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          numericInput("player_id_manual", "ID", value = NULL, min = 1, step = 1),
                          textInput("player_name", "Player", ""),
                          selectInput("sport", "Sport", choices = c("Basketball", "Field hockey", "Handball", "Roller hockey", "Rugby", 
                                                                    "Soccer", "Volleyball", "Water polo", "Other")),
                          conditionalPanel(
                            condition = "input.sport == 'Other'",
                            textInput("other_sport", "Enter another sport", "")
                          ),
                          textInput("team_name", "Team", ""),
                          selectInput("age_category", "Category", 
                                      choices = c("Senior", "U-20", "U-18", "U-16", "U-14", "U-12", "U-10", "U-8", "Other"), 
                                      selected = "Senior"),
                          conditionalPanel(
                            condition = "input.age_category == 'Other'",
                            textInput("other_age_category", "Enter another category", "")
                          ),
                          uiOutput("position_ui"),  # "Position"
                          conditionalPanel(
                            condition = "input.position == 'Other'",
                            textInput("other_position", "Enter another position", "")
                          ),
                          actionButton("register", "Add player", class = "btn btn-success")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("create_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_create", "Download data table")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can add a new player to the data table."),
                          helpText("Fill in the required fields and click 'Add player'."),
                          helpText("Ensure that the ID is unique and not already assigned to another player.")
                 )
               ),
               mainPanel(
                 DTOutput("player_table")  # Tabla interactiva con los jugadores
               )
             )
    ),
    
    # Pestaña para actualizar jugadores
    tabPanel("Modify players",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          numericInput("player_id", "Input player ID to modify", value = NA, min = 1, step = 1)),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          numericInput("player_id_edit", "ID", value = NA, min = 1, step = 1),
                          textInput("player_name_edit", "Player", ""),
                          textInput("sport_edit", "Sport", ""),
                          textInput("team_name_edit", "Team", ""),
                          textInput("age_category_edit", "Category", ""),
                          textInput("position_edit", "Position", ""),
                          actionButton("update", "Modify player", class = "btn btn-success"),
                          actionButton("renumber_ids", "Renumber IDs", class = "btn btn-info")),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("modify_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_update", "Download data table")),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can modify the details of an existing player."),
                          helpText("Input the ID of the player you wish to modify."),
                          helpText("After making the necessary changes, click 'Modify player' to save."),
                          helpText("You can also click 'Renumber IDs' to automatically renumber player IDs according to their current position in the table.")
                          
                 )
               ),
               mainPanel(
                 DTOutput("edit_player_table")
               )
             )
    ),
    
    # Pestaña para eliminar jugadores
    tabPanel("Delete players",  
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          textInput("delete_player_id", "ID", value = ""),
                          actionButton("delete", "Delete player/s", class = "btn btn-danger"),
                          actionButton("undo_delete", "Undo delete", class = "btn btn-info")  # Botón para deshacer
                 ),
                 
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("delete_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_delete", "Download data table")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can delete one or more players by their ID number."),
                          helpText("Simply enter the player ID(s), separating multiple IDs with a comma."),
                          helpText("Once you've entered the IDs, click 'Delete player/s' to remove them from the data table."),
                          helpText("If you accidentally delete one or more unwanted players, you can click 'Undo delete' to restore them.")
                 )
               ),
               mainPanel(
                 DTOutput("delete_player_table")  # Tabla interactiva con los jugadores
               )
             )
    ),
    
    # Pestaña para crear grupos aleatorizados
    tabPanel("Randomize groups",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          selectInput("filter_sport_random", "Filter players by sport", choices = NULL),
                          selectInput("filter_team_random", "Filter players by team", choices = NULL),
                          selectInput("filter_category_random", "Filter players by category", choices = NULL),
                          selectInput("filter_position_random", "Filter players by position", choices = NULL),
                          numericInput("num_teams_random", "Number of groups to create", value = 1, min = 1, step = 1),
                          actionButton("randomization_button", "Randomize groups", class = "btn btn-success")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("random_groups_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_random_groups", "Download data table")
                 ),
                 # Añadir helpText para la pestaña de grupos aleatorizados
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can create randomized groups of players based on selected criteria."),
                          helpText("Use the optional filters to specify which players to include in the randomization by sport, team, category, and position."),
                          helpText("For position filtering, 'All positions (unequal distribution)' allows random assignment without balancing positions, while 'All positions (equal distribution)' ensures balanced representation across groups."),
                          helpText("Specify the number of groups you want to create and click 'Randomize groups' to proceed.")
                 )),
               mainPanel(
                 textOutput("num_players_available"),
                 br(),
                 textOutput("name_players_available"),
                 br(),
                 DTOutput("randomization_result")
               )
             )
    )
  )
)

################################################################################

# Servidor de la aplicación
server <- function(input, output, session) {
  
  # Inicializar reactiveValues para almacenar jugadores
  players <- reactiveValues(data = data.frame(ID = integer(),
                                              Player = character(),
                                              Sport = character(),
                                              Team = character(),
                                              Category = character(),
                                              Position = character(),
                                              stringsAsFactors = FALSE),
                            temp_data = NULL)  # Variable para almacenar datos temporales
  
  # Mostrar mensajes de estado
  output$status_message <- renderUI({
    if (exists("message", envir = session$userData)) {
      message_type <- ifelse(grepl("successfully", session$userData$message), "green", "red")
      div(style = paste("color:", message_type, ";"), session$userData$message)
    }
  })
  
  # Definir las posiciones según el deporte seleccionado (para crear jugadores)
  sport_positions <- reactive({
    switch(input$sport,
           "Basketball" = c("Point guard", "Shooting guard", "Small forward", "Power forward", "Center", "Other"),
           "Field hockey" = c("Goalkeeper", 
                              "Left defender", "Right defender",
                              "Left midfielder", "Center midfielder", "Right midfielder",
                              "Left inner", "Right inner",
                              "Left forward", "Center forward", "Right forward", "Other"),
           "Handball" = c("Goalkeeper", "Left back", "Center back", "Right back", "Left wing", "Right wing", "Pivot", "Other"),
           "Roller hockey" = c("Goalie", "Left defender", "Right defender", "Left wing", "Right wing", "Center", "Other"),
           "Rugby" = c("Prop", "Hooker", "Lock", "Flanker", "Number 8", "Scrum half", "Fly half", "Wing", "Centre", "Full back", "Other"),
           "Soccer" = c("Goalkeeper", 
                        "Left back", "Center back", "Right back", 
                        "Defensive midfield", "Central midfield", "Attacking midfield", 
                        "Left wing", "Right wing", 
                        "Striker", "Other"),
           "Volleyball" = c("Setter", "Opposite hitter", "Outside hitter", "Middle blocker", "Libero", "Other"),
           "Water polo" = c("Goalkeeper", "Left flat", "Point", "Right flat", "Left wing", "Right wing", "Hole set", "Other"),
           "Other" = "Other",
           character(0))
  })
  
  # Renderizar el selector de posición para crear jugadores
  output$position_ui <- renderUI({
    if (is.null(input$sport)) return(NULL)
    selectInput("position", "Position", choices = sport_positions())
  })
  
  ###LOAD PLAYERS###
  
  # Función para validar los datos importados
  validate_players_data <- function(data) {
    if (!is.data.frame(data)) {
      stop("The imported file is not a data frame.")
    }
    
    required_columns <- c("ID", "Player", "Sport", "Team", "Category", "Position")
    if (!all(required_columns %in% names(data))) {
      stop(paste("File does not contain the required columns (", paste(required_columns, collapse = ", "), ").", sep = ""))
    }
    
    data <- data %>%
      mutate(
        ID = as.integer(ID),
        Player = as.character(Player),
        Sport = as.character(Sport),
        Team = as.character(Team),
        Category = as.character(Category),
        Position = as.character(Position)
      )
    
    if (any(is.na(data$ID))) {
      stop("The ID column contains NA or non-numeric values.")
    }
    
    if (any(duplicated(data$ID))) {
      stop("The ID column contains duplicate values.")
    }
    
    return(data)
  }
  
  # Lógica del botón para cargar jugadores
  observeEvent(input$load_players_btn, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        players$temp_data <- import(input$file$datapath)
      } else if (ext == "csv") {
        players$temp_data <- read.table(input$file$datapath, header = TRUE, sep = ifelse(grepl(";", readLines(input$file$datapath, n = 1)), ";", ","), stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file type.")
      }
      
      # Preguntar al usuario qué hacer con datos previos
      if (nrow(players$data) > 0) {
        showModal(modalDialog(
          title = "Data already exists",
          "Choose an option: cancel, remove previous players, or append new players.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("remove_players", "Remove"),
            actionButton("append_players", "Append")
          )
        ))
        
        return()
      }
      
      # Validar los datos importados
      players$temp_data <- validate_players_data(players$temp_data)
      
      players$data <- players$temp_data  
      players$data <- players$data[order(players$data$ID), ]
      
      showNotification("Players loaded successfully.", type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Error loading file.", e$message), type = "error", duration = 5)
    })
  })
  
  # Acciones de los botones en el modal
  observeEvent(input$remove_players, {
    # Almacenar los datos existentes para restaurar si es necesario
    previous_data <- players$data
    
    # Eliminar los datos existentes
    players$data <- data.frame(ID = integer(),
                               Player = character(),
                               Sport = character(),
                               Team = character(),
                               Category = character(),
                               Position = character(),
                               stringsAsFactors = FALSE)
    
    # Cerrar el modal
    removeModal()  
    
    # Cargar el nuevo archivo automáticamente después de eliminar
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        players$temp_data <- import(input$file$datapath)  # Cargar el nuevo dataframe
      } else if (ext == "csv") {
        players$temp_data <- read.table(input$file$datapath, header = TRUE, sep = ifelse(grepl(";", readLines(input$file$datapath, n = 1)), ";", ","), stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file type.")  # Manejar el caso de archivo no soportado
      }
      
      # Validar los datos importados
      players$temp_data <- validate_players_data(players$temp_data)
      
      # Ordenar la tabla por ID de menor a mayor
      players$data <- players$temp_data[order(players$temp_data$ID), ]
      
      showNotification("Players loaded successfully after removal.", type = "message", duration = 5)
      
    }, error = function(e) {
      # Restaurar los datos anteriores si hay un error de validación
      players$data <- previous_data
      showNotification(paste("Error loading file after removal.", e$message, "Previous data preserved."), type = "error", duration = 10)
    })
  })
  
  observeEvent(input$append_players, {
    # Cerrar el modal antes de verificar duplicados
    removeModal()
    
    if (any(players$data$ID %in% players$temp_data$ID)) {
      showNotification("Cannot append. Duplicate IDs found.", type = "error", duration = 5)
      return()
    }
    
    players$data <- rbind(players$data, players$temp_data)
    players$data <- players$data[order(players$data$ID), ]
    
    showNotification("Players appended successfully.", type = "message", duration = 5)
  })
  
  # Mostrar la tabla de jugadores cargados
  output$loaded_player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  ###ADD PLAYERS###
  
  # Registrar un nuevo jugador
  observeEvent(input$register, {
    # Verificar si los campos requeridos están completos
    if (is.null(input$player_name) || input$player_name == "" ||
        is.null(input$sport) || input$sport == "" ||
        is.null(input$team_name) || input$team_name == "" ||
        is.null(input$age_category) || input$age_category == "" ||
        is.null(input$player_id_manual) || input$player_id_manual == "") {
      showNotification("Please, fill in the empty fields.", type = "error")
      return()
    }
    
    current_list <- players$data
    
    # Verificar que el ID manual no esté duplicado
    if (any(current_list$ID == input$player_id_manual)) {
      showNotification("The player ID already exists. Please, choose a different ID.", type = "error")
      return()  # Salir de la función si el ID ya existe
    }
    
    # Determinar el deporte correcto
    sport_value <- if (input$sport == "Other" && input$other_sport != "") {
      input$other_sport  # Usar el valor de 'Enter sport' si es 'Other'
    } else {
      input$sport  # Usar el valor del selectInput si no es 'Other'
    }
    
    # Determinar la categoría correcta
    category_value <- if (input$age_category == "Other" && input$other_age_category != "") {
      input$other_age_category  # Usar el valor de 'Enter sport' si es 'Other'
    } else {
      input$age_category  # Usar el valor del selectInput si no es 'Other'
    }
    
    # Determinar la posición correcta
    position_value <- if (input$position == "Other" && input$other_position != "") {
      input$other_position  # Usar el valor de 'Enter position' si es 'Other'
    } else {
      input$position  # Usar el valor del selectInput si no es 'Other'
    }
    
    new_player <- data.frame(
      ID = input$player_id_manual,
      Player = input$player_name,
      Sport = sport_value,
      Team = input$team_name,
      Category = category_value,
      Position = position_value,
      stringsAsFactors = FALSE
    )
    
    players$data <- bind_rows(current_list, new_player)  # Añadir el nuevo jugador a la lista reactiva
    
    # Ordenar la tabla por ID de menor a mayor
    players$data <- players$data[order(players$data$ID), ]
    
    # Mensaje de éxito
    showNotification("Player added successfully.", type = "message")
  })
  
  # Mostrar tabla de jugadores registrados
  output$player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  # Descargar la lista de jugadores en un archivo XLSX desde "Add players" con fecha
  output$download_xlsx_create <- downloadHandler(
    filename = function() {
      # Obtener la fecha seleccionada y formatearla
      selected_date <- format(input$create_date, "%Y_%m_%d")
      paste0("players_list_", selected_date, ".xlsx")
    },
    content = function(file) {
      write_xlsx(players$data, path = file)  # Usar writexl para exportar a XLSX
    }
  )
  
  ###MODIFY PLAYERS###
  
  # Actualizar datos de un jugador existente por ID con confirmación
  observeEvent(input$update, {
    # Verificar si los campos requeridos están completos
    if (is.null(input$player_id_edit) || input$player_id_edit == "" ||
        is.null(input$player_name_edit) || input$player_name_edit == "" ||
        is.null(input$sport_edit) || input$sport_edit == "" ||
        is.null(input$team_name_edit) || input$team_name_edit == "" ||
        is.null(input$age_category_edit) || input$age_category_edit == "" ||
        is.null(input$position_edit) || input$position_edit == "") {
      showNotification("Please, fill in the empty fields.", type = "error")
      return()
    }
    
    current_list <- players$data
    player_id <- input$player_id
    
    # Verificar si el ID existe en la lista antes de proceder
    if (!player_id %in% current_list$ID) {
      showNotification("No player found with the provided ID. Please, select a valid ID.", type = "error")
      return()  # Detener la ejecución si el ID no es válido
    }
    
    # Si el ID es válido, proceder con la confirmación
    showModal(modalDialog(
      title = "Confirm update",
      paste("Are you sure you want to update the information for player with ID", player_id, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_update", "Yes, update")
      )
    ))
  })
  
  # Actualizar inputs según el ID seleccionado
  observeEvent(input$player_id, {
    req(input$player_id)  # Asegurarse de que haya un ID seleccionado
    
    current_list <- players$data
    player_id <- input$player_id
    
    # Verificar si el ID está en la lista de jugadores
    if (player_id %in% current_list$ID) {
      # Obtener la fila del jugador correspondiente al ID seleccionado
      player_data <- current_list[current_list$ID == player_id, ]
      
      # Actualizar los inputs con los datos del jugador
      updateNumericInput(session, "player_id_edit", value = player_data$ID)
      updateTextInput(session, "player_name_edit", value = player_data$Player)
      updateTextInput(session, "sport_edit", value = player_data$Sport)
      updateTextInput(session, "team_name_edit", value = player_data$Team)
      updateTextInput(session, "age_category_edit", value = player_data$Category)
      updateTextInput(session, "position_edit", value = player_data$Position)
    } else {
      # Limpiar los inputs si el ID no es válido
      updateNumericInput(session, "player_id_edit", value = NA)
      updateTextInput(session, "player_name_edit", value = "")
      updateTextInput(session, "sport_edit", value = "")
      updateTextInput(session, "team_name_edit", value = "")
      updateTextInput(session, "age_category_edit", value = "")
      updateTextInput(session, "position_edit", value = "")
      
      # Mostrar notificación de advertencia
      showNotification("No player found with the provided ID.", type = "error")
    }
  })
  
  # Confirmar actualización del jugador
  observeEvent(input$confirm_update, {
    req(input$player_id, input$player_id_edit, input$player_name_edit, input$sport_edit, input$team_name_edit, input$age_category_edit, input$position_edit)
    
    current_list <- players$data
    player_id <- input$player_id
    
    current_list[current_list$ID == player_id, ] <- c(
      input$player_id_edit, 
      input$player_name_edit, 
      input$sport_edit, 
      input$team_name_edit, 
      input$age_category_edit, 
      input$position_edit
    )
    
    players$data <- current_list
    
    # Mostrar notificación de éxito
    showNotification(paste("Player with ID", player_id, "updated successfully."), type = "message")
    
    removeModal() 
  })
  
  # Evento para renumerar IDs
  observeEvent(input$renumber_ids, {
    showModal(modalDialog(
      title = "Confirm renumbering",
      "Are you sure you want to renumber all player IDs according to the current order in the table?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_renumber", "Yes, renumber")
      )
    ))
  })
  
  # Confirmar renumeración de IDs
  observeEvent(input$confirm_renumber, {
    current_list <- players$data
    if (nrow(current_list) > 0) {
      # Obtener el orden actual de la tabla
      current_order <- input$edit_player_table_rows_current
      
      # Renumerar según el orden actual
      players$data <- current_list[current_order, ] %>%
        mutate(ID = row_number())
      
      # Mostrar notificación de éxito
      showNotification("Player IDs have been successfully renumbered.", type = "message")
      
    } else {
      showNotification("No players to renumber.", type = "error")
    }
    
    removeModal()  # Cerrar el modal
  })
  
  
  # Descargar la lista de jugadores en un archivo XLSX desde "Upadate players" con fecha
  output$download_xlsx_update <- downloadHandler(
    filename = function() {
      # Obtener la fecha seleccionada y formatearla
      selected_date <- format(input$modify_date, "%Y_%m_%d")
      paste0("players_list_", selected_date, ".xlsx")
    },
    content = function(file) {
      write_xlsx(players$data, path = file)
    }
  )
  
  # Mostrar la tabla de jugadores para la pestaña modificar
  output$edit_player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  ### DELETE PLAYERS ###
  
  # Lista para almacenar jugadores eliminados para deshacer (como lista de data.frames)
  deleted_players <- reactiveVal(list())
  
  # Evento para eliminar jugadores
  observeEvent(input$delete, {
    # Verificar si el campo delete_player_id está vacío
    if (is.null(input$delete_player_id) || input$delete_player_id == "") {
      showNotification("Please, enter a player ID to delete.", type = "error")
      return()
    }
    
    # Separar los IDs ingresados en un vector
    ids_to_delete <- unlist(strsplit(input$delete_player_id, ","))
    ids_to_delete <- as.numeric(trimws(ids_to_delete))
    
    # Filtrar jugadores que existen en el data frame
    existing_players <- players$data[players$data$ID %in% ids_to_delete, ]
    
    if (nrow(existing_players) == 0) {
      showNotification("No players found with the provided IDs.", type = "error")
    } else {
      showModal(modalDialog(
        title = "Confirm deletion",
        paste("Are you sure you want to delete the following players:", 
              paste(existing_players$ID, collapse = ", "), "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Yes, delete")
        )
      ))
    }
  })
  
  # Confirmar eliminación de jugadores
  observeEvent(input$confirm_delete, {
    req(input$delete_player_id)
    
    # Separar IDs de nuevo
    ids_to_delete <- unlist(strsplit(input$delete_player_id, ","))
    ids_to_delete <- as.numeric(trimws(ids_to_delete))
    
    # Almacenar jugadores eliminados en la lista existente
    current_deleted <- deleted_players()
    deleted_players(c(list(players$data[players$data$ID %in% ids_to_delete, ]), current_deleted))  # Agregar como primer elemento de la lista
    
    # Eliminar jugadores del data frame
    players$data <- players$data[!players$data$ID %in% ids_to_delete, ]
    
    showNotification("Players deleted successfully.", type = "message")
    removeModal()  # Cerrar el modal
  })
  
  # Botón para deshacer eliminación
  observeEvent(input$undo_delete, {
    if (length(deleted_players()) > 0) {
      # Recuperar el último jugador eliminado
      last_deleted <- deleted_players()[[1]]  # Obtener el primer elemento de la lista
      players$data <- rbind(players$data, last_deleted)  # Agregar al data frame
      
      # Eliminar el jugador de la lista de eliminados
      deleted_players(deleted_players()[-1])  # Eliminar el primer elemento de la lista
      
      # Ordenar por ID
      players$data <- players$data[order(players$data$ID), ]
      showNotification("Player restored successfully.", type = "message")
    } else {
      showNotification("No players to undo deletion.", type = "error")
    }
  })
  
  # Descargar la lista de jugadores en un archivo XLSX desde "Delete players" con fecha
  output$download_xlsx_delete <- downloadHandler(
    filename = function() {
      # Obtener la fecha seleccionada y formatearla
      selected_date <- format(input$delete_date, "%Y_%m_%d")
      paste0("players_list_", selected_date, ".xlsx")
    },
    content = function(file) {
      write_xlsx(players$data, path = file) 
    }
  )
  
  # Mostrar la tabla de jugadores para la pestaña eliminar
  output$delete_player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  ###RANDOMIZATION###
  
  # Crear una variable reactiva para almacenar los equipos combinados
  randomized_teams <- reactiveVal()
  
  # Actualizar selectores de filtro dinámicamente
  observe({
    updateSelectInput(session, "filter_sport_random", 
                      choices = c("All sports", sort(unique(players$data$Sport))))
    updateSelectInput(session, "filter_team_random", 
                      choices = c("All teams", sort(unique(players$data$Team))))
    updateSelectInput(session, "filter_category_random", 
                      choices = c("All categories", sort(unique(players$data$Category))))
    
    # Agregar opciones específicas para considerar o no las posiciones de juego
    updateSelectInput(session, "filter_position_random", 
                      choices = c("All positions (unequal distribution)", 
                                  "All positions (equal distribution)", 
                                  sort(unique(players$data$Position))))
  })
  
  # Mostrar número de jugadores disponibles
  output$num_players_available <- renderText({
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    
    # Filtrar según la posición seleccionada
    if (input$filter_position_random != "All positions (unequal distribution)") {
      if (input$filter_position_random != "All positions (equal distribution)") {
        filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
      }
    }
    
    num_players <- nrow(filtered_data)
    paste("Number of players available for randomization:", num_players)
  })
  
  # Mostrar lista de nombres de jugadores disponibles
  output$name_players_available <- renderText({
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    
    # Filtrar según la posición seleccionada
    if (input$filter_position_random == "All positions (unequal distribution)") {
      # No se aplica ningún filtro
    } else if (input$filter_position_random != "All positions (equal distribution)") {
      filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
    }
    
    # Obtener la lista de nombres
    player_names <- filtered_data$Player  # Cambia 'Player' al nombre de la columna que contiene los nombres
    
    # Ordenar los nombres alfabéticamente por nombre y luego por apellido si los nombres coinciden
    if (length(player_names) > 0) {
      # Separar el nombre y el apellido en dos columnas temporales
      name_parts <- strsplit(player_names, " ")
      first_names <- sapply(name_parts, `[`, 1)  # Obtener el primer nombre
      last_names <- sapply(name_parts, function(x) if (length(x) > 1) x[length(x)] else "")  # Obtener el apellido, si existe
      
      # Ordenar primero por nombre y luego por apellido
      sorted_indices <- order(first_names, last_names)
      sorted_names <- player_names[sorted_indices]
      
      paste("Name of the players available for randomization:", paste(sorted_names, collapse = ", "))
    } else {
      "Name of the players available for randomization: no name available"
    }
  })
  
  # Lógica para aleatorizar
  observeEvent(input$randomization_button, {
    req(players$data)
    
    # Filtrar datos según selecciones del usuario
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    
    # Filtrar según la posición seleccionada
    if (input$filter_position_random == "All positions (unequal distribution)") {
      # No se aplica ningún filtro
    } else if (input$filter_position_random != "All positions (equal distribution)") {
      filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
    }
    
    # Número de jugadores en datos filtrados
    num_players <- nrow(filtered_data)
    
    # Validar el valor máximo de equipos basado en la cantidad de jugadores
    updateNumericInput(session, "num_teams_random", 
                       min = 1,
                       max = num_players,
                       value = min(input$num_teams_random, num_players))
    
    # Mostrar advertencia si el número de equipos es mayor al número de jugadores
    if (input$num_teams_random > num_players) {
      showNotification("Number of groups cannot exceed the number of players available for randomization.", type = "error")
      return()
    }
    
    # Mostrar advertencia si el número de equipos es 0 o menos
    if (input$num_teams_random < 1) {
      showNotification("Number of groups must be at least 1.", type = "error")
      return()
    } 
    
    # Número de equipos
    num_teams <- input$num_teams_random
    
    # Si se selecciona 'All positions (equal distribution)' en el filtro 'Position', se hace el sorteo distinguiendo la posición de juego de cada jugador
    if (input$filter_position_random == "All positions (equal distribution)") {
      # Crear una lista de posiciones
      position_list <- split(filtered_data, filtered_data$Position)
      
      # Verificar si hay suficientes jugadores para cada posición
      for (position in names(position_list)) {
        if (nrow(position_list[[position]]) < num_teams) {
          # Mostrar mensaje de error si no hay suficientes jugadores para la posición
          showNotification(paste("At least one player from each position is needed to do this type of random."), type = "error")
          return(NULL)  # Salir de la función si hay un error
        }
      }
      
      # Crear lista de equipos vacíos
      team_list <- vector("list", num_teams)
      for (i in seq_along(team_list)) {
        team_list[[i]] <- data.frame()  # Inicializar equipos vacíos
      }
      
      # Asignar jugadores de manera equitativa a cada equipo
      for (position in names(position_list)) {
        players_for_position <- position_list[[position]]
        player_indices <- sample(1:nrow(players_for_position))
        team_sizes <- rep(floor(nrow(players_for_position) / num_teams), num_teams)
        remainder <- nrow(players_for_position) %% num_teams
        
        if (remainder > 0) {
          extra_teams <- sample(1:num_teams, remainder)
          team_sizes[extra_teams] <- team_sizes[extra_teams] + 1
        }
        
        # Asignación a cada equipo
        index <- 1
        for (i in seq_along(team_sizes)) {
          team_list[[i]] <- rbind(team_list[[i]], players_for_position[player_indices[index:(index + team_sizes[i] - 1)], ])
          index <- index + team_sizes[i]
        }
      }
    } else {
      # Asignación aleatoria de jugadores a los equipos de forma equitativa
      player_indices <- sample(1:num_players)  # Mezclar índices de jugadores
      team_sizes <- rep(floor(num_players / num_teams), num_teams)  # Tamaño base para cada equipo
      remainder <- num_players %% num_teams  # Jugadores adicionales a distribuir
      
      # Distribuir jugadores adicionales aleatoriamente entre los equipos
      if (remainder > 0) {
        extra_teams <- sample(1:num_teams, remainder)
        team_sizes[extra_teams] <- team_sizes[extra_teams] + 1
      }
      
      # Crear equipos según los tamaños calculados
      team_list <- split(filtered_data[player_indices, ], rep(1:num_teams, times = team_sizes))
    }
    
    # Ordenar cada equipo por la columna ID o por la columna Position dependiendo de la opción seleccionada
    if (input$filter_position_random == "All positions (equal distribution)") {
      team_list <- lapply(team_list, function(team) {
        team[order(team$Position), ]  # Ordenar por Position
      })
    } else {
      team_list <- lapply(team_list, function(team) {
        team[order(team$ID), ]  # Ordenar por ID
      })
    }
    
    
    # Crear un datatable para imprimir los resultados
    output$randomization_result <- renderDT({
      # Combinar los equipos en un solo data frame
      combined_teams <- do.call(rbind, lapply(seq_along(team_list), function(i) {
        team <- team_list[[i]]
        team$`Assigned group` <- paste("Group", i)
        return(team)
      }))
      
      # Almacenar los equipos combinados en la variable reactiva
      randomized_teams(combined_teams)
      
      # Mostrar la tabla como datatable
      datatable(combined_teams, 
                options = list(
                  headerCallback = JS(c("function(thead, data, start, end, display){",
                                        "  $('th', thead).css('border-top', '1px solid #b3b3b3');","}")),
                  autoWidth = TRUE,
                  buttons = list(),
                  lengthChange = TRUE,
                  searching = TRUE,
                  info = TRUE,
                  paging = TRUE,
                  pageLength = nrow(combined_teams),
                  lengthMenu = list(c(5, 10, 15, 20, 25, nrow(combined_teams)), 
                                    c(5, 10, 15, 20, 25, "All")),
                  language = list(
                    search = "<i class='glyphicon glyphicon-search'></i>"
                  ),
                  columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  
                ),
                rownames = FALSE)
    })
    
    # Mostrar notificación de éxito
    showNotification("Randomization completed successfully.", type = "message")
  })
  
  # Observar los cambios en los selectores de filtro para actualizar el valor máximo de num_teams_random
  observe({
    # Obtener el número de jugadores filtrados al aplicar filtros
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    if (input$filter_position_random == "All positions (unequal distribution)") {
      # No se aplica ningún filtro
    } else if (input$filter_position_random != "All positions (equal distribution)") {
      filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
    }
    
    # Actualizar el valor máximo de num_teams_random
    num_players <- nrow(filtered_data)
    updateNumericInput(session, "num_teams_random", max = num_players)
  })
  
  # Descargar los grupos aleatorizados en un archivo XLSX desde "Randomize groups" con fecha
  output$download_xlsx_random_groups <- downloadHandler(
    filename = function() {
      # Obtener la fecha seleccionada y formatearla
      selected_date <- format(input$random_groups_date, "%Y_%m_%d")
      paste0("groups_randomized_", selected_date, ".xlsx")
    },
    content = function(file) {
      write_xlsx(randomized_teams(), path = file)  # Usar la variable reactiva
    }
  )
  
  ###RESTART APP###
  
  # Reiniciar la aplicación (eliminar todos los registros)
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Restart the application",
      "Are you sure you want to restart the application? This will delete all players.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_restart", "Yes, restart")
      )
    ))
  })
  
  # Confirmar reinicio de la aplicación
  observeEvent(input$confirm_restart, {
    # Reiniciar los jugadores a un dataframe vacío
    players$data <- data.frame(ID = integer(),
                               Player = character(),
                               Sport = character(),
                               Team = character(),
                               Position = character(),
                               stringsAsFactors = FALSE)
    
    # Reiniciar inputs a sus valores por defecto
    updateNumericInput(session, "player_id_manual", value = "")
    updateTextInput(session, "player_name", value = "")
    updateSelectInput(session, "sport", selected = "Basketball")
    updateTextInput(session, "other_sport", value = "")
    updateTextInput(session, "team_name", value = "")
    updateSelectInput(session, "age_category", selected = "Senior")
    updateTextInput(session, "other_age_category", value = "")
    updateSelectInput(session, "position", selected = NULL)
    updateTextInput(session, "other_position", value = "")
    
    updateNumericInput(session, "player_id", value = "")
    updateNumericInput(session, "player_id_edit", value = "")
    updateTextInput(session, "player_name_edit", value = "")
    updateTextInput(session, "sport_edit", value = "")
    updateTextInput(session, "team_name_edit", value = "")
    updateTextInput(session, "age_category_edit", value = "")
    updateTextInput(session, "position_edit", value = "")
    
    updateTextInput(session, "delete_player_id", value = "")
    
    updateSelectInput(session, "filter_sport_random", selected = NULL)
    updateSelectInput(session, "filter_team_random", selected = NULL)
    updateSelectInput(session, "filter_category_random", selected = NULL)
    updateSelectInput(session, "filter_position_random", selected = NULL)
    updateNumericInput(session, "num_teams_random", value = 1)
    
    # Actualizar la tabla de randomización
    output$randomization_result <- renderDT({
      combined_teams <- data.frame()  # Crear un data frame vacío si no hay jugadores
      datatable(combined_teams,
                options = list(
                  headerCallback = JS(c("function(thead, data, start, end, display){",
                                        "  $('th', thead).css('border-top', '1px solid #b3b3b3');","}")),
                  autoWidth = TRUE,
                  buttons = list(),
                  lengthChange = TRUE,
                  searching = TRUE,
                  info = TRUE,
                  paging = TRUE,
                  pageLength = nrow(combined_teams),
                  lengthMenu = list(c(5, 10, 15, 20, 25, nrow(combined_teams)), 
                                    c(5, 10, 15, 20, 25, "All")),
                  language = list(
                    search = "<i class='glyphicon glyphicon-search'></i>"
                  ),
                  columnDefs = list(list(className = 'dt-left', targets = "_all"))
                ),
                rownames = FALSE)
    })
    
    # Reiniciar la lista de eliminaciones
    deleted_players(list())
    
    # Mostrar mensaje de estado
    showNotification("Application has been restarted successfully.", type = "message", duration = 5)
    
    # Cerrar la ventana emergente
    removeModal()
  })
}

################################################################################

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
