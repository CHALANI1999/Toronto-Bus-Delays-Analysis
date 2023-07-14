
ui = dashboardPage(
  dashboardHeader(
    disable = TRUE
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Add logo and title above the background image
    
    
    
    # Set background image
    
    tags$img(
      #src = "https://png.pngtree.com/thumb_back/fh260/background/20210403/pngtree-simple-white-flat-vectors-with-diagonal-cuts-decorative-backdrops-for-your-image_597612.jpg",
      src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcReuphgqOT1ON5TPVpSpSrd6WVndmj_VH9lsw&usqp=CAU",
      
      style = "position: absolute; top: 0; left: 0; right: 0; bottom: 0; width: 100%; height: 100%; object-fit: cover; filter: blur(5px);",
      alt = "Background Image",
      fluidRow(
        column(width = 12, align = "center", 
               img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/TTC.svg/2560px-TTC.svg.png", height = "100px")),
        fluidRow(
          column(width = 12, align = "center",
                 h3(HTML('<strong>TIME TO SAVE TIME</strong>'))
          ))
      ),
      tags$head(tags$style(HTML('
    /* change font color of all text to white */
    body {
      color: black;
    }
    /* change color of box headings to white */
    .box-header { color: red; }
    .box .box-body .help-block {
      color: black;
    }
  '))),
      # Set background color to transparent for all boxes
      tags$style(HTML(".box { background-color: rgba(0, 0, 0, 0); }")),
      
      box(
        title = 'Categorical variables',
        status = 'primary', width = 12,
        style = "color: black",
        fluidRow(
          column(width = 4, offset = 0,
                 selectInput(
                   'p_route', 'Route',
                   c(
                     "Regular and limited service routes(7-189)", "Blue Night Routes(300 -399)", "Express Routes(900-999)", "Community and other routes"
                   )
                 )
          ),
          column(width = 2,
                 selectInput(
                   'p_direction', 'Direction of the bus',
                   c(
                     "N - North","S - South","E - East","W - West","B - (Both E&W OR Both N&S)"
                   )
                 )
          ),
          column(width = 2,
                 selectInput(
                   'p_incident', 'Incident occured',
                   c(
                     "General Delay","Mechanical","Cleaning - Unsanitary","Diversion","Security","Vision",
                     "Collision - TTC","Emergency Services","Investigation","Operations - Operator",
                     "Road Blocked","Utilized Off Route","Others"
                   )
                 )
          ),
          column(width = 2,
                 selectInput(
                   'p_month', 'Month',
                   c(
                     "January","February","March","April","May","June"
                   )
                 )
          ),
          column(width = 2,
                 selectInput(
                   'p_day', 'Day of the Week',
                   c(
                     "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"
                   )
                 )
          ),
          
          column(width = 2,
                 selectInput(
                   'p_hour', 'Hour of the day',
                   c(
                     "0", "1", "2", "3", "4", "5", "6",
                     "7", "8", "9", "10", "11", "12",
                     "13", "14", "15", "16", "17", "18",
                     "19", "20", "21", "22", "23"
                   )
                 )
          )
        )
      ),
      # Filters for numeric variables
      box(
        title = 'Numerical variables',
        status = 'primary', width = 12,
        style = "color: black",
        fluidRow(
          column(width = 4, offset = 0,
                 numericInput(
                   'p_gap', 'Time gap with the next bus(In minutes)', min = 1,max=250, value = 10
                 )
          )
        )
      ),
      # Box to display the prediction results
      box(
        title = 'Prediction result', status = 'success', solidHeader = FALSE,
        width = 4, height = 100,
        style = "color: black",
        fluidRow(
          column(width = 12,
                 h5(HTML('<strong>Predicted Delay</strong>'))
          ),
          column(width = 12,
                 verbatimTextOutput("value", placeholder = TRUE)
          )
        ),
        fluidRow(
          column(width = 12, align = "right",
                 actionButton('cal','Calculate', icon = icon('calculator'))
          )
        )
      ),
      box(title = 'Model explanation', status = 'success', width = 8, height = 260,
          helpText(HTML('<strong>This model will predict the total minutes of possible delay relevant to the buses functioning under the Toronto Transit Commission for a specific day of the week, hour, route type, direction, month, and time gap between two scheduled buses, in order to facilitate the ridership of the TTC passengers.</strong>')),
          helpText(HTML('<strong>The dataset used to train the model is "Toronto Bus Delays 2022 dataset", taken from the Kaggle website.</strong>'))
      )
    )
  )
)


