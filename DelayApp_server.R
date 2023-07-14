rf_model = readRDS("D:/3RD YEAR/SEMESTER II/ST3082/Project II Materials/rf_model.rds")
#Importing datasets
data1 = read.csv('D:/3RD YEAR/SEMESTER II/ST3082/Project II Materials/data1.csv')
data2 = read.csv('D:/3RD YEAR/SEMESTER II/ST3082/Project II Materials/data2.csv')

data1$Route_New = factor(data1$Route_New,level=c("Regular and limited service routes","Blue Night Routes",
                                                 "Express Routes","Others"))
data1$Day = factor(data1$Day,level=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data1$Month = factor(data1$Month,level=c("January","February","March","April","May","June"))
data1$Incident_New = factor(data1$Incident_New,level=c("Cleaning - Unsanitary","Diversion","General Delay","Mechanical","Security","Vision",
                                                       "Collision - TTC","Emergency Services","Investigation","Operations - Operator",
                                                       "Road Blocked","Utilized Off Route","Others"))
data1$Direction=factor(data1$Direction,level=c("N","S","E","W","B"))
data1$is_Weekday=factor(data1$is_Weekday,level=c("Weekday","Weekend"))
data1$Hour=as.factor(data1$Hour)

data2$Route_New = factor(data2$Route_New,level=c("Regular and limited service routes","Blue Night Routes",
                                                 "Express Routes","Others"))
data2$Day = factor(data2$Day,level=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data2$Month = factor(data2$Month,level=c("January","February","March","April","May","June"))
data2$Incident_New = factor(data2$Incident_New,level=c("Cleaning - Unsanitary","Diversion","General Delay","Mechanical","Security","Vision",
                                                       "Collision - TTC","Emergency Services","Investigation","Operations - Operator",
                                                       "Road Blocked","Utilized Off Route","Others"))
data2$Direction=factor(data2$Direction,level=c("N","S","E","W","B"))
data2$is_Weekday=factor(data2$is_Weekday,level=c("Weekday","Weekend"))
data2$Hour=as.factor(data2$Hour)


server = function(input, output, session) {
  #React value when using the action button
  a = reactiveValues(result = NULL)
  
  observeEvent(input$cal, {
    
    
    if((input$p_day=="Saturday") | (input$p_day=="Sunday") ) {
      is_Weekday="Weekend"
    }else{
      is_Weekday="Weekday"
    }
    
    
    #Dataframe for the single prediction
    values = data.frame(    Route_New = sub("\\s*\\(.*\\)", "",input$p_route),
                            #Direction= sub("-.*", "", input$p_direction),
                            Direction= sub("\\s*(\\w).*", "\\1", input$p_direction),
                            Incident_New = input$p_incident,
                            is_Weekday = is_Weekday,
                            Day=input$p_day,
                            Hour =input$p_hour,
                            Month = input$p_month,
                            Min.Gap = input$p_gap
                            
                            
                            
    )
    #Copy of the test data without the dependent variable
    data2_new= data2[,-2]
    #Include the values into the new data
    data2_new = rbind(data2_new,values)
    data2_new[,2]=as.numeric(scale(data2_new[,2], center =TRUE, scale =TRUE))
    
    
    #Single preiction using the randomforest model
    a$result =  predict(rf_model, 
                            newdata = data2_new[nrow(data2_new),])
    a$result=paste(as.integer(a$result),"minutes")
  })
  
  output$value <- renderText({
    #Display the prediction value
    paste(a$result)
  })
  
  
  
}
