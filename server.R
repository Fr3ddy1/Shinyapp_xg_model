shinyServer(function(input, output,session) {

  #////////////////////////#
  #/// GUIA INTERACTIVA ///#
  #////////////////////////#
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Siguiente",
                                               "prevLabel"="Regresar",
                                               "skipLabel"="Salir")
                       )
  )
  

  #PASOS - BIENVENIDA
  steps_welcome <- reactive(data.frame(
    element=c("#menu1", "#wel"),
    intro=c("Sección de Bienvenida","Bienvenido!!!"),
    position=c("bottom", "bottom")
  ))
  
  #PASOS - MODELOS
  steps_e1 <- reactive(data.frame(
    element=c("#menu2", "#e1","#uno","#tres"),
    intro=c("Sección modelos de predicción","Modelo goles esperados","Nombre del modelo de la sección",
            "Entradas del modelo"),
    position=c("bottom", "bottom","bottom","bottom")
  ))
  
  
  #PASOS - ACERCA 
  steps_acerca <- reactive(data.frame(
    element=c("#menu4", "#inf"),
    intro=c("Sección de información","Información sobre el desarrollador"),
    position=c("bottom", "bottom")
  ))
  
  #BOTON
  boton <- reactive({
    if(input$tabs=="welcome"){
      return(steps_welcome())
    }else if(input$tabs=="etapas_logit_1"){
      return(steps_e1())
    }else if(input$tabs=="acerca"){
      return(steps_acerca())
    }else{}
    
    
  })
  
  #DEFINO BOTON
  observeEvent(input$help_menu, { introjs(session, options=list("nextLabel"="Siguiente",
                                                                "prevLabel"="Regresar",
                                                                "skipLabel"="Salir",
                                                                "doneLabel"="Aceptar",steps=boton())) })
  

  # uses 'helpfiles' directory by default
  # in this example, we use the withMathJax parameter to render formulae
  observe_helpers(withMathJax = TRUE)

  #///////////////#
  #/// MODELOS ///#
  #///////////////#
  
  #/////////////////#
  #/// MODELO XG ///#
  #/////////////////#
  
  #APOYO NIVELES TRES VAR CATEGORICAS
  #FUNCION QUE ME DEVUELVES NIVELES DE PAISES Y SECTORES
  niveles <- reactive({
    #CARGO DATA
    df <- read.csv(paste0(getwd(),"/data_raw/Niveles_cat.csv"))
    a1 <- as.character(unique(as.factor(df$nombre[which(df$var == "position.id")])))
    a2 <- as.character(unique(as.factor(df$nombre[which(df$var == "shot.body_part.id")])))
    #a3 <- as.character(unique(as.factor(df$nombre[which(df$var == "shot.technique.id")])))
    
    le <- list(a1,a2)
    return(le)
  })
  
  #NIVELES2
  niveles2 <- reactive({
    
    
    if (length(input$input_tec) != 0) {
    #a2 <- ifelse(input$input_tec != "Palomita",c("Pie izquierdo","Pie derecho","Otro"),c("Cabeza"))
    if(input$input_tec == "Taco" | input$input_tec == "Chilena" | 
       input$input_tec == "Media volea" | input$input_tec == "Volea" |
       input$input_tec == "Colocado" ) {
      a2 <- c("Pie derecho","Pie izquierdo")
    }else if(input$input_tec == "Palomita"){
      a2 <- c("Cabeza")
    }else if(input$input_tec == "Normal"){
      a2 <- c("Otro","Cabeza","Pie derecho","Pie izquierdo")
    }
    
    # if(input$input_tec == "Palomita"){
    #   
    #   a2 <- c("Cabeza")
    # }
      
    }else{
      a2 <- c(" ")
      
    }
    
      
   
    
    #SALIDA
    #le <- list(a2)
    return(a2)
    
  })
  
  #ALTERNATIVA
  observe({
    updateSelectInput(session, "input_body", choices = niveles2())
  })
  
  
  #DEFINO BOTONES A USAR COMO INPUTS
  output$input_etapa_l1 <- renderUI({ 
    
    box(id="tres",width=12,title="Entradas del modelo",status="primary",solidHeader=TRUE ,
        
        column(width = 4,
               numericInput("input_locx", "Coordenada x disparo:", 111, min = 0, max = 119) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada x del campo donde el disparo fué efectuado, para más detalles ir a la sección de Bienvenida."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               numericInput("input_locy", "Coordenada y disparo:", 35, min = 0, max = 80) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada y del campo donde el disparo fué efectuado, para más detalles ir a la sección de Bienvenida."
                                 ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               numericInput("input_locxGk", "Coordenada x Arquero:", 119, min = 0, max = 119) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada x del campo donde se ubica el arquero, para más detalles ir a la sección de Bienvenida. Favor tener cuidado al ingresar 
                                    coordenadas que indiquen que el arquero está muy lejos de la arquería, los resultados pueden ser imprecisos."
                         ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               numericInput("input_locyGk", "Coordenada y Arquero:", 40, min = 0, max = 62) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada y del campo donde se ubica el arquero, para más detalles ir a la sección de Bienvenida. Favor tener cuidado al ingresar 
                                    coordenadas que indiquen que el arquero está muy lejos de la arquería, los resultados pueden ser imprecisos."
                                  ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               selectInput(inputId = "input_pos",label =  "Posición:", choices = niveles()[[1]],selected = "Delantero centro - ST") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor indique la posición del jugador, para más información ver la descripción en la sección de bienvenida."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        ##
        column(width = 4,
               selectInput(inputId = "input_tec",label =  "Técnica de disparo :", choices = c("Normal","Media volea","Volea","Colocado","Palomita","Taco","Chilena")) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor indique la técnica con la cual el jugador remata, para más detalles ir a la sección de Bienvenida."
                      ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        
        column(width = 4,
               selectInput(inputId = "input_bajo_pres",label =  "Bajo presión :", choices = c("Si","No"),selected = "No") %>% 
                  helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor indique si el jugador está o no bajo presión, para más detalles ir a la sección de Bienvenida."
                         ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               selectInput(inputId = "input_first_time",label =  "Disparo de primera:", choices = c("Si","No"),selected = "No") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor indique si el jugador disparó de primera, es decir sin controlar previamente el balón, para más detalles ir a la sección de Bienvenida."
                          ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        column(width = 4,
               selectInput(inputId = "input_body",label =  "Parte del cuerpo :", choices = niveles()[[2]],selected = "Pie derecho") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor indique la parte del cuerpo con la cual el jugador remata, para más detalles ir a la sección de Bienvenida."
                                ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 12,
               actionButton("boton_e1_log", "Calcular",
                            style="color: #fff; background-color: #04B404; border-color: #04B404")
        )

        
    ) # final box

  })
  
  #DEFINO INFORMACION A PRESENTAR
  observeEvent(input$boton_e1_log, {
    output$tabla_etapa_l1 <-  renderUI(
        box(title = "Resultados",style="overflow-x:scroll",width = 12,status="primary",solidHeader=TRUE,
      
          h4("Advertencias:")  %>% 
            helper(type = "inline",
                   title = "Descripción",
                   content = c("Esta sección indicará una advertencia en caso que
                               los inputs ingresados estén fuera de rango, es importante
                               tener en cuenta que aún en caso de tener valores fuera
                               de rango el modelo arrojará resultado, sin embargo el resultado
                               será impreciso."
                              ),
                   buttonLabel = "Entendido!",
                   easyClose = FALSE,
                   fade = TRUE,
                   size = "m"),
          verbatimTextOutput("adv_etapa_l1"),
          #VARIABLES CREADAS INTERNAMENTE
          h4("Variables calculadas internamente:")  %>% 
            helper(type = "inline",
                   title = "Descripción",
                   content = c("En esta sección se muestran las variables generadas internamente
                               a partir de las coordenadas del remate y del arquero, para más detalle ver
                               la sección de bienvenida."
                             ),
                   buttonLabel = "Entendido!",
                   easyClose = FALSE,
                   fade = TRUE,
                   size = "m"),
          DTOutput("result_etapa_l1"),
          
           h4("xG obtenido:")  %>% 
            helper(type = "inline",
                   title = "Descripción",
                   content = c("En esta sección se muestran el xG obtenido, es decir, la probabilidad de que
                               con las condiciones dadas el remate termine en gol."
                     ),
                   buttonLabel = "Entendido!",
                   easyClose = FALSE,
                   fade = TRUE,
                   size = "m"),
          verbatimTextOutput("prob_etapa_l1"),
          h4("Ubicación disparo y arquero:")  %>% 
            helper(type = "inline",
                   title = "Descripción",
                   content = c("En esta sección se muestra un campo de futbol con las ubicaciones
                               del disparo y arquero consideradas."
                      ),
                   buttonLabel = "Entendido!",
                   easyClose = FALSE,
                   fade = TRUE,
                   size = "m"),
          
          plotOutput("graf_etapa_l1"),
         
      )#final box
    )
  }) #final observeEvent
  
  #ADVERTENCIAS KNN
  output$adv_etapa_l1 <- renderText({
    
    #agrego dependencia 
    input$boton_e1_log
    #
    isolate({ 
      a <- c(
             input$input_locx, input$input_locy, input$input_locxGk,
             input$input_locyGk
      )
      
      #print(a)
      #CASO COODERNADAS BALON
      n <- 0
      m <- " "
      #X
      if(121 < a[1] | a[1] < 0){
        m <- paste(m,"Variable coordenada x fuera de rango",sep = " || ")
        n <- n+1
      }
      
      #X
      if(80 < a[2] | a[2] < 0){
        m <- paste(m,"Variable coordenada y fuera de rango",sep = " || ")
        n <- n+1
      }
      
      #CASO COODERNADAS ARQUERO
      #X
      if(121 < a[3] | a[3] < 0){
        m <- paste(m,"Variable coordenada x arquero fuera de rango",sep = " || ")
        n <- n+1
      }
      
      #X
      if(80 < a[4] | a[4] < 0){
        m <- paste(m,"Variable coordenada y arquero fuera de rango",sep = " || ")
        n <- n+1
      }
      
      #IF FINAL
      if (n!=0) {
        print(m)
      }else{
        print("No hay advertencias que mostrar")
      }
      
      
    })#final isolate  
    
  })
  
  #ME DEVUELVE VARIABLES CREADAS INTERNAMENTE
  output$result_etapa_l1 <- renderDT({ 
    
    #agrego dependencia 
    input$boton_e1_log
    #
    isolate({ 
      
      Valor <-  c(
                  input$input_locx, input$input_locy, input$input_locxGk,
                  input$input_locyGk
      )
      
     
      #PUNTO FIJO
      p1 <- c(120,40)
      
      #
      p2 <- c(120,36)
      p3 <- c(120,44)
      
      #
      l1 <- sqrt((Valor[1] - p2[1])**2+((80 - Valor[2])-p2[2])**2)
      l2 <- sqrt((Valor[1] - p3[1])**2+((80 - Valor[2])-p3[2])**2)
      
      #VARIABLES INTERNAS
      DistToGoal <- sqrt((Valor[1]-p1[1])**2+(Valor[2]-p1[2])**2)
      DistToKeeper <- sqrt((Valor[3]-p1[1])**2+(Valor[4]-p1[2])**2)
      shot.angle <- acos((l1**2 + l2**2 - 8**2)/(2*l1*l2))*180/pi
      
      #CREO DF
      df <- as.data.frame(matrix(NA,nrow = 3,ncol = 2))
      names(df) <- c("Variable","Valor")
      
      df$Variable <- c("Distancia a gol","Distancia al arquero","Ángulo de tiro")
      df$Valor <- c(DistToGoal,DistToKeeper,shot.angle)
      df$Valor <- round(df$Valor,2)
      
      print(df)
      
    }) #final Isolate
    
  })
  
 
  #DEVUELVO PROBABILIDADES DEL MODELO
  output$prob_etapa_l1 <- renderText({ 
    
    #agrego dependencia 
    input$boton_e1_log
    #
    isolate({ 
      
      withProgress(message = 'Calculando probabilidad ...', value = 1/2, {
      modelo <- readRDS(paste0(getwd(),"/data_raw/xg_model_FT_caret_inicio.rds"))
      
      
      #TRABAJO IMPUT
      a <- c(input$input_bajo_pres,
             input$input_pos,input$input_first_time,
             input$input_body,input$input_tec,input$input_locx,
             input$input_locy, input$input_locxGk,
             input$input_locyGk

      )

      #PASO A NUMERICOS VALORES CATEGORICOS
      #VARIABLES BINARIAS
      a[1] <- ifelse(a[1]=="Si",1,0)
      a[3] <- ifelse(a[3]=="Si",1,0)

      #VARIABLES CON DESCRIPCION
      #CARGO BASE
      val <- read.csv(paste0(getwd(),"/data_raw/Niveles_cat.csv"))
      
      #CREO DF AUX
      a1 <- as.data.frame(a)
      names(a1) <- "nombre"
      
      #MERGE
      a1 <- left_join(a1,val[,c(5,1)],by="nombre")
      a1
      
      #ACTUALIZO
      a[c(2,4,5)] <- a1$id[c(2,4,5)]
      
      #
      a <- t(as.data.frame(as.numeric(a)))
      a <- as.data.frame(a)

      #AGREGO DISTANCIAS
      p1 <- c(120,40)
      
      names(a) <- c("under_pressure"  ,    
                    "position.id", "shot.first_time" ,    
                    "shot.body_part.id" ,   "shot.technique.id"  ,  "location.x"    ,      
                    "location.y"   ,   "location.x.GK" ,   "location.y.GK")
      
      
      a$DistToGoal <- sqrt((a$location.x-p1[1])**2+(a$location.y-p1[2])**2)
      a$DistToKeeper <- sqrt((a$location.x.GK-p1[1])**2+(a$location.y.GK-p1[2])**2)

      #
      p2 <- c(120,36)
      p3 <- c(120,44)
      
      #
      l1 <- sqrt((a$location.x - p2[1])**2+((80 - a$location.y)-p2[2])**2)
      l2 <- sqrt((a$location.x - p3[1])**2+((80 - a$location.y)-p3[2])**2)
      
      
      a$shot.angle <- acos((l1**2 + l2**2 - 8**2)/(2*l1*l2))*180/pi
      
      #FILTRO COORDENADAS
      a <- a[-c(6:9)]
  
      #APLICO MODELO
      pred <- predict(modelo, newdata = as.matrix(a), type = "prob",reshape = TRUE)$x1
      
      #OUTPUT
      print(pred)
      
      }) #FINAL BARRA PROGRESO
      
    }) #final Isolate
    
  })
  
  
  #GRAFICO DEL CAMPO
  output$graf_etapa_l1 <- renderPlot({

    #agrego dependencia
    input$boton_e1_log
    #
    isolate({

      
      #TRABAJO IMPUT
      a <- c(input$input_locx,input$input_locy, input$input_locxGk,
             input$input_locyGk
      )

      #CONVIERTO INPUT
      a <- t(as.data.frame(as.numeric(a)))
      a <- as.data.frame(a)
      #print(a)

      names(a) <- c( "location.x"    ,
                    "location.y"   ,   "location.x.GK" ,   "location.y.GK")

      #CREO DF AUXILIAR PARA GRAFICAR ARQUERO
      a1 <- data.frame(a$location.x,a$location.y)
      a1$player <- "Jugador"

      aux <- data.frame(a$location.x.GK,a$location.y.GK,"Arquero")
      names(aux) <- names(a1)

      a1 <- rbind.data.frame(a1,aux)
      a1$player <- as.factor(a1$player)

      #print(a1)

      #GRAFICO
      ggplot() +
        ggplot2::annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "green", size = 0.6) +
        ggplot2::annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "green", size = 0.6)+
        ggplot2::annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "green", size = 0.6)+
        ggplot2::annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "green", size = 0.6)+
        theme(rect = element_blank(),
              line = element_blank()) +
        # add penalty spot right
        ggplot2::annotate("point", x = 108 , y = 40, colour = "green", size = 1.05) +
        ggplot2::annotate("path", colour = "green", size = 0.6,
                          x=60+10*cos(seq(0,2*pi,length.out=2000)),
                          y=40+10*sin(seq(0,2*pi,length.out=2000)))+
        # add centre spot
        ggplot2::annotate("point", x = 60 , y = 40, colour = "green", size = 1.05) +
        ggplot2::annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
                          y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="green") +
        ggplot2::annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
                          y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="green") +

        geom_point(data = a1, aes(x = a.location.x, y = 80 - a.location.y, colour = player), size = 4
        )+
        scale_size(range = c(0, 10))+
        labs(title = "Disparo elegido", subtitle = "Coordenadas consideradas", colour = "Player"
        )+
        theme(
          plot.title = element_text(color="blue", size=18, face="bold.italic",hjust = 0.5),
          plot.subtitle = element_text(color="black", size=16, face="bold",hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank()
        ) +
        guides(size = guide_legend(title.position = "top"),
               colour = guide_legend(title.position = "top",override.aes = list(size = 6, fill = "black")))



    }) #final Isolate

  }) #final renderploty

 
}) #final Shinyserver