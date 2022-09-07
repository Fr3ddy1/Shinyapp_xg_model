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
  
  #PASOS - ESTACION 1
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
  
  #DEFINO BOTONES A USAR COMO INPUTS
  output$input_etapa_l1 <- renderUI({ 
    
    box(id="tres",width=12,title="Entradas del modelo",status="primary",solidHeader=TRUE ,
        
        column(width = 4,
               #box( width = 6, background = "navy",
               numericInput("input_locx", "Coordenada x disparo:", 111, min = 0, max = 120) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada x del campo donde el disparo fué efectuado, para más detalles ir a la sección de Bienvenida."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
               #)#final box
        ),
        
        column(width = 4,
               #box( width = 6, background = "navy",
               numericInput("input_locy", "Coordenada y disparo:", 35, min = 0, max = 80) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada y del campo donde el disparo fué efectuado, para más detalles ir a la sección de Bienvenida."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
               #)#final box
        ),
        
        column(width = 4,
               #box( width = 6, background = "navy",
               numericInput("input_locxGk", "Coordenada x Arquero:", 119, min = 0, max = 120) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada x del campo donde se ubica el arquero, para más detalles ir a la sección de Bienvenida. Favor tener cuidado al ingresar 
                                    coordenadas que indiquen que el arquero está muy lejos de la arquería, los resultados pueden ser imprecisos."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
               #)#final box
        ),
        
        column(width = 4,
               #box( width = 6, background = "navy",
               numericInput("input_locyGk", "Coordenada y Arquero:", 40, min = 0, max = 62) %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese la coordenada y del campo donde se ubica el arquero, para más detalles ir a la sección de Bienvenida.Favor tener cuidado al ingresar 
                                    coordenadas que indiquen que el arquero está muy lejos de la arquería, los resultados pueden ser imprecisos."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
               #)#final box
        ),
        
        column(width = 4,
               #box( width = 6, background = "navy",
               #numericInput("input2_e1_log", "Período:", 249, min = 1, max = 1000000000) %>% 
               selectInput(inputId = "input_pos",label =  "ID posición:", choices = seq(2,25),selected = "22") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese el ID de la posición del jugador, para más detalles ir a la sección de Bienvenida."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
               #)#final box
        ),
        
        column(width = 4,
               #box( width = 6, background = "navy",
               #numericInput("input5_e1_log", "ID patrón de juego :", 20, min = 1, max = 1000000000) %>% 
               selectInput(inputId = "input_body",label =  "ID parte del cuerpo :", choices = c("37","38","40","70"),selected = "40") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese el id de la parte del cuerpo con la cual el jugador remata, para más detalles ir a la sección de Bienvenida."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               selectInput(inputId = "input_bajo_pres",label =  "Bajo presión :", choices = c("1","0"),selected = "0") %>% 
                  helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese 1 si el jugador está bajo presión, ingrese 0 en caso contrario."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        
        column(width = 4,
               selectInput(inputId = "input_first_time",label =  "Disparo por primera vez :", choices = c("1","0"),selected = "0") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese 1 si el jugador disparó por primera vez, ingrese 0 en caso contrario."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
                        ),
                        buttonLabel = "Entendido!",
                        easyClose = FALSE,
                        fade = TRUE,
                        size = "m")
               
        ),
        column(width = 4,
               selectInput(inputId = "input_tec",label =  "ID técnica de disparo :", choices = seq(89,95),selected = "92") %>% 
                 helper(type = "inline",
                        title = "Descripción",
                        content = c("Por favor ingrese el id de la técnica con la cual el jugador remata, para más detalles ir a la sección de Bienvenida."
                                    #"This is on a new line.",
                                    #"This is some <b>HTML</b>.",
                                    #"Note this modal has a different button label, fades in and is harder to close."
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
                               #"This is on a new line.",
                               #"This is some <b>HTML</b>.",
                               #"Note this modal has a different button label, fades in and is harder to close."
                   ),
                   buttonLabel = "Entendido!",
                   easyClose = FALSE,
                   fade = TRUE,
                   size = "m"),
          verbatimTextOutput("adv_etapa_l1"),
           h4("xG obtenido:")  %>% 
            helper(type = "inline",
                   title = "Descripción",
                   content = c("En esta sección se muestran el xG obtenido, es decir, la probabilidad de que
                               con las condiciones dadas el remate termine en gol."
                               #"This is on a new line.",
                               #"This is some <b>HTML</b>.",
                               #"Note this modal has a different button label, fades in and is harder to close."
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
                               #"This is on a new line.",
                               #"This is some <b>HTML</b>.",
                               #"Note this modal has a different button label, fades in and is harder to close."
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
  
  #ME DEVUELVE INPUTS ELEGIDOS POR EL USUARIO
  output$result_etapa_l1 <- renderDT({ 
    
    #agrego dependencia 
    input$boton_e1_log
    #
    isolate({ 
      
      Valor <-  c(input$input_pos,
                  input$input_body,input$input_bajo_pres,
                  input$input_first_time,input$input_tec,
                  input$input_locx, input$input_locy, input$input_locxGk,
                  input$input_locyGk
      )
      
      names(Valor) <- c("ID posición","ID parte del cuerpo",
                        "Bajo presión","Primer disparo","ID técnica",
                        "Coordenada x disparo","Coordenada y disparo",
                        "Coordenada x arquero","Coordenada y arquero"
                        )
      
      as.data.frame(Valor)
      
    }) #final Isolate
    
  })
  
 
  #DEVUELVO PROBABILIDADES DEL MODELO
  output$prob_etapa_l1 <- renderText({ 
    
    #agrego dependencia 
    input$boton_e1_log
    #
    isolate({ 
      
      withProgress(message = 'Calculando probabilidad ...', value = 1/2, {
      modelo <- xgb.load(paste0(getwd(),"/data_raw/xg_model_FT.model"))
      
      
      #TRABAJO IMPUT
      a <- c(input$input_bajo_pres,
             input$input_pos,input$input_first_time,
             input$input_body,input$input_tec,input$input_locx,
             input$input_locy, input$input_locxGk,
             input$input_locyGk

      )

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
      
  
      #APLICO MODELO
      pred <- predict(modelo, as.matrix(a), reshape = TRUE)
      
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

        geom_point(data = a1, aes(x = a.location.x, y = a.location.y, colour = player), size = 4
        )+
        scale_size(range = c(0, 10))+
        labs(title = "Disparo elegido", subtitle = "xG obtenido", colour = "Player"
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