shinyUI(
#fluidPage(
  dashboardPage(title="Dashboard Goles esperados",

    #//////////////#
    #/// HEADER ///#
    #//////////////#

    dashboardHeader(title = "Modelo xG", titleWidth = 188
                    
                  ),#final dashboardheader
    
    #///////////////#
    #/// SIDEBAR ///#
    #///////////////#
    
    dashboardSidebar(introjsUI(),

                sidebarMenu(id = "tabs",

                        menuItem(tags$span(id="menu1","Bienvenido"), icon = icon("door-open"),tabName = "welcome"),
                 
                        menuItem(tags$span(id="menu2","Modelos de predicción"), icon = icon("list-ul"),tabName = "etapas_logit",
                     
                                menuSubItem(tags$span(id="e1","Modelo xG"), tabName = "etapas_logit_1", icon = icon("circle-o"))

                                ),#final menuitem


                        menuItem(tags$span(id="menu4","Acerca de"), icon = icon("info-circle"),tabName = "acerca"),
                        
                        actionButton("help_menu", "Instrucciones",width='180px') %>% 
                          helper(type = "inline",
                                 icon = "exclamation",
                                 title = "Boton de instrucciones",
                                 content = c("Presionar este botón para obtener un tour
                                             sobre la sección en la que se encuentra."),
                                 buttonLabel = "Entendido!",
                                 easyClose = FALSE,
                                 fade = TRUE,
                                 size = "m",
                                 colour = "white" ) #FINAL HELPER

                            )#final sidebarmenur

                    ), #final dashboardsidebar

    #////////////#
    #/// BODY ///#
    #////////////#

    dashboardBody(
      
    
                  tags$style(HTML("
                            .box.box-solid.box-primary>.box-header {
                            color:#fff;
                            background:#024A86;
                        
                                              }
                        
                            .box.box-solid.box-primary{
                            border-bottom-color:#00FF00;
                            border-left-color:#00FF00;
                            border-right-color:#00FF00;
                            border-top-color:#00FF00;}")), 

                  tabItems(

              #//////////////////#
              #/// BIENVENIDA ///#
              #//////////////////#
              
              
              tabItem(tabName = "welcome",
                  
                      fluidPage(id="wel",
                      includeMarkdown("www/interfaz/bienvenida_1.Rmd")
                      )
                  
                      ),#FINAL TABITEM
              
              
              #//////////////////#
              #/// MODELOS    ///#
              #//////////////////#
              
              
              #//////////////////#
              #/// MODELO XG ///#
              #//////////////////#
              
              tabItem(tabName = "etapas_logit_1",
                      
                      h2(id="uno","Modelo xG") ,
                      htmlOutput("input_etapa_l1"),
                      fluidRow(
                        uiOutput("tabla_etapa_l1")
                      )#final fluidrow
                    
                      ),#FINAL TABITEM
              

              #/////////////////#
              #/// ACERCA DE ///#
              #/////////////////#
              
              tabItem(tabName = "acerca",
                      fluidPage(id="inf",
                      includeMarkdown("www/interfaz/acerca.Rmd")
                      )
                      )#FINAL TABITEM
             
                       
                    )#final tabitems
                  )#final dashboardbody
                )#final dashboardpage
  )#final shinyui