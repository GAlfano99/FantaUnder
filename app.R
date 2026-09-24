library(tidyverse)
library(reshape2)
library(plotly)
library(glue)
require(rlang)
require(shiny)
require(shinyWidgets)
require(crosstalk)
require(bslib)
require(REdaS)
require(soccermatics)
require(DT)
require(readxl)
require(ggsoccer)
require(data.table)
require(reticulate)
#use_python_version(version = "3.12:latest")
reticulate::py_install("understatapi")
py_require("understatapi")
understatapi=import("understatapi")
understat = understatapi$UnderstatClient()

carnot<-function(x){
  goal_line=(dist(cords_goal))^2
  dist_post1=(dist(rbind(cords_goal[1,],x)))^2
  dist_post2=(dist(rbind(cords_goal[2,],x)))^2
  
  resu=(-goal_line+dist_post1+dist_post2)/(2*sqrt(dist_post1)*sqrt(dist_post2))
  angl<-acos(resu)
  return(angl)
}
simulations<-function(dati){
  
  shots=dati%>%
    mutate(Gol=case_when(result=='Goal' ~ 1,
                         result!='Goal' ~ 0))
  
  
  xg<-shots%>%pull(xG)
  
  n_goal<-shots%>%
    summarise(n=sum(Gol==1))%>%
    pull(n)
  
  
  N<-length(xg)
  N<-as.numeric(N)
  
  sim<-matrix(0,10000,N)
  
  
  for(i in 1:N) {
    sim[,i]=rbinom(10000,1,xg[i])
  }
  
  
  gol<-apply(sim,1,sum)
  
  
  prob<-table(gol)
  prob<-prop.table(prob)
  prob<-as.data.frame(prob)%>%
    mutate(gol=as.numeric(gol)-1,
           shade=ifelse(gol==n_goal,'1','0'),
           gol=factor(gol),
           Freq=round(Freq,4))
  
  colnames(prob)[2]<-shots$player[1]
  
  return(list(sims=prob,N=N))
}

cords_goal<-rbind(c(105,30.34),c(105,37.66))


situation=c('Punizione diretta',"Da Calcio d'angolo",'In Movimento',
            'Rigore','Da Calcio da fermo')
bodyp=c('Piede Sx','Piede Dx','Testa','Altro')
result=c('Tiro Bloccato','Goal','Tiro Fuori','Tiro Parato','Tiro sul palo')


conflicted::conflicts_prefer(plotly::layout)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(DT::dataTableOutput)
conflicted::conflicts_prefer(DT::renderDataTable)


vals_att=read_xlsx("data/vals_att.xlsx")
voti=read_xlsx("data/voti_2627.xlsx")
ponte=read_xlsx("data/identificatori understat fantacalcio.xlsx")

vals=voti%>%
  select(Nome,Ruolo,Squadra,Giornata,Voto,FantaVoto,everything())%>%
  mutate(matchDayIndex=parse_number(Giornata),
         Cs=case_when(Ruolo=="P" & Gs==0 ~1,
                      Ruolo=="P" & Gs>0 ~0),
         BonMal=case_when(Ruolo=="P" ~ 3*Gf+3*Rp+Ass-Esp-2*Au-0.5*Amm-1*Gs+1*Cs-3*Rs+3*Rf,
                          Ruolo!="P" ~ 3*Gf+3*Rp+Ass-Esp-2*Au-0.5*Amm-1*Gs-3*Rs+3*Rf))%>%
  left_join(vals_att%>%select(PlayerName,FantaId,Giornata,partite)%>%rename(Nome=PlayerName))%>%
  mutate(partite=partite*90)


teams=vals_att%>%
  filter(!str_detect(Squadra,","))%>%
  pull(Squadra)%>%
  unique()

merged_att=vals_att%>%
  select(PlayerName,Ruolo,FantaId,UnderstatId,everything())%>%
  group_by(PlayerName,FantaId,UnderstatId)%>%
  summarise(Ruolo=unique(Ruolo),
            Squadra=paste0(unique(Squadra),collapse = ","),
            "Partite a voto"=sum(!is.na(Voto)),
            Partite=sum(partite,na.rm=T),
            Tiri=sum(shots,na.rm=T),
            Gol=sum(goals,na.rm = T),
            "Non Penalty Gol"=sum(npg,na.rm = T),
            "Key Pass"=sum(key_passes,na.rm=T),
            Assist=sum(assists,na.rm=T),
            xG=sum(xG,na.rm=T),
            npxG=sum(npxG,na.rm = T),
            xA=sum(xA,na.rm = T),
            xGChain=sum(xGChain,na.rm=T),
            xGBuildup=sum(xGBuildup,na.rm=T),
            Bonus=sum(Bonus,na.rm=T),
            xBonus=sum(xBonus,na.rm=T),
            npBonus=sum(npBonus,na.rm=T),
            npxBonus=sum(npxBonus,na.rm=T),            
            "Rigori Sbagliati"=sum(Rs,na.rm=T),
            "Rigori Segnati"=sum(Rf,na.rm=T),
            Autogol=sum(Au,na.rm=T),
            Ammonizioni=sum(Amm,na.rm=T),
            Espulsioni=sum(Esp,na.rm=T),
            MV=mean(Voto,na.rm=T),
            FM=mean(FantaVoto,na.rm=T))%>%
  ungroup()%>%
  mutate(Partite=round(Partite,2),
         Ruolo=factor(Ruolo,levels=c("Por","Dif","Cen","Att")))%>%
  mutate(FM=round(FM,2),
         MV=round(MV,2),
         Bonus=round(Bonus,2),
         xBonus=round(xBonus,2),
         npBonus=round(npBonus,2),
         npxBonus=round(npxBonus,2),
         xGChain=round(xGChain,2),
         xGBuildup=round(xGBuildup,2),
         across(c(Gol,Tiri,Assist,`Key Pass`,xG,xA,npxG,`Non Penalty Gol`,Bonus,npBonus,
                  xBonus,npxBonus),~round(.x/Partite,2)),
         txt=glue("<b>{PlayerName}<br><b>Minutip90:</b> {Partite}<br><b>B90:</b> {Bonus}<br><b>xB90:</b> {xBonus}"))%>%
  select(PlayerName,Squadra,Ruolo,Partite,`Partite a voto`,everything())%>%
  rename("Minutip90"=Partite)



pll<-value_box(title="Giocatore:",
               value=textOutput("pl_ch"),
               theme="light")

pll1<-value_box(title="Giocatore:",
                value=textOutput("pl_ch1"),
                theme="light")

vbs <- list(
  value_box(
    title = "Presenze:",
    value = textOutput("pp"),
    theme = "light"),
  value_box(
    title = "Media Voto:",
    value = textOutput("mv"),
    theme = "blue"),
  value_box(
    value = textOutput("fm"),
    theme = "orange",
    title ="FantaMedia:"))

vbs_pag4 <- list(
  value_box(
    title = "Tiri effettuati (Gol):",
    value = textOutput("ntiri"),
    theme = "light"),
  value_box(
    value = textOutput("xG"),
    theme = "red",
    title ="xG totale:"))


ui <- page_navbar(
  title = "",
  id = "nav",
  sidebar = sidebar(
    conditionalPanel(
      "input.nav === 'Bonus vs Expected Bonus'",
      "",
      pickerInput("rol",
                  "Seleziona il ruolo:",
                  choices = c("Dif","Cen","Att"),
                  selected = "Dif"),
      sliderInput("partite",
                  label="Minimo di partite giocate:",
                  min=0,
                  max=38,
                  value=1),
      pickerInput("team",
                  "Seleziona la squadra:",
                  choices = teams,
                  multiple = T,
                  selected = teams,
                  options = list(`live-search`=TRUE,
                                 `actions-box` = TRUE))),
    conditionalPanel(
      "input.nav === 'MV e FM'",
      "",
      pickerInput("pl",
                  label="Seleziona il giocatore:",
                  choices = sort(unique(vals$Nome)),
                  selected= sort(unique(vals$Nome))[1],
                  options = list(`live-search`=TRUE))),
    conditionalPanel(
      "input.nav === 'Tiri'",
      "",
      pickerInput("pltr",
                  label="Seleziona il giocatore:",
                  choices = sort(unique(merged_att$PlayerName)),
                  selected= sort(unique(merged_att$PlayerName))[1],
                  options = list(`live-search`=TRUE)),
      pickerInput("st_type",
                  label="Situazione di gioco:",
                  choices = situation ,
                  selected= situation,
                  multiple = T,
                  options = list(`live-search`=TRUE)),
      pickerInput("body_type",
                  label="Parte del corpo:",
                  choices = bodyp ,
                  selected= bodyp,
                  multiple = T,
                  options = list(`live-search`=TRUE)),
      pickerInput("res_type",
                  label="Esito:",
                  choices = result ,
                  selected= result,
                  multiple = T,
                  options = list(`live-search`=TRUE)),
      actionButton('go','Estrai Tiri',icon=icon("fa-regular fa-square-caret-right",lib = "font-awesome")))),
  nav_panel("Bonus vs Expected Bonus",
            card(card_body(plotlyOutput("pl_xB")),
                 full_screen = TRUE),
            card(card_body(dataTableOutput("pl_xtable")),
                 full_screen = T)),
  nav_panel("MV e FM",
            layout_column_wrap(pll,
                               fill=F,
                               width = "250px",
                               max_height = "75px"),
            layout_column_wrap(
              width = "250px",
              max_height = "75px",
              fill = FALSE,
              vbs[[1]], vbs[[2]],vbs[[3]]),
            card(card_body(plotlyOutput("and")),
                 full_screen = T,
                 max_height = "325px"),
            card(card_body(dataTableOutput("tbl_and")),
                 full_screen = T,
                 max_height = "200px")),
  nav_panel("Tiri",
            layout_column_wrap(pll1,
                               fill=F,
                               width = "250px",
                               max_height = "75px"),
            layout_column_wrap(
              width = "250px",
              max_height = "75px",
              fill = FALSE,
              vbs_pag4[[1]], vbs_pag4[[2]]),
            card(card_body(plotlyOutput("sim_tiri")),
                 full_screen = T,
                 max_height = "200px"),
            card(card_body(plotlyOutput("loc_tiri")),
                 full_screen = T,
                 max_height = "325px")))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  sh_mergedAtt<-reactive({
    
    merged_att%>%
      filter(Minutip90>=input$partite &
               Ruolo==input$rol & Squadra%in%input$team)
    
    
  })
  
  
  output$pl_xtable<-renderDataTable({
    
    sels=event_data(event="plotly_click",source="xB")
    
    tt=sh_mergedAtt()%>%
      select(PlayerName,Squadra,Ruolo,Minutip90,`Partite a voto`,Ammonizioni,Espulsioni,
             Autogol,`Rigori Sbagliati`,`Rigori Segnati`,MV,FM,Gol,`Non Penalty Gol`,Assist,xG,xA,
             Bonus,xBonus,npBonus,npxBonus)%>%
      rename(Giocatore=PlayerName,
             "Minuti p90"=Minutip90,
             "Gol p90"=Gol,
             "Non Penalty Gol p90"=`Non Penalty Gol`,
             "Assist p90"=Assist,
             "xG p90"=xG,
             "xA p90"=xA,
             "Bonus p90"=Bonus,
             "xBonus p90"=xBonus,
             "npBonus p90"=npBonus,
             "npxBonus p90"=npxBonus)%>%
      datatable(rownames = F,
                filter = "top")
  })
  
  output$pl_xB<-renderPlotly({
    
    
    s <- input$pl_xtable_rows_selected
    
    p=ggplotly(ggplot(sh_mergedAtt())+
                 geom_point(size=3,
                            aes(x=xBonus,y=Bonus,
                                text=txt),
                            col="blue3")+
                 labs(x="xBonus p90 minuti",
                      y="Bonus p90 minuti")+
                 xlim(0,ceiling(max(c(max(sh_mergedAtt()$Bonus),
                                      max(sh_mergedAtt()$xBonus)))))+
                 ylim(0,ceiling(max(c(max(sh_mergedAtt()$Bonus),
                                      max(sh_mergedAtt()$xBonus)))))+
                 geom_abline(aes(intercept = 0,slope=1),linetype="dashed",
                             color="grey")+
                 theme_minimal(),tooltip = "text")
    
    if (length(s)) {
      
      pplot=sh_mergedAtt()%>%
        mutate(alp=ifelse(row_number()%in%s,T,F))
      
      p=ggplotly(ggplot(pplot)+
                   geom_point(size=3,
                              aes(x=xBonus,y=Bonus,
                                  text=txt,
                                  alpha=alp),
                              col="blue3")+
                   labs(x="xBonus p90 minuti",
                        y="Bonus p90 minuti")+
                   scale_alpha_manual(values=setNames(c(1,0.2),c(T,F)))+
                   xlim(0,ceiling(max(c(max(sh_mergedAtt()$Bonus),
                                        max(sh_mergedAtt()$xBonus)))))+
                   ylim(0,ceiling(max(c(max(sh_mergedAtt()$Bonus),
                                        max(sh_mergedAtt()$xBonus)))))+
                   geom_abline(aes(intercept = 0,slope=1),linetype="dashed",
                               color="grey")+
                   theme_minimal()+
                   guides(alpha="none"),tooltip = "text")
    }
    p
    
  })
  
  
  ff=reactive({
    vals%>%
      filter(Nome==input$pl)%>%
      complete(matchDayIndex=full_seq(matchDayIndex,1))
    
  })
  
  output$pp <-renderText({
    ff()%>%
      ungroup()%>%
      drop_na(Voto)%>%
      summarise(pp=n())%>%
      pull(pp)
  })
  
  output$fm <- renderText({
    
    ff()%>%
      summarise(FM=mean(FantaVoto,na.rm=T))%>%
      mutate(FM=round(FM,2))%>%
      pull(FM)
  })
  
  output$mv <- renderText({
    ff()%>%
      summarise(MV=mean(Voto,na.rm=T))%>%
      mutate(MV=round(MV,2))%>%
      pull(MV)
  })
  
  
  output$and<-renderPlotly({
    
    pl_and=plot_ly(ff(), x = ~matchDayIndex)%>%
      add_trace(y=~Voto,mode="lines+markers",
                name="Voto",
                hovertemplate = paste(
                  "<b>Giornata:<b> %{x}<br>",
                  "<b>Voto:<b> %{y}<extra></extra>"))%>%
      add_trace(y=~FantaVoto,mode="lines+markers",
                name="FantaVoto",
                hovertemplate = paste(
                  "<b>Giornata:<b> %{x}<br>",
                  "<b>FantaVoto:<b> %{y}<extra></extra>")) %>%
      layout(xaxis = list(title = "Giornata",
                          tickvals = list(1,2,3,4,5,6,7,8,9,10,11,12,13,
                                          14,15,16,17,18,19,20,21,22,23,24,25,26,
                                          27,28,29,30,31,32,33,34,35,36,37,38)),
             yaxis = list(title='Voto/FantaVoto'),
             legend=list(x=0.5,y=1.2,orientation="h"))
    
    
    br_and=plot_ly(ff()) %>% 
      add_trace(x = ~matchDayIndex, y = ~BonMal, color=~BonMal>0,
                type = 'bar',
                colors = c("red3","green3"),
                showlegend = FALSE,
                hovertemplate = paste(
                  "<b>Giornata:<b> %{x}<br>",
                  "<b>Bonus/Malus:<b> %{y}<extra></extra>")) %>%
      layout(xaxis = list(title = "Giornata",
                          tickvals = list(1,2,3,4,5,6,7,8,9,10,11,12,13,
                                          14,15,16,17,18,19,20,21,22,23,24,25,26,
                                          27,28,29,30,31,32,33,34,35,36,37,38)),
             yaxis = list(title='Bonus/Malus'))
    
    and=subplot(pl_and,br_and,nrows=2, shareX = TRUE,titleY = T,titleX = T)
    
    
  })
  
  output$tbl_and<-renderDataTable({
    
    ff()%>%
      select(-Giornata,-Cod.,-FantaId)%>%
      relocate(partite,.after=Squadra)%>%
      rename(Giocatore=Nome,
             Giornata=matchDayIndex,
             "Porta Inviolata"=Cs,
             "Gol subiti"=Gs,
             Gol=Gf,
             "Minuti giocati"=partite,
             Assist=Ass,
             Ammonizione=Amm,
             Espulsione=Esp,
             Autogol=Au,
             "Rigore Segnato"=Rf,
             "Rigore Sbagliato"=Rs,
             "Rigore Parato"=Rp)%>%
      select(-BonMal)%>%
      drop_na(Voto)%>%
      datatable(rownames = F)
  })
  
  output$pl_ch <- reactive({
    
    ff()%>%
      drop_na(Nome)%>%
      pull(Nome)%>%
      unique()%>%
      str_remove_all(pattern=",")
  })
  
  
  output$pl_ch1 <-reactive({
    
    tiri_sel()%>%
      pull(player)%>%
      unique()
  })
  
  tiri_sel=eventReactive(input$go,{
    
    
    a=try(understat$player(player=as.character(ponte$UnderstatId[ponte$Nome==input$pltr]))$get_shot_data())
    
    a=a%>%
      rbindlist()%>%
      filter(season==2026)%>%
      mutate(situation=case_when(situation=='DirectFreekick'~'Punizione diretta',
                                 situation=='FromCorner'~"Da Calcio d'angolo",
                                 situation=='OpenPlay'~'In Movimento',
                                 situation=='Penalty'~'Rigore',
                                 situation=='SetPiece'~'Da Calcio da fermo'),
             shotType=case_when(shotType=='Head'~'Testa',
                                shotType=='LeftFoot'~'Piede Sx',
                                shotType=='RightFoot'~'Piede Dx',
                                shotType=='OtherBodyPart'~'Altro'),
             result=case_when(result=='BlockedShot'~'Tiro Bloccato',
                              result=='MissedShots'~'Tiro Fuori',
                              result=='SavedShot'~'Tiro Parato',
                              result=='ShotOnPost'~'Tiro sul palo',
                              .default = result),
             across(c(X,Y,xG,season),~as.numeric(.x)))%>%
      filter(situation%in%input$st_type,result%in%input$res_type,shotType%in%input$body_type)
  })
  
  output$ntiri <- renderText({
    
    tiri_sel()%>%
      summarise(tiri=n(),
                gol=sum(result=="Goal",na.rm = T))%>%
      mutate(tiri=glue("{tiri} ({gol})"))%>%
      pull(tiri)
  })
  
  output$xG <- renderText({
    
    tiri_sel()%>%
      summarise(xG=sum(xG,na.rm=T))%>%
      mutate(xG=round(xG,2))%>%
      pull(xG)
  })
  
  
  output$loc_tiri <-renderPlotly({
    
    n<-length(tiri_sel()%>%pull(xG))
    
    shots=tiri_sel()%>%
      mutate(X=105*X,
             Y=68*Y,
             Y=68-Y)
    
    
    cords_goal<-rbind(c(105,30.34),c(105,37.66))
    
    cords_shot<-shots%>%
      select(X,Y)
    
    # formula di carnot
    
    # calcolo angolo (in gradi)
    
    angle<-apply(cords_shot, 1, carnot)
    shots<-tibble(shots,angle=rad2deg(angle))
    
    midpoint_goal<-cbind(105,34)
    
    distances<-proxy::dist(cords_shot,midpoint_goal,method = 'euclidean')
    
    shots<-tibble(shots,dist=as.vector(distances))
    
    #result=c("BlockedShot","Goal","MissedShots","SavedShot","ShotOnPost" )
    colors<-c('Tiro Parato'='light blue', 'Goal'='green','Tiro Fuori'='red3',
              'Tiro sul palo'='yellow','Tiro Bloccato'='orange')
    
    shapes=c('Punizione diretta'=21,"Da Calcio d'angolo"=22,'In Movimento'=23,
             'Rigore'=24,'Da Calcio da fermo'=25)
    
    a=soccerPitchHalf()+
      geom_point(data = shots,aes(x = Y, y = X,fill=result,shape=situation,
                                  text=glue("Parte del corpo:{shotType} \n xG:{round(xG,3)} \n Angolo:{round(angle,2)}° \n Distanza:{round(dist,2)}m"),
                                  size=5),col='black')+
      theme(
        legend.text =element_text(size=13),
        legend.key.size = unit(2,'cm'),
        legend.title = element_text(size=15))+
      labs(fill='',shape='Situazione di gioco e Risultato:')+
      guides(col='none',size='none')+
      scale_fill_manual(values = colors)+
      scale_shape_manual(values=shapes)+
      theme(legend.position = "top",
            legend.direction = "horizontal")
    
    a=ggplotly(a,tooltip = 'text')%>%
      config(fillFrame=T)
    
    
    
  })
  
  sss=reactive({simulations(tiri_sel())})
  output$sim_tiri <- renderPlotly({
    
    
    
    sims=sss()[["sims"]]
    
    #print(sims)
    
    sim_plot<-ggplot(data = sims,aes(x=gol,y=.data[[tiri_sel()%>%pull(player)%>%unique()]],alpha=shade,
                                     text=glue("Gol:{gol}
                                                     Probabilità: {.data[[tiri_sel()%>%pull(player)%>%unique()]]*100}%")))+
      geom_bar(stat='identity',position='dodge',fill='red2',col='black')+
      labs(x = 'Gol Simulati',y = 'Probabilità')+
      scale_alpha_manual(name = 'Gol realizzati in base alla selezione',
                         values = setNames(c(1,0.2),c('1','0')),
                         labels=c('No','Sì'))+
      theme_minimal()+
      guides(col='none',fill='none',alpha='none')+
      scale_y_continuous(labels=scales::percent)+
      theme(title = element_text(size=10))
    
    sim_plot=ggplotly(sim_plot,tooltip = 'text')
    sim_plot
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

