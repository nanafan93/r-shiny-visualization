#library requirements

require(ggplot2)
require(shiny)

# reading data into r and removing unreliable statistics (could go into server)

data <- read.csv('EPL_Data.csv')
data <- data[(data$Appearances >= 100),]
data <- data[(data$Appearances <= 500),]
data$X <- NULL
data[is.na(data)] <- 0

# split the players according to their position

Forwards <- data$Position == "F"
Forwards <- data[Forwards,]
Forwards <- transform(Forwards,Footing = ifelse(Goals.with.right.foot>Goals.with.left.foot,'R','L'))
Defenders <- data$Position == "D"
Defenders <- data[Defenders,]
Midfielders <- data$Position == "M"
Midfielders <- data[Midfielders,]
Midfielders <- transform(Midfielders,Footing = ifelse(Goals.with.right.foot>Goals.with.left.foot,'R','L'))
Goalies <- data$Position == "G"
Goalies <- data[Goalies,]

# Testing plots / code in r before deploying in app 

# END OF TEST PAD
#---------------------------------------
#---------------------------------------
    #UI OF SHINY APP
#---------------------------------------
#---------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(
      ".title {margin:auto;text-align:center;font-family:garamond;font-size:40px;"
               )
            ),
  tags$div(class="title", titlePanel("The rise of Data in Sports")),
  hr(),
  h2('Motivation'),
  p('I have developed this visualization for football enthusiasts like myself to analyse the performance of players in the \'18/19 English premier league(EPL) season. Data has become a very important asset for the strategy formation in football. Teams are using data analytics to gain competitive advantage in professional matches. Sensors are available to track everything from heartrate, vision, distance, touches, acceleration, pitch coverage and many more things. Although it is really unlikely that data will completely swamp out intuition, data can lead to surprising discoveries and competitive strategies.'),
  h2('Data Sourcing/Tools Used'),
  p('As the data I had used for the previous project had a lot of errors, for this project, I scraped the data myself from the official premier league website. Scraping was done using the BeautifulSoup4 Library in python. Preliminary data checking and cleaning was done in R.'),
  tags$b(p('Let\'s get started !')),
  h3('Working Dataset'),
  p('From the initial dataset of around 900 players, I have removed the players with unreliable statistics due to low number of observations. The dataset which we will be working with contains 198 players with around 58 variables. Let us further split the players by their position and view their specializations.'),
  h3('Fluidity of player roles: The curious case of the attacking defender'),
  tags$b('Improvement on exploration project:'),
  p('In the exploration project, we had pointed out that classifying players into just four categories is not a valid representation of the game. In the new dataset, I have managed to capture the specializations of the players. We can view the specializations by selecting the broad category. In the current game, the positions are a lot more fluid. In the current season of the EPL, wing backs(traditionally defenders) have provided more assists leading in goals than many midfielders !'), 
  sidebarLayout(
    sidebarPanel(
      radioButtons('Pos','Select Position:',c('Forwards' = 'f','Midfielders'='m','Defenders'='d'))
                ),
    mainPanel(
      plotOutput('Distribution of Players')
)
),
hr(),
h3('Nadal(12),Pacquiao and Messi: The Gift of the Left Foot(Or Hand)!'),
p('I have often been fascinated by people who do things in an unorthodox manner. From childhood we are always taught methods or techniques or systems which we try to perfect.In competitive sport, unorthodox technique is often a competitive advantage. Even though being left footed (or handed) has nothing to do with technique per se, there is something about facing such opponents that just throws you off. You simply cannot anticipate how they are going to move or how to respond to their moves. It is a well known fact that left-footedness like left-handedness is rare. However, if you look at the top performers in any sport, this undeniable advantage makes a strong appearance.'),
p('You can see how players have performed by their footing. We can see names like David Silva, Ozil, Lukaku come to the front. Defenders have been left out of this comparison because its hard to estimate a defenders footing based on the data given.'),
sidebarLayout(
  sidebarPanel(
    radioButtons('Footing','Select Footedness:',c('Left' = 'l','Right' = 'r')),
    radioButtons('Position','Select Position:',c('Forwards'='F','Midfielders'='M')),
    uiOutput('SecondSelection')
  ),
  mainPanel(
    tabsetPanel(
    tabPanel("Seperated by footing",plotOutput('VitalStats')),
    tabPanel('All Players',plotOutput('FootingAll'))
    )
  )
),
hr(),
h3('The Master has failed more times than the beginner has even tried'),
p('One would think that missing many goals would make you a bad striker in the game of football. However, it is very important in this sport to be able to get in a position to be able to miss a chance rather than not having a chance at all !. In the latest season, the top scorer for the league and winner of the golden boot missed 23 big chances in a single season ! '),
sidebarLayout(
  sidebarPanel(
    checkboxInput('show','Show the Best!','value' = FALSE)
  ),
  mainPanel(
    plotOutput('Best Strikers')
  )
),
p('We can see that the best strikers in the game have missed many more chances than their peers. For the casual observer this might seem strange at the start because how would one expect missing big chances to be an indicator of skill'),
h3('What\'s next?'),
p('The data which was available in this exercise was an aggregation over time and not a moving series. I believe that more complex analysis dealing with trends could have been done if data for individual seasons was available for analysis. Unfortunately, considering that players transfer and change leagues pretty often and other data collection issues, this data is harder to acquire.'),
tags$a(href='https://www.premierleague.com',' Visit the official website of the English premier league from where the data was scraped!'),
hr()
)
#-----------------------------------------
#-----------------------------------------
    # SERVER OF SHINY APP
#-----------------------------------------
#-----------------------------------------

  server <- function(input, output) {
# PLOT 1 DISTRIBUTION OF PLAYER SPECIALIZATION    

        output$'Distribution of Players' <- renderPlot({
      if (input$Pos == "f") {
        printframe = Forwards
        selSpec = 'Forwards'
      }
      else if (input$Pos == 'm') {
        printframe = Midfielders
        selSpec = 'Midfielders'
      }
      else {
        printframe = Defenders
        selSpec = 'Defenders'
      }
    ggplot(printframe,aes(printframe$Specialization,fill=Specialization)) +geom_histogram(stat = 'count') + labs(title = 'Player Specialization',x = selSpec) + theme_bw() + theme(axis.text.x = element_blank(),plot.title = element_text(hjust = 0.5,size = 20),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
    })
    
# PLOT 2 VITAL STATS ACCORDING TO FOOT AND POSITION     
    
    output$'VitalStats' <- renderPlot({
      if (input$Footing == 'l'){
        FP <- Forwards$Goals.with.left.foot > Forwards$Goals.with.right.foot
        FP <- Forwards[FP,]
        MP <- Midfielders$Goals.with.left.foot > Midfielders$Goals.with.right.foot
        MP <- Midfielders[MP,]
      }
      else {
        FP <- Forwards$Goals.with.right.foot > Forwards$Goals.with.left.foot
        FP <- Forwards[FP,]
        MP <- Midfielders$Goals.with.right.foot > Midfielders$Goals.with.left.foot
        MP <- Midfielders[MP,]
      }
      if(input$Position == 'F')
        if(input$stat == 'gol')
          outThis <- ggplot(FP,aes(y=FP$Goals,x=FP$Name)) +geom_histogram(stat = 'identity',fill='#c96b24') + theme_bw() + labs(title = 'Goals',x='Players',y='Goals') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else if(input$stat == 'sot')
          outThis <- ggplot(FP,aes(y=FP$Shots.on.target,x=FP$Name)) +geom_histogram(stat = 'identity',fill='#c96b24') + theme_bw() + labs(title = 'Shots on Target',x='Players',y='Shots on Target') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else
          outThis <- ggplot(FP,aes(y=FP$Big.chances.missed,x=FP$Name)) +geom_histogram(stat = 'identity',fill='#c96b24') + theme_bw() + labs(title = 'Big Chances Missed',x='Players',y='Big Chances missed') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
      else
        if(input$stat == 'ass')
          outThis <- ggplot(MP,aes(y=MP$Assists,x=MP$Name)) +geom_histogram(stat = 'identity',fill='#c96b24') + theme_bw() + labs(title = 'Assists',x='Players',y='Assists') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else if(input$stat == 'bcc')
          outThis <- ggplot(MP,aes(y=MP$Big.chances.created,x=MP$Name)) +geom_histogram(stat = 'identity',fill='#c96b24') + theme_bw() + labs(title = 'Big Chances Created',x='Players',y='Big Chances created') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else
          outThis <- ggplot(MP,aes(y=MP$Accurate.long.balls,x=MP$Name)) +geom_histogram(stat = 'identity',fill='#c96b24') + theme_bw() + labs(title = 'Accurate Long Balls',x='Players',y='Accurate Long balls') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
      outThis
      })
    
# RENDER UI FOR SECOND SELECTION 

    output$SecondSelection <- renderUI({
      if(input$Position == 'F')
        choiceStat = c('Goals'='gol','Shots on Target'='sot','Big Chances Missed'='bcm')
      if(input$Position == 'M')
        choiceStat = c('Assists'='ass','Big chances Created'='bcc','Accurate Long Balls'='alb')
      selectInput("stat", "Select Vital Stat",choiceStat)
    })
    

# INTEGRATED PLOT
     
    output$FootingAll <- renderPlot({
      if(input$Position == 'F')
        if(input$stat == 'gol')
          plot1 <- ggplot(Forwards,aes(y=Goals,x=Name,fill=Footing)) +geom_histogram(stat = 'identity') + theme_bw() + labs(title = 'Goals',x='Players',y='Goals') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else if(input$stat == 'sot')
          plot1 <- ggplot(Forwards,aes(y=Shots.on.target,x=Name,fill=Footing)) +geom_histogram(stat = 'identity') + theme_bw() + labs(title = 'Shots on Target',x='Players',y='Shots on Target') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else
          plot1 <- ggplot(Forwards,aes(y=Big.chances.missed,x=Name,fill=Footing)) +geom_histogram(stat = 'identity') + theme_bw() + labs(title = 'Big Chances Missed',x='Players',y='Big Chances missed') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
        else
          if(input$stat == 'ass')
            plot1 <- ggplot(Midfielders,aes(y=Assists,x=Name,fill=Footing)) +geom_histogram(stat = 'identity') + theme_bw() + labs(title = 'Assists',x='Players',y='Assists') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
          else if(input$stat == 'bcc')
            plot1 <- ggplot(Midfielders,aes(y=Big.chances.created,x=Name,fill=Footing)) +geom_histogram(stat = 'identity') + theme_bw() + labs(title = 'Big Chances Created',x='Players',y='Big Chances created') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
          else
            plot1 <- ggplot(Midfielders,aes(y=Accurate.long.balls,x=Name,fill=Footing)) +geom_histogram(stat = 'identity') + theme_bw() + labs(title = 'Accurate Long Balls',x='Players',y='Accurate Long balls') + theme(axis.text.x = element_text(angle = 90, vjust = 0.3,hjust = 0.8,size = 14),plot.title = element_text(hjust = 0.5,size = 20),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
      plot1
    })
  
# HYP 1 MISS VS GOALS #
    output$'Best Strikers' <- renderPlot({
      if(input$show == TRUE)
        gg<- ggplot(Forwards,aes(x=Big.chances.missed,y=Goals,size=Goals,shape=Footing,colour=Footing)) + geom_point() + geom_text(aes(label=ifelse(Goals > 100,as.character(Name),'')),hjust=0.5,vjust=1) + theme_minimal() + labs(x= 'Big Chances Missed')
      else
        gg <- 'Click the button'
      gg
})    
  }

#------------------------------------------
#------------------------------------------
   # RUN THE SHINY APP 
#------------------------------------------
#------------------------------------------

shinyApp(ui = ui, server = server)
