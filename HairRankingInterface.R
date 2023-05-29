#Hair ranking interface

library(shiny)
library(magick)
library(png)
library(tidyverse)

monthOpts = c('02','03','04')
dayOpts = 1:31
timeOpts = c('Morning','Midday','Night')
pathDF = expand.grid(month = monthOpts,day = dayOpts,time = timeOpts) %>% 
  mutate(Path = paste0(month,'-',ifelse(day < 10,paste0('0',day),day),'-',time,'.png')) %>%
  filter(!((month == '02') & (day <= 10 | day >= 29))) %>%
  filter(!((month == '04') & (day == 31 | day == 22 | day == 23))) %>%
  filter(!(month == '03' & day == 12 & time == 'Morning')) %>% 
  filter(!(month == '02' & day == 28 & time == 'Midday')) %>% 
  arrange(month,day,time) %>% 
  mutate(PictureID = 1:229)

compList <- read.csv('Min5CompsList.csv')

# urlOpts = urlDF %>% pull(URL)
pathOpts = pathDF %>% pull(Path)

compareDF <- data.frame(NameTop = compList$IDTop,NameBot = compList$IDBot)
maxCounter = nrow(compareDF)

saveDF = compareDF %>% select(NameTop,NameBot) %>% mutate(SelectedPictures = 'Undecided')
write.csv(saveDF,file = 'HairSelections.csv',row.names = F)

myImgResources <- paste0('imgResources/',pathOpts)
addResourcePath(prefix = 'imgResources', directoryPath = 'Pictures/Trios/')

ui <- fluidPage(
  mainPanel(
    
    fluidRow(
      column(width = 8,
             h1("Hair Image Ranking"),       
      ),
      column(width = 4,
             br(),
             actionButton('loadButton','Load Past Selections'), 
      )
    ),
    
    br(),

    fluidRow(
      column(width = 8,
             # column(width = 12,
             #        lapply(X = seq_len(maxCounter), FUN = function(i) {
             #          # condition on the slider value
             #          conditionalPanel(condition = paste0("input.slider == ", i),
             #                           # images are on github
             #                           preloadedImgTop[[i]]
             #          )
             #        })
             # ),
             # column(width = 12,
             #        lapply(X = seq_len(maxCounter), FUN = function(i) {
             #          # condition on the slider value
             #          conditionalPanel(condition = paste0("input.slider == ", i),
             #                           # images are on github
             #                           preloadedImgBot[[i]]
             #          )
             #        }),
             # )
             column(width = 12,
                    lapply(X = seq_len(maxCounter), FUN = function(i) {
                      # condition on the slider value
                      conditionalPanel(condition = paste0("input.slider == ", i),
                                       # images are on github
                                       img(src = myImgResources[compList$IDTop[i]],width = '600px')
                      )
                    })
             ),
             column(width = 12,
                    lapply(X = seq_len(maxCounter), FUN = function(i) {
                      # condition on the slider value
                      conditionalPanel(condition = paste0("input.slider == ", i),
                                       # images are on github
                                       img(src = myImgResources[compList$IDBot[i]],width = '600px')
                      )
                    }),
             )
      ),
      column(width = 4,
             fluidRow(
               column(width = 6,
                      radioButtons('pictureSelection',label = 'Which hair pictures are better?',
                                   c('Top' = 'Top',
                                     'Bottom' = 'Bottom',
                                     'About the Same' = 'Same')),
                      actionButton('submitButton','Submit',width = 400)
               ),
               column(width = 6,
                      h3('Your current selection: \n'),
                      h3(textOutput('currentSelection'))
               )
             ),    
             br(),
             fluidRow(
               column(width = 6,
                      actionButton('backButton','Back',
                                   style = 'width:200px'),
               ),
               column(width = 6,
                      actionButton('skipButton','Skip',
                                   style = 'width:200px'),
               )
             ),
             br(),
             fluidRow(
               column(width = 8,
                      sliderInput(inputId = "slider", label = "Progress:", 
                                  min = 1, max = maxCounter, value = 1,step = 1)
               ),
               column(width = 3,
                      numericInput(inputId = 'skipTo',label = 'Skip to:',
                                   min = 1,max = maxCounter,value = 1,step = 1)
               ),
               column(width = 1,
                      actionButton(inputId = 'skipToButton',label = 'Go'))
             ),
             br(),
             fluidRow(
               actionButton('saveButton','Save',width = 400)
             )
      )
    )
  )
)

server <- function(input, output) {
  currentSelections = reactiveValues()
  lapply(1:maxCounter,function(x) currentSelections[[as.character(x)]] <- 'Undecided')
  observeEvent(input$submitButton,{
    x <- input$slider
    currentSelections[[as.character(x)]] <- input$pictureSelection
    updateSliderInput(inputId = 'slider',value = ifelse(x >= maxCounter,maxCounter,x + 1))
    output$currentSelection <- renderText({
      currentSelections[[as.character(input$slider)]]
    })
  })
  observeEvent(input$backButton,{
    x <- input$slider
    updateSliderInput(inputId = 'slider',value = ifelse(x <= 1,1,x - 1))
  })
  observeEvent(input$skipButton,{
    x <- input$slider
    updateSliderInput(inputId = 'slider',value = ifelse(x >= maxCounter,maxCounter,x + 1))
  })
  observeEvent(input$saveButton,{
    currentRanking <- read.csv('HairSelections.csv')
    rankingVector = lapply(1:maxCounter,function(x) isolate(currentSelections[[as.character(x)]])) %>% unlist()
    currentRanking[,'NewSelections'] = rankingVector
    currentRanking <- currentRanking %>% 
      mutate(SelectedPictures = ifelse(NewSelections != 'Undecided',NewSelections,SelectedPictures)) %>% 
      select(1:3)
    write.csv(currentRanking,file = 'HairSelections.csv',row.names = F)
    print('Saved!')
  })
  observeEvent(input$loadButton,{
    currentRanking <- read.csv('HairSelections.csv')
    lapply(1:maxCounter,function(x) currentSelections[[as.character(x)]] <- currentRanking[x,'SelectedPictures'])
    output$currentSelection <- renderText({
      currentSelections[[as.character(input$slider)]]
    })
  })
  observeEvent(input$skipToButton,{
    skipToVal <- input$skipTo
    updateSliderInput(inputId = 'slider',value = skipToVal)
  })
}

shinyApp(ui = ui, server = server)

#To do:
#-Make save feature streamlined to clean csv
#-Make load feature
#-Make images load locally
#-Make skip button
#-*Make picture comparison lists
#-Clean up interface
#Add up down and equal icons

