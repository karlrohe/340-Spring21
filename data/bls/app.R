library(shiny)
library(tidyverse)
library(ggplot2)
library(blscrapeR)
library(magrittr)
library(ggiraph)

ui <- fillPage(
    tags$head(
        tags$style(HTML("
      div.girafe_container_std>svg{
        max-height: 90vh !important;
      }
    "))
    ),
    girafeOutput("unemployment",height="88%"),
    div(style = "margin: auto; width: 80%",
        sliderInput("year","Year:",min=1990,max=2019,value=2019,width="100%",)
    )
    
)

server <- function(input, output, session) {
    
    # read in csv, making sure to keep fips codes as characters
    unemployment = read.csv("unemployment.csv",colClasses=c(rep("character",5),rep("numeric",5)))
    
    # create id column for matching values to regions in geom_map, and convert year to integer
    unemployment %<>% mutate(id = as.character(paste0(stateFIPS,countyFIPS)),
                             year = as.integer(year))
    
    # create tooltip description column
    unemployment %<>% mutate(desc=sprintf("%s, %s\n%.1f%%",countyName,stateName,unemployment_rate))
    
    output$unemployment <- renderGirafe({
        G = unemployment %>% 
            filter(year==input$year) %>%
            ggplot(aes(map_id=id)) + theme_void() + coord_fixed() +
            geom_map_interactive(aes(fill=unemployment_rate,tooltip=desc,data_id=id),map=county_map_data,color="black",size=.1) +
            expand_limits(x=county_map_data$long,y=county_map_data$lat) +
            geom_map(data=state_map_data,aes(group=group),map=state_map_data,fill=NA,color="black",size=.2) +
            scale_fill_continuous(type="viridis",trans='log10',
                                  breaks=B<-c(.7,1,1.5,2,2.5,3,4,5,6,8,10,12,14,16,18,20),labels=B,
                                  limits=c(0.7,20)) +
            guides(fill = guide_colourbar(barwidth=1,barheight=20,title="Rate (%)",
                                          title.theme=element_text(margin=unit(c(0,0,12,0),"pt")))) +
            ggtitle(sprintf("US unemployment by county (%d)",input$year)) + theme(plot.title=element_text(hjust=.5,size=16))
        girafe(code = print(G),width_svg=8.6,height_svg=5.8)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
