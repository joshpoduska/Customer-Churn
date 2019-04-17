# load the required packages

list.of.packages <- c("MASS", "shiny", "shinydashboard", "Rcpp", "httpuv", 
                      "moments", "shinycssloaders", "shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(MASS)
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(gtable)
library(grid)
library(gridExtra)
library(moments)
library(lubridate)
library(scales)
library(shinycssloaders)
library(shinyjs)

list.of.packages <- c("lime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_github("thomasp85/lime")
library(lime)






#############################################################################
################################## UI #######################################
#############################################################################

# read data and get list of customers
df <- read.csv("data/modelOut.csv", header = TRUE)
custs <- sort(unique(df$custid))


# height of graphs
distheight = "80px"

#for refreshing pages
jscode <- "shinyjs.refresh = function() { history.go(0); }"

# define the content of the dashboard page
frow <- fluidRow(
  #pos and size of progress bar
  tags$head(
  tags$style(
    HTML(".shiny-notification {
              height: 70px;
              width: 400px;
              position:fixed;
              top: calc(50% - 30px);;
              left: calc(50% - 200px);;
            }
           "
    )
  )
),
#refresh the html page with js$refresh(); in observeEvent
useShinyjs(),
extendShinyjs(text = jscode, functions = c("refresh")),
  column( width = 8,
          box(
            title = "Likelihood to Churn"
            #,background = "blue"
            ,collapsible = FALSE
            ,solidHeader = FALSE
            ,width = NULL
            ,status = "primary"
            ,plotOutput("churn", height = "200px") %>% withSpinner(color="#0dc5c1")
          ),
          fluidRow(
            column(width=6,
                   valueBoxOutput("value1", width=NULL) %>% withSpinner(color="#0dc5c1")
            ),
            column(width=6,
                   valueBoxOutput("value2", width=NULL) %>% withSpinner(color="#0dc5c1")
            )
          ),
          box(
            title = "Customer Churn Explanations via Lime"
            ,collapsible = F
            ,solidHeader = F
            ,width = NULL
            ,status = "primary"
            ,plotOutput('lime') %>% withSpinner(color="#0dc5c1")
          )
  ),
  column(width=4, 
         h4("Customer Profile Compared to Baseline", align='center')
         ,box(
           title = "% Dropped Calls"
           ,status = "primary"
           ,solidHeader = F
           ,collapsible = F 
           ,width = NULL
           ,plotOutput("dropperc", height = distheight) %>% withSpinner(color="#0dc5c1")
         )
         ,box(
           title = "Mins Used per Month"
           ,status = "primary"
           ,solidHeader = F
           ,collapsible = F
           ,width = NULL
           ,plotOutput("mins", height = distheight) %>% withSpinner(color="#0dc5c1")
         )
         ,box(
           title = "Consec Months as Cust"
           ,status = "primary"
           ,solidHeader = F
           ,collapsible = F
           ,width = NULL
           ,plotOutput("consecmonths", height = distheight) %>% withSpinner(color="#0dc5c1")
         )
         ,box(
           title = "Income"
           ,status = "primary"
           ,solidHeader = F 
           ,collapsible = F 
           ,width = NULL
           ,plotOutput("income", height = distheight) %>% withSpinner(color="#0dc5c1")
         )
  )
)

frow2 <- fluidRow(height = 1000,
  h3("Model Outputs", align='center'),
  plotOutput("img_output") %>% withSpinner(color="#0dc5c1"),
  
  fluidRow(height = 750,
           h3("Model Inputs", align='center'),
           plotOutput("img_dropperc") %>% withSpinner(color="#0dc5c1")
           ),
  
  fluidRow(height = 750,
           plotOutput("img_consecmonths") %>% withSpinner(color="#0dc5c1")
  ),
  
  fluidRow(height = 750,
           plotOutput("img_mins") %>% withSpinner(color="#0dc5c1")
  ),
  
  fluidRow(height = 750,
           plotOutput("img_income") %>% withSpinner(color="#0dc5c1")
  )
)
 

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Churn Modeling")  

sidebar <- dashboardSidebar(
  sidebarMenu(id="menu",
    menuItem("Customer Dashboard"
             ,tabName = "dashboard"
             #,p(HTML("<A HREF=\"javascript:history.go(0)\">Redraw Graphs</A>"))
             ,icon = icon("dashboard")
             ,menuSubItem(tabName = "dashboard"
                          ,selectInput("custID"
                                       ,"Customer ID"
                                       ,choices = custs,
                                       multiple=F,
                                       width = '98%'
                                       )
                         )
             ,startExpanded = T
             ),
    menuItem("Model Health"
             ,icon = icon("cog")
             ,tabName = "model"
             ,menuSubItem(tabName = "model"
               ,column(width=4 
                      ,actionButton("model_call", "New Model API Call")
                      ,actionButton("get_logs", "Get Latest Logs")
               )
             )
             ,startExpanded = T
  )
)
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard"
            ,frow
    ),
    
    tabItem(tabName = "model"
            ,frow2
    )
  )
)






# Put them together into a dashboardPage
ui <- dashboardPage(header, sidebar, body)






##################################################################################
###################################### SERVER ####################################
##################################################################################





server <- function(input, output, session) { 
  
  # get and prepare logs
  observeEvent(input$get_logs, {
    withProgress(message = 'Getting logs',
                 detail = 'This should take < 1 min...', value = 0, {
                   for (i in 1:10) {
                     if (i == 7) {
                       system("jupyter nbconvert --to notebook --execute ./code/parse_logs.ipynb")
                     }
                     incProgress(1/10)
                     if (i < 6) {
                       Sys.sleep(0.25)
                       } 
                     if (i >= 6 ) {
                       Sys.sleep(0.15)
                     }
                   }
                 })
    js$refresh();
    
    #set focus to model tab
    #can't get it to work. think i may need to specify tab stuff in dashboard body
    #updateTabItems(session, "menu", "model")
  })
  
  # make a new API call
  observeEvent(input$model_call, {
    withProgress(message = 'Making API Call',
                 detail = 'with random values...', value = 0, {
                   for (i in 1:10) {
                     if (i == 7) {
                       system("jupyter nbconvert --to notebook --execute ./code/call_churn_api.ipynb")
                     }
                     incProgress(1/10)
                     if (i < 6) {
                       Sys.sleep(0.15)
                     } 
                     if (i >= 6 ) {
                       Sys.sleep(0.1)
                     }
                   }
                 })
  })
  
  
  
  output$churn <- renderPlot({
    df$rank <- order(df$prob)
    rmax <- max(df$rank)
    currank <- filter(df, custid == input$custID)$rank
    gauge <- data.frame(matrix(nrow=1, ncol = 1))
    names(gauge) <- c("rank")
    gauge$rank <- round(currank/rmax,2)
    gauge <- gauge %>% mutate(group=ifelse(rank <=0.5, "green",
                                           ifelse(rank>0.5 & rank<0.75, "orange","red")),
                              label=paste0(rank*100, "%"),
                              title=paste("Customer ", as.character(input$custID), sep=""))
    ggplot(gauge, aes(fill = group, ymax = rank, ymin = 0, xmax = 2, xmin = 1)) +
      geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
      geom_rect() +
      coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
      geom_text(aes(x = 0, y = 0, label = label, color=group), size=6.5, family="Poppins SemiBold") +
      geom_text(aes(x=1.5, y=1.5, label=title), family="Poppins Light", size=5.2) +
      #facet_wrap(~title, ncol = 1) +
      theme_void() +
      scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
      scale_color_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank()) +
      guides(fill=FALSE) +
      guides(color=FALSE)
    })
  
  #some data manipulation to derive the values of KPI boxes
  #lastbill <- filter(df, custid == input$custID)$bill
  #age <- filter(df, custid == input$custID)$age
  #datausage <- filter(df, custid == input$custID)$datause
  #creating the valueBoxOutput content
  
  output$value1 <- renderValueBox({
    valueBox(
      formatC(filter(df, custid == input$custID)$age, format="d", big.mark=',')
      ,'Age'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(filter(df, custid == input$custID)$bill)#, format="d", big.mark=',')
      ,'Last Bill'
      ,icon = icon("usd",lib='glyphicon')
      ,color = "green")  
  })
  
  
  #dropperc
  output$dropperc <- renderPlot({
    x <- df$dropperc
    bw <-  (2 * IQR(x)) / length(x)^(1/3)
    value <- round(filter(df, custid == input$custID)$dropperc,5)
    nb <- ceiling(diff(range(x))/bw)
    hl <- round((value - (min(x) - bw)) /bw)
    colors <- c(rep("grey45",hl-1), rep("red",1), rep("grey45",nb-hl))
    x_title <- paste("% Dropped = ", as.character(value), sep="")
    ggplot() + geom_histogram(aes(x), fill=colors, bins = nb) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),legend.position="none")+
      labs(x = x_title)
  })
  
  
  #mins
  output$mins <- renderPlot({
    x <- df$mins
    bw <-  (2 * IQR(x)) / length(x)^(1/3)
    value <- filter(df, custid == input$custID)$mins
    nb <- ceiling(diff(range(x))/bw)
    hl <- round((value - (min(x) - bw)) /bw)
    colors <- c(rep("grey45",hl-1), rep("red",1), rep("grey45",nb-hl))
    x_title <- paste("Mins = ", as.character(value), sep="")
    ggplot() + geom_histogram(aes(x), fill=colors, bins = nb) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),legend.position="none")+
      labs(x = x_title)
  })
  
  
  #consecmonths
  output$consecmonths <- renderPlot({
    x <- df$consecmonths
    bw <-  (2 * IQR(x)) / length(x)^(1/3)
    value <- filter(df, custid == input$custID)$consecmonths
    nb <- ceiling(diff(range(x))/bw)
    hl <- round((value - (min(x) - bw)) /bw)
    colors <- c(rep("grey45",hl-1), rep("red",1), rep("grey45",nb-hl))
    x_title <- paste("Months = ", as.character(value), sep="")
    ggplot() + geom_histogram(aes(x), fill=colors, bins = nb) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),legend.position="none")+
      labs(x = x_title)
  })
  
  
  #income
  output$income <- renderPlot({
    x <- df$income
    bw <-  (2 * IQR(x)) / length(x)^(1/3)
    value <- filter(df, custid == input$custID)$income
    nb <- ceiling(diff(range(x))/bw)
    hl <- round((value - (min(x) - bw)) /bw)
    colors <- c(rep("grey45",hl-1), rep("red",1), rep("grey45",nb-hl))
    x_title <- paste("Income = $", as.character(value), "K", sep="")
    ggplot() + geom_histogram(aes(x), fill=colors, bins = nb) +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.y=element_blank(),legend.position="none")+
      labs(x = x_title)
  })
  
  
  
  
  ################ lime
  
  
  
  
  output$lime <- renderPlot({
    
    df <- read.csv("data/modelOut.csv", header = TRUE)
    y <- ifelse(df$churn_Y==1,"1","0")
    myvars <- c("dropperc", "income", "mins", "consecmonths")
    df2 <- df[myvars]
    model2 <- lda(df2, y)
    selectedCust = input$custID
    test_set <- df[df$custid == selectedCust, ]
    test_set <- test_set[myvars]
    explanation <- lime(df2, model2)
    explanations <- explain(test_set[myvars], explanation, n_labels = 1, n_features = 4)
    plot_features(explanations)
  })
    
  
  
  
  ##################### model health
  
  
  
  
  
  #get fresh data
  df_logs <- read.csv("data/parsed_model_log.csv", header = T, stringsAsFactors=F)
  df_logs_baseline <- read.csv("data/parsed_model_log_baseline.csv", header = T, 
                               stringsAsFactors=F)
  
  plot_logs <- function(var) {
    
    #manually test function
    #var <- 'output'
    
    df <- na.omit(df_logs[,c(var,"timeStamp")])
    df_baseline <- na.omit(df_logs_baseline[,c(var,"timeStamp")])
    
    #graph title
    gtitle = paste('Trend of ', var, '\nFor Selected Model Runs\nCompared to Baseline', sep="")
    
       #format dates
    t <- substr(df$timeStamp, 1, nchar(df$timeStamp)-4)
    dates <- ymd_hms(t)
    
    #feed in parameters
    y <- df[,c(1)]
    y_baseline <- df_baseline[,c(1)]
    
    #get data ready for graphing
    dat <- data.frame(y=y, x=dates)
    
    min_baseline <- min(y_baseline)
    max_baseline <- max(y_baseline)
    min_y <- min(y)
    max_y <- max(y)
    max_both <- max(max_baseline, max_y)
    min_both <- min(min_baseline, min_y)
    baseline_999 = quantile(y_baseline, c(.999)) 
    baseline_001 = quantile(y_baseline, c(.001)) 
    
    oob = y > max_baseline | y < min_baseline
    warn = (y > baseline_999 & y <= max_baseline) | (y < baseline_001 & y >= min_baseline)
    reg = !oob & !warn
    
    dat_oob <- data.frame()
    dat_warn <- data.frame()
    dat_reg <- data.frame()
    
    if (length(y[oob])>0) dat_oob <- data.frame(y = y[oob],x = dates[oob],type='OOB')
    if (length(y[warn])>0) dat_warn <- data.frame(y = y[warn],x = dates[warn],type='Warn')
    if (length(y[reg])>0) dat_reg <- data.frame(y = y[reg],x = dates[reg],type="Normal")
    dat_all <- data.frame(y=y,x=dates)
    dat <- bind_rows(dat_oob, dat_warn, dat_reg)
    dat_all$type <- NA
    
    #main graph components
    p <- ggplot(data=dat_reg,aes(linetype=type, color=type, fill=type)) +
      geom_line(data=dat_all, aes(x=x, y=y),size=1, linetype='solid', color='blue') + 
      geom_point(aes(x=x, y=y), size=2, shape=21) +
      expand_limits(y=c(min_both, max_both)) +
      xlab("Date") +
      #scale_x_datetime(breaks = date_breaks("24 hours")) +
      scale_x_datetime(labels = date_format("%Y-%m-%d")) + 
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      geom_hline(yintercept = min_baseline, color = "red") +
      geom_hline(data=dat_all, aes(yintercept=max_baseline,
                                   linetype="Historical Min/Max", color="Historical Min/Max", 
                                   fill="Historical Min/Max")) +
      ggtitle(gtitle) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title=element_blank()) + 
      geom_hline(aes(yintercept=baseline_999, linetype="1/1000 Event",color="1/1000 Event",
                     fill="1/1000 Event")) + 
      geom_hline(yintercept=baseline_001, linetype='dashed', color='magenta') +
      theme(plot.margin=unit(c(.2,.2,.2,.2), "cm")) +
      theme(axis.title.y=element_blank())#,
    #     axis.text.y=element_blank(),
    #       axis.ticks.y=element_blank())
    
    #warn and oob
    if ((nrow(dat_warn)>0) & (nrow(dat_oob)>0)) {
      p = p + geom_point(data=dat_warn, aes(x=x, y=y, linetype="Warning", color="Warning", fill="Warning")
                         , size=2, shape=21)
      p = p + geom_point(data=dat_oob, aes(x=x, y=y, linetype="Out of Bounds", 
                                           color="Out of Bounds", fill="Out of Bounds"), size=2, shape=21)
      breaks_list = c("Historical Min/Max","1/1000 Event", "Normal", "Warning", "Out of Bounds")
      color_values_list = c("Historical Min/Max"="red","1/1000 Event"="magenta","Normal"="blue",
                            "Out of Bounds"="red","Warning"="magenta")
      linetpye_values_list = c("Historical Min/Max"=1,"1/1000 Event"=2,"Normal"=0,"Out of Bounds"=0,
                               "Warning"=0)
      shape_list = c("Historical Min/Max"=NA,"1/1000 Event"=NA,"Normal"=16,"Out of Bounds"=16,"Warning"=16)
      #print('all')
    }
    
    #warn only
    if ((nrow(dat_warn)>0) & (!(nrow(dat_oob)>0))) {
      p = p + geom_point(data=dat_warn, aes(x=x, y=y, linetype="Warning", color="Warning", fill="Warning")
                         , size=2, shape=21)
      breaks_list = c("Historical Min/Max","1/1000 Event", "Normal", "Warning")
      color_values_list = c("Historical Min/Max"="red","1/1000 Event"="magenta","Normal"="blue",
                            "Warning"="magenta")
      linetpye_values_list = c("Historical Min/Max"=1,"1/1000 Event"=2,"Normal"=0,"Warning"=0)
      shape_list = c("Historical Min/Max"=NA,"1/1000 Event"=NA,"Normal"=16,"Warning"=16)
      #print('warn')
    }
    
    #oob only
    if (!(nrow(dat_warn)>0) & (nrow(dat_oob)>0)) {
      p = p + geom_point(data=dat_oob, aes(x=x, y=y, linetype="Out of Bounds", 
                                           color="Out of Bounds", fill="Out of Bounds"), size=2, shape=21)
      breaks_list = c("Historical Min/Max","1/1000 Event", "Normal", "Out of Bounds")
      color_values_list = c("Historical Min/Max"="red","1/1000 Event"="magenta","Normal"="blue",
                            "Out of Bounds"="red")
      linetpye_values_list = c("Historical Min/Max"=1,"1/1000 Event"=2,"Normal"=0,"Out of Bounds"=0)
      shape_list = c("Historical Min/Max"=NA,"1/1000 Event"=NA,"Normal"=16,"Out of Bounds"=16)
      #print('oob')
    }
    
    #neither
    if (!(nrow(dat_warn)>0) & (!(nrow(dat_oob)>0))) {
      breaks_list = c("Historical Min/Max","1/1000 Event", "Normal")
      color_values_list = c("Historical Min/Max"="red","1/1000 Event"="magenta","Normal"="blue")
      linetpye_values_list = c("Historical Min/Max"=1,"1/1000 Event"=2,"Normal"=0)
      shape_list = c("Historical Min/Max"=NA,"1/1000 Event"=NA,"Normal"=16)
      #print('none')
    }
    
    p = p + scale_fill_manual(breaks=breaks_list, values=color_values_list) +
      scale_color_manual(breaks=breaks_list, values=color_values_list) +
      scale_linetype_manual(breaks=breaks_list, values=linetpye_values_list)
    p = p + guides(color=guide_legend(override.aes=list(shape=shape_list)))
    
    return(p)
  }
  
  #function to make the histogram
  plot_hist <- function(var) {
    
    #test function
    #var <- 'data.mins'
    
    #feed in parameters
    y <- data.frame(na.omit(df_logs[,c(var)]))
    colnames(y) <- c("y")
    y_baseline <- data.frame(na.omit(df_logs_baseline[,c(var)]))
    colnames(y_baseline) <- c("y")
    
    #graph title
    gtitle = paste("Count\nof\n", var, sep="")
    
    #get data ready for graphing
    min_baseline <- min(y_baseline)
    max_baseline <- max(y_baseline)
    min_y <- min(y)
    max_y <- max(y)
    max_both <- max(max_baseline, max_y)
    min_both <- min(min_baseline, min_y)
    
    bins = as.integer(min(max(-1/10 * abs(abs(kurtosis(y_baseline)) - 3)**2 + 20, 5),30))
    bin_width = (max_baseline - min_baseline)/30
    
    hist <- ggplot(data=y_baseline, aes(x=y)) + 
      geom_histogram(color="blue", fill="blue", binwidth = bin_width, boundary = 0, closed = "left") +
      ggtitle(" \n \n ") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(expand = c(0, -3)) +
      expand_limits(x=c(min_both, max_both)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position="none",
            plot.margin=unit(c(.2,0,.3,.3), "cm")) +
      coord_flip() +
      labs(y = gtitle) +
      labs(x = var)
    
    return(hist)
  }
  
  

  output$img_output <- renderPlot({
    
    #rerun when button is clicked
    #input$get_logs
    
    #get fresh data
    df_logs <- read.csv("data/parsed_model_log.csv", header = T, stringsAsFactors=F)
    df_logs_baseline <- read.csv("data/parsed_model_log_baseline.csv", header = T, 
                                 stringsAsFactors=F)
   
    #make plots
    plt <- plot_logs('output')
    hst <- plot_hist('output')
    grid.arrange(ggplotGrob(hst), ggplotGrob(plt), nrow=1, widths=c(1.5,6))
  })
  
  
  
  
  
  
  output$img_consecmonths <- renderPlot({
    
    #rerun when button is clicked
    #input$get_logs
    
    #get fresh data
    df_logs <- read.csv("data/parsed_model_log.csv", header = T, stringsAsFactors=F)
    df_logs_baseline <- read.csv("data/parsed_model_log_baseline.csv", header = T, 
                                 stringsAsFactors=F)
    
    #make plots
    plt <- plot_logs('data.consecmonths')
    hst <- plot_hist('data.consecmonths')
    grid.arrange(ggplotGrob(hst), ggplotGrob(plt), nrow=1, widths=c(1.5,6))
  })
  
  
  
  
  
  
  output$img_income <- renderPlot({
    
    #rerun when button is clicked
    #input$get_logs
    
    #get fresh data
    df_logs <- read.csv("data/parsed_model_log.csv", header = T, stringsAsFactors=F)
    df_logs_baseline <- read.csv("data/parsed_model_log_baseline.csv", header = T, 
                                 stringsAsFactors=F)
    
    #make plots
    plt <- plot_logs('data.income')
    hst <- plot_hist('data.income')
    grid.arrange(ggplotGrob(hst), ggplotGrob(plt), nrow=1, widths=c(1.5,6))
  })
  
  
  
  
  
  output$img_dropperc <- renderPlot({
    
    #rerun when button is clicked
    #input$get_logs
    
    #get fresh data
    df_logs <- read.csv("data/parsed_model_log.csv", header = T, stringsAsFactors=F)
    df_logs_baseline <- read.csv("data/parsed_model_log_baseline.csv", header = T, 
                                 stringsAsFactors=F)
    
    #make plots
    plt <- plot_logs('data.dropperc')
    hst <- plot_hist('data.dropperc')
    grid.arrange(ggplotGrob(hst), ggplotGrob(plt), nrow=1, widths=c(1.5,6))
  })
  
  
  
  
  output$img_mins <- renderPlot({
    
    #rerun when button is clicked
    #input$get_logs
    
    #get fresh data
    df_logs <- read.csv("data/parsed_model_log.csv", header = T, stringsAsFactors=F)
    df_logs_baseline <- read.csv("data/parsed_model_log_baseline.csv", header = T, 
                                 stringsAsFactors=F)
    
    #make plots
    plt <- plot_logs('data.mins')
    hst <- plot_hist('data.mins')
    grid.arrange(ggplotGrob(hst), ggplotGrob(plt), nrow=1, widths=c(1.5,6))
  })
  
  
  }

#run/call the shiny app
shinyApp(ui, server)
