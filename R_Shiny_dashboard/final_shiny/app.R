library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(colorspace)
library(dashboardthemes)
library(kableExtra)
library(maps)
library(ggplot2)
library(leaflet)
library(plotly)
library(mapproj)
library(dplyr)
library(ggiraphExtra)
library(dplyr)
library(showtext)
library(RColorBrewer)
library(sf)
library(ggrepel)
library(paletteer)
library(viridis)
library(ggthemes)
library(leaflegend)
library(thematic)
library(sjmisc)
library(bslib)
library(bsicons)
library(factoextra)
library(cluster)
library(lattice)
library(tidystats)
library(tsibble)
library(bsicons)

### 데이터 불러오기 및 전처리
data <- read.csv("./sales.csv")
data = na.omit(data)
data$order_date = as.Date(data$order_date)
data$ship_date = as.Date(data$ship_date)
measure_type <- c("합계", "판매량", "평균")
week_sale = data %>% group_by(week) %>%
  summarise(sum = sum(Sales), mean = mean(Sales), n=n())
sta_sale = data %>% group_by(State, Lat, Long) %>%
  summarise(sum = sum(Sales), n = n(), mean = mean(Sales))
state_map <- map("state", fill=TRUE, plot=FALSE)
state_map$names = str_to_title(state_map$names)
sta_sale2 = sta_sale
current_date = as.Date("2019-01-01")


for (i in c(1:length(state_map$names))) {
  for (j in c(1: length(sta_sale$State))) {
    if (str_contains(state_map$names[i], sta_sale$State[j])) {
      state_map$names[i] <- sta_sale$State[j]
    }
  }
}

repeat_dt = c()
for(i in sta_sale2$State){
  num=0
  for(j in state_map$names){
    if(i == j){
      print(i)
      num= num+1
    }
  }
  if(num>1){
    repeat_dt = rbind(repeat_dt,c(i,num))}
}

sta_sale3 = sta_sale2

for(i in 1:length(repeat_dt[,1])){
  for(j in c(1:(as.integer(repeat_dt[i,2])-1))){
    sta_sale3=rbind(sta_sale3,sta_sale2[sta_sale2$State==repeat_dt[i,1],])}
}
sta_sale4 = arrange(sta_sale3,(State),Lat,Long,sum,n,mean)
sta_sale5 = sta_sale4[-56,]
tmp = sta_sale5[c(61,62,63),]
sta_sale5 = sta_sale5[-c(61,62,63),]
sta_sale5 = rbind(sta_sale5,sta_sale5[55,])
sta_sale5 = rbind(sta_sale5,tmp)

### value box에 사용할 데이터 전처리
kpi_dt=data%>%group_by(year)%>%summarise(sum_of_sale = sum(Sales))%>%filter(year == 2017 | year==2018)
sale17 = kpi_dt[1,2]
sale18 = kpi_dt[2,2]
rate_17_18 = sale17/sale18
rate_18_17= sale18/sale17
rate = rbind(rate_17_18 = sale17/sale18,rate_18_17= sale18/sale17)
rate = data.frame(rate)
rate = rename(rate,"Rate" = "sum_of_sale")
rownames(rate) = c("2017","2018")
value_17_18 = sale17 - sale18
value_18_17 = sale18-sale17
value = rbind(value_17_18,value_18_17)
value = data.frame(value)
value = rename(value,"Value" = "sum_of_sale")
rownames(value) = c("2017","2018")
kpi_dt = data.frame(kpi_dt)
kpi_dt = cbind(kpi_dt,rate,value)

### 크루스컬-월리스 검정 결과 저장 및 불러오기
ktest <- kruskal.test(data = data,Sales~Sub_Category)
sink("ktest.txt")
ktest
sink()

### 기본으로 지정할 테마 설정
thematic::thematic_shiny(font="auto")
light_mode <- bs_theme(version = 5, bootswatch = "minty",
                       bg='#f5f5f5', fg='#030303',
                       primary='#87AEDF', secondary = '#E494AB',
                       base_font = font_collection(font_google("Bree Serif"),
                                                   font_google('Gowun Batang')), #
                       code_font = font_google("Space Mono"),
                       heading_font = font_collection(font_google("Bree Serif"),
                                                      font_google('Gowun Batang'))
)

ui <- page_navbar(
  
  theme = light_mode,
  title = "Market Analysis",
  underline = TRUE,
  
  ### first tab
  nav_panel(title = "Sales",
            h2("거래 현황"),
            fluidRow(
              layout_column_wrap(
                width = 1/2, height = 100,
                value_box(
                  title = "YoY",
                  value = round((kpi_dt$Rate[2]-1)*100,1),
                  p("increased"),
                  showcase = bs_icon('percent'),
                  showcase_layout = "left center",
                  theme = 'dark'
                ),
                value_box(
                  title = "YoY",
                  value = round(kpi_dt$Value[2]),
                  p("increased"),
                  showcase = bs_icon('currency-dollar'),
                  showcase_layout = "left center",
                  theme = 'dark'
                ),
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              leafletOutput("map"),
            ),
            card(
              height = 600,
              card_header(class = "bg-dark",
                          "주별 판매액"),
              
              card_body(selectInput("type1",
                                    label="판매액 형태",
                                    choices = measure_type),
                        fluidRow(plotOutput("Sum")))
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                height = 500,
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                card_header(class = "bg-dark",
                            "월별 판매액"),
                card_body(
                  selectInput("type2",
                              label="판매액 형태",
                              choices = measure_type),
                  plotOutput("by_month")
                )
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                card_header(class = "bg-dark",
                            "연도별 판매액"),
                card_body(
                  selectInput("type3",
                              label="판매액 형태",
                              choices = measure_type),
                  plotOutput("by_year")
                )
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                card_header(class = "bg-dark",
                            "분기별 판매액"),
                card_body(
                  class = "px-1 justify-content-center align-items-center",
                  plotOutput("quater"),
                  plotOutput("quater_by_cat")
                )
              )
            )
  ),
  ### second tab
  nav_panel(title = "Marketing",
            h2("세그먼트 분석"),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 600,
                card_header(class = "bg-dark",
                            "지역별 고객 현황"),
                card_body(
                  class = 'px-4',
                  selectInput("cust",
                              label="지역별 고객 수/비율",
                              choices = c("수", "비율")),
                  fluidRow(
                    class = 'ps-0 pe-4 w-90',
                    plotOutput("customer_num")
                  ),
                ),
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 600,
                card_header(class = "bg-dark",
                            "세그먼트별 판매액 합과 평균"),
                card_body(
                  class = 'px-4',
                  selectInput("type5",
                              label="세그먼트별 판매액",
                              choices = c("합계", "평균")),
                  plotOutput("by_cust")
                ),
              ),
            ),
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 500,
                card_header(class="bg-dark",
                            "세그먼트별 배송 형태"),
                card_body(
                  class = 'px-4',
                  fluidRow(
                    plotOutput("cust_by_delivery")
                  )
                )
              ),
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 1600,
                card_header(class = "bg-dark",
                            "RFM 분석"),
                card_body(
                  class = 'px-1 justify-content-center align-items-center',
                  h4("변수변환 통한 데이터 치우침 완화"),
                  layout_column_wrap(
                    width = 1/3, height = 300,
                    plotOutput("hist_for_RFM1") |> tooltip("log 변환"),
                    plotOutput("hist_for_RFM2") |> tooltip("square root 변환"),
                    plotOutput("hist_for_RFM3") |> tooltip("log & square 변환"),
                  )),
                card_body(
                  layout_column_wrap(
                    width = 1/2,
                    height = 150,
                    class = 'px-1 justify-content-center align-items-center',
                    actionButton("before", "Before"),
                    actionButton("after", "After"),
                    h3("K-Means Clustering"),
                    sliderInput("centers", "Centers:",
                                min = 2, max = 9,
                                value = 4, step = 1),
                  )
                ),
                card_body(
                  height = 1000,
                  
                  fluidRow(
                    class = 'px-2',
                    layout_column_wrap(
                      width = 1/2,
                      plotOutput("silhouette"),
                      plotOutput("clustered_plot"),
                    )
                  ),
                  
                  fluidRow(
                    class = 'px-4 w-90 justify-content-center align-items-center',
                    layout_column_wrap(
                      width = 1/2, height = 100,
                      h3("클러스터 그룹별 특징"),
                      selectInput("type6",
                                  label="Grouping feature",
                                  choices = c("Recency", "Frequency", "Monetary"))),
                    plotOutput("feature_plot"),
                  )
                )
              ),
            ),
  ),
  ### third tab
  nav_panel(title = "Product & Delivery",
            h2("제품군 및 배송 분석"),
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 600,
                card_header(class = "bg-dark",
                            "제품군 판매"),
                card_body(
                  class = 'px-4',
                  selectInput("prd_type",
                              label=" ",
                              choices = c("판매액", "판매량")),
                  plotOutput("product"),
                )
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                card_header(class = "bg-dark",
                            "연도별 카테고리 판매액 추이"),
                card_body(
                  class = 'px-4',
                  selectInput("type4",
                              label = " ",
                              choices = measure_type),
                  plotOutput("cat_sales_by_year"),
                )
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 1500,
                card_header(class = "bg-dark",
                            "카테고리별 판매액"),
                card_body(
                  class = 'px-4',
                  plotOutput("prod_box", height = 800,
                             dblclick = "box_dbclick",
                             brush = brushOpts(id = "outlier_brush",
                                               resetOnNew = TRUE)),
                ),
                card_body(
                  height = 300,
                  class = 'px-4',
                  h3("카테고리별 판매액 - 제품군 상세"),
                  tableOutput("outlier")
                  ,
                ),
              )),
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 1200,
                card_header(class = "bg-dark",
                            "배송 관리"),
                card_body(
                  class = 'px-4',
                  h4("배송 유형별 지연 날짜"),
                  plotOutput("delivery_type_by_sub_cat"),
                ),
                card_body(
                  class = 'px-4',
                  h4("카테고리별 평균 배송 지연날짜"),
                  plotOutput("delay_by_cat"),
                ),
              )
            ),
            
            fluidRow(
              class = 'mx-2',
              card(
                class = "w-80 p-0 shadow p-0 mb-3 bg-body rounded",
                height = 1000,
                card_header(class = 'bg-dark',
                            "크루스컬 월리스 비모수 검정"),
                card_body(
                  class = 'px-4',
                  plotOutput("kruskal"),
                ),
                card_body(
                  height = 200,
                  class = 'px-4 justify-content-center align-items-center',
                  tableOutput("conf_lev")
                ),
                card_body(
                  class = 'px-4',
                  plotOutput("wilcoxon")
                ))
            )
  ),
)


server <- function(input, output, session) {
  
  bs_themer()
  
  #state(주)별 판매액
  sta_sale = data %>% group_by(State, Lat, Long) %>%
    summarise(sum = sum(Sales), n = n(), mean = mean(Sales))
  sta_sale
  ggplot(sta_sale, aes(x = n, y = sum, fill = State, color = State)) + geom_point()
  ggplot(sta_sale, aes(x = State, y = sum , fill = State)) +
    geom_bar(stat = 'identity', position='dodge') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggplot(sta_sale, aes(x = State, y = mean, fill = State)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  cc <- paletteer_c("grDevices::Dark 2", 30)
  cc2 <- paletteer_c("grDevices::Blue-Red 2", 30)
  cc <- colorRampPalette(cc)(49)
  cc2 <- colorRampPalette(cc2)(50)
  
  domain <- sta_sale$sum
  domain2 <- sta_sale$mean
  pp <- paletteer_c("ggthemes::Sunset-Sunrise Diverging", 30)
  pa <- colorNumeric(palette = as.character(pp), domain=domain)
  pa2 <- colorNumeric(palette = as.character(cc2), domain=domain2)
  
  popup_sb <- paste(
    "State: ", as.character(sta_sale5$State), "<br/>", #
    "Sum of Sales: ", as.character(sta_sale5$sum), "<br/>",
    "Amount of Sales: ", as.character(sta_sale5$n), sep="") %>%
    lapply(htmltools::HTML)
  
  popup_sb2 <- paste(
    "State: ", as.character(state_map$names), "<br/>",
    "Sum of Sales: ", as.character(sta_sale5$sum), "<br/>",
    "Amount of Sales: ", as.character(sta_sale5$n), sep="") %>%
    lapply(htmltools::HTML)
  
  
  
  output$map <- renderLeaflet(
    leaflet(state_map) %>%
      setView(lat=36.818, lng=-99.009, zoom=4) %>%
      addTiles() %>%
      addPolygons(fillColor = cc,
                  color="black",
                  label = popup_sb2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "13px", direction = "auto"),
                  weight=1) %>%
      addCircleMarkers(data=sta_sale5,
                       lng= ~Long, lat=~Lat,
                       stroke = FALSE,
                       label = popup_sb,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "3px 8px"),
                         textsize = "13px", direction = "auto"),
                       radius = sta_sale5$n * 0.05,
                       color = ~pa(domain),
                       fillOpacity = 0.6,
                       fillColor = ~pa(domain),
                       group = "Sum"
      ) %>%
      addCircleMarkers(data=sta_sale5,
                       lng= ~Long, lat=~Lat,
                       stroke = FALSE,
                       label = popup_sb,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal",
                                      padding = "3px 8px"),
                         textsize = "13px", direction = "auto"),
                       radius = sta_sale5$n * 0.05,
                       color = ~pa2(domain2),
                       fillOpacity = 0.6,
                       fillColor = ~pa2(domain2),
                       group = "Mean") %>%
      addLegend('bottomright', pal=pa, values=~sta_sale5$sum,
                title='Sales', opacity=1) %>%
      addLayersControl(baseGroups = c("Sum", "Mean"),
                       options = layersControlOptions(collapsed = FALSE)))
  
  output$Sum <- renderPlot({
    ## 주별 판매액 추이
    if (input$type1 == "합계"){
      ggplot(week_sale, aes(x = week, y = sum)) + geom_line() + geom_point()}
    
    else if (input$type1 == "판매량"){
      ggplot(week_sale, aes(x = week, y = n)) + geom_line() + geom_point()
    }
    
    else if (input$type1 == "평균"){
      ggplot(week_sale, aes(x = week, y = mean)) + geom_line() + geom_point()
    }
  })
  
  output$by_month <- renderPlot({
    ### 월별 판매액 추이
    month_sale = data %>% group_by(month) %>%
      summarise(sum = sum(Sales), mean = mean(Sales), n=n())
    month_sale
    
    if (input$type2 == "합계"){
      ggplot(month_sale, aes(x = month, y = sum)) + geom_line() + geom_point()
    }
    else if (input$type2 == "판매량"){
      ggplot(month_sale, aes(x = month, y = n)) + geom_line() + geom_point() +
        ylab("count")
    }
    else if (input$type2 == "평균"){
      ggplot(month_sale, aes(x = month, y = mean)) + geom_line() + geom_point()
    }
  })
  
  output$by_year <- renderPlot({
    ### 연도별 판매액 추이
    year_sale = data %>% group_by(year) %>%
      summarise(sum = sum(Sales), mean = mean(Sales), n=n())
    year_sale
    
    if (input$type3 == "합계"){
      ggplot(year_sale, aes(x = year, y = sum)) + geom_line() + geom_point()
    }
    else if (input$type3 == "판매량"){
      ggplot(year_sale, aes(x = year, y = n)) + geom_line() + geom_point() +
        ylab("count")
    }
    else if (input$type3 == "평균"){
      ggplot(year_sale, aes(x = year, y = mean)) + geom_line() + geom_point()
    }
  })
  
  output$quater <- renderPlot({
    tt_data = data
    tt_data[,"new_od"] = as.Date(tt_data$order_date)
    tt_data[, "X"] = c(1:length(tt_data$new_od))
    tt_data = as_tsibble(tt_data, index = new_od, key = X)
    tt_yearquarter = tt_data %>% index_by(year_quarter = ~yearquarter(.)) %>%
      summarise(sum_of_sales = sum(Sales), mean_of_sales = mean(Sales), n = n())
    ggplot(tt_yearquarter, aes(x = year_quarter,y = sum_of_sales)) +
      geom_line() + xlab("Quater") + ylab("Sum of sales")
  })
  
  output$quater_by_cat <- renderPlot({
    tt_data = data
    tt_data[,"new_od"] = as.Date(tt_data$order_date)
    tt_data[, "X"] = c(1:length(tt_data$new_od))
    tt_data = as_tsibble(tt_data, index = new_od, key = X)
    tt_yearquarter_group = tt_data %>% group_by(Category) %>%
      index_by(year_quarter = ~yearquarter(.)) %>%
      summarise(sum_of_sales = sum(Sales), mean_of_sales = mean(Sales), n = n())
    ggplot(tt_yearquarter_group, aes(x = year_quarter, y = sum_of_sales,
                                     color = Category)) + geom_line() +
      facet_grid(~Category) +
      scale_colour_manual(values = c("#87AEDF", "#A4D390", "#E494AB")) +
      xlab("Quater") + ylab("Sum of sales")
  })
  
  output$customer_num <- renderPlot({
    #state 별 segment 수/비율 비교
    state_segment = data %>% group_by(State, Segment) %>% summarise(n = n())
    if (input$cust == "수") {
      ggplot(state_segment, aes(x = State, y = n, fill = Segment)) +
        geom_bar(stat = 'identity', position = "dodge") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_manual(values = c("#87AEDF", "#A4D390", "#E494AB")) +
        ylab("Count")
    } else if (input$cust == "비율"){
      ggplot(state_segment, aes(x = State, y = n, fill = Segment)) +
        geom_bar(stat = 'identity', position = "fill") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        scale_fill_manual(values = c("#87AEDF", "#A4D390", "#E494AB")) +
        ylab("Rate")
    }
    
  })
  
  sub_col <- paletteer_c("grDevices::Blue-Red 2", 17)
  
  output$product <- renderPlot({
    # 품목별 판매액 합계
    test_df = data %>% group_by(Category,Sub_Category) %>%
      summarise(sum_of_sales = sum(Sales, na.rm = T), n = n())
    if (input$prd_type == "판매액") {
      ggplot(test_df,aes(x = Category, y = sum_of_sales, fill = Sub_Category)) +
        geom_bar(stat='identity',position='dodge') +
        scale_fill_manual(values = c(sub_col)) + ylab("Sum of sales")
    } else if (input$prd_type == "판매량") {
      ggplot(test_df,aes(x = Category, y = n, fill = Sub_Category)) +
        geom_bar(stat='identity',position='dodge') +
        scale_fill_manual(values = c(sub_col)) + ylab("Count")
    }
  })
  
  sub_col2 <- paletteer_c("grDevices::Blue-Red 2", 4)
  
  output$cat_sales_by_year <- renderPlot({
    ## 연도별 category sales 추이
    year_category = data %>% group_by(year,Category) %>%
      summarise(sum = sum(Sales), n = n(), Mean = mean(Sales))
    
    if (input$type4 == "합계"){
      ggplot(year_category, aes(x = year, y = sum, color =Category)) +
        geom_line() + geom_point() +
        scale_colour_manual(values = c(sub_col2)) +
        ylab("Sum of sales")
    }
    else if (input$type4 == "판매량"){
      ggplot(year_category, aes(x = year, y = n, color = Category)) +
        geom_line() + geom_point() +
        scale_colour_manual(values = c(sub_col2)) +
        ylab("Count")
    }
    else if (input$type4 == "평균"){
      ggplot(year_category, aes(x = year, y = Mean, color = Category)) +
        geom_line() + geom_point() +
        scale_colour_manual(values = c(sub_col2)) +
        ylab("Mean of sales")
    }
  })
  
  
  ranges <-  reactiveValues(x = NULL, y = NULL)
  
  output$prod_box <- renderPlot({
    test_df = data %>% group_by(Category, Sub_Category) %>%
      summarise(sum_of_sales = sum(Sales, na.rm = T))
    
    ### boxplot 영역 잡아서(드래그) 이상치를 테이블로 확인할 수 있도록 구현하기
    ggplot(data, aes(x = Sub_Category, y = Sales, fill=Category)) + geom_boxplot() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = c("#87AEDF", "#A4D390", "#E494AB")) +
      xlab("Products")
  })
  
  observeEvent(input$box_dbclick, {
    brush <- input$outlier_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$outlier <- function(){
    brushedPoints(data, input$outlier_brush) %>%
      kbl() %>%
      kable_styling(bootstrap_options = c("striped", "hover","condensed"), full_width = F)
  }
  
  output$by_cust <- renderPlot({
    ##세그먼트별 판매액 합과 평균
    seg_sale = data %>% group_by(Segment) %>%
      summarise(sum_of_sales = sum(Sales), mean_of_sales = mean(Sales))
    seg_sale
    if (input$type5 == "합계"){
      ggplot(seg_sale, aes(x = Segment, y = sum_of_sales, fill=Segment)) +
        geom_bar(stat='identity', position='dodge') +
        scale_fill_manual(values = c("#87AEDF", "#A4D390", "#E494AB")) +
        ylab("Sum of sales")
    }
    else if (input$type5 == "평균"){
      ggplot(seg_sale, aes(x = Segment, y = mean_of_sales, fill=Segment)) +
        geom_bar(stat='identity', position='dodge') +
        scale_fill_manual(values = c("#87AEDF", "#A4D390", "#E494AB")) +
        ylab("Mean of sales")
    }
  })
  
  
  output$cust_by_delivery <- renderPlot({
    delay_df3 = data %>% group_by(Segment, Category, Sub_Category, Ship_Mode) %>%
      summarise(number_of_mode = n())
    delay_df3
    ggplot(delay_df3, aes(x = Sub_Category, y = number_of_mode, fill = Ship_Mode)) +
      geom_bar(stat = 'identity', position = 'fill') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(~Segment) +
      scale_fill_manual(values = c(sub_col2)) + xlab("Products") +
      ylab("Rate") + guides(fill=guide_legend(title = "Ship mode"))
    
  })
  
  df = data %>% group_by(Customer_ID) %>%
    reframe(recency = min(current_date - order_date),
            frequency = n(), monetary = sum(Sales))
  df = data %>% group_by(Customer_ID,Segment) %>%
    reframe(recency = min(current_date - order_date),
            frequency=n(), monetary = sum(Sales))
  
  
  output$hist_for_RFM1 <- renderPlot({hist(as.integer(df$recency),
                                           xlab = 'Recency', ylab = 'Count',
                                           main = 'Histogram of recency')})
  output$hist_for_RFM2 <- renderPlot({hist(df$frequency, xlab = 'Frequency',
                                           ylab = 'Count',
                                           main = 'Histogram of frequency')})
  output$hist_for_RFM3 <- renderPlot({hist(df$monetary, xlab = 'Monetary',
                                           ylab = 'Count',
                                           main = 'Histogram of monetary')})
  
  observeEvent(input$before, {
    df = data %>% group_by(Customer_ID) %>%
      reframe(recency = min(current_date - order_date),
              frequency = n(), monetary = sum(Sales))
    
    output$hist_for_RFM1 <- renderPlot({hist(as.integer(df$recency), xlab = 'Recency', ylab = 'Count', main = 'Histogram of recency')})
    output$hist_for_RFM2 <- renderPlot({hist(df$frequency, xlab = 'Frequency', ylab = 'Count', main = 'Histogram of frequency')})
    output$hist_for_RFM3 <- renderPlot({hist(df$monetary, xlab = 'Monetary', ylab = 'Count', main = 'Histogram of monetary')})
    
  })
  
  observeEvent(input$after, {
    df_trans = data %>% group_by(Customer_ID,Segment) %>%
      reframe(recency = min(current_date - order_date), frequency=n(),
              monetary = sum(Sales))
    df_trans$recency = log(as.integer(df_trans$recency))
    df_trans$frequency = (df_trans$frequency)^0.5
    df_trans$monetary = log(df_trans$monetary)^2
    #   #데이터 변환
    output$hist_for_RFM1 <- renderPlot({hist(df_trans$recency,
                                             xlab = 'Recency', ylab = 'Count',
                                             main = 'Histogram of recency')})
    output$hist_for_RFM2 <- renderPlot({hist(df_trans$frequency,
                                             xlab = 'Frequency',
                                             ylab = 'Count',
                                             main = 'Histogram of frequency')})
    output$hist_for_RFM3 <- renderPlot({hist(df_trans$monetary,
                                             xlab = 'Monetary', ylab = 'Count',
                                             main = 'Histogram of monetary')})
    
  })
  
  
  
  cluster_col <- paletteer_d("vapoRwave::vapoRwave")
  # K-means clustering
  output$silhouette <- renderPlot({
    current_date = as.Date("2019-01-01")
    # RFM 구하기 위해 데이터 전처리(변환)
    # 치우친 히스토그램을 보이므로 적절한 변수변환 통해 치우침 완화
    
    #RFM 분석
    df = data %>% group_by(Customer_ID,Segment) %>%
      reframe(recency = min(current_date - order_date), frequency=n(), monetary = sum(Sales))
    df_trans = df
    df_trans$recency = log(as.integer(df_trans$recency))
    df_trans$frequency = (df_trans$frequency)^0.5
    df_trans$monetary = log(df_trans$monetary)^2
    # K-means clustering
    set.seed(1004)
    
    km.res <- kmeans(df_trans[, c("recency", "frequency", "monetary")],
                     centers = input$centers)
    cluster_result = km.res$cluster
    df_trans = cbind(df_trans, cluster_result)
    length(df_trans$Customer_ID)
    # Visualize silhouhette information
    sil <- silhouette(km.res$cluster, dist(df_trans[, c("recency", "frequency",
                                                        "monetary")]))
    fviz_silhouette(sil) +
      scale_fill_manual(values = c(cluster_col)) +
      scale_colour_manual(values = c(cluster_col))
  })
  
  output$clustered_plot <- renderPlot({
    current_date = as.Date("2019-01-01")
    # RFM 구하기 위해 데이터 전처리(변환)
    # 치우친 히스토그램을 보이므로 적절한 변수변환 통해 치우침 완화
    
    #RFM 분석
    df = data %>% group_by(Customer_ID,Segment) %>%
      reframe(recency = min(current_date - order_date), frequency=n(),
              monetary = sum(Sales))
    df_trans = df
    df_trans$recency = log(as.integer(df_trans$recency))
    df_trans$frequency = (df_trans$frequency)^0.5
    df_trans$monetary = log(df_trans$monetary)^2
    # K-means clustering
    set.seed(1004)
    
    km.res <- kmeans(df_trans[, c("recency", "frequency", "monetary")],
                     centers = input$centers)
    cluster_result = km.res$cluster
    df_trans = cbind(df_trans, cluster_result)
    length(df_trans$Customer_ID)
    # Visualize silhouhette information
    sil <- silhouette(km.res$cluster, dist(df_trans[, c("recency", "frequency",
                                                        "monetary")]))
    fviz_cluster(km.res, df_trans[, c("recency", "frequency", "monetary")],
                 ellipse.type = "norm") +
      theme_minimal(base_family ="Bree Serif") +
      scale_fill_manual(values = c(cluster_col)) +
      scale_colour_manual(values = c(cluster_col))
  })
  
  
  output$feature_plot <- renderPlot({
    df = data %>% group_by(Customer_ID,Segment) %>%
      reframe(recency = min(current_date - order_date), frequency=n(),
              monetary = sum(Sales))
    df_trans = df
    df_trans$recency = log(as.integer(df_trans$recency))
    df_trans$frequency = (df_trans$frequency)^0.5
    df_trans$monetary = log(df_trans$monetary)^2
    set.seed(1004)
    
    km.res <- kmeans(df_trans[, c("recency", "frequency", "monetary")],
                     centers = 4)
    cluster_result = km.res$cluster
    df_trans = cbind(df_trans, cluster_result)
    mean_cluster_data = df_trans %>% group_by(cluster_result) %>%
      summarise(mean_re = mean(recency), mean_fre = mean(frequency),
                mean_mone = mean(monetary))
    
    if (input$type6 == "Recency"){
      ggplot(mean_cluster_data, aes(x = reorder(cluster_result, mean_re),
                                    y=mean_re,
                                    fill = factor(cluster_result))) +
        xlab('Recency') + ylab('Mean') +
        geom_bar(stat = 'identity') + labs(fill='Cluster') +
        scale_fill_manual(values = sub_col2)
    } else if (input$type6 == "Frequency") {
      ggplot(mean_cluster_data, aes(x = reorder(cluster_result, mean_fre),
                                    y=mean_fre,
                                    fill = factor(cluster_result))) +
        xlab('Frequency') + ylab('Mean') +
        geom_bar(stat = 'identity') + labs(fill='Cluster') +
        scale_fill_manual(values = sub_col2)
    } else if (input$type6 == "Monetary") {
      ggplot(mean_cluster_data, aes(x = reorder(cluster_result, mean_mone),
                                    y = mean_mone,
                                    fill = factor(cluster_result))) +
        xlab('Monetary') + ylab('Mean') +
        geom_bar(stat = 'identity') + labs(fill='Cluster') +
        scale_fill_manual(values = sub_col2)
    }
    
  })
  
  output$delivery_type_by_sub_cat <- renderPlot({
    # 배송 유형별 지연 날짜
    delay_df2 = data %>% group_by(Ship_Mode, Category, Sub_Category) %>%
      summarise(mean_of_delay_time = mean(ship_date - order_date))
    delay_df2
    ggplot(delay_df2,aes(x = Ship_Mode , y = mean_of_delay_time,fill = Sub_Category)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      scale_fill_manual(values = c(sub_col)) +
      xlab("Ship mode") + ylab("Mean of delay time") +
      labs(fill = "Products")
  })
  
  output$delay_by_cat <- renderPlot({
    #Category별 평균 지연날짜
    delay_df = data %>% group_by(Category, Sub_Category) %>%
      summarise(mean_of_delay_time = mean(ship_date - order_date))
    delay_df
    ggplot(delay_df,aes(x = Category,
                        y = mean_of_delay_time,
                        fill = Sub_Category)) +
      geom_bar(stat='identity', position='dodge') +
      scale_fill_manual(values = c(sub_col)) +
      ylab("Mean of delay time") + labs(fill = "Products")
  })
  
  output$kruskal <- renderPlot({
    data %>% ggplot(aes(sample = Sales, color = Sub_Category)) +
      geom_qq() + geom_qq_line() + facet_grid(.~Sub_Category) +
      xlab("") + ylab("Sales") +
      guides(fill=guide_legend(title = "Products"))
  })
  
  output$conf_lev <- renderTable({
    htest <- readLines("./ktest.txt")
    h <- data.frame(htest[5:6])
    h <- rename(h, 'Kruskal result'='htest.5.6.')
    h
  })
  
  
  heat_col <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  output$wilcoxon <- renderPlot({
    group_data <- data[, c("Sub_Category", "Sales")]
    re = pairwise.wilcox.test(group_data$Sales, g = group_data$Sub_Category)
    levelplot(re$p.value, scales = list(x = list(rot = 90)),
              col.regions=heat_col)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
