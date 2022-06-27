library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)
library(shiny)
library(stats)

# import data path
setwd("D:\\Github_version_file_R\\data_set\\marchine_learning_data\\Nashik_apartment_price_prediction")

# =========================================================================
# ui setting
ui <- navbarPage(
    # top title
    title = "Nashik Apartment Price Analyze", collapsible = TRUE,

    # -------------------------------------------
    # page 1 setting - sidebar layout
    tabPanel(
        title = "房價數據基本分析 | Analyze Chart",
        fluidPage(
            # theme style
            theme = bslib::bs_theme(bootswatch = "darkly"),
            sidebarLayout(
                # left setting
                sidebarPanel = sidebarPanel(
                    width = 2,
                    radioButtons(
                        inputId = "data_col_choices",
                        label = h4("Choice Y axis | Y軸單選"),
                        choices = c("房型總數 | Total number of the rooms type", "平均月支付額EMI | Avg EMI per Month", "平均取得面積SQFT | Avg SQFT", "公寓、獨棟型態比例 | Apartment and Independent type ratio", "新、舊狀態比例 | New and old state ratio", "EMI與面積 | EMI per month and SQFT", "等級劃分的分類情形 | Classification between the obtaining area and EMI payment", "房數對應面積分布 | The number of rooms corresponds to the regional distribution")
                    )
                ),
                # right setting
                mainPanel = mainPanel(
                    width = 10,
                    tabsetPanel(
                        id = "all_data_plot",
                        tabPanel(title = h4("Plot Chart | 數據繪圖"), plotOutput(outputId = "house_plot", width = "100%", height = "600px"))
                    )
                )
            )
        )
    ),

    # -------------------------------------------
    # page 2 setting
    tabPanel(
        title = "各房型價格範圍情形 | Selling price for room type",
        fluidRow(
            column(width = 6, h3("價格與房屋型態數據 | Data of the selling price and type")),
            column(width = 6, h3("圖表輸出 | Chart"))
        ),
        br(),
        fluidRow(
            column(
                width = 6,
                dataTableOutput(outputId = "housedf")
            ),
            column(
                width = 6,
                plotOutput(outputId = "price_range_plot", height = "600px")
            )
        )
    ),

    # page 3 setting
    tabPanel(
        title = "條件搜尋物件 | Conditional search object",
        fluidPage(
            sidebarLayout(
                sidebarPanel = sidebarPanel(
                    width = 3,
                    # choice setting
                    radioButtons(
                        inputId = "emi_level_choice",
                        label = h4("請選擇EMI支付分級，月均14044.83INR為基礎分級 | Choices EMI and House Area Level, The condition base uses the 14044.83INR per month"),
                        choices = c("低於水平0% ~ 10% | expected 0% ~ 10%", "負擔尚可10% ~ 40% | affordable 10% ~ 40%", "投資等級40% ~ 80% | Investable 40% ~ 80%", "趨於昂貴(遠超出人均範圍) | costly (It is far away from the capita range)", "過度高價(遠超出人均範圍) | inflated(It is far away from the capita range)"),
                        selected = "負擔尚可10% ~ 40% | affordable 10% ~ 40%"
                    ),
                    radioButtons(
                        inputId = "sqft_level_choice",
                        label = h4("請選擇面積分級 | Choices area level"),
                        choices = c("小坪數(低於20台坪) | small(1 ~ 710sqft)", "中等坪數(20 ~ 45台坪) | median(711 ~ 1600sqft)", "大坪數(46 ~ 65台坪) | large(1601 ~ 2300sqft)", "巨大坪數(65台坪以上) | huge(2301 ~ 40000sqft)"),
                        selected = "中等坪數(20 ~ 45台坪) | median(711 ~ 1600sqft)"
                    ),
                    # input numeric
                    numericInput(
                        inputId = "price_level",
                        label = h4("請輸入房價搜尋上限(單位 : INR, 最高7,000萬INR) | Enter a price search limit(Unit : INR, Max limit : 70 million INR)"),
                        value = 5000000,
                        min = 100000,
                        max = 70000000
                    )
                ),
                mainPanel = mainPanel(
                    width = 9,
                    tabsetPanel(
                        tabPanel(
                            title = "搜尋結果 | Search result",
                            tableOutput(outputId = "search_result")
                        )
                    )
                )
            )
        )
    ),

    # -------------------------------------------
    # page 4 setting
    tabPanel(
        title = "所有數據資料 | All data set",
        fluidRow(
            column(
                width = 12,
                selectInput(
                    inputId = "select_data_set",
                    label = h4("請選擇數據集 | Choices data set"),
                    choices = c("清洗完成原始數據集 | Cleaned data set", "整併完成數據集 | Integrate the complete data set", "等級分類數據 | Classified data set", "新、舊比例數據 | New、old ratio data set", "型態比例數據 | Type ratio data set")
                )
            ),
            column(
                width = 12,
                h4("數據集資料顯示處 | Data set data display"),
                tableOutput(
                    outputId = "data_set_output"
                )
            )
        )
    ),
    # footer
    footer = "By Rex Li 2022"
)

# =========================================================================
# server setting
server <- function(input, output, session) {
    # load data set
    house_df_v2 <- read.csv("house_df_v2.csv")
    level_df <- read.csv("level_df.csv")
    type_price <- read.csv("type_price.csv")
    condition_df <- read.csv("condition_df.csv")
    house_df <- read.csv("house_df.csv")

    # -------------------------------------------
    # change cols type
    level_df$emi_level <- factor(level_df$emi_level, levels = c("expected", "affordable", "Investable", "costly", "inflated"))
    level_df$sqft_level <- factor(level_df$sqft_level, levels = c("small", "median", "large", "huge"))
    level_df$BHK <- factor(level_df$BHK, level = c("1", "2", "3", "4", "5", "6", "7", "8", "10"))
    house_df_v2$BHK <- factor(house_df_v2$BHK, level = c("1", "2", "3", "4", "5", "6", "7", "8", "10"))
    house_df$BHK <- factor(house_df$BHK, level = c("1", "2", "3", "4", "5", "6", "7", "8", "10"))

    # -------------------------------------------
    # output chart page 1
    output$house_plot <- renderPlot({
        if (input$data_col_choices == "平均月支付額EMI | Avg EMI per Month") {
            ggplot(
                data = house_df_v2,
                mapping = aes(x = reorder(BHK, -avg_emi_INR), y = avg_emi_INR, fill = BHK)
            ) +
                geom_col() +
                scale_fill_brewer(palette = "Set3") +
                geom_text(aes(label = emi_percentage), vjust = -0.3) +
                labs(title = "支付越多是否與房間數成比例? | Is paying more proportional to the number of rooms?", subtitle = "房間數與每月支付EMI比較 | The EMI payment between the room type", caption = "Price Unit : INR") +
                xlab("Rooms type") +
                ylab("AVG EMI") +
                guides(fill = guide_legend(titile = "Rooms Type"))
        } else if (input$data_col_choices == "平均取得面積SQFT | Avg SQFT") {
            ggplot(
                data = house_df_v2,
                mapping = aes(x = reorder(BHK, -avg_sqft), y = avg_sqft, fill = BHK)
            ) +
                geom_col() +
                scale_fill_brewer(palette = "Paired") +
                geom_text(aes(label = sqft_percentage), vjust = -0.3) +
                labs(title = "房間數越多是否取得面積越大 | Whether the larger the number of rooms, the larger the area obtained", subtitle = "房間數與面積比較 | All objects obtaining area and rooms type", caption = "Area Unit : Square Feet") +
                xlab("Rooms Type") +
                ylab("AVG Sqft") +
                guides(fill = guide_legend(title = "Rooms Type"))
        } else if (input$data_col_choices == "房型總數 | Total number of the rooms type") {
            ggplot(
                data = house_df_v2,
                mapping = aes(x = reorder(BHK, total), y = total, fill = BHK)
            ) +
                geom_col() +
                scale_fill_brewer(palette = "Set1") +
                geom_text(
                    aes(label = total_percentage),
                    vjust = 0.5, # adjust position
                    hjust = 0.5
                ) +
                labs(
                    title = "市場上流通的房型數比較 | The proportion of the ratio of houses circulating in the market",
                    subtitle = "不同房數總數與所占比例 | The total number and proportion of different rooms"
                ) +
                xlab("Rooms Type") +
                scale_y_continuous(
                    name = "Total Number",
                    breaks = c(0, max(house_df_v2$total), 150)
                ) +
                coord_flip() + # graph flip
                guides(fill = guide_legend(title = "Rooms Type"))
        } else if (input$data_col_choices == "公寓、獨棟型態比例 | Apartment and Independent type ratio") {
            type_pie <- pie(
                type_price$price_INR_sum,
                labels = type_price$total_percentage,
                col = c("slateblue3", "tan2"),
                main = "不同房屋型態所占比例 | Different House Type Percentage"
            )
            # add notes
            legend(
                "topright",
                legend = c("Apartment", "Independent"),
                title = "House Type",
                fill = c("slateblue3", "tan2"),
                cex = 1.2
            )
        } else if (input$data_col_choices == "新、舊狀態比例 | New and old state ratio") {
            # New & Old house percentage
            pie(
                condition_df$price_INR_sum,
                labels = condition_df$price_percentage,
                col = c("#d425ae", "#5bb91d"),
                main = "新、舊房屋所佔比例 | New、Old house percentage"
            )
            legend(
                "topright",
                legend = c("New House", "Old House"),
                title = "House Condition",
                fill = c("#d425ae", "#5bb91d"),
                cex = 1.2
            ) # 字符大小
        } else if (input$data_col_choices == "EMI與面積 | EMI per month and SQFT") {
            ggplot(
                data = house_df_v2,
                mapping = aes(x = avg_emi_INR, y = avg_sqft)
            ) +
                geom_point(
                    alpha = 0.6, # transparency透明度
                    size = 2.0
                ) +
                geom_smooth(
                    method = "loess",
                    aes(x = avg_emi_INR, y = avg_sqft)
                ) +
                labs(
                    title = "每月EMI越高，所得的面積是否越大? | The higher the monthly EMI, the larger the resulting area?",
                    subtitle = "平均EMI vs 平均面積 | Avg EMI vs Avg SQFT",
                    caption = "EMI Unit = INR"
                ) +
                xlab("AVG EMI") +
                scale_y_continuous(
                    name = "AVG Square Feet",
                    breaks = c(0, max(house_df_v2$avg_sqft))
                )
        } else if (input$data_col_choices == "等級劃分的分類情形 | Classification between the obtaining area and EMI payment") {
            ggplot(
                data = level_df,
                mapping = aes(
                    x = emi_level,
                    fill = sqft_level
                )
            ) +
                geom_bar() +
                guides(fill = guide_legend(title = "Sqft Level")) +
                xlab("EMI Level") +
                ylab("Total") +
                labs(title = "經分類後的分布情形 | Distribution after classification", subtitle = "EMI & Sqft")
        } else {
            ggplot(
                data = level_df,
                mapping = aes(
                    x = sqft_level,
                    fill = BHK
                )
            ) +
                geom_bar() +
                guides(fill = guide_legend(title = "Rooms Type")) +
                xlab("Sqft Level") +
                ylab("Total") +
                labs(title = "各房數對應的面積分布 | Area distribution corresponding to the number of rooms", subtitle = "BHK & Sqft Level")
        }
    })

    # -------------------------------------------
    # output page 2
    output$housedf <- renderDataTable(
        select(house_df, c("BHK", "price", "total_sqft", "month_emi", "price_USD", "price_INR_sqft")),
        options = list(pageLength = 10)
    )
    output$price_range_plot <- renderPlot({
        ggplot(
            data = house_df,
            mapping = aes(x = reorder(BHK, price), y = price, fill = BHK)
        ) +
            geom_boxplot() +
            scale_y_continuous(
                name = "Price Unit : Lakh",
                breaks = c(0, max(house_df$price), 100)
            ) +
            scale_x_discrete(name = "Rooms Type") +
            labs(
                title = "各房數的價格範圍比較 | Comparison of price ranges by number of rooms",
                caption = "1 Lakh = 100,000 INR"
            ) + # add note
            guides(fill = guide_legend(title = "Rooms Type"))
    })

    # -------------------------------------------
    # output page 3
    output$search_result <- renderTable(
        level_df %>%
            select(address, owners, housetype, house_condition, BHK, month_emi, price_INR, emi_level, sqft_level) %>%
            filter(
                if (input$emi_level_choice == "低於水平0% ~ 10% | expected 0% ~ 10%") {
                    emi_level == "expected"
                } else if (input$emi_level_choice == "負擔尚可10% ~ 40% | affordable 10% ~ 40%") {
                    emi_level == "affordable"
                } else if (input$emi_level_choice == "投資等級40% ~ 80% | Investable 40% ~ 80%") {
                    emi_level == "Investable"
                } else if (input$emi_level_choice == "趨於昂貴(遠超出人均範圍) | costly (It is far away from the capita range)") {
                    emi_level == "costly"
                } else {
                    emi_level == "inflated"
                },
                if (input$sqft_level_choice == "小坪數(低於20台坪) | small(1 ~ 710sqft)") {
                    sqft_level == "small"
                } else if (input$sqft_level_choice == "中等坪數(20 ~ 45台坪) | median(711 ~ 1600sqft)") {
                    sqft_level == "median"
                } else if (input$sqft_level_choice == "大坪數(46 ~ 65台坪) | large(1601 ~ 2300sqft)") {
                    sqft_level == "large"
                } else {
                    sqft_level == "huge"
                },
                price_INR < input$price_level
            ),
        bordered = TRUE,
        width = "100%"
    )

    # -------------------------------------------
    # output page 4
    output$data_set_output <- renderTable(
        if (input$select_data_set == "清洗完成原始數據集 | Cleaned data set") {
            head(house_df)
        } else if (input$select_data_set == "整併完成數據集 | Integrate the complete data set") {
            head(house_df_v2)
        } else if (input$select_data_set == "等級分類數據 | Classified data set") {
            head(level_df)
        } else if (input$select_data_set == "新、舊比例數據 | New、old ratio data set") {
            condition_df
        } else {
            type_price
        },
        bordered = TRUE,
        width = "100%"
    )
}

# =========================================================================
# run app
shinyApp(ui = ui, server = server)
