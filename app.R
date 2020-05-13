
library(dplyr)
library(tidyr)
library(forcats)
library(readr)
library(stringr)
library(ggplot2)
library(gghighlight)
library(hrbrthemes)
library(rvest)
library(anytime)
library(shiny)
library(shinydashboard)
library(effects)
library(corrplot)
library(choroplethr)
library(choroplethrMaps)


theme_set(theme_ipsum())


data("df_pop_state")
data("df_county_demographics")

us_current <- read_csv("https://covidtracking.com/api/v1/us/current.csv")

us_daily <- read_csv("https://covidtracking.com/api/v1/us/daily.csv") %>%
    mutate(date = anydate(date))

states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv")

df_pop_state2 <- states_info %>%
    inner_join(
        df_pop_state %>%
            mutate(name = str_to_title(region))
    ) %>%
    transmute(
        state,
        name,
        pop = value
    )

states_current <- read_csv("https://covidtracking.com/api/v1/states/current.csv")

states_daily <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
    mutate(date = anydate(date))

stayhomepage <- read_html("https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide")
stayhometable <- html_table(stayhomepage)[[1]]
colnames(stayhometable) <- as.character(stayhometable[1,])
stayhometable <- tail(stayhometable, -1)
stayhometable$`Effective Date` <- anydate(stayhometable$`Effective Date`)
stayhometable$`Duration or End Date` <- anydate(stayhometable$`Duration or End Date`)

stayhometable <- stayhometable %>%
    mutate(
        State = case_when(
            State == "District of Columbia" ~ "District Of Columbia",
            State == "Massachusetts*" ~ "Massachusetts",
            State == "Missouri1" ~ "Missouri",
            State == "Oklahoma – for vulnerable individuals only" ~ "Oklahoma",
            State == "Washington State" ~ "Washington",
            TRUE ~ State
        )
    )

stayhometable <- stayhometable %>%
    inner_join(
        states_info %>%
            transmute(StateCode = state, State = name)
    )

rt_data <- read_csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv")

raw_county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

all_county_data <- raw_county_data %>%
    filter(!is.na(fips)) %>%
    mutate(region = as.numeric(fips)) %>%
    inner_join(df_county_demographics, by = "region") %>%
    group_by(region) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    ungroup()

county_data_cases <- all_county_data %>%
    transmute(
        region = region,
        value = 100000 * cases / total_population,
        state = state,
        county = county,
        population = total_population,
        cases = cases
    )

county_data_deaths <- all_county_data %>%
    transmute(
        region = region,
        value = 100000 * deaths / total_population,
        state = state,
        county = county,
        population = total_population,
        deaths = deaths
    )

model_deaths_input <- all_county_data %>%
    transmute(
        `Dead/100k` = 100000 * deaths / total_population,
        `Age` = median_age,
        `Income` = per_capita_income,
        `Rent` = median_rent,
        `White` = percent_white,
        `Black` = percent_black,
        `Hispanic` = percent_hispanic,
        `Asian` = percent_asian
    )

model_deaths_cor <- cor(model_deaths_input)
model_deaths_cor_mtest <- cor.mtest(model_deaths_input)

model_deaths_aov <- aov(
    `Dead/100k` ~ .,
    data = model_deaths_input
)

model_deaths_effects <- allEffects(model_deaths_aov)


ui <- dashboardPage(
    header = dashboardHeader(title = "COVID-19"),
    title = "COVID-19",
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem(text = "About", tabName = "about_tab"),
            menuItem(text = "Cumulative Counts", tabName = "total_tab"),
            menuItem(
                text = "Daily Counts",
                menuSubItem(text = "National", tabName = "us_charts_tab"),
                menuSubItem(text = "State", tabName = "state_charts_tab"),
                menuSubItem(text = "State / 100k", tabName = "state_capcharts_tab"),
                menuSubItem(text = "National Hospital", tabName = "us_hosp_tab"),
                menuSubItem(text = "State Hospital", tabName = "state_hosp_tab"),
                menuSubItem(text = "State Hospital / 100k", tabName = "state_caphosp_tab")
            ),
            menuItem(text = "Daily State Rt", tabName = "state_rt_tab"),
            menuItem(text = "Positive Tests Heatmap", tabName = "heatmaps_positive_tab"),
            menuItem(text = "Deaths Heatmap", tabName = "heatmaps_death_tab"),
            menuItem(text = "Stay At Home Orders", tabName = "stayhome_tab"),
            menuItem(
                text = "Maps",
                menuSubItem(text = "State Percentages", tabName = "state_percent_tab"),
                menuSubItem(text = "State Counts / 100k", tabName = "state_capita_tab"),
                menuSubItem(text = "All Counties", tabName = "county_natcapita_tab"),
                menuSubItem(text = "Selected Counties", tabName = "county_capita_tab")
            ),
            menuItem(text = "Demographics", tabName = "county_aov_tab"),
            menuItem(text = "Data Tables", tabName = "data_tables_tab")
        ),
        selectInput(
            inputId = "statepicker",
            label = "Select State(s):",
            choices = states_info$state,
            selected = "MI",
            multiple = TRUE
        ),
        checkboxInput(
            inputId = "inc_se",
            label = "Show Confidence Intervals",
            value = FALSE
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "about_tab",
                box(
                    "This is still a test dashboard and may contain errors.",
                    title = "About",
                    status = "danger"
                ),
                box(
                    "National and State data is downloaded from https://covidtracking.com/api",
                    title = "National and State Data Source"
                ),
                box(
                    "County level data is downloaded from https://github.com/nytimes/covid-19-data",
                    title = "County Data Source"
                ),
                box(
                    "Rt data is downloaded from https://rt.live",
                    title = "Rt Data Source"
                ),
                box(
                    "State level data about governors' Stay Home orders is downloaded from https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide",
                    title = "Stay Home Orders Data Source"
                ),
                box(
                    "Demographics data is pulled from the 2012 US American Community Survey (ACS) 5 year estimates included in the 'choroplethr' R package.",
                    title = "Demographics Data Source"
                ),
                box(
                    "Please consider helping the Folding@home project by installing the software from https://foldingathome.org which lets you share unused computer time with COVID-19 (and other) researchers around the world.",
                    title = "Folding@home",
                    status = "success"
                ),
                box(
                    "The source code is available at https://github.com/lab1702/coviddash",
                    title = "Source Code"
                )
            ),
            tabItem(
                tabName = "total_tab",
                box(plotOutput("us_total_chart", height = 768)),
                box(plotOutput("state_total_chart", height = 768))
            ),
            tabItem(
                tabName = "us_charts_tab",
                box(plotOutput("us_tests_chart")),
                box(plotOutput("us_cases_chart"), status = "warning"),
                box(plotOutput("us_deaths_chart"), status = "danger")
            ),
            tabItem(
                tabName = "state_charts_tab",
                box(plotOutput("state_tests_chart")),
                box(plotOutput("state_cases_chart"), status = "warning"),
                box(plotOutput("state_deaths_chart"), status = "danger")
            ),
            tabItem(
                tabName = "state_capcharts_tab",
                box(plotOutput("state_tests_capchart")),
                box(plotOutput("state_cases_capchart"), status = "warning"),
                box(plotOutput("state_deaths_capchart"), status = "danger")
            ),
            tabItem(
                tabName = "us_hosp_tab",
                box(plotOutput("us_hosp_chart")),
                box(plotOutput("us_icu_chart"), status = "warning"),
                box(plotOutput("us_vent_chart"), status = "danger")
            ),
            tabItem(
                tabName = "state_hosp_tab",
                box(plotOutput("state_hosp_chart")),
                box(plotOutput("state_icu_chart"), status = "warning"),
                box(plotOutput("state_vent_chart"), status = "danger")
            ),
            tabItem(
                tabName = "state_caphosp_tab",
                box(plotOutput("state_caphosp_chart")),
                box(plotOutput("state_capicu_chart"), status = "warning"),
                box(plotOutput("state_capvent_chart"), status = "danger")
            ),
            tabItem(
                tabName = "state_rt_tab",
                box(plotOutput("state_rt_chart", height = 768)),
                box(plotOutput("state_rtcases_chart", height = 768), status = "warning"),
                box("Rt = Average number of people who become infected by an infectious person. Rt > 1 = the virus will spread quickly, Rt < 1 = the virus will stop spreading. Data on this page is sourced from https://rt.live", width = 12)
            ),
            tabItem(
                tabName = "heatmaps_positive_tab",
                box(plotOutput("state_positive_heatmap", height = 768), status = "warning", width = 12)
            ),
            tabItem(
                tabName = "heatmaps_death_tab",
                box(plotOutput("state_death_heatmap", height = 768), status = "danger", width = 12)
            ),
            tabItem(
                tabName = "stayhome_tab",
                box(plotOutput("stayhome_chart", height = 768), width = 12)
            ),
            tabItem(
                tabName = "state_percent_tab",
                box(plotOutput("percent_tests_map")),
                box(plotOutput("pos_tests_map"), status = "warning"),
                box(plotOutput("cap_deaths_cases_map"), status = "danger"),
                box(tableOutput("percent_states_top10_table"), status = "danger", title = "Top 10 States by Mortality Rate")
            ),
            tabItem(
                tabName = "state_capita_tab",
                box(plotOutput("cap_tests_map")),
                box(plotOutput("cap_cases_map"), status = "warning"),
                box(plotOutput("cap_deaths_map"), status = "danger"),
                box(tableOutput("cap_states_top10_table"), status = "danger", title = "Top 10 States by Deaths / 100k people")
            ),
            tabItem(
                tabName = "county_natcapita_tab",
                box(plotOutput("cty_natcases_map"), status = "warning"),
                box(plotOutput("cty_natdeaths_map"), status = "danger"),
                box(tableOutput("cty_natcases_table"), status = "warning", title = "Top 10 Counties by Cases / 100k"),
                box(tableOutput("cty_natdeaths_table"), status = "danger", title = "Top 10 Counties by Deaths / 100k")
            ),
            tabItem(
                tabName = "county_capita_tab",
                box(plotOutput("cty_cases_map"), status = "warning"),
                box(plotOutput("cty_deaths_map"), status = "danger"),
                box(tableOutput("cty_cases_table"), status = "warning", title = "Top 10 Counties by Cases / 100k"),
                box(tableOutput("cty_deaths_table"), status = "danger", title = "Top 10 Counties by Deaths / 100k")
            ),
            tabItem(
                tabName = "county_aov_tab",
                box(plotOutput("county_aov_effects", height = 768)),
                box(plotOutput("county_cor_plot", height = 768)),
                box("This page includes all counties, not just ones in states that are selected.", width = 12)
            ),
            tabItem(
                tabName = "data_tables_tab",
                box(tableOutput("data_us"), title = "National Data Table", width = 12),
                box(tableOutput("data_states"), title = "State Data Table", width = 12)
            )
        )
    )
)


server <- function(input, output, session) {

    output$us_total_chart <- renderPlot({
        us_daily %>%
            select(
                date,
                totalTestResults,
                positive,
                death,
                recovered
            ) %>%
            pivot_longer(
                cols = c(totalTestResults, positive, death, recovered)
            ) %>%
            mutate(
                name = case_when(
                    name == "totalTestResults" ~ "A. Tests",
                    name == "positive" ~ "B. Positive Tests",
                    name == "death" ~ "C. Deaths",
                    name == "recovered" ~ "D. Recovered",
                    TRUE ~ "ERROR"
                )
            ) %>%
            ggplot(aes(x = date, y = value)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Count") +
            facet_wrap(~ name, ncol = 1, scales = "free_y") +
            ggtitle("National Cumulative Totals")
    })

    output$state_total_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            select(
                date,
                state,
                totalTestResults,
                positive,
                death,
                recovered
            ) %>%
            pivot_longer(
                cols = c(totalTestResults, positive, death, recovered)
            ) %>%
            mutate(
                name = case_when(
                    name == "totalTestResults" ~ "A. Tests",
                    name == "positive" ~ "B. Positive Tests",
                    name == "death" ~ "C. Deaths",
                    name == "recovered" ~ "D. Recovered",
                    TRUE ~ "ERROR"
                )
            ) %>%
            ggplot(aes(x = date, y = value, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Count", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            facet_wrap(~ name, ncol = 1, scales = "free_y") +
            ggtitle("State Cumulative Totals")
    })
    
    output$us_tests_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = totalTestResultsIncrease)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Tests") +
            ggtitle("Daily National Tests")
    })
    
    output$us_cases_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = positiveIncrease)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Positive Tests") +
            ggtitle("Daily National Positive Tests")
    })

    output$us_deaths_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = deathIncrease)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Deaths") +
            ggtitle("Daily National Deaths")
    })

    output$state_tests_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date, y = totalTestResultsIncrease, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Tests", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Tests")
    })
    
    output$state_cases_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date, y = positiveIncrease, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Positive Tests", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Positive Tests")
    })
    
    output$state_deaths_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date, y = deathIncrease, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Deaths", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Deaths")
    })

    output$state_tests_capchart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            inner_join(df_pop_state2) %>%
            ggplot(aes(x = date, y = 100000 * totalTestResultsIncrease / pop, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Tests / 100k", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Tests / 100k people")
    })
    
    output$state_cases_capchart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            inner_join(df_pop_state2) %>%
            ggplot(aes(x = date, y = 100000 * positiveIncrease / pop, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Positive Tests / 100k", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Positive Tests / 100k people")
    })

    output$state_deaths_capchart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            inner_join(df_pop_state2) %>%
            ggplot(aes(x = date, y = 100000 * deathIncrease / pop, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Deaths / 100k", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Deaths / 100k people")
    })
    
    output$us_hosp_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = hospitalizedCurrently)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently Hospitalized") +
            ggtitle("Daily National Currently Hospitalized")
    })
    
    output$us_icu_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = inIcuCurrently)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently in ICU") +
            ggtitle("Daily National Currently in ICU")
    })
    
    output$us_vent_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = onVentilatorCurrently)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently on Ventilator") +
            ggtitle("Daily National Currently on Ventilator")
    })
    
    output$state_hosp_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date, y = hospitalizedCurrently, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently Hospitalized", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Currently Hospitalized")
    })

    output$state_icu_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date, y = inIcuCurrently, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently in ICU", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Currently in ICU")
    })

    output$state_vent_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date, y = onVentilatorCurrently, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently on Ventilator", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Currently on Ventilator")
    })
    
    output$state_caphosp_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            inner_join(df_pop_state2) %>%
            ggplot(aes(x = date, y = 100000 * hospitalizedCurrently / pop, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Hospitalized / 100k", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Currently Hospitalized / 100k people")
    })
    
    output$state_capicu_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            inner_join(df_pop_state2) %>%
            ggplot(aes(x = date, y = 100000 * inIcuCurrently / pop, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently in ICU / 100k", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Currently in ICU / 100k people")
    })
    
    output$state_capvent_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            inner_join(df_pop_state2) %>%
            ggplot(aes(x = date, y = 100000 * onVentilatorCurrently / pop, color = state)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently on Ventilator / 100k", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Currently on Ventilator / 100k people")
    })
    
    output$state_rt_chart <- renderPlot({
        rt_data %>%
            ggplot(aes(x = as.Date(date), y = mean, color = region)) +
            geom_hline(yintercept = 1, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            gghighlight(
                region %in% toupper(input$statepicker),
                unhighlighted_params = list(alpha = 0.5, size = 0.5)
            ) +
            labs(x = "Date", y = "Rt", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State Rt")
    })
    
    output$state_rtcases_chart <- renderPlot({
        rt_data %>%
            ggplot(aes(x = as.Date(date), y = new_cases, color = region)) +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            gghighlight(
                region %in% toupper(input$statepicker),
                unhighlighted_params = list(alpha = 0.5, size = 0.5)
            ) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "New Cases", color = "State", caption = "Vertical lines represent Stay Home Orders.") +
            ggtitle("Daily State New Cases")
    })
    
    output$state_positive_heatmap <- renderPlot({
        d <- states_daily %>%
            filter(
                state %in% states_info$state,
                positiveIncrease > 0
            ) %>%
            group_by(state) %>%
            mutate(
                positiveIncreaseMax = max(positiveIncrease, na.rm = TRUE),
                dayType = positiveIncrease / positiveIncreaseMax,
                worstDays = positiveIncrease == positiveIncreaseMax
            ) %>%
            ungroup()
        
        worst <- d %>%
            filter(worstDays) %>%
            group_by(state) %>%
            arrange(desc(date)) %>%
            filter(row_number() == 1) %>%
            ungroup() %>%
            transmute(
                state,
                worstdate = date
            )

        d %>%
            inner_join(worst) %>%
            ggplot(aes(x = date, y = fct_reorder(state, desc(worstdate)), fill = dayType)) +
            geom_hline(
                data = d %>%
                    filter(state %in% toupper(input$statepicker)),
                aes(yintercept = state, color = state),
                size = 1
            ) +
            geom_tile(color = "white") +
            geom_point(aes(x = worstdate), color = "white") +
            scale_fill_viridis_c(labels = scales::percent, direction = -1, option = "B") +
            labs(x = "Date", y = "States ordered by days since worst count", fill = "", color = "") +
            ggtitle("Positive Tests per Day as percent of Highest Single Day Count")
    })

    output$state_death_heatmap <- renderPlot({
        d <- states_daily %>%
            filter(
                state %in% states_info$state,
                deathIncrease > 0
            ) %>%
            group_by(state) %>%
            mutate(
                deathIncreaseMax = max(deathIncrease, na.rm = TRUE),
                dayType = deathIncrease / deathIncreaseMax,
                worstDays = deathIncrease == deathIncreaseMax
            ) %>%
            ungroup()
        
        worst <- d %>%
            filter(worstDays) %>%
            group_by(state) %>%
            arrange(desc(date)) %>%
            filter(row_number() == 1) %>%
            ungroup() %>%
            transmute(
                state,
                worstdate = date
            )
        
        d %>%
            inner_join(worst) %>%
            ggplot(aes(x = date, y = fct_reorder(state, desc(worstdate)), fill = dayType)) +
            geom_hline(
                data = d %>%
                    filter(state %in% toupper(input$statepicker)),
                aes(yintercept = state, color = state),
                size = 1
            ) +
            geom_tile(color = "white") +
            geom_point(aes(x = worstdate), color = "white") +
            scale_fill_viridis_c(labels = scales::percent, direction = -1, option = "B") +
            labs(x = "Date", y = "States ordered by days since worst count", fill = "", color = "") +
            ggtitle("Deaths per Day as percent of Highest Single Day Count")
    })
    
    output$stayhome_chart <- renderPlot({
        stayhometable %>%
            ggplot(aes(
                x = `Effective Date`,
                y = fct_reorder(factor(State), `Effective Date`, .desc = TRUE),
                xend = `Duration or End Date`,
                yend = fct_reorder(factor(State), `Effective Date`, .desc = TRUE),
                color = fct_rev(ifelse(is.na(`Duration or End Date`), "Yes", "No"))
            )) +
            geom_vline(xintercept = Sys.Date()) +
            geom_point(size = 4) +
            geom_segment(arrow = arrow()) +
            labs(x = "Date", y = "State", color = "Open Ended", caption = "Vertical line represents today.") +
            ggtitle("Stay At Home Orders")
    })
    
    output$cap_cases_map <- renderPlot({
        state_choropleth(
            states_current %>%
                inner_join(states_info, by = "state") %>%
                mutate(region = tolower(name)) %>%
                inner_join(df_pop_state, by="region") %>%
                rename(pop = value) %>%
                transmute(
                    region,
                    value = 100000 * positive / pop
                ),
            title = "State Positive Tests / 100k people"
        )
    })

    output$cap_deaths_cases_map <- renderPlot({
        state_choropleth(
            states_current %>%
                inner_join(states_info, by = "state") %>%
                mutate(region = tolower(name)) %>%
                inner_join(df_pop_state, by="region") %>%
                rename(pop = value) %>%
                transmute(
                    region,
                    value = 100 * death / positive
                ),
            title = "State Mortality Rate %"
        )
    })
    
    output$cap_deaths_map <- renderPlot({
        state_choropleth(
            states_current %>%
                inner_join(states_info, by = "state") %>%
                mutate(region = tolower(name)) %>%
                inner_join(df_pop_state, by="region") %>%
                rename(pop = value) %>%
                transmute(
                    region,
                    value = 100000 * death / pop
                ),
            title = "State Deaths / 100k people"
        )
    })

    output$cap_tests_map <- renderPlot({
        state_choropleth(
            states_current %>%
                inner_join(states_info, by = "state") %>%
                mutate(region = tolower(name)) %>%
                inner_join(df_pop_state, by="region") %>%
                rename(pop = value) %>%
                transmute(
                    region,
                    value = 100000 * totalTestResults / pop
                ),
            title = "State Tests / 100k people"
        )
    })
    
    output$percent_states_top10_table <- renderTable({
        states_current %>%
            inner_join(states_info, by = "state") %>%
            mutate(region = tolower(name)) %>%
            inner_join(df_pop_state, by="region") %>%
            rename(pop = value) %>%
            transmute(
                State = state,
                `Population Tested` = totalTestResults / pop,
                `Positive Tests` = positive / totalTestResults,
                `Mortality Rate` = death / positive
            ) %>%
            arrange(
                desc(`Mortality Rate`),
                desc(`Positive Tests`),
                desc(`Population Tested`)
            ) %>%
            head(10) %>%
            mutate(
                `Population Tested` = scales::percent(`Population Tested`),
                `Positive Tests` = scales::percent(`Positive Tests`),
                `Mortality Rate` = scales::percent(`Mortality Rate`)
            )
    }, striped = TRUE, bordered = TRUE)
    
    output$cap_states_top10_table <- renderTable({
        states_current %>%
            inner_join(states_info, by = "state") %>%
            mutate(region = tolower(name)) %>%
            inner_join(df_pop_state, by="region") %>%
            rename(pop = value) %>%
            transmute(
                State = state,
                `Tests / 100k` = 100000 * totalTestResults / pop,
                `Positive Tests / 100k` = 100000 * positive / pop,
                `Deaths / 100k` = 100000 * death / pop
            ) %>%
            arrange(
                desc(`Deaths / 100k`),
                desc(`Positive Tests / 100k`),
                desc(`Tests / 100k`)
            ) %>%
            head(10) %>%
            mutate(
                `Tests / 100k` = scales::comma(`Tests / 100k`),
                `Positive Tests / 100k` = scales::comma(`Positive Tests / 100k`),
                `Deaths / 100k` = scales::comma(`Deaths / 100k`)
            )
    }, striped = TRUE, bordered = TRUE)
    
    output$percent_tests_map <- renderPlot({
        state_choropleth(
            states_current %>%
                inner_join(states_info, by = "state") %>%
                mutate(region = tolower(name)) %>%
                inner_join(df_pop_state, by="region") %>%
                rename(pop = value) %>%
                transmute(
                    region,
                    value = 100 * totalTestResults / pop
                ),
            title = "State % Population Tested"
        )
    })
    
    output$pos_tests_map <- renderPlot({
        state_choropleth(
            states_current %>%
                inner_join(states_info, by = "state") %>%
                mutate(region = tolower(name)) %>%
                inner_join(df_pop_state, by="region") %>%
                rename(pop = value) %>%
                transmute(
                    region,
                    value = 100 * positive / totalTestResults
                ),
            title = "State % Positive Tests"
        )
    })

    output$cty_natcases_map <- renderPlot({
        county_choropleth(
            county_data_cases,
            title = "County Deaths / 100k people"
        )
    })
    
    output$cty_natdeaths_map <- renderPlot({
        county_choropleth(
            county_data_deaths,
            title = "County Deaths / 100k people"
        )
    })

    output$cty_natcases_table <- renderTable({
        county_data_cases %>%
            arrange(desc(value)) %>%
            head(10) %>%
            transmute(
                State = state,
                County = county,
                Population = scales::comma(population),
                Cases = scales::comma(cases),
                `Cases / 100k` = scales::comma(value)
            )
    }, striped = TRUE, bordered = TRUE)
    
    output$cty_natdeaths_table <- renderTable({
        county_data_deaths %>%
            arrange(desc(value)) %>%
            head(10) %>%
            transmute(
                State = state,
                County = county,
                Population = scales::comma(population),
                Deaths = scales::comma(deaths),
                `Deaths / 100k` = scales::comma(value)
            )
    }, striped = TRUE, bordered = TRUE)

    output$cty_cases_map <- renderPlot({
        county_choropleth(
            county_data_cases,
            state_zoom = tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]),
            title = "County Cases / 100k people"
        )
    })
    
    output$cty_deaths_map <- renderPlot({
        county_choropleth(
            county_data_deaths,
            state_zoom = tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]),
            title = "County Deaths / 100k people"
        )
    })
    
    output$cty_cases_table <- renderTable({
        county_data_cases %>%
            filter(state %in% df_pop_state2$name[df_pop_state2$state %in% input$statepicker]) %>%
            arrange(desc(value)) %>%
            head(10) %>%
            transmute(
                State = state,
                County = county,
                Population = scales::comma(population),
                Cases = scales::comma(cases),
                `Cases / 100k` = scales::comma(value)
            )
    }, striped = TRUE, bordered = TRUE)
    
    output$cty_deaths_table <- renderTable({
        county_data_deaths %>%
            filter(state %in% df_pop_state2$name[df_pop_state2$state %in% input$statepicker]) %>%
            arrange(desc(value)) %>%
            head(10) %>%
            transmute(
                State = state,
                County = county,
                Population = scales::comma(population),
                Deaths = scales::comma(deaths),
                `Deaths / 100k` = scales::comma(value)
            )
    }, striped = TRUE, bordered = TRUE)
    
    output$data_us <- renderTable({
        us_current %>%
            transmute(
                Tests = scales::comma(totalTestResults),
                Positive = scales::comma(positive),
                Deaths = scales::comma(death),
                Recovered = scales::comma(recovered),
                Modified = lastModified
            )
    }, striped = TRUE, bordered = TRUE)
    
    output$data_states <- renderTable({
        states_current %>%
            transmute(
                State = state,
                Tests = scales::comma(totalTestResults),
                Positive = scales::comma(positive),
                Deaths = scales::comma(death),
                Recovered = scales::comma(recovered),
                Updated = lastUpdateEt,
                Checked = checkTimeEt,
                Quality = dataQualityGrade
            ) %>%
            arrange(State)
    }, striped = TRUE, bordered = TRUE)
    
    output$county_aov_effects <- renderPlot({
        plot(
            model_deaths_effects,
            rotx = 90
        )
    })
    
    output$county_cor_plot <- renderPlot({
        corrplot.mixed(
            corr = model_deaths_cor,
            p.mat = model_deaths_cor_mtest$p,
            main = "\nDemographic Correlations"
        )
    })
}

shinyApp(ui = ui, server = server)
