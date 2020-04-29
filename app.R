
library(dplyr)
library(readr)
library(forcats)
library(rvest)
library(stringi)
library(anytime)
library(ggplot2)
library(ggeffects)
library(ggcorrplot)
library(choroplethr)
library(choroplethrMaps)
library(shiny)
library(shinycssloaders)
library(shinydashboard)

options(stringsAsFactors = FALSE)

theme_set(theme_bw(base_size = 15) + theme(legend.position = "top"))


data("df_pop_state")
data("df_county_demographics")

us_daily <- read_csv("https://covidtracking.com/api/v1/us/daily.csv") %>%
    mutate(date = anydate(date))

states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv")

df_pop_state2 <- states_info %>%
    inner_join(
        df_pop_state %>%
            mutate(name = stri_trans_totitle(region))
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

all_county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
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
model_deaths_cor_pmat <- cor_pmat(model_deaths_input)

model_deaths_cor_plot <- ggcorrplot(
    model_deaths_cor,
    p.mat = model_deaths_cor_pmat,
    ggtheme = theme_bw(base_size = 15) + theme(legend.position = "top"),
    title = "Correlations",
    colors = c("darkred", "white", "darkblue"),
    lab = TRUE,
    lab_size = 5,
    pch.cex = 10
)

model_deaths_aov <- aov(
    `Dead/100k` ~ .,
    data = model_deaths_input
)

model_deaths_effects <- ggeffect(model_deaths_aov)

model_deaths_effects_plot <- plot(
    model_deaths_effects,
    facets = TRUE,
    colors = "flat",
    use.theme = FALSE,
    show.x.title = FALSE
) + ggtitle("Deaths / 100k people - Effects")


ui <- dashboardPage(
    header = dashboardHeader(title = "COVID-19"),
    title = "COVID-19",
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem(text = "About", tabName = "about_tab"),
            menuItem(text = "Cumulative National Counts", tabName = "us_combined_tab"),
            menuItem(text = "Cumulative State Counts", tabName = "state_combined_tab"),
            menuItem(text = "Daily National Counts", tabName = "us_charts_tab"),
            menuItem(text = "Daily State Counts", tabName = "state_charts_tab"),
            menuItem(text = "Daily State Counts / 100k", tabName = "state_capcharts_tab"),
            menuItem(text = "Daily State Rt", tabName = "state_rt_tab"),
            menuItem(text = "Stay At Home Orders", tabName = "stayhome_tab"),
            menuItem(text = "State Maps (%)", tabName = "state_percent_tab"),
            menuItem(text = "State Maps (#)", tabName = "state_capita_tab"),
            menuItem(text = "National County Map (#)", tabName = "county_natcapita_tab"),
            menuItem(text = "State County Maps (#)", tabName = "county_capita_tab"),
            menuItem(text = "Demographics", tabName = "county_aov_tab"),
            menuItem(text = "State Data Table", tabName = "data_states_tab")
        ),
        hr(),
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
                    status = "danger",
                    solidHeader = TRUE
                ),
                box(
                    "National and State data is downloaded from https://covidtracking.com/api",
                    title = "National and State Data Source",
                    status = "primary",
                    solidHeader = TRUE
                ),
                box(
                    "County level data is downloaded from https://github.com/nytimes/covid-19-data",
                    title = "County Data Source",
                    status = "primary",
                    solidHeader = TRUE
                ),
                box(
                    "Rt data is downloaded from https://rt.live",
                    title = "Rt Data Source",
                    status = "primary",
                    solidHeader = TRUE
                ),
                box(
                    "State level data about governors' Stay Home orders is downloaded from https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide",
                    title = "Stay Home Orders Data Source",
                    status = "primary",
                    solidHeader = TRUE
                ),
                box(
                    "Demographics data is pulled from the 2012 US American Community Survey (ACS) 5 year estimates included in the 'choroplethr' R package.",
                    title = "Demographics Data Source",
                    status = "primary",
                    solidHeader = TRUE
                ),
                box(
                    "Please consider helping the Folding@home project by installing the software from https://foldingathome.org which lets you share unused computer time with COVID-19 (and other) researchers around the world.",
                    title = "Folding@home",
                    status = "success",
                    solidHeader = TRUE
                ),
                box(
                    "This dashboard was created by Lars Bernhardsson as a way to explore different ways to look at this data, and to exercise my R/Shiny skills. The code is available at https://github.com/lab1702/coviddash",
                    title = "Author",
                    status = "primary",
                    solidHeader = TRUE
                )
            ),
            tabItem(
                tabName = "us_combined_tab",
                box(withSpinner(plotOutput("us_combined_chart", height = 768)), status = "primary", width = 12)
            ),
            tabItem(
                tabName = "us_charts_tab",
                box(withSpinner(plotOutput("us_tests_chart")), status = "primary"),
                box(withSpinner(plotOutput("us_cases_chart")), status = "warning"),
                box(withSpinner(plotOutput("us_hosp_chart")), status = "warning"),
                box(withSpinner(plotOutput("us_deaths_chart")), status = "danger")
            ),
            tabItem(
                tabName = "state_combined_tab",
                box(withSpinner(plotOutput("state_combined_chart", height = 768)), status = "primary", width = 12)
            ),
            tabItem(
                tabName = "state_charts_tab",
                box(withSpinner(plotOutput("state_tests_chart")), status = "primary"),
                box(withSpinner(plotOutput("state_cases_chart")), status = "warning"),
                box(withSpinner(plotOutput("state_hosp_chart")), status = "warning"),
                box(withSpinner(plotOutput("state_deaths_chart")), status = "danger")
            ),
            tabItem(
                tabName = "state_capcharts_tab",
                box(withSpinner(plotOutput("state_tests_capchart")), status = "primary"),
                box(withSpinner(plotOutput("state_cases_capchart")), status = "warning"),
                box(withSpinner(plotOutput("state_hosp_capchart")), status = "warning"),
                box(withSpinner(plotOutput("state_deaths_capchart")), status = "danger")
            ),
            tabItem(
                tabName = "state_rt_tab",
                box(withSpinner(plotOutput("state_rt_chart", height = 768)), status = "primary", width = 12)
            ),
            tabItem(
                tabName = "stayhome_tab",
                box(withSpinner(plotOutput("stayhome_chart", height = 768)), status = "primary", width = 12)
            ),
            tabItem(
                tabName = "state_percent_tab",
                box(withSpinner(plotOutput("percent_tests_map")), status = "primary"),
                box(withSpinner(plotOutput("pos_tests_map")), status = "warning"),
                box(withSpinner(plotOutput("cap_deaths_cases_map")), status = "danger"),
                box(tableOutput("percent_states_top10_table"), status = "danger", title = "Top 10 States by Mortality Rate", solidHeader = TRUE)
            ),
            tabItem(
                tabName = "state_capita_tab",
                box(withSpinner(plotOutput("cap_tests_map")), status = "primary"),
                box(withSpinner(plotOutput("cap_cases_map")), status = "warning"),
                box(withSpinner(plotOutput("cap_deaths_map")), status = "danger"),
                box(tableOutput("cap_states_top10_table"), status = "danger", title = "Top 10 States by Deaths / 100k people", solidHeader = TRUE)
            ),
            tabItem(
                tabName = "county_natcapita_tab",
                box(withSpinner(plotOutput("cty_natcases_map")), status = "warning"),
                box(withSpinner(plotOutput("cty_natdeaths_map")), status = "danger"),
                box(tableOutput("cty_natcases_table"), status = "warning", title = "Top 10 Counties by Cases / 100k", solidHeader = TRUE),
                box(tableOutput("cty_natdeaths_table"), status = "danger", title = "Top 10 Counties by Deaths / 100k", solidHeader = TRUE)
            ),
            tabItem(
                tabName = "county_capita_tab",
                box(withSpinner(plotOutput("cty_cases_map")), status = "warning"),
                box(withSpinner(plotOutput("cty_deaths_map")), status = "danger"),
                box(tableOutput("cty_cases_table"), status = "warning", title = "Top 10 Counties by Cases / 100k", solidHeader = TRUE),
                box(tableOutput("cty_deaths_table"), status = "danger", title = "Top 10 Counties by Deaths / 100k", solidHeader = TRUE)
            ),
            tabItem(
                tabName = "county_aov_tab",
                box(withSpinner(plotOutput("county_aov_effects", height = 768)), status = "primary"),
                box(withSpinner(plotOutput("county_cor_plot", height = 768)), status = "primary")
            ),
            tabItem(
                tabName = "data_states_tab",
                box(tableOutput("data_states"), status = "primary", title = "State Data Table", solidHeader = TRUE, width = 12)
            )
        )
    )
)


server <- function(input, output, session) {

    output$us_combined_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date)) +
            geom_line(aes(y = positive, color = "Positive Tests"), size = 1) +
            geom_line(aes(y = death, color = "Deaths"), size = 1) +
            geom_line(aes(y = recovered, color = "Recovered"), size = 1) +
            scale_y_continuous(labels = scales::comma) +
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Count", color = "Events") +
            ggtitle("Cumulative National Counts")
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
            labs(x = "Date", y = "Cases") +
            ggtitle("Daily National Cases")
    })

    output$us_hosp_chart <- renderPlot({
        us_daily %>%
            ggplot(aes(x = date, y = hospitalizedCurrently)) +
            geom_hline(yintercept = 0, color = "dimgray") +
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            labs(x = "Date", y = "Currently Hospitalized") +
            ggtitle("Daily National Currently Hospitalized")
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

    output$state_combined_chart <- renderPlot({
        states_daily %>%
            filter(state %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = date)) +
            geom_line(aes(y = positive, color = "Positive Tests"), size = 1) +
            geom_line(aes(y = death, color = "Deaths"), size = 1) +
            geom_line(aes(y = recovered, color = "Recovered"), size = 1) +
            scale_y_continuous(labels = scales::comma) +
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Count", color = "Events") +
            facet_wrap(~ state) +
            ggtitle("Cumulative State Counts")
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
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Tests", color = "State") +
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
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Cases", color = "State") +
            ggtitle("Daily State Cases")
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
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Currently Hospitalized", color = "State") +
            ggtitle("Daily State Currently Hospitalized")
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
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Deaths", color = "State") +
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
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Tests / 100k", color = "State") +
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
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Cases / 100k", color = "State") +
            ggtitle("Daily State Cases  / 100k people")
    })

    output$state_hosp_capchart <- renderPlot({
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
            geom_line() +
            geom_smooth(se = input$inc_se) +
            scale_y_continuous(labels = scales::comma) +
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Currently Hospitalized / 100k", color = "State") +
            ggtitle("Daily State Currently Hospitalized  / 100k people")
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
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Deaths / 100k", color = "State") +
            ggtitle("Daily State Deaths / 100k people")
    })
    
    output$state_rt_chart <- renderPlot({
        rt_data %>%
            filter(region %in% toupper(input$statepicker)) %>%
            ggplot(aes(x = as.Date(date), y = mean, color = region)) +
            geom_hline(yintercept = 1, color = "dimgray") +
            geom_vline(
                data = stayhometable %>%
                    filter(StateCode %in% toupper(input$statepicker)),
                aes(xintercept = `Effective Date`, color = StateCode)
            ) +
            geom_line(size = 1) +
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "Rt", color = "State", linetype = "Aggregation") +
            ggtitle("Daily State Rt", "Rt = Average number of people who become infected by an infectious person. Rt > 1 = the virus will spread quickly, Rt < 1 = the virus will stop spreading.")
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
            geom_vline(xintercept = Sys.Date(), linetype = "longdash") +
            geom_point(size = 4) +
            geom_segment(size = 1, arrow = arrow()) +
            scale_color_brewer(palette = "Set1") +
            labs(x = "Date", y = "State", color = "Open Ended") +
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
            title = "State Cases / 100k people",
            num_colors = 1
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
            title = "State Mortality Rate %",
            num_colors = 1
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
            title = "State Deaths / 100k people",
            num_colors = 1
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
            title = "State Tests / 100k people",
            num_colors = 1
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
    }, striped = TRUE)
    
    output$cap_states_top10_table <- renderTable({
        states_current %>%
            inner_join(states_info, by = "state") %>%
            mutate(region = tolower(name)) %>%
            inner_join(df_pop_state, by="region") %>%
            rename(pop = value) %>%
            transmute(
                State = state,
                `Tests / 100k` = 100000 * totalTestResults / pop,
                `Cases / 100k` = 100000 * positive / pop,
                `Deaths / 100k` = 100000 * death / pop
            ) %>%
            arrange(
                desc(`Deaths / 100k`),
                desc(`Cases / 100k`),
                desc(`Tests / 100k`)
            ) %>%
            head(10) %>%
            mutate(
                `Tests / 100k` = scales::comma(`Tests / 100k`),
                `Cases / 100k` = scales::comma(`Cases / 100k`),
                `Deaths / 100k` = scales::comma(`Deaths / 100k`)
            )
    }, striped = TRUE)
    
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
            title = "State % Population Tested",
            num_colors = 1
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
            title = "State % Positive Tests",
            num_colors = 1
        )
    })

    output$cty_natcases_map <- renderPlot({
        county_choropleth(
            county_data_cases,
            title = "County Cases / 100k people",
            num_colors = 1,
        )
    })
    
    output$cty_natdeaths_map <- renderPlot({
        county_choropleth(
            county_data_deaths,
            title = "County Deaths / 100k people",
            num_colors = 1
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
    }, striped = TRUE)
    
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
    }, striped = TRUE)

    output$cty_cases_map <- renderPlot({
        county_choropleth(
            county_data_cases,
            state_zoom = tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]),
            title = "County Cases / 100k people",
            num_colors = 1,
        )
    })
    
    output$cty_deaths_map <- renderPlot({
        county_choropleth(
            county_data_deaths,
            state_zoom = tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]),
            title = "County Deaths / 100k people",
            num_colors = 1
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
    }, striped = TRUE)
    
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
    }, striped = TRUE)
    
    output$data_states <- renderTable({
        states_current %>%
            inner_join(states_info, by = "state") %>%
            mutate(region = tolower(name)) %>%
            inner_join(df_pop_state, by="region") %>%
            transmute(
                State = region,
                lastUpdateEt,
                dataQualityGrade,
                Population = scales::comma(value, accuracy = 1),
                Tests = scales::comma(totalTestResults, accuracy = 1),
                Negative = scales::comma(negative, accuracy = 1),
                Positive = scales::comma(positive, accuracy = 1),
                Deaths = scales::comma(death, accuracy = 1),
                `% Tested` = scales::percent(totalTestResults / value, accuracy = 0.1),
                `% Positive` = scales::percent(positive / totalTestResults, accuracy = 0.1),
                Mortality = scales::percent(death / positive, accuracy = 0.1),
                `Tests / 100k` = scales::comma(100000 * totalTestResults / value, accuracy = 1),
                `Positive / 100k` = scales::comma(100000 * positive / value, accuracy = 1),
                `Deaths / 100k` = scales::comma(100000 * death / value, accuracy = 1)
            ) %>%
            arrange(State)
    }, striped = TRUE)
    
    output$county_aov_effects <- renderPlot({
        model_deaths_effects_plot
    })
    
    output$county_cor_plot <- renderPlot({
        model_deaths_cor_plot
    })
}

shinyApp(ui = ui, server = server)
