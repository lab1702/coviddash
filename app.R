
library(tidyverse)
library(lubridate)
library(zoo)
library(rvest)
library(shiny)
library(shinydashboard)
library(choroplethr)
library(choroplethrMaps)
library(plotly)


# theme_set(theme_minimal())


data("df_pop_state")
data("df_county_demographics")


us_current <- read_csv("https://covidtracking.com/api/v1/us/current.csv")

us_daily <- read_csv("https://covidtracking.com/api/v1/us/daily.csv") %>%
  mutate(date = ymd(date))

states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv")

states_current <- read_csv("https://covidtracking.com/api/v1/states/current.csv")

states_daily <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
  mutate(date = ymd(date))

rt_data <- read_csv("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv")

raw_county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


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
    cases = cases,
    deaths = deaths,
    mort = deaths / cases
  )

county_data_deaths <- all_county_data %>%
  transmute(
    region = region,
    value = 100000 * deaths / total_population,
    state = state,
    county = county,
    population = total_population,
    cases = cases,
    deaths = deaths,
    mort = deaths / cases
  )

states_grade <- states_current %>%
  inner_join(df_pop_state2) %>%
  transmute(
    region = tolower(name),
    value = ordered(dataQualityGrade, levels = c("A+", "A", "B", "C", "D"))
  )


ui <- dashboardPage(
  header = dashboardHeader(title = "COVID-19"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(text = "About", tabName = "about_tab"),
      menuItem(text = "Cumulative Counts", tabName = "total_tab"),
      menuItem(text = "Cumulative Percentages", tabName = "perc_tab"),
      menuItem(text = "Daily State Rt", tabName = "state_rt_tab"),
      menuItem(text = "Summary Tables", tabName = "data_tables_tab"),
      menuItem(
        text = "Daily Counts",
        menuSubItem(text = "National", tabName = "us_charts_tab"),
        menuSubItem(text = "State", tabName = "state_charts_tab"),
        menuSubItem(text = "State / 100k", tabName = "state_capcharts_tab"),
        menuSubItem(text = "National Hospital", tabName = "us_hosp_tab"),
        menuSubItem(text = "State Hospital", tabName = "state_hosp_tab"),
        menuSubItem(text = "State Hospital / 100k", tabName = "state_caphosp_tab")
      ),
      menuItem(
        text = "Trajectories",
        menuSubItem(text = "All States", tabName = "states_charts_tab"),
        menuSubItem(text = "Focused Counties", tabName = "county_charts_tab")
      ),
      menuItem(
        text = "Heat Maps",
        menuItem(text = "States 2D", tabName = "state_2d_tab"),
        menuItem(text = "Counties 2D", tabName = "county_2d_tab"),
        menuItem(text = "States 3D", tabName = "state_3d_tab"),
        menuItem(text = "Counties 3D", tabName = "county_3d_tab")
      ),
      menuItem(
        text = "Maps",
        menuSubItem(text = "All States", tabName = "state_capita_tab"),
        menuSubItem(text = "All Counties", tabName = "county_natcapita_tab"),
        menuSubItem(text = "Focused Counties", tabName = "county_capita_tab")
      )
    ),
    hr(),
    selectInput(
      inputId = "statepicker",
      label = "Focus State(s) in Charts:",
      choices = states_info$state,
      selected = "MI",
      multiple = TRUE
    ),
    selectInput(
      inputId = "state3d_select",
      label = "Focus State in County Charts:",
      choices = sort(unique(raw_county_data$state)),
      selected = "Michigan"
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "about_tab",
        box(
          plotOutput("data_quality_map")
        ),
        box(
          "This is still a test dashboard and may contain errors.",
          title = "About",
          status = "danger"
        ),
        box(
          HTML("National and State data is downloaded from <A HREF='https://covidtracking.com/api'>https://covidtracking.com/api</A>"),
          title = "National and State Data Source"
        ),
        box(
          HTML("County level data is downloaded from <A HREF='https://github.com/nytimes/covid-19-data'>https://github.com/nytimes/covid-19-data</A>"),
          title = "County Data Source"
        ),
        box(
          HTML("Rt data is available from <A HREF='https://rt.live'>https://rt.live</A>"),
          title = "Rt Data Source"
        ),
        box(
          HTML("Population data is pulled from the 2012 US American Community Survey (ACS) 5 year estimates included in the <A HREF='https://cran.r-project.org/package=choroplethr'>choroplethr</A> R package."),
          title = "Population Data Source"
        ),
        box(
          HTML("The inspiration for the 3D charts came from the 'Dr. Frank Models' Facebook group which can be found at <A HREF='https://www.facebook.com/groups/158015618707622'>https://www.facebook.com/groups/158015618707622</A>"),
          title = "3D Charts"
        ),
        box(
          HTML("Please consider helping the Folding@home project by installing the software from <A HREF='https://foldingathome.org'>https://foldingathome.org</A> which lets you share unused computer time with COVID-19 (and other) researchers around the world."),
          title = "Folding@home",
          status = "success"
        ),
        box(
          HTML("The source code for this R/Shiny app is available at <A HREF='https://github.com/lab1702/coviddash'>https://github.com/lab1702/coviddash</A>"),
          title = "Source Code"
        )
      ),
      tabItem(
        tabName = "total_tab",
        box(plotOutput("us_total_chart", height = 800)),
        box(plotOutput("state_total_chart", height = 800))
      ),
      tabItem(
        tabName = "perc_tab",
        box(plotOutput("us_perc_pos_chart"), status = "warning"),
        box(plotOutput("us_perc_mort_chart"), status = "danger"),
        box(plotOutput("state_perc_pos_chart"), status = "warning"),
        box(plotOutput("state_perc_mort_chart"), status = "danger")
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
        tabName = "states_charts_tab",
        box(plotlyOutput("states_cases_chart", height = 800), status = "warning"),
        box(plotlyOutput("states_deaths_chart", height = 800), status = "danger")
      ),
      tabItem(
        tabName = "county_charts_tab",
        box(plotlyOutput("county_cases_chart", height = 800), status = "warning"),
        box(plotlyOutput("county_deaths_chart", height = 800), status = "danger")
      ),
      tabItem(
        tabName = "state_rt_tab",
        box(plotlyOutput("state_rt_chart", height = 800)),
        box(plotlyOutput("state_rtcases_chart", height = 800), status = "warning"),
        box("Rt = Average number of people who become infected by an infectious person. Rt > 1 = the virus will spread quickly, Rt < 1 = the virus will stop spreading. Data on this page is sourced from https://rt.live", width = 12)
      ),
      tabItem(
        tabName = "state_2d_tab",
        box(plotlyOutput("state_positive_heatmap", height = 800), status = "warning"),
        box(plotlyOutput("state_death_heatmap", height = 800), status = "danger")
      ),
      tabItem(
        tabName = "county_2d_tab",
        box(plotlyOutput("county_positive_heatmap", height = 800), status = "warning"),
        box(plotlyOutput("county_death_heatmap", height = 800), status = "danger")
      ),
      tabItem(
        tabName = "state_capita_tab",
        box(plotOutput("cap_cases_map"), status = "warning"),
        box(plotOutput("cap_deaths_map"), status = "danger"),
        box(plotOutput("cap_hotcases_map"), status = "warning"),
        box(plotOutput("cap_hotdeaths_map"), status = "danger")
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
        tabName = "state_3d_tab",
        box(plotlyOutput("state_3d_chart1", height = 800), status = "warning"),
        box(plotlyOutput("state_3d_chart2", height = 800), status = "danger")
      ),
      tabItem(
        tabName = "county_3d_tab",
        box(plotlyOutput("county_3d_chart1", height = 800), status = "warning"),
        box(plotlyOutput("county_3d_chart2", height = 800), status = "danger")
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
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Count") +
      facet_wrap(~name, ncol = 1, scales = "free_y") +
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
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Count", color = "State") +
      facet_wrap(~name, ncol = 1, scales = "free_y") +
      ggtitle("State Cumulative Totals")
  })

  output$us_perc_pos_chart <- renderPlot({
    us_daily %>%
      select(
        date,
        totalTestResults,
        positive
      ) %>%
      filter(
        date > Sys.Date() - 90
      ) %>%
      mutate(
        value = positive / totalTestResults
      ) %>%
      ggplot(aes(x = date, y = value)) +
      geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Date", y = "% Positive Tests") +
      ggtitle("90 Day National Cumulative % Positive Tests")
  })

  output$us_perc_mort_chart <- renderPlot({
    us_daily %>%
      select(
        date,
        positive,
        death
      ) %>%
      filter(
        date > Sys.Date() - 90
      ) %>%
      mutate(
        value = death / positive
      ) %>%
      ggplot(aes(x = date, y = value)) +
      geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Date", y = "% Deaths per Positive Tests") +
      ggtitle("90 Day National Cumulative % Deaths per Positive Tests")
  })

  output$state_perc_pos_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      select(
        date,
        state,
        totalTestResults,
        positive
      ) %>%
      filter(
        date > Sys.Date() - 90
      ) %>%
      mutate(
        value = positive / totalTestResults
      ) %>%
      ggplot(aes(x = date, y = value, color = state)) +
      geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Date", y = "% Positive Tests") +
      ggtitle("90 Day State Cumulative % Positive Tests")
  })

  output$state_perc_mort_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      select(
        date,
        state,
        positive,
        death
      ) %>%
      filter(
        date >= Sys.Date() - 90
      ) %>%
      mutate(
        value = death / positive
      ) %>%
      ggplot(aes(x = date, y = value, color = state)) +
      geom_point() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Date", y = "% Deaths per Positive Tests") +
      ggtitle("90 Day State Cumulative % Deaths per Positive Tests")
  })

  output$us_tests_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = totalTestResultsIncrease)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rollmean(x = totalTestResultsIncrease, k = 7, fill = NA)), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Tests") +
      ggtitle("Daily National Tests")
  })

  output$us_cases_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = positiveIncrease)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rollmean(x = positiveIncrease, k = 7, fill = NA)), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Positive Tests") +
      ggtitle("Daily National Positive Tests")
  })

  output$us_deaths_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = deathIncrease)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rollmean(x = deathIncrease, k = 7, fill = NA)), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Deaths") +
      ggtitle("Daily National Deaths")
  })

  output$state_tests_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      group_by(state) %>%
      mutate(rmean = rollmean(x = totalTestResultsIncrease, k = 7, fill = NA)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = totalTestResultsIncrease, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rmean), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Tests", color = "State") +
      ggtitle("Daily State Tests")
  })

  output$state_cases_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      group_by(state) %>%
      mutate(rmean = rollmean(x = positiveIncrease, k = 7, fill = NA)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = positiveIncrease, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rmean), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Positive Tests", color = "State") +
      ggtitle("Daily State Positive Tests")
  })

  output$state_deaths_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      group_by(state) %>%
      mutate(rmean = rollmean(x = deathIncrease, k = 7, fill = NA)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = deathIncrease, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rmean), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Deaths", color = "State") +
      ggtitle("Daily State Deaths")
  })

  output$state_tests_capchart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      group_by(state) %>%
      mutate(rmean = rollmean(x = 100000 * totalTestResultsIncrease / pop, k = 7, fill = NA)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = 100000 * totalTestResultsIncrease / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rmean), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Tests / 100k", color = "State") +
      ggtitle("Daily State Tests / 100k people")
  })

  output$state_cases_capchart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      group_by(state) %>%
      mutate(rmean = rollmean(x = 100000 * positiveIncrease / pop, k = 7, fill = NA)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = 100000 * positiveIncrease / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rmean), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Positive Tests / 100k", color = "State") +
      ggtitle("Daily State Positive Tests / 100k people")
  })

  output$state_deaths_capchart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      group_by(state) %>%
      mutate(rmean = rollmean(x = 100000 * deathIncrease / pop, k = 7, fill = NA)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = 100000 * deathIncrease / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_line(aes(y = rmean), size = 1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Deaths / 100k", color = "State") +
      ggtitle("Daily State Deaths / 100k people")
  })

  output$us_hosp_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = hospitalizedCurrently)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently Hospitalized") +
      ggtitle("Daily National Currently Hospitalized")
  })

  output$us_icu_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = inIcuCurrently)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently in ICU") +
      ggtitle("Daily National Currently in ICU")
  })

  output$us_vent_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = onVentilatorCurrently)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently on Ventilator") +
      ggtitle("Daily National Currently on Ventilator")
  })

  output$state_hosp_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = date, y = hospitalizedCurrently, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently Hospitalized", color = "State") +
      ggtitle("Daily State Currently Hospitalized")
  })

  output$state_icu_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = date, y = inIcuCurrently, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently in ICU", color = "State") +
      ggtitle("Daily State Currently in ICU")
  })

  output$state_vent_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = date, y = onVentilatorCurrently, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently on Ventilator", color = "State") +
      ggtitle("Daily State Currently on Ventilator")
  })

  output$state_caphosp_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      ggplot(aes(x = date, y = 100000 * hospitalizedCurrently / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Hospitalized / 100k", color = "State") +
      ggtitle("Daily State Currently Hospitalized / 100k people")
  })

  output$state_capicu_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      ggplot(aes(x = date, y = 100000 * inIcuCurrently / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently in ICU / 100k", color = "State") +
      ggtitle("Daily State Currently in ICU / 100k people")
  })

  output$state_capvent_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      ggplot(aes(x = date, y = 100000 * onVentilatorCurrently / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Currently on Ventilator / 100k", color = "State") +
      ggtitle("Daily State Currently on Ventilator / 100k people")
  })

  output$states_cases_chart <- renderPlotly({
    states_daily %>%
      transmute(
        State = state,
        Date = date,
        Cases = positive
      ) %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Cases,
        color = ~State,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = "State Cases", x = 0)
      )
  })

  output$states_deaths_chart <- renderPlotly({
    states_daily %>%
      transmute(
        State = state,
        Date = date,
        Deaths = death
      ) %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Deaths,
        color = ~State,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = "State Deaths", x = 0)
      )
  })

  output$county_cases_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Cases = cases
      ) %>%
      ungroup() %>%
      arrange(County, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Cases,
        color = ~County,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "- County Cases"), x = 0)
      )
  })

  output$county_deaths_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Deaths = deaths
      ) %>%
      ungroup() %>%
      arrange(County, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Deaths,
        color = ~County,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "- County Deaths"), x = 0)
      )
  })


  output$state_rt_chart <- renderPlotly({
    rt_data %>%
      filter(region %in% toupper(input$statepicker)) %>%
      transmute(
        Date = date,
        Rt = mean,
        State = region
      ) %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Rt,
        color = ~State,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = "Daily State Rt", x = 0)
      )
  })

  output$state_rtcases_chart <- renderPlotly({
    rt_data %>%
      filter(region %in% toupper(input$statepicker)) %>%
      group_by(region) %>%
      arrange(date) %>%
      transmute(
        Date = date,
        Cases = new_cases,
        AvgCases = rollmean(x = new_cases, k = 7, fill = NA),
        State = region
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Cases,
        color = ~State,
        type = "scatter",
        mode = "markers"
      ) %>%
      add_lines(
        y = ~AvgCases
      ) %>%
      layout(
        title = list(text = "Daily State New Cases", x = 0)
      )
  })

  output$state_positive_heatmap <- renderPlotly({
    states_daily %>%
      filter(date >= "2020-03-01") %>%
      group_by(state) %>%
      transmute(
        Date = date,
        State = state,
        Positives = rollmean(x = positiveIncrease, k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~State,
        z = ~Positives,
        type = "heatmap"
      ) %>%
      layout(
        yaxis = list(autorange = "reversed"),
        title = list(text = "State Positive Tests - Rolling 7 Day Average", x = 0)
      )
  })

  output$state_death_heatmap <- renderPlotly({
    states_daily %>%
      filter(date >= "2020-03-01") %>%
      group_by(state) %>%
      arrange(date) %>%
      transmute(
        Date = date,
        State = state,
        Deaths = rollmean(x = deathIncrease, k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~State,
        z = ~Deaths,
        type = "heatmap"
      ) %>%
      layout(
        yaxis = list(autorange = "reversed"),
        title = list(text = "State Deaths - Rolling 7 Day Average", x = 0)
      )
  })

  output$county_positive_heatmap <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Cases = rollmean(x = c(min(cases), diff(cases)), k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(County, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~County,
        z = ~Cases,
        type = "heatmap"
      ) %>%
      layout(
        yaxis = list(autorange = "reversed"),
        title = list(text = paste(input$state3d_select, "- County Cases - Rolling 7 Day Average"), x = 0)
      )
  })

  output$county_death_heatmap <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Deaths = rollmean(x = c(min(deaths), diff(deaths)), k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(County, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~County,
        z = ~Deaths,
        type = "heatmap"
      ) %>%
      layout(
        yaxis = list(autorange = "reversed"),
        title = list(text = paste(input$state3d_select, "- County Deaths - Rolling 7 Day Average"), x = 0)
      )
  })

  output$state_3d_chart1 <- renderPlotly({
    states_daily %>%
      filter(date >= "2020-03-01") %>%
      group_by(state) %>%
      arrange(date) %>%
      transmute(
        State = state,
        Date = date,
        Positives = rollmean(x = positiveIncrease, k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~State,
        y = ~Date,
        z = ~Positives,
        intensity = ~Positives,
        colors = "Paired",
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = "Positive Tests by State - Rolling 7 Day Average", x = 0)
      )
  })

  output$state_3d_chart2 <- renderPlotly({
    states_daily %>%
      filter(date >= "2020-03-01") %>%
      group_by(state) %>%
      arrange(date) %>%
      transmute(
        State = state,
        Date = date,
        Deaths = rollmean(x = deathIncrease, k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~State,
        y = ~Date,
        z = ~Deaths,
        intensity = ~Deaths,
        colors = "Paired",
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = "Deaths by State - Rolling 7 Day Average", x = 0)
      )
  })

  output$county_3d_chart1 <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Cases = rollmean(x = c(min(cases), diff(cases)), k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(County, Date) %>%
      plot_ly(
        x = ~County,
        y = ~Date,
        z = ~Cases,
        intensity = ~Cases,
        colors = "Paired",
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "- Cases by County - Rolling 7 Day Average"), x = 0)
      )
  })

  output$county_3d_chart2 <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Deaths = rollmean(x = c(min(deaths), diff(deaths)), k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(County, Date) %>%
      plot_ly(
        x = ~County,
        y = ~Date,
        z = ~Deaths,
        intensity = ~Deaths,
        colors = "Paired",
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "- Deaths by County - Rolling 7 Day Average"), x = 0)
      )
  })

  output$cap_cases_map <- renderPlot({
    state_choropleth(
      states_current %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        transmute(
          region,
          value = 100000 * positive / pop
        ),
      num_colors = 1,
      title = "State Cases / 100k people - Total"
    )
  })

  output$cap_hotcases_map <- renderPlot({
    state_choropleth(
      states_daily %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        group_by(region) %>%
        arrange(desc(date)) %>%
        slice_head(7) %>%
        summarise(value = 100000 * sum(positiveIncrease, na.rm = TRUE) / pop) %>%
        ungroup() %>%
        select(region, value),
      num_colors = 1,
      title = "State Cases / 100k people - Last 7 Days"
    )
  })

  output$cap_deaths_map <- renderPlot({
    state_choropleth(
      states_current %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        transmute(
          region,
          value = 100000 * death / pop
        ),
      num_colors = 1,
      title = "State Deaths / 100k people - Total"
    )
  })

  output$cap_hotdeaths_map <- renderPlot({
    state_choropleth(
      states_daily %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        group_by(region) %>%
        arrange(desc(date)) %>%
        slice_head(7) %>%
        summarise(value = 100000 * sum(deathIncrease, na.rm = TRUE) / pop) %>%
        ungroup() %>%
        select(region, value),
      num_colors = 1,
      title = "State Deaths / 100k people - Last 7 Days"
    )
  })


  output$data_quality_map <- renderPlot({
    state_choropleth(
      states_grade,
      title = "Data Quality by State - According to covidtracking.com"
    )
  })

  output$cty_natcases_map <- renderPlot({
    county_choropleth(
      county_data_cases,
      num_colors = 1,
      title = "County Cases / 100k people"
    )
  })

  output$cty_natdeaths_map <- renderPlot({
    county_choropleth(
      county_data_deaths,
      num_colors = 1,
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
        Deaths = scales::comma(deaths),
        `Cases / 100k` = scales::comma(value)
      )
  })

  output$cty_natdeaths_table <- renderTable({
    county_data_deaths %>%
      arrange(desc(value)) %>%
      head(10) %>%
      transmute(
        State = state,
        County = county,
        Population = scales::comma(population),
        Cases = scales::comma(cases),
        Deaths = scales::comma(deaths),
        `Deaths / 100k` = scales::comma(value)
      )
  })

  output$cty_cases_map <- renderPlot({
    county_choropleth(
      county_data_cases,
      num_colors = 1,
      state_zoom = tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]),
      title = "County Cases / 100k people"
    )
  })

  output$cty_deaths_map <- renderPlot({
    county_choropleth(
      county_data_deaths,
      num_colors = 1,
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
        Deaths = scales::comma(deaths),
        `Cases / 100k` = scales::comma(value)
      )
  })

  output$cty_deaths_table <- renderTable({
    county_data_deaths %>%
      filter(state %in% df_pop_state2$name[df_pop_state2$state %in% input$statepicker]) %>%
      arrange(desc(value)) %>%
      head(10) %>%
      transmute(
        State = state,
        County = county,
        Population = scales::comma(population),
        Cases = scales::comma(cases),
        Deaths = scales::comma(deaths),
        `Deaths / 100k` = scales::comma(value)
      )
  })

  output$data_us <- renderTable({
    us_current %>%
      transmute(
        Tests = scales::comma(totalTestResults),
        Positive = scales::comma(positive),
        Deaths = scales::comma(death),
        Recovered = scales::comma(recovered),
        Modified = as.character(lastModified)
      )
  })

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
  })
}

shinyApp(ui = ui, server = server)
