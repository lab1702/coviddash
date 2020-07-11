
library(tidyverse)
library(lubridate)
library(rvest)
library(shiny)
library(shinydashboard)
library(choroplethr)
library(choroplethrMaps)
library(plotly)


theme_set(theme_minimal(base_size = 14))


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
      menuItem(
        text = "Maps",
        menuSubItem(text = "All States", tabName = "state_capita_tab"),
        menuSubItem(text = "All Counties", tabName = "county_natcapita_tab"),
        menuSubItem(text = "Focused Counties", tabName = "county_capita_tab")
      ),
      menuItem(text = "State 3D Chart", tabName = "state_3d_tab"),
      menuItem(text = "County 3D Chart", tabName = "county_3d_tab"),
      menuItem(text = "Data Tables", tabName = "data_tables_tab")
    ),
    selectInput(
      inputId = "statepicker",
      label = "Focus State(s) in Charts:",
      choices = states_info$state,
      selected = "MI",
      multiple = TRUE
    ),
    checkboxInput(
      inputId = "inc_se",
      label = "Show Confidence Intervals",
      value = FALSE
    ),
    selectInput(
      inputId = "state3d_select",
      label = "Focus State in County 3D Chart:",
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
          "National and State data is downloaded from https://covidtracking.com/api",
          title = "National and State Data Source"
        ),
        box(
          "County level data is downloaded from https://github.com/nytimes/covid-19-data",
          title = "County Data Source"
        ),
        box(
          "Rt data is downloaded from https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv which is used by https://rt.live",
          title = "Rt Data Source"
        ),
        box(
          "Population data is pulled from the 2012 US American Community Survey (ACS) 5 year estimates included in the 'choroplethr' R package.",
          title = "Population Data Source"
        ),
        box(
          "The inspiration for the 3D charts came from the 'Dr. Frank Models' Facebook group which can be found at https://www.facebook.com/groups/158015618707622",
          title = "3D Charts"
        ),
        box(
          "Please consider helping the Folding@home project by installing the software from https://foldingathome.org which lets you share unused computer time with COVID-19 (and other) researchers around the world.",
          title = "Folding@home",
          status = "success"
        ),
        box(
          "The source code for this R/Shiny app is available at https://github.com/lab1702/coviddash",
          title = "Source Code"
        )
      ),
      tabItem(
        tabName = "total_tab",
        box(plotOutput("us_total_chart", height = 768)),
        box(plotOutput("state_total_chart", height = 768))
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
        tabName = "state_capita_tab",
        box(plotOutput("cap_cases_map"), status = "warning"),
        box(plotOutput("cap_deaths_map"), status = "danger"),
        box(tableOutput("cap_states_cases_table"), status = "warning", title = "Top 10 States by Cases / 100k people"),
        box(tableOutput("cap_states_deaths_table"), status = "danger", title = "Top 10 States by Deaths / 100k people")
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
        box(plotlyOutput("state_3d_chart1", height = 768), status = "warning"),
        box(plotlyOutput("state_3d_chart2", height = 768), status = "danger")
      ),
      tabItem(
        tabName = "county_3d_tab",
        box(plotlyOutput("county_3d_chart1", height = 768), status = "warning"),
        box(plotlyOutput("county_3d_chart2", height = 768), status = "danger")
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
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Tests") +
      ggtitle("Daily National Tests")
  })

  output$us_cases_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = positiveIncrease)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Positive Tests") +
      ggtitle("Daily National Positive Tests")
  })

  output$us_deaths_chart <- renderPlot({
    us_daily %>%
      ggplot(aes(x = date, y = deathIncrease)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
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
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Tests", color = "State") +
      ggtitle("Daily State Tests")
  })

  output$state_cases_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = date, y = positiveIncrease, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Positive Tests", color = "State") +
      ggtitle("Daily State Positive Tests")
  })

  output$state_deaths_chart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = date, y = deathIncrease, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Deaths", color = "State") +
      ggtitle("Daily State Deaths")
  })

  output$state_tests_capchart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      ggplot(aes(x = date, y = 100000 * totalTestResultsIncrease / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Tests / 100k", color = "State") +
      ggtitle("Daily State Tests / 100k people")
  })

  output$state_cases_capchart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      ggplot(aes(x = date, y = 100000 * positiveIncrease / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "Positive Tests / 100k", color = "State") +
      ggtitle("Daily State Positive Tests / 100k people")
  })

  output$state_deaths_capchart <- renderPlot({
    states_daily %>%
      filter(state %in% toupper(input$statepicker)) %>%
      inner_join(df_pop_state2) %>%
      ggplot(aes(x = date, y = 100000 * deathIncrease / pop, color = state)) +
      geom_hline(yintercept = 0, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
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

  output$state_rt_chart <- renderPlot({
    rt_data %>%
      filter(region %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = as.Date(date), y = mean, color = region)) +
      geom_hline(yintercept = 1, color = "dimgray") +
      geom_point() +
      labs(x = "Date", y = "Rt", color = "State") +
      ggtitle("Daily State Rt")
  })

  output$state_rtcases_chart <- renderPlot({
    rt_data %>%
      filter(region %in% toupper(input$statepicker)) %>%
      ggplot(aes(x = as.Date(date), y = new_cases, color = region)) +
      geom_hline(yintercept = 1, color = "dimgray") +
      geom_point() +
      geom_smooth(se = input$inc_se) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Date", y = "New Cases", color = "State") +
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

  output$cap_cases_map <- renderPlot({
    cc <- StateChoropleth$new(
      states_current %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        transmute(
          region,
          value = 100000 * positive / pop
        )
    )

    cc$title <- "State Cases / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)

    cc$render()
  })

  output$cap_deaths_map <- renderPlot({
    cc <- StateChoropleth$new(
      states_current %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        transmute(
          region,
          value = 100000 * death / pop
        )
    )

    cc$title <- "State Deaths / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)

    cc$render()
  })

  output$cap_tests_map <- renderPlot({
    cc <- StateChoropleth$new(
      states_current %>%
        inner_join(states_info, by = "state") %>%
        mutate(region = tolower(name)) %>%
        inner_join(df_pop_state, by = "region") %>%
        rename(pop = value) %>%
        transmute(
          region,
          value = 100000 * totalTestResults / pop
        )
    )

    cc$title <- "State Tests / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)

    cc$render()
  })

  output$cap_states_cases_table <- renderTable({
    states_current %>%
      inner_join(states_info, by = "state") %>%
      mutate(region = tolower(name)) %>%
      inner_join(df_pop_state, by = "region") %>%
      rename(pop = value) %>%
      transmute(
        State = state,
        Population = pop,
        Cases = positive,
        Deaths = death,
        `Cases / 100k` = 100000 * positive / pop
      ) %>%
      arrange(
        desc(`Cases / 100k`),
        desc(Deaths),
        desc(Cases)
      ) %>%
      head(10) %>%
      mutate(
        `Population` = scales::comma(Population),
        `Cases` = scales::comma(Cases),
        `Deaths` = scales::comma(Deaths),
        `Cases / 100k` = scales::comma(`Cases / 100k`)
      )
  })

  output$cap_states_deaths_table <- renderTable({
    states_current %>%
      inner_join(states_info, by = "state") %>%
      mutate(region = tolower(name)) %>%
      inner_join(df_pop_state, by = "region") %>%
      rename(pop = value) %>%
      transmute(
        State = state,
        Population = pop,
        Cases = positive,
        Deaths = death,
        `Deaths / 100k` = 100000 * death / pop
      ) %>%
      arrange(
        desc(`Deaths / 100k`),
        desc(Deaths),
        desc(Cases)
      ) %>%
      head(10) %>%
      mutate(
        `Population` = scales::comma(Population),
        `Cases` = scales::comma(Cases),
        `Deaths` = scales::comma(Deaths),
        `Deaths / 100k` = scales::comma(`Deaths / 100k`)
      )
  })

  output$data_quality_map <- renderPlot({
    cc <- StateChoropleth$new(states_grade)

    cc$title <- "Data Quality by State - According to covidtracking.com"
    cc$ggplot_scale <- scale_fill_brewer(palette = "RdYlGn", direction = -1)

    cc$render()
  })

  output$cty_natcases_map <- renderPlot({
    cc <- CountyChoropleth$new(county_data_cases)

    cc$title <- "County Cases / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)

    cc$render()
  })

  output$cty_natdeaths_map <- renderPlot({
    cc <- CountyChoropleth$new(county_data_deaths)

    cc$title <- "County Deaths / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)

    cc$render()
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
    cc <- CountyChoropleth$new(county_data_cases)

    cc$title <- "County Cases / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)
    cc$set_zoom(tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]))

    cc$render()
  })

  output$cty_deaths_map <- renderPlot({
    cc <- CountyChoropleth$new(county_data_deaths)

    cc$title <- "County Deaths / 100k people"
    cc$set_num_colors(1)
    cc$ggplot_scale <- scale_fill_viridis_c("", na.value = "white", option = "B", direction = -1)
    cc$set_zoom(tolower(df_pop_state2$name[df_pop_state2$state %in% input$statepicker]))

    cc$render()
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

  output$county_3d_chart1 <- renderPlotly({
    temp_data <- raw_county_data %>%
      filter(state == input$state3d_select) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Cases = c(min(cases), diff(cases))
      ) %>%
      ungroup() %>%
      arrange(County, Date)

    temp_data %>%
      plot_ly(
        x = ~County,
        y = ~Date,
        z = ~Cases,
        intensity = ~Cases,
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "- Cases by County and Date"))
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$county_3d_chart2 <- renderPlotly({
    temp_data <- raw_county_data %>%
      filter(state == input$state3d_select) %>%
      group_by(county) %>%
      arrange(date) %>%
      transmute(
        County = county,
        Date = date,
        Deaths = c(min(deaths), diff(deaths))
      ) %>%
      ungroup() %>%
      arrange(County, Date)

    temp_data %>%
      plot_ly(
        x = ~County,
        y = ~Date,
        z = ~Deaths,
        intensity = ~Deaths,
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "- Deaths by County and Date"))
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$state_3d_chart1 <- renderPlotly({
    states_daily %>%
      transmute(
        State = state,
        Date = date,
        Cases = positiveIncrease
      ) %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~State,
        y = ~Date,
        z = ~Cases,
        intensity = ~Cases,
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = "Cases by State and Date")
      ) %>%
      config(displayModeBar = FALSE)
  })

  output$state_3d_chart2 <- renderPlotly({
    states_daily %>%
      transmute(
        State = state,
        Date = date,
        Deaths = deathIncrease
      ) %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~State,
        y = ~Date,
        z = ~Deaths,
        intensity = ~Deaths,
        type = "mesh3d"
      ) %>%
      layout(
        title = list(text = "Deaths by State and Date")
      ) %>%
      config(displayModeBar = FALSE)
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
