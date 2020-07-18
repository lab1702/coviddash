
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(shiny)
library(shinydashboard)
library(plotly)
library(rjson)


state_name_to_code <- function(s) {
  sapply(s, function(x) {
    case_when(
      tolower(x) == "district of columbia" ~ "DC",
      TRUE ~ state.abb[tolower(state.name) == tolower(x)]
    )
  })
}

us_current <- read_csv("https://covidtracking.com/api/v1/us/current.csv")

us_daily <- read_csv("https://covidtracking.com/api/v1/us/daily.csv") %>%
  mutate(date = ymd(date))

states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv") %>%
  filter(state %in% c("DC", state.abb))

states_current <- read_csv("https://covidtracking.com/api/v1/states/current.csv") %>%
  filter(state %in% c("DC", state.abb))

states_daily <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
  filter(state %in% c("DC", state.abb)) %>%
  mutate(date = ymd(date))

raw_county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(
    state %in% c(state.name, "District of Columbia"),
    !is.na(fips),
    county != "Unknown"
  )

county_data <- fromJSON(file = "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")

states_grade <- states_current %>%
  transmute(
    State = state,
    Grade = ordered(dataQualityGrade, levels = c("A+", "A", "B", "C", "D"))
  )

data_summary <- tibble(
  National = format(max(us_daily$date), "%A, %B %d, %Y"),
  State = format(max(states_daily$date), "%A, %B %d, %Y"),
  County = format(max(raw_county_data$date), "%A, %B %d, %Y")
)


ui <- dashboardPage(
  header = dashboardHeader(title = "COVID-19"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(text = "About", tabName = "about_tab"),
      menuItem(text = "Summary Tables", tabName = "data_tables_tab"),
      menuItem(
        text = "Daily Charts",
        menuSubItem(text = "National Combined", tabName = "us_overlay_tab"),
        menuSubItem(text = "State Combined", tabName = "states_overlay_tab"),
        menuSubItem(text = "County Combined", tabName = "counties_overlay_tab"),
        menuSubItem(text = "National % Positive Tests", tabName = "national_perc_tab"),
        menuSubItem(text = "State % Positive Tests", tabName = "state_perc_tab"),
        menuSubItem(text = "National Hospitalization", tabName = "national_hosp_tab"),
        menuSubItem(text = "State Hospitalization", tabName = "state_hosp_tab")
      ),
      menuItem(
        text = "Cumulative Charts",
        menuSubItem(text = "National", tabName = "national_charts_tab"),
        menuSubItem(text = "States", tabName = "states_charts_tab"),
        menuSubItem(text = "Counties", tabName = "county_charts_tab")
      ),
      menuItem(
        text = "Heat Maps",
        menuItem(text = "States 2D", tabName = "state_2d_tab"),
        menuItem(text = "Counties 2D", tabName = "county_2d_tab"),
        menuItem(text = "States 3D", tabName = "state_3d_tab"),
        menuItem(text = "Counties 3D", tabName = "county_3d_tab")
      ),
      menuItem(
        text = "Geographical Hotspots",
        menuSubItem(text = "State Positive Tests", tabName = "national_positive_hotspots_tab"),
        menuSubItem(text = "State Death", tabName = "national_death_hotspots_tab"),
        menuSubItem(text = "Focused County Cases", tabName = "fcounty_cases_hotspots_tab"),
        menuSubItem(text = "Focused County Deaths", tabName = "fcounty_deaths_hotspots_tab"),
        menuSubItem(text = "All County Cases", tabName = "county_cases_hotspots_tab"),
        menuSubItem(text = "All County Deaths", tabName = "county_deaths_hotspots_tab"),
        em("Please note: County maps load slow")
      )
    ),
    hr(),
    selectInput(
      inputId = "state3d_select",
      label = "Focus State:",
      choices = sort(unique(raw_county_data$state)),
      selected = "Michigan"
    ),
    selectInput(
      inputId = "county3d_select",
      label = "Focus County:",
      choices = "Select Focus State..."
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "about_tab",
        box(
          tableOutput("data_summary_table"),
          title = "Most Recent Data By Type"
        ),
        box(
          tableOutput("data_quality_table"),
          title = "Data Quality, according to covidtracking.com"
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
          HTML("The inspiration for the 3D charts came from the 'Dr. Frank Models' Facebook group which can be found at <A HREF='https://www.facebook.com/groups/158015618707622'>https://www.facebook.com/groups/158015618707622</A>"),
          title = "3D Charts"
        ),
        box(
          HTML("The source code for this R/Shiny app is available at <A HREF='https://github.com/lab1702/coviddash'>https://github.com/lab1702/coviddash</A>"),
          title = "Source Code"
        )
      ),
      tabItem(
        tabName = "national_perc_tab",
        box(plotlyOutput("us_perc_pos_chart", height = 800), width = 12)
      ),
      tabItem(
        tabName = "state_perc_tab",
        box(plotlyOutput("state_perc_pos_chart", height = 800), width = 12)
      ),
      tabItem(
        tabName = "national_hosp_tab",
        box(plotlyOutput("national_hosp_chart", height = 800), width = 12)
      ),
      tabItem(
        tabName = "state_hosp_tab",
        box(plotlyOutput("state_hosp_chart", height = 800), width = 12)
      ),
      tabItem(
        tabName = "national_charts_tab",
        box(plotlyOutput("national_cases_chart", height = 800), status = "warning"),
        box(plotlyOutput("national_deaths_chart", height = 800), status = "danger")
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
        tabName = "us_overlay_tab",
        box(plotlyOutput("us_overlay_chart", height = 800), width = 12),
      ),
      tabItem(
        tabName = "states_overlay_tab",
        box(plotlyOutput("state_overlay_chart", height = 800), width = 12),
      ),
      tabItem(
        tabName = "counties_overlay_tab",
        box(plotlyOutput("county_overlay_chart", height = 800), width = 12),
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
        tabName = "national_positive_hotspots_tab",
        box(plotlyOutput("national_positive_hotspots_map", height = 800), width = 12)
      ),
      tabItem(
        tabName = "national_death_hotspots_tab",
        box(plotlyOutput("national_death_hotspots_map", height = 800), width = 12)
      ),
      tabItem(
        tabName = "fcounty_cases_hotspots_tab",
        box(plotlyOutput("fcounty_cases_hotspots_map", height = 800), width = 12)
      ),
      tabItem(
        tabName = "fcounty_deaths_hotspots_tab",
        box(plotlyOutput("fcounty_deaths_hotspots_map", height = 800), width = 12)
      ),
      tabItem(
        tabName = "county_cases_hotspots_tab",
        box(plotlyOutput("county_cases_hotspots_map", height = 800), width = 12)
      ),
      tabItem(
        tabName = "county_deaths_hotspots_tab",
        box(plotlyOutput("county_deaths_hotspots_map", height = 800), width = 12)
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
  observe({
    updateSelectInput(
      session = session,
      inputId = "county3d_select",
      choices = sort(unique(raw_county_data$county[raw_county_data$state %in% input$state3d_select]))
    )
  })

  output$us_perc_pos_chart <- renderPlotly({
    us_daily %>%
      filter(
        totalTestResultsIncrease > 10000,
        date >= Sys.Date() - 90
      ) %>%
      mutate(
        value = positiveIncrease / totalTestResultsIncrease
      ) %>%
      plot_ly(
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "% Positive Tests", tickformat = "%"),
        title = list(text = "National % Positive Tests", x = 0)
      )
  })

  output$state_perc_pos_chart <- renderPlotly({
    states_daily %>%
      filter(
        state %in% state_name_to_code(input$state3d_select),
        totalTestResultsIncrease > 1000,
        date >= Sys.Date() - 90
      ) %>%
      mutate(
        value = positiveIncrease / totalTestResultsIncrease
      ) %>%
      plot_ly(
        x = ~date,
        y = ~value,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "% Positive Tests", tickformat = "%"),
        title = list(text = paste(input$state3d_select, "% Positive Tests"), x = 0)
      )
  })

  output$national_hosp_chart <- renderPlotly({
    us_daily %>%
      transmute(
        Date = date,
        Hospitalized = hospitalizedCurrently
      ) %>%
      arrange(Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Hospitalized,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = "National Hospitalized", x = 0)
      )
  })

  output$state_hosp_chart <- renderPlotly({
    states_daily %>%
      filter(state %in% state_name_to_code(input$state3d_select)) %>%
      transmute(
        State = state,
        Date = date,
        Hospitalized = hospitalizedCurrently
      ) %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Hospitalized,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "Hospitalized"), x = 0)
      )
  })

  output$national_cases_chart <- renderPlotly({
    us_daily %>%
      transmute(
        Date = date,
        Cases = positive
      ) %>%
      arrange(Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Cases,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = "National Positive Tests", x = 0)
      )
  })

  output$national_deaths_chart <- renderPlotly({
    us_daily %>%
      transmute(
        Date = date,
        Deaths = death
      ) %>%
      arrange(Date) %>%
      plot_ly(
        x = ~Date,
        y = ~Deaths,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        title = list(text = "National Deaths", x = 0)
      )
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
        title = list(text = "State Positive Tests", x = 0)
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
        title = list(text = paste(input$state3d_select, "Cases by County"), x = 0)
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
        title = list(text = paste(input$state3d_select, "Deaths by County"), x = 0)
      )
  })

  output$us_overlay_chart <- renderPlotly({
    us_daily %>%
      mutate(
        positiveIncrease = ifelse(positiveIncrease < 0, NA, positiveIncrease),
        deathIncrease = ifelse(deathIncrease < 0, NA, deathIncrease)
      ) %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("red"),
        name = "Deaths",
        type = "scatter",
        mode = "lines"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("black"),
        name = "Positive Tests",
        type = "scatter",
        mode = "lines",
        line = list(dash = "dot"),
        yaxis = "y2"
      ) %>%
      layout(
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Deaths"
        ),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = "Positive Tests"
        ),
        title = list(
          text = "National Deaths & Positive Tests",
          x = 0
        )
      )
  })

  output$state_overlay_chart <- renderPlotly({
    states_daily %>%
      filter(state == state_name_to_code(input$state3d_select)) %>%
      mutate(
        positiveIncrease = ifelse(positiveIncrease < 0, NA, positiveIncrease),
        deathIncrease = ifelse(deathIncrease < 0, NA, deathIncrease)
      ) %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("red"),
        name = "Deaths",
        type = "scatter",
        mode = "lines"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("black"),
        name = "Positive Tests",
        type = "scatter",
        mode = "lines",
        line = list(dash = "dot"),
        yaxis = "y2"
      ) %>%
      layout(
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Deaths"
        ),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = "Positive Tests"
        ),
        title = list(
          text = paste(input$state3d_select, "Deaths & Positive Tests"),
          x = 0
        )
      )
  })

  output$county_overlay_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state %in% input$state3d_select,
        county %in% input$county3d_select
      ) %>%
      mutate(
        positiveIncrease = c(min(cases), diff(cases)),
        deathIncrease = c(min(deaths), diff(deaths))
      ) %>%
      mutate(
        positiveIncrease = ifelse(positiveIncrease < 0, NA, positiveIncrease),
        deathIncrease = ifelse(deathIncrease < 0, NA, deathIncrease)
      ) %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("red"),
        name = "Deaths",
        type = "scatter",
        mode = "lines",
        legendgroup = "group1"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("black"),
        name = "Cases",
        type = "scatter",
        mode = "lines",
        line = list(dash = "dot"),
        yaxis = "y2",
        legendgroup = "group2"
      ) %>%
      layout(
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Deaths"
        ),
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = "Cases"
        ),
        title = list(
          text = paste0(input$county3d_select, ", ", input$state3d_select, " Deaths & Cases"),
          x = 0
        )
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
        title = list(text = "Positive Tests by State, Rolling 7 Day Average", x = 0)
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
        title = list(text = "Deaths by State, Rolling 7 Day Average", x = 0)
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
        title = list(text = paste(input$state3d_select, "Cases by County, Rolling 7 Day Average"), x = 0)
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
        title = list(text = paste(input$state3d_select, "Deaths by County, Rolling 7 Day Average"), x = 0)
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
        title = list(text = "Positive Tests by State, Rolling 7 Day Average", x = 0)
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
        title = list(text = "Deaths by State, Rolling 7 Day Average", x = 0)
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
        title = list(text = paste(input$state3d_select, "Cases by County, Rolling 7 Day Average"), x = 0)
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
        title = list(text = paste(input$state3d_select, "Deaths by County, Rolling 7 Day Average"), x = 0)
      )
  })

  output$national_positive_hotspots_map <- renderPlotly({
    states_daily %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state) %>%
      summarise(Positives = sum(positiveIncrease, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_geo(
        locationmode = "USA-states"
      ) %>%
      add_trace(
        z = ~Positives,
        locations = ~state,
        color = ~Positives
      ) %>%
      layout(
        title = list(text = "Positive Tests by State, Last 7 Days", x = 0),
        geo = list(scope = "usa")
      )
  })

  output$national_death_hotspots_map <- renderPlotly({
    states_daily %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state) %>%
      summarise(Deaths = sum(deathIncrease, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_geo(
        locationmode = "USA-states"
      ) %>%
      add_trace(
        z = ~Deaths,
        locations = ~state,
        color = ~Deaths
      ) %>%
      layout(
        title = list(text = "Deaths by State, Last 7 Days", x = 0),
        geo = list(scope = "usa")
      )
  })

  output$fcounty_cases_hotspots_map <- renderPlotly({
    raw_county_data %>%
      filter(
        date >= Sys.Date() - 7,
        state == input$state3d_select
      ) %>%
      group_by(state, county, fips) %>%
      summarise(Cases = max(cases, na.rm = TRUE) - min(cases, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_ly() %>%
      add_trace(
        type = "choropleth",
        geojson = county_data,
        locations = ~fips,
        z = ~Cases,
        text = ~ paste(county, state, sep = "<br />"),
        color = ~Cases
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "Cases by County, Last 7 Days"), x = 0),
        geo = list(scope = "usa", fitbounds = "locations", visible = FALSE)
      )
  })

  output$fcounty_deaths_hotspots_map <- renderPlotly({
    raw_county_data %>%
      filter(
        date >= Sys.Date() - 7,
        state == input$state3d_select
      ) %>%
      group_by(state, county, fips) %>%
      summarise(Deaths = max(deaths, na.rm = TRUE) - min(deaths, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_ly() %>%
      add_trace(
        type = "choropleth",
        geojson = county_data,
        locations = ~fips,
        z = ~Deaths,
        text = ~ paste(county, state, sep = "<br />"),
        color = ~Deaths
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "Deaths by County, Last 7 Days"), x = 0),
        geo = list(scope = "usa", fitbounds = "locations", visible = FALSE)
      )
  })

  output$county_cases_hotspots_map <- renderPlotly({
    raw_county_data %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state, county, fips) %>%
      summarise(Cases = max(cases, na.rm = TRUE) - min(cases, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_ly() %>%
      add_trace(
        type = "choropleth",
        geojson = county_data,
        locations = ~fips,
        z = ~Cases,
        text = ~ paste(county, state, sep = "<br />"),
        color = ~Cases
      ) %>%
      layout(
        title = list(text = "Cases by County, Last 7 Days", x = 0),
        geo = list(scope = "usa", visible = FALSE)
      )
  })

  output$county_deaths_hotspots_map <- renderPlotly({
    raw_county_data %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state, county, fips) %>%
      summarise(Deaths = max(deaths, na.rm = TRUE) - min(deaths, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_ly() %>%
      add_trace(
        type = "choropleth",
        geojson = county_data,
        locations = ~fips,
        z = ~Deaths,
        text = ~ paste(county, state, sep = "<br />"),
        color = ~Deaths
      ) %>%
      layout(
        title = list(text = "Deaths by County, Last 7 Days", x = 0),
        geo = list(scope = "usa", visible = FALSE)
      )
  })

  output$data_summary_table <- renderTable({
    data_summary
  })

  output$data_quality_table <- renderTable({
    states_grade %>%
      group_by(Grade) %>%
      arrange(State) %>%
      summarise(States = paste(State, collapse = ", ")) %>%
      ungroup()
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
