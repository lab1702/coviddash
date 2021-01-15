
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(shiny)
library(shinydashboard)
library(plotly)
library(jsonlite)


single_state_name_to_code <- function(s) {
  ifelse(
    tolower(s) == "district of columbia",
    "DC",
    state.abb[tolower(state.name) == tolower(s)]
  )
}

state_name_to_code <- Vectorize(single_state_name_to_code)


county_data <- fromJSON(txt = "geojson-counties-fips.json")

census_state <- read_csv("nst-est2019-alldata.csv") %>%
  filter(
    STATE != "00"
  ) %>%
  transmute(
    fips = STATE,
    population = POPESTIMATE2019,
    yearlydeaths = DEATHS2019
  )

census_county <- read_csv("co-est2019-alldata.csv") %>%
  filter(
    STATE != "00",
    COUNTY != "000"
  ) %>%
  transmute(
    fips = paste0(STATE, COUNTY),
    population = POPESTIMATE2019,
    yearlydeaths = DEATHS2019
  )


api_info <- read_csv("https://api.covidtracking.com/v1/status.csv")

us_current <- read_csv("https://api.covidtracking.com/v1/us/current.csv") %>%
  mutate(date = ymd(date))

us_daily <- read_csv("https://api.covidtracking.com/v1/us/daily.csv") %>%
  mutate(date = ymd(date))

states_info <- read_csv("https://api.covidtracking.com/v1/states/info.csv") %>%
  filter(state %in% c("DC", state.abb))

states_current <- read_csv("https://api.covidtracking.com/v1/states/current.csv") %>%
  filter(state %in% c("DC", state.abb)) %>%
  mutate(date = ymd(date)) %>%
  inner_join(census_state)

states_daily <- read_csv("https://api.covidtracking.com/v1/states/daily.csv") %>%
  filter(state %in% c("DC", state.abb)) %>%
  mutate(date = ymd(date)) %>%
  inner_join(census_state)

raw_county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(
    state %in% c(state.name, "District of Columbia"),
    county != "Unknown",
    !is.na(fips)
  ) %>%
  inner_join(census_county)

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

top10_states <- states_current %>%
  arrange(desc(death)) %>%
  head(10) %>%
  pull(state)

top10_counties <- function(x) {
  raw_county_data %>%
    filter(state == x) %>%
    group_by(county) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(desc(deaths)) %>%
    head(10) %>%
    pull(county)
}


ui <- dashboardPage(
  tags$head(includeHTML(("google-analytics.html"))),
  header = dashboardHeader(title = "COVID-19"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(text = "About", tabName = "about_tab"),
      menuItem(text = "Summary Charts", tabName = "overall_summary_tab"),
      menuItem(
        text = "Daily Charts",
        menuSubItem(text = "National", tabName = "us_overlay_tab"),
        menuSubItem(text = "Focused State", tabName = "states_overlay_tab"),
        menuSubItem(text = "Focused County", tabName = "counties_overlay_tab")
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
        menuSubItem(text = "States Positive Tests", tabName = "national_positive_hotspots_tab"),
        menuSubItem(text = "States Deaths", tabName = "national_death_hotspots_tab"),
        menuSubItem(text = "Counties Cases", tabName = "fcounty_cases_hotspots_tab"),
        menuSubItem(text = "Counties Deaths", tabName = "fcounty_deaths_hotspots_tab"),
        menuSubItem(text = "All Counties Cases", tabName = "county_cases_hotspots_tab"),
        menuSubItem(text = "All Counties Deaths", tabName = "county_deaths_hotspots_tab"),
        em("Please note: County maps load slow")
      ),
      menuItem(
        text = "Deaths per Capita",
        menuSubItem(text = "Ranked", tabName = "cap_death_tab"),
        menuSubItem(text = "vs. Mortality", tabName = "cap_dvsc_tab")
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
    ),
    checkboxInput(
      inputId = "top10",
      label = "Top 10 Cumulative Charts",
      value = TRUE
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
          tableOutput("api_info_table"),
          title = "covidtracking.com API Status"
        ),
        box(
          tableOutput("data_quality_table"),
          title = "Data Quality, according to covidtracking.com"
        ),
        box(
          HTML("National and State data is downloaded from <A HREF='https://api.covidtracking.com', TARGET='_blank'>https://api.covidtracking.com</A>"),
          title = "National and State Data Source"
        ),
        box(
          HTML("County level data is downloaded from <A HREF='https://github.com/nytimes/covid-19-data', TARGET='_blank'>https://github.com/nytimes/covid-19-data</A>"),
          title = "County Data Source"
        ),
        box(
          HTML("State and county population data is downloaded from <A HREF='https://census.gov' TARGET='_blank'>census.gov</A>"),
          title = "Population Data Source"
        ),
        box(
          HTML("The source code for this R/Shiny app is available at <A HREF='https://github.com/lab1702/coviddash', TARGET='_blank'>https://github.com/lab1702/coviddash</A>"),
          title = "Source Code"
        )
      ),
      tabItem(
        tabName = "overall_summary_tab",
        box(plotlyOutput("overall_state_chart", height = 800)),
        box(plotlyOutput("overall_county_chart", height = 800))
      ),
      tabItem(
        tabName = "national_charts_tab",
        box(plotlyOutput("national_cases_chart"), status = "warning"),
        box(plotlyOutput("national_deaths_chart"), status = "danger"),
        box(plotlyOutput("national_cases_chart2"), status = "warning"),
        box(plotlyOutput("national_deaths_chart2"), status = "danger")
      ),
      tabItem(
        tabName = "states_charts_tab",
        box(plotlyOutput("states_cases_chart"), status = "warning"),
        box(plotlyOutput("states_deaths_chart"), status = "danger"),
        box(plotlyOutput("states_cases_chart2"), status = "warning"),
        box(plotlyOutput("states_deaths_chart2"), status = "danger")
      ),
      tabItem(
        tabName = "county_charts_tab",
        box(plotlyOutput("county_cases_chart"), status = "warning"),
        box(plotlyOutput("county_deaths_chart"), status = "danger"),
        box(plotlyOutput("county_cases_chart2"), status = "warning"),
        box(plotlyOutput("county_deaths_chart2"), status = "danger")
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
        tabName = "cap_death_tab",
        box(plotlyOutput("cap_death_state_chart"), width = 12, status = "danger"),
        box(plotlyOutput("cap_death_county_chart"), width = 12, status = "danger"),
      ),
      tabItem(
        tabName = "cap_dvsc_tab",
        box(plotlyOutput("cap_dvsc_state_chart", height = 800)),
        box(plotlyOutput("cap_dvsc_county_chart", height = 800)),
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

  output$overall_state_chart <- renderPlotly({
    states_current %>%
      plot_ly(
        type = "pie",
        labels = ~state,
        values = ~death,
        pull = ~ ifelse(state %in% state_name_to_code(input$state3d_select), 0.1, 0),
        textinfo = "label+value+percent",
        textposition = "inside",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = "Deaths by State", x = 0)
      )
  })

  output$overall_county_chart <- renderPlotly({
    raw_county_data %>%
      filter(state %in% input$state3d_select) %>%
      group_by(county) %>%
      arrange(desc(date)) %>%
      slice(1) %>%
      ungroup() %>%
      plot_ly(
        type = "pie",
        labels = ~county,
        values = ~deaths,
        pull = ~ ifelse(county %in% input$county3d_select, 0.1, 0),
        textinfo = "label+value+percent",
        textposition = "inside",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(text = paste(input$state3d_select, "Deaths by County"), x = 0)
      )
  })

  output$national_cases_chart <- renderPlotly({
    us_daily %>%
      plot_ly(
        x = ~date,
        y = ~positive,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "Positive Tests"),
        xaxis = list(title = "Date"),
        title = list(text = "National Positive Tests", x = 0)
      )
  })

  output$national_cases_chart2 <- renderPlotly({
    us_daily %>%
      plot_ly(
        x = ~date,
        y = ~ rollmean(x = positiveIncrease, k = 7, fill = NA),
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "7-Day Average Positive Tests"),
        xaxis = list(title = "Date"),
        title = list(text = "7-Day Average National Positive Tests", x = 0)
      )
  })

  output$national_deaths_chart <- renderPlotly({
    us_daily %>%
      plot_ly(
        x = ~date,
        y = ~death,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "Deaths"),
        xaxis = list(title = "Date"),
        title = list(text = "National Deaths", x = 0)
      )
  })

  output$national_deaths_chart2 <- renderPlotly({
    us_daily %>%
      plot_ly(
        x = ~date,
        y = ~ rollmean(x = deathIncrease, k = 7, fill = NA),
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "7-Day Average Deaths"),
        xaxis = list(title = "Date"),
        title = list(text = "7-Day Average National Deaths", x = 0)
      )
  })

  output$states_cases_chart <- renderPlotly({
    states_daily %>%
      filter(!input$top10 | (input$top10 & state %in% top10_states)) %>%
      plot_ly(
        x = ~date,
        y = ~positive,
        color = ~state,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "Positive Tests"),
        xaxis = list(title = "Date"),
        title = list(text = paste("State Positive Tests", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$states_cases_chart2 <- renderPlotly({
    states_daily %>%
      filter(!input$top10 | (input$top10 & state %in% top10_states)) %>%
      group_by(state) %>%
      arrange(date) %>%
      mutate(positiveIncrease = rollmean(x = positiveIncrease, k = 7, fill = NA)) %>%
      ungroup() %>%
      plot_ly(
        x = ~date,
        y = ~positiveIncrease,
        color = ~state,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "7-Day Average Positive Tests"),
        xaxis = list(title = "Date"),
        title = list(text = paste("7-Day Average State Positive Tests", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$states_deaths_chart <- renderPlotly({
    states_daily %>%
      filter(!input$top10 | (input$top10 & state %in% top10_states)) %>%
      plot_ly(
        x = ~date,
        y = ~death,
        color = ~state,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "Deaths"),
        xaxis = list(title = "Date"),
        title = list(text = paste("State Deaths", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$states_deaths_chart2 <- renderPlotly({
    states_daily %>%
      filter(!input$top10 | (input$top10 & state %in% top10_states)) %>%
      group_by(state) %>%
      arrange(date) %>%
      mutate(deathIncrease = rollmean(x = deathIncrease, k = 7, fill = NA)) %>%
      ungroup() %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = ~state,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "7-Day Average Deaths"),
        xaxis = list(title = "Date"),
        title = list(text = paste("7-Day Average State Deaths", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$county_cases_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select,
        !input$top10 | (input$top10 & county %in% top10_counties(input$state3d_select))
      ) %>%
      plot_ly(
        x = ~date,
        y = ~cases,
        color = ~county,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "Cases"),
        xaxis = list(title = "Date"),
        title = list(text = paste(input$state3d_select, "Cases by County", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$county_cases_chart2 <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select,
        !input$top10 | (input$top10 & county %in% top10_counties(input$state3d_select))
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      mutate(cases = rollmean(x = c(min(cases), diff(cases)), k = 7, fill = NA)) %>%
      ungroup() %>%
      plot_ly(
        x = ~date,
        y = ~cases,
        color = ~county,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "7-Day Average Cases"),
        xaxis = list(title = "Date"),
        title = list(text = paste("7-Day Average", input$state3d_select, "Cases by County", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$county_deaths_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select,
        !input$top10 | (input$top10 & county %in% top10_counties(input$state3d_select))
      ) %>%
      plot_ly(
        x = ~date,
        y = ~deaths,
        color = ~county,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "Deaths"),
        xaxis = list(title = "Date"),
        title = list(text = paste(input$state3d_select, "Deaths by County", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$county_deaths_chart2 <- renderPlotly({
    raw_county_data %>%
      filter(
        state == input$state3d_select,
        !input$top10 | (input$top10 & county %in% top10_counties(input$state3d_select))
      ) %>%
      group_by(county) %>%
      arrange(date) %>%
      mutate(deaths = rollmean(x = c(min(deaths), diff(deaths)), k = 7, fill = NA)) %>%
      ungroup() %>%
      plot_ly(
        x = ~date,
        y = ~deaths,
        color = ~county,
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        yaxis = list(title = "7-Day Average Deaths"),
        xaxis = list(title = "Date"),
        title = list(text = paste("7-Day Average", input$state3d_select, "Deaths by County", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$us_overlay_chart <- renderPlotly({
    us_daily %>%
      filter(date >= "2020-03-01") %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("pink"),
        name = "Deaths",
        type = "bar"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = deathIncrease, k = 7, fill = NA),
        color = I("darkred"),
        name = "7-Day Deaths",
        type = "scatter",
        mode = "lines",
        yaxis = "y1"
      ) %>%
      add_trace(
        x = ~date,
        y = ~hospitalizedCurrently,
        color = I("cornflowerblue"),
        name = "Hospitalized",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~inIcuCurrently,
        color = I("orange"),
        name = "ICU",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~onVentilatorCurrently,
        color = I("orangered"),
        name = "Ventilator",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("gray"),
        name = "Positive",
        type = "bar",
        yaxis = "y3"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ positiveIncrease / totalTestResultsIncrease,
        color = I("lightgray"),
        name = "% Positive",
        type = "scatter",
        mode = "lines",
        yaxis = "y4"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = positiveIncrease, k = 7, fill = NA),
        color = I("black"),
        name = "7-Day Positive",
        type = "scatter",
        mode = "lines",
        yaxis = "y3"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = positiveIncrease, k = 7, fill = NA) / rollmean(x = totalTestResultsIncrease, k = 7, fill = NA),
        color = I("dimgray"),
        name = "7-Day % Positive",
        type = "scatter",
        mode = "lines",
        yaxis = "y4"
      ) %>%
      subplot(
        nrows = 4,
        shareX = TRUE
      ) %>%
      layout(
        hovermode = "x",
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Deaths",
          fixedrange = TRUE
        ),
        yaxis2 = list(
          title = "Hospitalized",
          fixedrange = TRUE
        ),
        yaxis3 = list(
          title = "Positive",
          fixedrange = TRUE
        ),
        yaxis4 = list(
          title = "% Positive",
          fixedrange = TRUE,
          tickformat = ".2%"
        ),
        title = list(
          text = "National Daily Numbers",
          x = 0
        )
      )
  })

  output$state_overlay_chart <- renderPlotly({
    states_daily %>%
      filter(
        state == state_name_to_code(input$state3d_select),
        date >= "2020-03-01"
      ) %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("pink"),
        name = "Deaths",
        type = "bar"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = deathIncrease, k = 7, fill = NA),
        color = I("darkred"),
        name = "7-Day Deaths",
        type = "scatter",
        mode = "lines",
        yaxis = "y1"
      ) %>%
      add_trace(
        x = ~date,
        y = ~hospitalizedCurrently,
        color = I("cornflowerblue"),
        name = "Hospitalized",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~inIcuCurrently,
        color = I("orange"),
        name = "ICU",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~onVentilatorCurrently,
        color = I("orangered"),
        name = "Ventilator",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("gray"),
        name = "Positive",
        type = "bar",
        yaxis = "y3"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = positiveIncrease, k = 7, fill = NA),
        color = I("black"),
        name = "7-Day Positive",
        type = "scatter",
        mode = "lines",
        yaxis = "y3"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ positiveIncrease / totalTestResultsIncrease,
        color = I("lightgray"),
        name = "% Positive",
        type = "scatter",
        mode = "lines",
        yaxis = "y4"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = positiveIncrease, k = 7, fill = NA) / rollmean(x = totalTestResultsIncrease, k = 7, fill = NA),
        color = I("dimgray"),
        name = "7-Day % Positive",
        type = "scatter",
        mode = "lines",
        yaxis = "y4"
      ) %>%
      subplot(
        nrows = 4,
        shareX = TRUE
      ) %>%
      layout(
        hovermode = "x",
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Deaths",
          fixedrange = TRUE
        ),
        yaxis2 = list(
          title = "Hospitalized",
          fixedrange = TRUE
        ),
        yaxis3 = list(
          title = "Positive",
          fixedrange = TRUE
        ),
        yaxis4 = list(
          title = "% Positive",
          fixedrange = TRUE,
          tickformat = ".2%"
        ),
        title = list(
          text = paste(input$state3d_select, "Daily Numbers"),
          x = 0
        )
      )
  })

  output$county_overlay_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state %in% input$state3d_select,
        county %in% input$county3d_select,
        date >= "2020-03-01"
      ) %>%
      mutate(
        positiveIncrease = c(min(cases), diff(cases)),
        deathIncrease = c(min(deaths), diff(deaths))
      ) %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("pink"),
        name = "Deaths",
        type = "bar"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = deathIncrease, k = 7, fill = NA),
        color = I("darkred"),
        name = "7-Day Deaths",
        type = "scatter",
        mode = "lines",
        yaxis = "y1"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("gray"),
        name = "Cases",
        type = "bar",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ rollmean(x = positiveIncrease, k = 7, fill = NA),
        color = I("black"),
        name = "7-Day Cases",
        type = "scatter",
        mode = "lines",
        yaxis = "y2"
      ) %>%
      subplot(
        nrows = 2,
        shareX = TRUE
      ) %>%
      layout(
        hovermode = "x",
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Deaths",
          fixedrange = TRUE
        ),
        yaxis2 = list(
          title = "Cases",
          fixedrange = TRUE
        ),
        title = list(
          text = paste(input$county3d_select, "County,", input$state3d_select, "Daily Numbers"),
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
        Positive = rollmean(x = positiveIncrease, k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~Date,
        y = ~State,
        z = ~Positive,
        type = "heatmap"
      ) %>%
      layout(
        yaxis = list(autorange = "reversed", fixedrange = TRUE),
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
        yaxis = list(autorange = "reversed", fixedrange = TRUE),
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
        yaxis = list(autorange = "reversed", fixedrange = TRUE),
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
        yaxis = list(autorange = "reversed", fixedrange = TRUE),
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
        Positive = rollmean(x = positiveIncrease, k = 7, fill = NA)
      ) %>%
      ungroup() %>%
      arrange(State, Date) %>%
      plot_ly(
        x = ~State,
        y = ~Date,
        z = ~Positive,
        intensity = ~Positive,
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
      summarise(Positive = mean(positiveIncrease, na.rm = TRUE)) %>%
      ungroup() %>%
      plot_geo(
        locationmode = "USA-states"
      ) %>%
      add_trace(
        z = ~Positive,
        locations = ~state,
        color = ~Positive
      ) %>%
      layout(
        title = list(text = "Average Daily Positive Tests by State, Last 7 Days", x = 0),
        geo = list(scope = "usa")
      )
  })

  output$national_death_hotspots_map <- renderPlotly({
    states_daily %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state) %>%
      summarise(Deaths = mean(deathIncrease, na.rm = TRUE)) %>%
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
        title = list(text = "Average Daily Deaths by State, Last 7 Days", x = 0),
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
      summarise(Cases = mean(diff(cases), na.rm = TRUE)) %>%
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
        title = list(text = paste(input$state3d_select, "Average Daily Cases by County, Last 7 Days"), x = 0),
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
      summarise(Deaths = mean(diff(deaths), na.rm = TRUE)) %>%
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
        title = list(text = paste(input$state3d_select, "Average Daily Deaths by County, Last 7 Days"), x = 0),
        geo = list(scope = "usa", fitbounds = "locations", visible = FALSE)
      )
  })

  output$county_cases_hotspots_map <- renderPlotly({
    raw_county_data %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state, county, fips) %>%
      summarise(Cases = mean(diff(cases), na.rm = TRUE)) %>%
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
        title = list(text = "Average Daily Cases by County, Last 7 Days", x = 0),
        geo = list(scope = "usa", visible = FALSE)
      )
  })

  output$county_deaths_hotspots_map <- renderPlotly({
    raw_county_data %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(state, county, fips) %>%
      summarise(Deaths = mean(diff(deaths), na.rm = TRUE)) %>%
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
        title = list(text = "Average Daily Deaths by County, Last 7 Days", x = 0),
        geo = list(scope = "usa", visible = FALSE)
      )
  })

  output$cap_death_state_chart <- renderPlotly({
    states_current %>%
      plot_ly(
        x = ~ reorder(state, -death / population),
        y = ~ 1000000 * death / population,
        color = ~ state %in% state_name_to_code(input$state3d_select),
        colors = c("dimgray", "darkred"),
        showlegend = FALSE,
        type = "bar"
      ) %>%
      layout(
        xaxis = list(title = "State"),
        yaxis = list(title = "Deaths per Million People"),
        title = list(text = "Deaths per Million People by State", x = 0)
      )
  })

  output$cap_death_county_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state %in% input$state3d_select
      ) %>%
      group_by(
        county,
        fips
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice(1) %>%
      ungroup() %>%
      plot_ly(
        x = ~ reorder(county, -deaths / population),
        y = ~ 1000000 * deaths / population,
        color = ~ county %in% input$county3d_select,
        colors = c("dimgray", "darkred"),
        showlegend = FALSE,
        type = "bar"
      ) %>%
      layout(
        xaxis = list(title = "County"),
        yaxis = list(title = "Deaths per Million People"),
        title = list(text = paste(input$state3d_select, "Deaths per Million People by County"), x = 0)
      )
  })

  output$cap_dvsc_state_chart <- renderPlotly({
    states_current %>%
      plot_ly(
        x = ~ death / positive,
        y = ~ 1000000 * death / population,
        color = ~ state %in% state_name_to_code(input$state3d_select),
        colors = c("dimgray", "darkred"),
        showlegend = FALSE,
        text = ~state,
        type = "scatter",
        mode = "text"
      ) %>%
      layout(
        xaxis = list(title = "Deaths per Positive Tests", tickformat = ".2%"),
        yaxis = list(title = "Deaths per Million People"),
        title = list(text = "Deaths per Million People vs. Deaths per Positive Tests", x = 0)
      )
  })

  output$cap_dvsc_county_chart <- renderPlotly({
    raw_county_data %>%
      filter(
        state %in% input$state3d_select
      ) %>%
      group_by(
        county,
        fips
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice(1) %>%
      ungroup() %>%
      plot_ly(
        x = ~ deaths / cases,
        y = ~ 1000000 * deaths / population,
        color = ~ state %in% input$state3d_select & county %in% input$county3d_select,
        colors = c("dimgray", "darkred"),
        showlegend = FALSE,
        text = ~county,
        type = "scatter",
        mode = "text"
      ) %>%
      layout(
        xaxis = list(title = "Deaths per Cases", tickformat = ".2%"),
        yaxis = list(title = "Deaths per Million People"),
        title = list(text = paste(input$state3d_select, "Deaths per Million People vs. Deaths per Cases"), x = 0)
      )
  })

  output$data_summary_table <- renderTable({
    data_summary
  })

  output$api_info_table <- renderTable({
    api_info %>%
      transmute(
        `Build Time` = as.character(buildTime),
        Production = production,
        `Run Number` = as.integer(runNumber)
      )
  })

  output$data_quality_table <- renderTable({
    states_grade %>%
      group_by(Grade) %>%
      arrange(State) %>%
      summarise(States = paste(State, collapse = ", ")) %>%
      ungroup()
  })

  output$data_us <- renderTable({
    us_current
  })

  output$data_states <- renderTable({
    states_current %>%
      arrange(state)
  })
}

shinyApp(ui = ui, server = server)
