
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(shiny)
library(shinydashboard)
library(plotly)
library(rjson)


single_state_name_to_code <- function(s) {
  ifelse(
    tolower(s) == "district of columbia",
    "DC",
    state.abb[tolower(state.name) == tolower(s)]
  )
}

state_name_to_code <- Vectorize(single_state_name_to_code)


api_info <- read_csv("https://covidtracking.com/api/v1/status.csv")

us_current <- read_csv("https://covidtracking.com/api/v1/us/current.csv") %>%
  mutate(date = ymd(date))

us_daily <- read_csv("https://covidtracking.com/api/v1/us/daily.csv") %>%
  mutate(date = ymd(date))

states_info <- read_csv("https://covidtracking.com/api/v1/states/info.csv") %>%
  filter(state %in% c("DC", state.abb))

states_current <- read_csv("https://covidtracking.com/api/v1/states/current.csv") %>%
  filter(state %in% c("DC", state.abb)) %>%
  mutate(date = ymd(date))

states_daily <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
  filter(state %in% c("DC", state.abb)) %>%
  mutate(date = ymd(date))

raw_county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>%
  filter(
    state %in% c(state.name, "District of Columbia"),
    county != "Unknown",
    !is.na(fips)
  )

county_data <- fromJSON(file = "geojson-counties-fips.json")

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
      menuItem(text = "Summary Tables", tabName = "data_tables_tab"),
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
          HTML("National and State data is downloaded from <A HREF='https://covidtracking.com/api', TARGET='_blank'>https://covidtracking.com/api</A>"),
          title = "National and State Data Source"
        ),
        box(
          HTML("County level data is downloaded from <A HREF='https://github.com/nytimes/covid-19-data', TARGET='_blank'>https://github.com/nytimes/covid-19-data</A>"),
          title = "County Data Source"
        ),
        box(
          HTML("The inspiration for the 3D charts came from the <A HREF='https://www.facebook.com/groups/158015618707622', TARGET='_blank'>Dr. Frank Models</A> Facebook group."),
          title = "3D Charts"
        ),
        box(
          HTML("A simplified dashboard for mobile devices is available at: <A HREF='https://lab1702.shinyapps.io/covidmobile/' TARGET='_blank'>https://lab1702.shinyapps.io/covidmobile/</A>"),
          title = "Mobile Version"
        ),
        box(
          HTML("The source code for this R/Shiny app is available at <A HREF='https://github.com/lab1702/coviddash', TARGET='_blank'>https://github.com/lab1702/coviddash</A>"),
          title = "Source Code"
        )
      ),
      tabItem(
        tabName = "data_tables_tab",
        box(tableOutput("data_us"), title = "National Data Table"),
        box(tableOutput("data_states"), title = "State Data Table")
      ),
      tabItem(
        tabName = "overall_summary_tab",
        box(plotlyOutput("overall_state_chart", height = 800)),
        box(plotlyOutput("overall_county_chart", height = 800))
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
        pull = ~ ifelse(state == state_name_to_code(input$state3d_select), 0.1, 0),
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
        title = list(text = "National Positive Tests", x = 0)
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
        title = list(text = "National Deaths", x = 0)
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
        title = list(text = paste("State Positive Tests", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
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
        title = list(text = paste("State Deaths", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
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
        title = list(text = paste(input$state3d_select, "Cases by County", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
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
        title = list(text = paste(input$state3d_select, "Deaths by County", ifelse(input$top10, "[Top 10]", "[All]")), x = 0)
      )
  })

  output$us_overlay_chart <- renderPlotly({
    us_daily %>%
      filter(date >= "2020-03-01") %>%
      plot_ly(
        x = ~date,
        y = ~deathIncrease,
        color = I("darkred"),
        name = "Deaths",
        type = "bar"
      ) %>%
      add_trace(
        x = ~date,
        y = ~hospitalizedCurrently,
        color = I("darkblue"),
        name = "Hospitalized",
        type = "bar",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("dimgray"),
        name = "Positive",
        type = "bar",
        yaxis = "y3"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ positiveIncrease / totalTestResultsIncrease,
        color = I("black"),
        name = "% Positive",
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
          tickformat = "%"
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
        color = I("darkred"),
        name = "Deaths",
        type = "bar"
      ) %>%
      add_trace(
        x = ~date,
        y = ~hospitalizedCurrently,
        color = I("darkblue"),
        name = "Hospitalized",
        type = "bar",
        yaxis = "y2"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("dimgray"),
        name = "Positive",
        type = "bar",
        yaxis = "y3"
      ) %>%
      add_trace(
        x = ~date,
        y = ~ positiveIncrease / totalTestResultsIncrease,
        color = I("black"),
        name = "% Positive",
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
          tickformat = "%"
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
        color = I("darkred"),
        name = "Deaths",
        type = "bar"
      ) %>%
      add_trace(
        x = ~date,
        y = ~positiveIncrease,
        color = I("dimgray"),
        name = "Cases",
        type = "bar",
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
    us_current %>%
      transmute(
        Tests = scales::comma(totalTestResults, accuracy = 1),
        Positive = scales::comma(positive, accuracy = 1),
        Deaths = scales::comma(death, accuracy = 1),
        Recovered = scales::comma(recovered, accuracy = 1),
        lastModified = as.character(lastModified),
        Collected = format(date, "%A, %B %d, %Y")
      )
  })

  output$data_states <- renderTable({
    states_current %>%
      transmute(
        State = state,
        Tests = scales::comma(totalTestResults, accuracy = 1),
        Positive = scales::comma(positive, accuracy = 1),
        Deaths = scales::comma(death, accuracy = 1),
        Recovered = scales::comma(recovered, accuracy = 1),
        lastUpdateEt = as.character(lastUpdateEt),
        Collected = format(date, "%A, %B %d, %Y"),
        Quality = dataQualityGrade
      ) %>%
      arrange(State)
  })
}

shinyApp(ui = ui, server = server)
