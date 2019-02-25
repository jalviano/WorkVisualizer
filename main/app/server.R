# server.R

library(shiny)
library(plotly)

source('../guided_work.R')

server <- function(input, output, session) {
    output$work_graph <- renderPlot({
        project_id <- input$project
        classifier <- input$classifier
        test_trials <- input$trials
        bug_id <- switch(input$project,
                         'Closure' = input$bug_closure,
                         'Lang' = input$bug_lang,
                         'Math' = input$bug_math)
        utility_function <- switch(input$utility,
                                   'Default' = function(eq, tr, dm) {
                                       return(1 - (5 * eq + 3 * tr + dm) / 9)},
                                   'domStrength (with interactions)' = function(eq, tr, dm) {
                                       return(0.47 - 1.56 * eq - 0.27 * tr + 0.46 * dm + 0.91 * eq * dm)},
                                   'domStrength (no interaction)s' = function(eq, tr, dm) {
                                       return(0.45 - 1.32 * eq - 0.11 * tr + 0.58 * dm)},
                                   'isDom (with interactions)' = function(eq, tr, dm) {
                                       return(0.01 + 0.09 * eq + 0.06 * tr + 0.97 * dm - 0.33 * eq * dm)},
                                   'isDom (no interactions)' = function(eq, tr, dm) {
                                       return(0.03 + 0.04 * eq - 0.04 * tr + 0.89 * dm)},
                                   'Custom' = function(eq, tr, dm) {
                                       return(input$intercept + input$eq_coeff * eq + input$tr_coeff * tr + input$dm_coeff * dm + input$eq_dm_inter * eq * dm)}
        )
        output$work_graph <- renderPlot({
            guide_work(project_id, bug_id, classifier, utility_function, test_trials)
        })
    })
}
