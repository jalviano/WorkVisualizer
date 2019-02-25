# server.R

library(shiny)
library(plotly)

source('guided_work.R')

server <- function(input, output, session) {
    observeEvent(input$trials,  {
        updateSliderInput(session=session, inputId='simulate', max=input$trials)
    })
    observeEvent(input$run, {
        output$work_graph <- renderPlot({
            project_id <- input$project
            test_trials <- input$trials
            bug_id <- switch(input$project,
                             'Closure' = input$bug_closure,
                             'Lang' = input$bug_lang,
                             'Math' = input$bug_math)
            classifier <- switch(input$classifier,
                                 'Random forest (with PCA)' = 'rf',
                                 'Random forest (no PCA)' = 'rf_nb',
                                 'Decision tree (with PCA)' = 'rf',
                                 'Decision tree (no PCA)' = 'rf_nb',
                                 'SGD (with PCA)' = 'rf',
                                 'SGD (no PCA)' = 'rf_nb')
            utility_function <- switch(input$utility,
                                       'Default' = function(eq, tr, dm) {
                                           return(1 - (5 * eq + 3 * tr + dm) / 9)},
                                       'domStrength (with interactions)' = function(eq, tr, dm) {
                                           return(0.47 - 1.56 * eq - 0.27 * tr + 0.46 * dm + 0.91 * eq * dm)},
                                       'domStrength (no interactions)' = function(eq, tr, dm) {
                                           return(0.45 - 1.32 * eq - 0.11 * tr + 0.58 * dm)},
                                       'isDom (with interactions)' = function(eq, tr, dm) {
                                           return(0.01 + 0.09 * eq + 0.06 * tr + 0.97 * dm - 0.33 * eq * dm)},
                                       'isDom (no interactions)' = function(eq, tr, dm) {
                                           return(0.03 + 0.04 * eq - 0.04 * tr + 0.89 * dm)},
                                       'Custom' = function(eq, tr, dm) {
                                           return(input$intercept + input$eq_coeff * eq + input$tr_coeff * tr 
                                                  + input$dm_coeff * dm + input$eq_dm_inter * eq * dm)}
            )
            results <- guide_work(project_id, bug_id, classifier, utility_function, test_trials)
            trial <- reactive({ (input$simulate)})
            output$work_graph <- renderPlot({
                scores <- format_results(results[1:trial()])
                lntyp <- append(rep('dotted', test_trials), 'solid')
                color <- append(rep('springgreen3', test_trials), 'dodgerblue')
                lnwd <- append(rep(1, test_trials), 3)
                matplot(scores, type='l', lwd=lnwd, lty=lntyp, col=color, xlab='Work', ylab='Test Completeness')
                grid()
            })
        })
    })
}
