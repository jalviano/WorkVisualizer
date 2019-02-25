# ui.R

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
    titlePanel('Mutation Testing Guided Work'),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId='project',
                        label='Project Id',
                        choices=c('Closure',
                                  'Lang',
                                  'Math'),
                        selected='Math'),
            conditionalPanel(condition='input.project == "Closure"',
                             sliderTextInput(inputId='bug_closure',
                                             label='Bug Id',
                                             choices=c(1, 5, 8, 9, 12, 13, 14, 15, 28, 36, 46, 49, 55, 58, 67, 72, 88, 91, 
                                                       92, 98, 102, 108, 111, 116, 124, 130, 132),
                                             selected=1)),
            conditionalPanel(condition='input.project == "Lang"',
                             sliderTextInput(inputId='bug_lang',
                                             label='Bug Id',
                                             choices=c(1, 2, 4, 11, 25, 28, 33, 37, 40, 44, 45, 49, 51, 53, 54, 55, 59, 62),
                                             selected=1)),
            conditionalPanel(condition='input.project == "Math"',
                             sliderTextInput(inputId='bug_math',
                                             label='Bug Id',
                                             choices=c(10, 2, 4, 7, 21, 22, 25, 26, 28, 33, 35, 39, 40, 51, 52, 57, 60, 68, 
                                                       69, 72, 76, 79, 82, 84, 86, 89, 95, 96, 100, 103, 105),
                                             selected=51)),
            selectInput(inputId='classifier',
                        label='Mutant classifier',
                        choices=c('Random forest (with PCA)',
                                  'Random forest (no PCA)',
                                  'Decision tree (with PCA)',
                                  'Decision tree (no PCA)',
                                  'SGD (with PCA)',
                                  'SGD (no PCA)'),
                        selected='Random forest (with PCA)'),
            selectInput(inputId='utility',
                        label='Utility function',
                        choices=c('Default',
                                  'domStrength (with interactions)',
                                  'domStrength (no interactions)',
                                  'isDom (with interactions)',
                                  'isDom (no interactions)',
                                  'Custom'),
                        selected='Default'),
            conditionalPanel(condition='input.utility == "Custom"',
                             fluidRow(
                                 column(4, numericInput(inputId='intercept', label='Intercept', value=0)),
                                 column(4, numericInput(inputId='eq_coeff', label='EQ', value=0)),
                                 column(4, numericInput(inputId='tr_coeff', label='TR', value=0)),
                                 column(4, numericInput(inputId='dm_coeff', label='DM', value=0)),
                                 column(4, numericInput(inputId='eq_dm_inter', label='EQ:DM', value=0)))),
            sliderInput(inputId='trials',
                        label='Number of trials',
                        min=1,
                        max=100,
                        value=10),
            sliderInput(inputId='simulate',
                        label='Animate simulation',
                        min=1,
                        max=10,
                        value=10,
                        animate = animationOptions(interval=400, loop=FALSE)),
            actionButton(inputId='run', label='Simulate Work')
        ),
        mainPanel(
            plotOutput(outputId='work_graph')
        )
    )
)
