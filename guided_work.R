# guided_work.R

library(ggplot2)

# Arguments
project_id <- 'Math'
bug_id <- 51
classifier <- 'rf'
ts_algorithm <- 'random'
utility_function <- 'etd'
test_trials = 100

# Get data
data_dir <- paste('../r_shiny/data/', project_id, '/', sep='')
detailed_results <- read.csv(paste(data_dir, 'detailedResults.csv', sep=''))
score_matrix <- paste(data_dir, bug_id, '/scoreMatrix.csv', sep='')
subject = subset(detailed_results, bugId == 51)
# Get test signature map
test_sig_map <- read.csv(score_matrix)
# Get utilities and mutants
utility <- 1 - (5 * subject$probEq + 3 * subject$probTr + subject$probDm) / 9
mutants <- subject$mutantId

get_tests_for_mutant <- function(mutant) {
    tests <- subset(test_sig_map, Mutant == mutant)
    tests <- as.data.frame(t(tests[-1]))
    colnames(tests) <- 'Result'
    tests <- subset(tests, (Result == 'FAIL' | Result == 'EXC'))
    tests <- rownames(tests)
    return(tests)
}

get_mutants_for_test <- function(test, live) {
    mutants <- subset(test_sig_map, ((test_sig_map[test] == 'FAIL' | test_sig_map[test] == 'EXC') & Mutant %in% live), 'Mutant')
    mutants <- as.vector(unlist(mutants), 'any')
    return(mutants)
}

get_test_sigs <- function(killed=NULL) {
    if (! is.null(killed)) {
        sigs <- subset(test_sig_map, (Mutant %in% killed & subject$isDm == 1))
    } else {
        sigs <- subset(test_sig_map, subject$isDm == 1)
    }
    sigs <- sigs[,2:length(sigs) - 2]
    sigs[1] <- NULL
    rownames(sigs) <- NULL
    sigs <- apply(sigs, 1, paste, collapse=',')
    return(sigs)
}

format_results <- function(results) {
    max_length <- max(unlist(lapply(results, FUN=length)))
    results <- sapply(results, function(x) {length(x) <- max_length; return(x)})
    # results <- data.frame(work=rep(1:nrow(results), times=test_trials), score=c(results))
    mean_results <- apply(results, 1, mean, na.rm=TRUE)
    std_results <- apply(results, 1, sd, na.rm=TRUE)
    results <- data.frame(work=1:nrow(results), score=mean_results, std=std_results)
    return(results)
}

plot_work <- function(dom_scores) {
    pdf(file='../r_shiny/guided_work.pdf', pointsize=20, family='serif', width=8, height=5)
    t <- theme(axis.title=element_text(size=20),
              legend.text=element_text(size=20),
              legend.title=element_text(''),
              legend.position='top',
              legend.direction='horizontal',
              text=element_text(size=24))
    ggplot(data=dom_scores, aes(x=work, y=score)) +
        geom_line(color='dodgerblue') +
        geom_ribbon(aes(ymin=score - std, ymax=score + std), fill='dodgerblue', alpha=0.1) +
        coord_cartesian(ylim=c(0, 1)) +
        xlab('Work') + ylab('Test Completeness') + theme_bw() +
        scale_y_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) + t
}

guide_work <- function() {
    total_dom_nodes <- length(unique(get_test_sigs()))
    dom_scores <- list('list', test_trials)
    for (n in 1:test_trials) {
        live <- mutants[order(utility, decreasing=TRUE)]
        scores <- list()
        test_sigs <- list()
        num_dom_nodes <- 0
        while (length(live) > 0) {
            sample <- live[1]
            tests <- get_tests_for_mutant(sample)
            if (length(tests) > 0) {
                test <- sample(tests, 1)
                # test <- tests[1]
                killed <- get_mutants_for_test(test, live)
                intersection <- intersect(killed, live)
                live <- live[! live %in% intersection]
                add_sigs <- get_test_sigs(killed)
                test_sigs <- append(test_sigs, add_sigs)
                num_dom_nodes <- length(unique(test_sigs))
            } else {
                live <- live[-1]
            }
            score <- num_dom_nodes / total_dom_nodes
            scores <- append(scores, score)
        }
        scores <- as.numeric(unlist(scores))
        dom_scores[n] <- list(scores)
    }
    dom_scores <- format_results(dom_scores)
    plot_work(dom_scores)
}

guide_work()
