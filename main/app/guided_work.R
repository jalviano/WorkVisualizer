# guided_work.R

# Method to get tests that kill mutant
get_tests_for_mutant <- function(mutant, test_sig_map) {
    tests <- subset(test_sig_map, Mutant == mutant)
    tests <- as.data.frame(t(tests[-1]))
    colnames(tests) <- 'Result'
    tests <- subset(tests, Result == 'FAIL' | Result == 'EXC')
    tests <- rownames(tests)
    return(tests)
}

# Method to get mutants killed by test that are still live
get_mutants_for_test <- function(test, live, test_sig_map) {
    mutants <- subset(test_sig_map, (test_sig_map[test] == 'FAIL' | test_sig_map[test] == 'EXC') 
                      & Mutant %in% live, 'Mutant')
    mutants <- as.vector(unlist(mutants), 'any')
    return(mutants)
}

# Method to get test signatures of live mutants
get_test_sigs <- function(subject, test_sig_map, killed=NULL) {
    if (! is.null(killed)) {
        sigs <- subset(test_sig_map, Mutant %in% killed & subject$isDm == 1)
    } else {
        sigs <- subset(test_sig_map, subject$isDm == 1)
    }
    sigs <- sigs[,2:length(sigs) - 2]
    sigs[1] <- NULL
    rownames(sigs) <- NULL
    sigs <- apply(sigs, 1, paste, collapse=',')
    return(sigs)
}

# Method to format results for plotting
format_results <- function(results) {
    max_length <- max(unlist(lapply(results, FUN=length)))
    results <- sapply(results, function(x) {length(x) <- max_length; return(x)})
    mean_results <- apply(results, 1, mean, na.rm=TRUE)
    results <- cbind(results, mean_results)
    return(results)
}

# Method to run work simulation
work <- function(subject, test_sig_map, utility, mutants, test_trials) {
    # Get total number of dominator nodes
    total_dom_nodes <- length(unique(get_test_sigs(subject, test_sig_map)))
    dom_scores <- list('list', test_trials)
    # Run N trials of work
    for (n in 1:test_trials) {
        # Sort live mutants by ascending utility
        live <- mutants[order(utility, decreasing=TRUE)]
        scores <- list()
        test_sigs <- list()
        num_dom_nodes <- 0
        while (length(live) > 0) {
            # Select the live mutant with the highest utility
            sample <- live[1]
            tests <- get_tests_for_mutant(sample, test_sig_map)
            # Check if mutant is equivalent
            if (length(tests) > 0) {
                # Get test that kills the selected mutant using chosen test selection method
                test <- sample(tests, 1)
                # Get all live mutants killed by selected test
                killed <- get_mutants_for_test(test, live, test_sig_map)
                intersection <- intersect(killed, live)
                # Remove killed mutants from list of live mutants
                live <- live[! live %in% intersection]
                add_sigs <- get_test_sigs(subject, test_sig_map, killed)
                # Update number of dominator nodes killed so far
                test_sigs <- append(test_sigs, add_sigs)
                num_dom_nodes <- length(unique(test_sigs))
            } else {
                live <- live[-1]
            }
            # Compute dominator score
            score <- num_dom_nodes / total_dom_nodes
            scores <- append(scores, score)
        }
        scores <- as.numeric(unlist(scores))
        dom_scores[n] <- list(scores)
    }
    return(dom_scores)
}

# Main method to setup and run simulation of guided work
guide_work <- function(project_id, bug_id, clf, utility_function, test_trials) {
    # Get data
    data_dir <- paste('data/', project_id, '/', sep='')
    detailed_results <- read.csv(paste(data_dir, 'detailedResults.csv', sep=''))
    score_matrix <- paste(data_dir, bug_id, '/scoreMatrix.csv', sep='')
    subject = subset(detailed_results, bugId == bug_id & classifier == clf)
    # Get test signature map
    test_sig_map <- read.csv(score_matrix)
    # Get utilities and mutants
    utility <- utility_function(subject$probEq, subject$probTr, subject$probDm)
    mutants <- subject$mutantId
    # Run guided work simulation
    work(subject, test_sig_map, utility, mutants, test_trials)
}
