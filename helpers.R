
cache <- function(id, computation, reload = FALSE) {
    if (reload | !file.exists(id)) {
        res <- computation
        saveRDS(object = res, file = id)
        return(res)
    } else {
        readRDS(file = id)
    }
}

fread_cache <- function(input, reload = FALSE, ...) {
    require(data.table)
    input_cache <- paste0(input, ".rds")
    cache(input_cache, fread(input, ...), reload)
}

gen_labels <- function(experiments) {
    dimensions <- experiments %>%
        select(-time, -timestamp, -host, -filename, -commit,
               -n_switches, -n_rotor, -n_cache, -n_xpand, -type) %>%
        reshape2::melt(id.vars = "uuid", factorAsStrings = TRUE) %>%
        group_by(variable) %>%
            summarize(n_distinct = n_distinct(value)) %>%
            ungroup() %>%
        filter(n_distinct > 1) %>%
        .[["variable"]] %>%
        as.character()

    labels <- experiments[c("uuid", dimensions)] %>%
        reshape2::melt(id.vars = "uuid") %>%
        group_by(uuid) %>%
            arrange(variable) %>%
            summarize(label = paste(variable, value, sep = ": ", collapse = " ")) %>%
            ungroup()

    experiments %>%
        left_join(labels, by = "uuid")
}

