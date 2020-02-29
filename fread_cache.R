require(data.table)

cache <- function(id, computation, reload = FALSE) {
    if (reload | !file.exists(id)) {
        res <- computation
        saveRDS(object = res, file = id)
        return(res)
    } else {
        readRDS(file = id)
    }
}

fread_cache <- function(input, ...) {
    input_cache <- paste0(input, ".rds")
    cache(input_cache, fread(input, ...))
}


f <- function(fn) {
    print(fn)
    if (!file.exists(paste0(fn, ".rds"))) {
        fread_cache(fn, skip = "flow_id")
    }
    if (file.exists(paste0(fn, ".rds"))) {
        if (file.exists(paste0("headers-", fn))) {
            file.remove(fn)
        } else {
            print("no headers")
        }
    }

}

fns <- sort(list.files(path = "~/rotorsim/data",
                       pattern = "^done-[0-9a-f-]*\\.csv$",
                       full.names = TRUE))
for (fn in fns) {f(fn)}
