get_aa_dict <- function() {
    return(list(A = "<SPAN style=\"color:#33cc00\">A</SPAN>",
                R = "<SPAN style=\"color:#cc0000\">R</SPAN>",
                N = "<SPAN style=\"color:#6600cc\">N</SPAN>",
                D = "<SPAN style=\"color:#0033ff\">D</SPAN>",
                C = "<SPAN style=\"color:#ffff00\">C</SPAN>",
                E = "<SPAN style=\"color:#0033ff\">E</SPAN>",
                Q = "<SPAN style=\"color:#6600cc\">Q</SPAN>",
                G = "<SPAN style=\"color:#33cc00\">G</SPAN>",
                H = "<SPAN style=\"color:#009900\">H</SPAN>",
                I = "<SPAN style=\"color:#33cc00\">I</SPAN>",
                L = "<SPAN style=\"color:#33cc00\">L</SPAN>",
                K = "<SPAN style=\"color:#cc0000\">K</SPAN>",
                M = "<SPAN style=\"color:#33cc00\">M</SPAN>",
                F = "<SPAN style=\"color:#009900\">F</SPAN>",
                P = "<SPAN style=\"color:#33cc00\">P</SPAN>",
                S = "<SPAN style=\"color:#0099ff\">S</SPAN>",
                T = "<SPAN style=\"color:#0099ff\">T</SPAN>",
                W = "<SPAN style=\"color:#009900\">W</SPAN>",
                Y = "<SPAN style=\"color:#009900\">Y</SPAN>",
                V = "<SPAN style=\"color:#33cc00\">V</SPAN>"))
}

read_in_dict <- function(char) {
    stopifnot(is.character(char) && length(char) == 1)
    aa_dict <- get_aa_dict()
    return(aa_dict[[char]])
}


aa2html <- function(aa_set)
    {
    stopifnot(class(aa_set) == "AAStringSet")
    aa_set <- as.character(aa_set)


    seq2html <- function(aa_string)
        {
        aa_vector <- strsplit(aa_string, split = "")[[1]]
        aa_html <- vapply(aa_vector, read_in_dict, character(1))
        aa_html <- paste(aa_html, collapse = "")
        return(aa_html)
    }
    aa_html <- vapply(aa_set,
                      FUN = seq2html,
                      FUN.VALUE = character(1),
                      USE.NAMES = TRUE)

    max_seq <- max(vapply(aa_set, nchar, numeric(1)))
    counter <- rep(" ", max_seq)
    for(i in 1:length(counter)) {
        if(i %% 10 == 0) {
            counter[i] <- "."
        }
        if(i %% 100 == 0) {
            counter[i] <- ":"
        }
    }
    counter <- paste(counter, collapse = "")

    max_names <- max(vapply(names(aa_html), nchar, numeric(1)))
    length_names <- max_names + 5
    upstring <- paste(paste(rep(" ",
                                length_names),
                            collapse = ""),
                      counter,
                      sep = "")

    for(i in 1:length(aa_html)) {
        name_in <- names(aa_html)[i]
        nspaces <- length_names - nchar(name_in)
        spaces <- paste(rep(" ", nspaces), collapse = "")
        name_in <- paste0(name_in, spaces)
        aa_html[i] <- paste0(name_in, aa_html[i])
    }


    out_seq <- paste(aa_html, collapse = "<br>")
    out_text <- paste(upstring, out_seq, sep = "<br>")
    out_text <- paste("<pre>", out_text, "</pre>", sep = "")
    return(out_text)
}

