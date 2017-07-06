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
    seq2html <- function(aa_string)
        {
        aa_vector <- strsplit(aa_string, split = "")[[1]]
        aa_html <- vapply(aa_vector, read_in_dict, character(1))
        aa_html <- paste(aa_html, collapse = "")
        return(aa_html)
    }

    aa_string <- as.character(aa_set[[1]])
    aa_html <- seq2html(aa_string = aa_string)
    aa_html <- paste(names(aa_set)[[1]], aa_html, sep = " ")

    counter <- rep(" ", nchar(aa_string))
    for(i in 1:length(counter)) {
        if(i %% 10 == 0) {
            counter[i] <- "."
        }
        if(i %% 100 == 0) {
            counter[i] <- ":"
        }
    }
    counter <- paste(counter, collapse = "")
    upstring <- paste(paste(rep(" ",
                                nchar(names(aa_set)[[1]]) + 1),
                            collapse = ""),
                      counter,
                      sep = "")


    out_text <- paste(upstring, aa_html, sep = "<br>")
    out_text <- paste("<pre>", out_text, "</pre>", sep = "")
    return(out_text)
}

