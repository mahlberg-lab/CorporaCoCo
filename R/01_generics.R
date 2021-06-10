# generic functions
corp_get_text <- function(obj) UseMethod("corp_get_text")
corp_get_text_obj <- function(obj) UseMethod("corp_get_text_obj")
corp_get_tokens <- function(obj) UseMethod("corp_get_tokens")
corp_get_counts <- function(obj) UseMethod("corp_get_counts")
corp_get_metadata <- function(obj) UseMethod("corp_get_metadata")

corp_set_tokens <- function(obj, tokens) UseMethod("corp_set_tokens")
corp_set_counts <- function(obj) UseMethod("corp_set_counts")

corp_type_lookup <- function(obj) UseMethod("corp_type_lookup")
