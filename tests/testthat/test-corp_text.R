test_that("main", {
    x <- "A man, a plan, a canal -- Panama!"
    got <- corp_text(x)
    expect_equal(is.corp_text(got), TRUE)

    expect_equal(corp_get_text(got), x)

    tokens <- corp_get_tokens(got)
    expect_equal(is.data.table(tokens), TRUE)
    expect_equal(all(c("idx", "type", "start", "end", "token") %in% names(tokens)), TRUE)
    expect_equal(length(names(tokens)), 5)

    metadata <- corp_get_metadata(got)
    expect_equal(all(c("PACKAGE_VERSION", "DATE") %in% names(metadata)), TRUE)
    expect_equal(length(names(metadata)), 2)

    lookup <- corp_type_lookup(got)
    expect_equal(is.data.table(lookup), TRUE)
    expect_equal(nrow(lookup), 5)
})

test_that("corp_text_rbindlist", {
    aa <- "The cat sat on the mat."
    bb <- "This dog ate this cat."
    cc <- "That badger has a funny face."
    aaa <- corp_text(aa)
    bbb <- corp_text(bb)
    ccc <- corp_text(cc)
    ddd <- list(aaa, bbb, ccc)
    got <- corp_text_rbindlist(ddd)
    expect_equal(corp_get_text(got), "The cat sat on the mat. This dog ate this cat. That badger has a funny face.")
    tokens <- corp_get_tokens(got)
    expect_equal(tokens[type == "badger", "start"]$start, 53)
    expect_equal(tokens[type == "badger", "end"]$end, 58)
})

test_that("corp_text_rbindlist-real-example", {
    # Replicate an example of how we use this
    text <- 'Mr. Sherlock Holmes, who was usually very late in the mornings, save upon those not infrequent occasions when he was up all night, was seated at the breakfast table. I stood upon the hearth-rug and picked up the stick which our visitor had left behind him the night before. It was a fine, thick piece of wood, bulbous-headed, of the sort which is known as a "Penang lawyer." Just under the head was a broad silver band nearly an inch across. "To James Mortimer, M.R.C.S., from his friends of the C.C.H.," was engraved upon it, with the date "1884." It was just such a stick as the old-fashioned family practitioner used to carry--dignified, solid, and reassuring.

"Well, Watson, what do you make of it?"

Holmes was sitting with his back to me, and I had given him no sign of my occupation.

"How did you know what I was doing? I believe you have eyes in the back of your head."

"I have, at least, a well-polished, silver-plated coffee-pot in front of me," said he. "But, tell me, Watson, what do you make of our visitor\'s stick? Since we have been so unfortunate as to miss him and have no notion of his errand, this accidental souvenir becomes of importance. Let me hear you reconstruct the man by an examination of it."'

    paragraphs <- strsplit(text, "\n\n")

    boundary_marker <- " _b_ _b_ _b_ _b_ _b_ "  # 5 x _b_

    corp_list <- list()
    i <- 1
    for (para in paragraphs[[1]]) {
        # Make a new sep object each time
        sep <- CorporaCoCo::corp_text(boundary_marker, tokens = NULL)
        expect_true(is.corp_text(sep))
        for (i in 1:5) {
            sep$tokens[i]$type = NA
        }
        #print(sep, as_data_table=TRUE)
        expect_equal(sep$tokens[1]$type, "NA")
        expect_equal(sep$tokens[4]$type, "NA")

        # Now append a corp_text object for the paragraph, and our new sep object
        corp_list <- rlist::list.append(corp_list, CorporaCoCo::corp_text(para, tokens = NULL), sep)
    }
    expect_length(corp_list, 10)  # 5 paras each followed by a sep (boundary marker)
    expect_true(is.corp_text(corp_list[[1]]))
    expect_true(is.data.table(corp_list[[1]]$tokens))
    expect_equal(corp_list[[1]]$tokens[2][, token], "Sherlock")
    expect_equal(corp_list[[9]]$token[10][, token], "coffee")
    expect_equal(corp_list[[9]]$token[11][, token], "pot")

    combined <- corp_text_rbindlist(corp_list)
    expect_true(is.corp_text(combined))

    expect_equal(corp_get_text(combined), "Mr. Sherlock Holmes, who was usually very late in the mornings, save upon those not infrequent occasions when he was up all night, was seated at the breakfast table. I stood upon the hearth-rug and picked up the stick which our visitor had left behind him the night before. It was a fine, thick piece of wood, bulbous-headed, of the sort which is known as a \"Penang lawyer.\" Just under the head was a broad silver band nearly an inch across. \"To James Mortimer, M.R.C.S., from his friends of the C.C.H.,\" was engraved upon it, with the date \"1884.\" It was just such a stick as the old-fashioned family practitioner used to carry--dignified, solid, and reassuring.  _b_ _b_ _b_ _b_ _b_  \"Well, Watson, what do you make of it?\"  _b_ _b_ _b_ _b_ _b_  Holmes was sitting with his back to me, and I had given him no sign of my occupation.  _b_ _b_ _b_ _b_ _b_  \"How did you know what I was doing? I believe you have eyes in the back of your head.\"  _b_ _b_ _b_ _b_ _b_  \"I have, at least, a well-polished, silver-plated coffee-pot in front of me,\" said he. \"But, tell me, Watson, what do you make of our visitor's stick? Since we have been so unfortunate as to miss him and have no notion of his errand, this accidental souvenir becomes of importance. Let me hear you reconstruct the man by an examination of it.\"  _b_ _b_ _b_ _b_ _b_ ")
    tokens <- corp_get_tokens(combined)

    # Check the real text
    #print(tokens, as_data_table=TRUE)
    expect_equal(tokens[token == "Penang", "start"]$start, 360)
    expect_equal(tokens[type == "penang", "start"]$start, 360)
    expect_equal(tokens[token == "Penang", "end"]$end, 365)
    expect_equal(tokens[type == "penang", "end"]$end, 365)

    # This token appears twice...
    expect_equal(tokens[token == "Holmes", "start"]$start, c(14, 749))

    # This token appears four times...
    expect_equal(tokens[token == "and", "start"]$start, c(195, 649, 789, 1166))

    # Now check the boundary markers
    expect_equal(tokens[token == "occupation", "end"]$end, 832)  #  immediately before a boundary
    expect_equal(tokens[token == "Well", "start"]$start, 688)  #  immediately after a boundary
    expect_equal(tokens[token == "_b_", "start"]$start, c(666, 670, 674, 678, 682, 728, 732, 736, 740, 744, 836, 840, 844, 848, 852, 945, 949, 953, 957, 961, 1311, 1315, 1319, 1323, 1327))
    expect_equal(tokens[token == "_b_", "end"]$end, c(668, 672, 676, 680, 684, 730, 734, 738, 742, 746, 838, 842, 846, 850, 854, 947, 951, 955, 959, 963, 1313, 1317, 1321, 1325, 1329))
    expect_equal(corp_list[[1]]$tokens[2]$token, "Sherlock")
    expect_equal(corp_list[[1]]$tokens[50]$type, "before")
    expect_equal(corp_list[[1]]$tokens[100]$type, "date")
    expect_equal(corp_list[[1]]$tokens[251]$type, "NA")
})
