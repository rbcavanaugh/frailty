
#' @title OMOP to FI
#'
#' @description Generates table of condition occurances from an OMOP database for
#' the EFI, EGRAGICAP, VAFI, and HFRS frailty indices.
#' This requires a database connection using dbConnect() or similar R package
#' The connection should be able to access the following OMOP CDM tables
#'
#' * concept
#' * condition_occurrence
#' * observation
#' * procedure_occurrence
#' * device_exposure
#' @md
#'
#' @param con a database connectino using dbConnect() or similar
#' @param index chr vector; a frailty index. One of "efi", "efragicap", "vafi", or "hfrs"
#' @param schema chr vector: a character vector of the schema holding the table. defaults to NULL (no schema)
#' @param collect log; should the query be collected at the end or kept as an SQL query? This must be TRUE for bigquery
#' because bigquery does not permit temporary tables.
#' @param .data_search sql; An SQL query from the same connection source with a column for person_id, start_date, and end_date.
#' @param search_person_id chr; The name of the person_id column
#' @param search_start_date chr/date; the name of the start_date column
#' @param search_end_date chr/date; the name of the end_date column
#' @param keep_columns chr vector: any additional columns to keep in the final dataframe
#' @param unique_categories log; Should the result return just the distinct FI category occurances in the interval
#' all concept_ids and concepts taht occcur in the interval for each person (much larger result)
#' @param concept_location pointer to a table that holds the concepts for the FI. Used when collect = FALSE.
#' I recommend pre-uploading the aouFI::fi_indices table to your schema in the database. If the database
#' is small enough, you can instead set collect to TRUE and concept_location to NA to use a local table from aouFI.
#' @param join_now whether to join back tot he concepts table at the end of the query
#' @param join_to_concept whether to join to the concept table at the beginning of the query (default) or filter for the concept_id column. Use filter only if a tbl is not accessible.
#' @param dbms one of redshift, bigquery etc.
#' @param chronic_lookback defaults to 3 for 3 years for chronic conditions
#' @param pp_date_minimum minimum drug era date  for polypharmacy (inclusive; 2 means drug_era >= 2 days)
#' @param pp_cutoff number of ingredients to designate poly-pharmacy (inclusive; 10 means 10 or more ingredients)
#'
#' @return dataframe with EFI occurences that can be summarized
#' @export
#'
#' @examples
#' # For All of Us, set collect = TRUE because no temp tables
#' omop2fi(con = con,
#'         index = "efi",
#'         collect = TRUE,
#'         .data_search = demo2,
#'         search_person_id = "person_id",
#'         search_start_date = "survey_date",

#'         search_end_date = "end_date",
#'         keep_columns = c("dob", "sex_at_birth"),
#'         unique_categories = TRUE,
#'         concept_location = NA
#' )
#'
#' # Otherwise, we can tell the function where to look for the concepts
#' omop2fi(con = con,
#'         index = "efi",
#'         collect = FALASE,
#'         .data_search = demo2,
#'         search_person_id = "person_id",
#'         search_start_date = "survey_date",
#'         search_end_date = "end_date",
#'         keep_columns = c("dob", "sex_at_birth"),
#'         unique_categories = TRUE,
#'         concept_location = tbl(con, inDatabaseSchema(my_schema, "efi")
#' )
omop2fi_lb <- function(con,
                       index,
                       schema = NULL,
                       collect = FALSE,
                       .data_search,
                       search_person_id,
                       search_start_date,
                       search_end_date,
                       keep_columns = NULL,
                       unique_categories = FALSE,
                       join_now = TRUE,
                       join_to_concept = TRUE,
                       dbms = "bigquery",
                       acute_lookback = 1,
                       chronic_lookback = 1,
                       concept_location
){



    keep_cols <- {{keep_columns}}

    if(!is.null(schema)){

        if(!is.character(schema)){stop("schema must be a character vector")}

        concept                 = inDatabaseSchema(schema, "concept")
        condition_occurrence    = inDatabaseSchema(schema, "condition_occurrence")
        observation             = inDatabaseSchema(schema, "observation")
        procedure_occurrence    = inDatabaseSchema(schema, "procedure_occurrence")
        device_exposure         = inDatabaseSchema(schema, "device_exposure")
        person                  = inDatabaseSchema(schema, "person")

    } else {
        concept                 = "concept"
        condition_occurrence    = "condition_occurrence"
        observation             = "observation"
        procedure_occurrence    = "procedure_occurrence"
        device_exposure         = "device_exposure"
        person                  = "person"
    }


    interval = "YEAR"
    date = "person_start_date"
    chronic_adjustment = -(chronic_lookback-1)
    acute_adjustment = -(acute_lookback-1)
    sql_chronic <- switch (dbms,
                           "redshift" = glue::glue("DATEADD({interval}, {chronic_adjustment}, {date})"),
                           #"oracle" = glue::glue("({date} + NUMTODSINTERVAL({chronic_adjustment}, 'day'))"),
                           #"postgresql" = glue::glue("({date} + {chronic_adjustment}*INTERVAL'1 {interval}')"),
                           #"sql server" = glue::glue("DATEADD({interval}, {chronic_adjustment}, {date})"),
                           #"spark" = glue::glue("date_add({date}, {chronic_adjustment})"),
                           #"duckdb" = glue::glue("({date} + {chronic_adjustment}*INTERVAL'1 {interval}')"),
                           #"sqlite" = glue::glue("CAST(STRFTIME('%s', DATETIME({date}, 'unixepoch', ({chronic_adjustment})||' {interval}s')) AS REAL)"),
                           "bigquery" = glue::glue("DATE_ADD({date}, INTERVAL {chronic_adjustment} {toupper(interval)})"),
                           #"snowflake" = glue::glue('DATEADD({interval}, {chronic_adjustment}, {date})'),
                           rlang::abort(glue::glue("Connection type {paste(class(dot$src$con), collapse = ', ')} is not supported!"))
    )
    sql_acute <- switch (dbms,
                         "redshift" = glue::glue("DATEADD({interval}, {acute_adjustment}, {date})"),
                         #"oracle" = glue::glue("({date} + NUMTODSINTERVAL({acute_adjustment}, 'day'))"),
                         #"postgresql" = glue::glue("({date} + {acute_adjustment}*INTERVAL'1 {interval}')"),
                         #"sql server" = glue::glue("DATEADD({interval}, {acute_adjustment}, {date})"),
                         #"spark" = glue::glue("date_add({date}, {acute_adjustment})"),
                         #"duckdb" = glue::glue("({date} + {acute_adjustment}*INTERVAL'1 {interval}')"),
                         #"sqlite" = glue::glue("CAST(STRFTIME('%s', DATETIME({date}, 'unixepoch', ({acute_adjustment})||' {interval}s')) AS REAL)"),
                         "bigquery" = glue::glue("DATE_ADD({date}, INTERVAL {acute_adjustment} {toupper(interval)})"),
                         #"snowflake" = glue::glue('DATEADD({interval}, {acute_adjustment}, {date})'),
                         rlang::abort(glue::glue("Connection type {paste(class(dot$src$con), collapse = ', ')} is not supported!"))
    )
    acute_date_sql = ifelse(acute_lookback == 1, "person_start_date", as.character(sql_acute))
    chronic_date_sql = ifelse(chronic_lookback == 1, "person_start_date", as.character(sql_chronic))

    # if(!("person_id" %in% colnames(eligible))){stop("eligible must contain person_id column")}

    index_ = tolower(index)

    if(!(index_ %in% c("efi", "efragicap", "hfrs", "vafi", "efi2", "efi_sno_expanded"))){
        stop("oops! frailty index not found; index must be one of: efi, efragicap, hfrs, or vafi.")
    }


    pid = .data_search |>
        dplyr::select(person_id = !!search_person_id,
                      person_start_date = !!search_start_date,
                      person_end_date = !!search_end_date,
                      !!!keep_cols) |>
        mutate(person_start_date = as.Date(person_start_date),
               person_end_date = as.Date(person_end_date))


    if(isTRUE(collect)){
       # concept_table = aouFI::fi_indices |> filter(fi == index)
       # categories_concepts <- aouFI::fi_indices |> filter(fi == index)
        stop("no more collecting")

    } else {
        # if(!DBI::isdb)
        # generalizing the lookback so that whatever you put in the function arguments will be implemented.
        concept_table = concept_location
        categories_concepts = concept_location
    }

    message(glue::glue("retrieving {index} concepts..."))

    # gets a dataframe with columns of category (general description of the FI category)
    # and concept_id which is an OMOP concept ID for the FI category. FI categories can
    # be repeated with different concept IDs, but there should be no concept_id duplicates.
    # If the index is hfrs, there is an additional column called 'hfrs_score' which
    # holds the point value for the hfrs concept.
    # This function is documented in the package and pulls from data sources we generated
    # using the AoU tables. Code for generating these tables can be made available.


    message("joining full concept id list...")

    # get full list of concepts from the concept table
    # originally, this pulled from the All of Us cb_cohort and other
    # tables specific to all of us. Switching to the general concept table
    # made very little, except for hfrs. There were 51 additional concepts in the
    # concept table that were not in the AoU table. We're still not sure what the
    # difference is, but perhaps related to the is_selectable aspect of AoU...

    if(isTRUE(join_to_concept)){


        condition_concept_ids <- tbl(con, concept) |>
            filter(standard_concept == "S") |>
            distinct(concept_id, name = concept_name) |>
            inner_join(concept_table |> distinct(concept_id, chronic_category),
                       by = c("concept_id"), x_as = "c1", y_as = "c2" )

    } else {

        condition_concept_ids <- tbl(con, concept) |>
            filter(standard_concept == "S") |>
            distinct(concept_id, name = concept_name) |>
            filter(concept_id %in% !!unique(categories_concepts$concept_id))

    }


    message("searching for condition occurrences...")

    # The following four calls go find the presence of the concept IDs in the
    # condition occurrence, procedure, observation, and device tabes, limiting
    # the search to the person_ids and concepts in teh above condition_concept_ids
    # table. They also calculate a start year and month which are important for
    # later analyses that are dependent on when the FI event occurs.

    # go find instances of our concepts in the condition occurrence table
    cond_occurrences <- tbl(con, condition_occurrence) |>
        inner_join(pid, by = "person_id", x_as = "x1", y_as = "y1") |>
        inner_join(condition_concept_ids, by = c("condition_concept_id" = "concept_id"), x_as = "x2", y_as = "y2") |>
        select(person_id, !!!keep_cols,
               concept_id = condition_concept_id,
               concept_name = name,
               start_date = condition_start_date,
               person_start_date,
               person_end_date,
               chronic_category
        ) |>
        mutate(person_start_date = ifelse(
            chronic_category == 3, dplyr::sql(!!chronic_date_sql), dplyr::sql(!!acute_date_sql))
        )|>
        filter(start_date >= person_start_date, start_date <= person_end_date) |>
        distinct()

    message("searching for observations...")

    # do the same for the observation table
    obs <- tbl(con, observation)  |>
        inner_join(pid, by = "person_id", x_as = "x3", y_as = "y3") |>
        inner_join(condition_concept_ids, by = c("observation_concept_id" = "concept_id"), x_as = "x4", y_as = "y4") |>
        select(person_id, !!!keep_cols,
               concept_id = observation_concept_id,
               concept_name = name,
               start_date = observation_date,
               person_start_date,
               person_end_date,
               chronic_category
        ) |>
        mutate(person_start_date = ifelse(
            chronic_category == 3, dplyr::sql(!!chronic_date_sql), dplyr::sql(!!acute_date_sql))
        )|>
        filter(start_date >= person_start_date, start_date <= person_end_date) |>
        distinct()

    message("searching for procedures...")

    # procedure table
    proc <- tbl(con, procedure_occurrence) |>
        inner_join(pid, by = "person_id", x_as = "x5", y_as = "y5") |>
        inner_join(condition_concept_ids, by = c("procedure_concept_id" = "concept_id"), x_as = "x6", y_as = "y6") |>
        select(person_id, !!!keep_cols,
               concept_id = procedure_concept_id,
               concept_name = name,
               start_date = procedure_date,
               person_start_date,
               person_end_date,
               chronic_category
        ) |>
        mutate(person_start_date = ifelse(
            chronic_category == 3, dplyr::sql(!!chronic_date_sql), dplyr::sql(!!acute_date_sql))
        )|>
        filter(start_date >= person_start_date, start_date <= person_end_date) |>
        distinct()


    message("searching for device exposures...")

    # device exposure
    dev <- tbl(con, device_exposure) |>
        inner_join(pid, by = "person_id", x_as = "x7", y_as = "y7") |>
        inner_join(condition_concept_ids, by = c("device_concept_id" = "concept_id"), x_as = "x8", y_as = "y8") |>
        select(person_id, !!!keep_cols,
               concept_id = device_concept_id,
               concept_name = name,
               start_date = device_exposure_start_date,
               person_start_date,
               person_end_date,
               chronic_category
        ) |>
        mutate(person_start_date = ifelse(
            chronic_category == 3, dplyr::sql(!!chronic_date_sql), dplyr::sql(!!acute_date_sql))
        )|>
        filter(start_date >= person_start_date, start_date <= person_end_date) |>
        distinct()

    # polypharmacy


    # print(head(obs))


    message("putting it all together union...")



    # put them all together, add the fi labels back
    dat <-
        union_all(cond_occurrences, obs) %>% union_all(dev) %>% union_all(proc)

    #  print(head(dat))


    # Logic to determine whether a collected df or just a query should be returned.
    # note that copying if false can still take some time...
    if(isTRUE(collect)){
        message("collecting...")
        if(isTRUE(unique_categories)){

            dat <- dat  |>
                select(person_id,
                       !!!keep_columns,
                       person_start_date,
                       person_end_date,
                       concept_id) |>
                distinct()  |>
                collect() |>
                left_join(categories_concepts, by = c("concept_id"))

        } else {

            dat <- dat %>%
                select(person_id,
                       !!!keep_columns,
                       person_start_date,
                       person_end_date,
                       concept_id)  |>
                collect() |>
                left_join(categories_concepts, by = c("concept_id"))
        }

        message(glue::glue("success! retrieved {nrow(dat)} records."))
    } else {

        #if(!dbExistsTable(con, concept_table)){stop("Please provide a database table with the FI concepts")}

        if(isTRUE(join_now)){

            message("Joining to FI table")
            dat <- dat |>
                left_join(concept_table,
                          by = c("concept_id"),
                          x_as = "x10", y_as = "y10") |>
                select(person_id,
                       !!!keep_columns,
                       person_start_date,
                       person_end_date,
                       category,
                       concept_id,
                       score)

            if(isTRUE(unique_categories)){dat <- dat |> select(-concept_id) |>  distinct()}


        }


        message(glue::glue("success! SQL query from dbplyr returned"))
    }


    return(dat)
}

cat("`omop2fi_lb()` retrieved successfully")


