Heatmap <- setRefClass("Heatmap",

    contains = "Chart",

    methods = list(

        get_params = function(){
            callSuper()

            params$palette <<- validate_palette(params$palette)

            params$color_domain <<- validate_color_domain(params$color_domain)
        },

        get_data = function(){
            data <<- params$data

            if (is.matrix(data)){
                data <<- as.data.frame(data, stringsAsFactors = FALSE)
            }

            rownames(data) <<- params$row_names
            colnames(data) <<- params$col_names

            # save colnames before adding extra columns (ex. color_groups, order_by)
            col_names <- colnames(data)

            # we only create data$color_groups when params$color_groups is not null
            # if (!is.null(params$color_groups)){
            #     data <- reorder_data_by_color_groups(data, params)
            # }

            data <- format_heatmap_data(data, col_names)

            data
        },

        get_col_values = function(data, col_names){
            col_values <- unname(apply(data, 1, function(row){
                row_values <- lapply(1:length(row), function(row_index){
                    list(cell_value = unname(row[row_index]))
                })
            }))

            col_values
        },

        # The heatmap data structure is not as straightforward as the points data structure because
        # it has overlapping definitions: col_group[col_values[row_values[cell_values]], col_group_name, col_names, row_group_names]
        format_heatmap_data = function(data, col_names) {
            if (is.null(params$col_groups)){
                col_values <- get_col_values(data, col_names)
                col_group_list <- list(
                   col_values = col_values,
                   col_names = col_names
                )
                if (!is.null(params$row_groups)){
                    col_group_list$row_group_names <- params$row_groups
                }
                browser()
                formatted_data <- list(col_group_list)
            } else {
                if (!is.factor(params$col_groups)){
                    params$col_groups <<- factor(params$col_groups)
                }
                formatted_data <- lapply(levels(params$col_groups), function(col_group){
                    col_names <- col_names[which(params$col_groups == col_group)]
                    data_col_group <- data[, which(params$col_groups == col_group), drop = FALSE]
                    col_values <- get_col_values(data_col_group, col_names)
                    col_group_list <- list(
                        col_values = col_values,
                        col_names = col_names,
                        col_group_name = col_group
                    )

                    if (!is.null(params$row_groups)){
                        col_group_list$row_group_names <- params$row_groups
                    }

                    col_group_list
                })
            }

            list(formatted = formatted_data,
                 unformatted = data)
        }

    )
)

clickme_helper$heatmap <- function(x,...){
    params <- list(x = x, ...)
    heatmap <- Heatmap$new(params)

    heatmap$display()
}




