options(menu.graphics=FALSE)

#.after.Rupdate <- function() {
#    update.packages(checkBuilt = TRUE, ask = FALSE)
#}

.Last <- function() {
        if (!any(commandArgs()=='--no-readline') && interactive()){
                require(utils)
                try(savehistory())
        }
}

