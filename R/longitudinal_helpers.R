# helper functions

longitudinal_continuous <- function(dataset, outcome){
  # fit full model: outcome ~ time + Tx + time*Tx + (1|subj) + (1|litter)

  # get type 3 p-values of interaction

  # if sig -> post hoc

  # if not sig -> main effects: outcome ~ time + Tx + (1|subj) + (1|litter)

  # type 3 p-val for Tx

  # if sig -> post hoc

}

longitudinal_binary <- function(dataset, outcome){
  # fit full model: outcome ~ time + Tx + time*Tx + (1|subj) + (1|litter)

  # get type 3 p-values of interaction

  # if sig -> post hoc

  # if not sig -> main effects: outcome ~ time + Tx + (1|subj) + (1|litter)

  # type 3 p-val for Tx

  # if sig -> post hoc
}

