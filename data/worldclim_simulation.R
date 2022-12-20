delayedAssign("worldclim_simulation", local({
  requireNamespace("sf", quietly = TRUE)
  waywiser:::worldclim_simulation
}))
