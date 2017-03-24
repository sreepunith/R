extract <- function(fit = NULL, field = NULL, field_idx = NULL) {
  filtered_output <- fit$pred[fit$pred[field] == field_idx,]
  
  roc_dt <- list()
  k <- 1
  for (resample in unique(filtered_output$Resample)) {
    roc_dt$predictions[[k]] <- apply(filtered_output[filtered_output$Resample == resample, 3:4], 1, max)
    roc_dt$labels[[k]] <- filtered_output$obs[filtered_output$Resample == resample]
    k <- k + 1
  }
  
  return (roc_dt)
}
