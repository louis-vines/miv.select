#' @export
auc <- function(predicted, observed){
  predicted <- as.vector(predicted)
  ROCR::performance(ROCR::prediction(predicted, observed),"auc")@y.values[[1]]
}


#' @export
roc_curve <- function(predicted, observed){
  predicted <- as.vector(predicted)
  auc <- ROCR::performance(ROCR::prediction(predicted, observed),"auc")@y.values[[1]]
  fpr_vs_tpr <- ROCR::performance(ROCR::prediction(predicted, observed),"tpr","fpr")

  data_frame(tpr=fpr_vs_tpr@x.values[[1]], fpr=fpr_vs_tpr@y.values[[1]]) %>%
    ggplot(aes(tpr, fpr)) +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, linetype='dashed', alpha = 0.5) +
    ggtitle(stringr::str_c('AUC: ', round(auc, 4)))
}


#' @export
plot_gam_feature <- function(model, feature_name, train_data){
  gam_plot <- plotGAM(candidate_model, smooth.cov = feature_name)

  if (missing(train_data)) {
    return(gam_plot)
  }

  y_mean <- mean(ggplot_build(gam_plot)$data[[1]]$y)

  gam_plot +
    geom_rug(aes(!!as.symbol(feature_name), y=y_mean), data = train_data,
             alpha = 0.2, position='jitter', sides='b')
}
