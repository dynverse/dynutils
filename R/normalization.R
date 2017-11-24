#' State-of-the-art preprocessing and normalization from https://f1000research.com/articles/5-2122/v2
#' @param counts The counts matrix, with genes in columns
#' @param has_spike Does this contain spike-ins, for which the gene names are preseded by ERCC
#' @param verbose Whether to add plots
#' @importFrom scater newSCESet calculateQCMetrics isOutlier normalize plotExplanatoryVariables plotExpression plotPCA plotQC nexprs
#' @importFrom SingleCellExperiment isSpike
#' @importFrom BiocGenerics counts sizeFactors
#' @importFrom Biobase pData
#' @importFrom scran computeSumFactors computeSpikeFactors trendVar decomposeVar
#' @importFrom grDevices recordPlot graphics.off
#' @export
normalize_filter_counts <- function(counts, has_spike=any(grepl("^ERCC", colnames(counts))), verbose = TRUE) {
  normalization_plots <- list()
  requireNamespace("ggplot2")

  sce <- scater::newSCESet(countData=t(counts))

  feature_controls <- list(Mt = grepl("^mt-", rownames(sce)))
  if (has_spike) {
    feature_controls$ERCC <- grepl("^ERCC", rownames(sce))
  }

  sce <- scater::calculateQCMetrics(sce, feature_controls = feature_controls)

  if (has_spike) {
    SingleCellExperiment::isSpike(sce) <- "ERCC"
  }

  if (verbose) {
    par(mfrow=c(1,2))
    hist(sce$total_counts/1e6, xlab="Library sizes (millions)", main="",
         breaks=20, col="grey80", ylab="Number of cells")
    hist(sce$total_features, xlab="Number of expressed genes", main="",
         breaks=20, col="grey80", ylab="Number of cells")
    par(mfrow=c(1, 1))
    normalization_plots$library <- grDevices::recordPlot()
  }

  libsize_drop <- scater::isOutlier(sce$total_counts, nmads=3, type="lower", log=TRUE)
  feature_drop <- scater::isOutlier(sce$total_features, nmads=3, type="lower", log=TRUE)

  if (verbose) {
    par(mfrow=c(1,2))
    hist(sce$pct_counts_feature_controls_Mt, xlab="Mitochondrial proportion (%)",
         ylab="Number of cells", breaks=20, main="", col="grey80")
    if (has_spike) hist(sce$pct_counts_feature_controls_ERCC, xlab="ERCC proportion (%)",
                        ylab="Number of cells", breaks=20, main="", col="grey80")
    par(mfrow=c(1, 1))
    normalization_plots$cell_quality <- grDevices::recordPlot()
  }

  nmads <- 3
  mito_drop <- scater::isOutlier(sce$pct_counts_feature_controls_Mt, nmads=nmads, type="higher")
  spike_drop <- rep(FALSE, length(mito_drop))
  if (has_spike) {
    spike_drop <- scater::isOutlier(sce$pct_counts_feature_controls_ERCC, nmads=nmads, type="higher")
  }

  sce_cell_filtered <- sce[,!(libsize_drop | feature_drop | mito_drop | spike_drop)]


  ########################################

  if (verbose) {
    fontsize <- theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=16)
    )

    normalization_plots$pca <-
      BiocGenerics::plotPCA(sce, pca_data_input="pdata") +
      fontsize
  }

  ave_counts <- rowMeans(BiocGenerics::counts(sce_cell_filtered))
  keep <- ave_counts >= 1

  if (verbose) {
    hist(log10(ave_counts), breaks=100, main="", col="grey80",
         xlab=expression(Log[10]~"average count"))
    abline(v=log10(1), col="blue", lwd=2, lty=2)
    normalization_plots$initial_gene_filter <- grDevices::recordPlot()
    print(scater::plotQC(sce_cell_filtered, type = "highest-expression", n=50) + fontsize)

    normalization_plots$top_genes_qc <- grDevices::recordPlot()
  }

  numcells <- scater::nexprs(sce_cell_filtered, byrow=TRUE)
  alt_keep <- numcells >= 10

  if (verbose) {
    smoothScatter(log10(ave_counts), numcells, xlab=expression(Log[10]~"average count"), ylab="Number of expressing cells")
    if (has_spike) {
      is_ercc <- SingleCellExperiment::isSpike(sce_cell_filtered, type="ERCC")
      points(log10(ave_counts[is_ercc]), numcells[is_ercc], col="red", pch=16, cex=0.5)
    }

    normalization_plots$cell_filtering <- grDevices::recordPlot()
  }

  sce_cellgene_filtered <- sce_cell_filtered[keep,]
  ########################################

  sce_cellgene_filtered <- scran::computeSumFactors(sce_cellgene_filtered, sizes=c(20, 40, 60, 80))

  if (verbose) {
    plot(sizeFactors(sce_cellgene_filtered), sce_cellgene_filtered$total_counts/1e6, log="xy",
         ylab="Library size (millions)", xlab="Size factor")
    normalization_plots$size_factor <- grDevices::recordPlot()
  }

  if(has_spike) {
    sce_cellgene_filtered <- scran::computeSpikeFactors(sce_cellgene_filtered, type="ERCC", general.use=FALSE)
  }

  sce_normalized <- scater::normalize(sce_cellgene_filtered)

  if (verbose) {
    if(has_spike) {
      normalization_plots$ercc <-
        plotExplanatoryVariables(sce_normalized, variables=c("counts_feature_controls_ERCC", "log10_counts_feature_controls_ERCC")) +
        fontsize
    }
  }

  var_fit <- scran::trendVar(sce_normalized, trend="loess", use.spikes=FALSE, span=0.2)
  var_out <- scran::decomposeVar(sce_normalized, var_fit)

  if (verbose) {
    plot(var_out$mean, var_out$total, pch=16, cex=0.6, xlab="Mean log-expression",
         ylab="Variance of log-expression")
    o <- order(var_out$mean)
    lines(var_out$mean[o], var_out$tech[o], col="dodgerblue", lwd=2)

    if (has_spike) {
      cur_spike <- SingleCellExperiment::isSpike(sce)
      points(var_out$mean[cur_spike], var_out$total[cur_spike], col="red", pch=16)
    }

    normalization_plots$gene_variance <- grDevices::recordPlot()
  }

  hvg_out <- var_out[which(var_out$FDR <= 0.05 & var_out$bio >= 0.5),]
  hvg_out <- hvg_out[order(hvg_out$bio, decreasing=TRUE),]

  if (verbose) {
    normalization_plots$top_genes <- scater::plotExpression(sce, rownames(hvg_out)[1:10]) + fontsize
    grDevices::graphics.off()
  }

  requireNamespace("scater")
  expression_normalized_filtered <- scater::exprs(sce[rownames(hvg_out),], "exprs") %>% t()
  counts_filtered <- counts[rownames(expression_normalized_filtered),colnames(expression_normalized_filtered)]

  lst(
    expression,
    counts,
    normalization_plots
  )
}
