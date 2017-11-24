#' State-of-the-art preprocessing and normalization from https://f1000research.com/articles/5-2122/v2
#' @param counts The counts matrix, with genes in columns
#' @param has_spike Does this contain spike-ins, for which the gene names are preseded by ERCC
#' @param verbose Whether to add plots
normalize_filter_counts <- function(counts, has_spike=any(colnames(counts) %>% grepl("^ERCC", .)), verbose=TRUE) {
  normalization_plots <- list()

  filter <- dplyr::filter;mutate <- dplyr::mutate;arrange <- dplyr::arrange
  sce <- newSCESet(countData=t(counts))
  dim(sce)

  if (has_spike) is.spike <- grepl("^ERCC", rownames(sce))
  is.mito <- grepl("^mt-", rownames(sce))

  feature_controls = list(Mt=is.mito)
  if (has_spike) feature_controls$ERCC <- is.spike

  sce <- calculateQCMetrics(sce, feature_controls=feature_controls)
  head(colnames(pData(sce)))

  if (has_spike) isSpike(sce) <- "ERCC"

  if (verbose) {
    par(mfrow=c(1,2))
    hist(sce$total_counts/1e6, xlab="Library sizes (millions)", main="",
         breaks=20, col="grey80", ylab="Number of cells")
    hist(sce$total_features, xlab="Number of expressed genes", main="",
         breaks=20, col="grey80", ylab="Number of cells")
    par(mfrow=c(1, 1))
    normalization_plots$library <- recordPlot()
  }

  libsize.drop <- isOutlier(sce$total_counts, nmads=3, type="lower", log=TRUE)
  feature.drop <- isOutlier(sce$total_features, nmads=3, type="lower", log=TRUE)

  if (verbose) {
    par(mfrow=c(1,2))
    hist(sce$pct_counts_feature_controls_Mt, xlab="Mitochondrial proportion (%)",
         ylab="Number of cells", breaks=20, main="", col="grey80")
    if (has_spike) hist(sce$pct_counts_feature_controls_ERCC, xlab="ERCC proportion (%)",
                        ylab="Number of cells", breaks=20, main="", col="grey80")
    par(mfrow=c(1, 1))
    normalization_plots$cell_quality <- recordPlot()
  }

  nmads <- 3
  mito.drop <- isOutlier(sce$pct_counts_feature_controls_Mt, nmads=nmads, type="higher")
  spike.drop <- rep(FALSE, length(mito.drop))
  if (has_spike) spike.drop <- isOutlier(sce$pct_counts_feature_controls_ERCC, nmads=nmads, type="higher")



  sce_cell_filtered <- sce[,!(libsize.drop | feature.drop | mito.drop | spike.drop)]
  data.frame(ByLibSize=sum(libsize.drop), ByFeature=sum(feature.drop), ByMito=sum(mito.drop), BySpike=sum(spike.drop), Remaining=ncol(sce_cell_filtered))



  ########################################

  if (verbose) {
    fontsize <- theme(axis.text=element_text(size=12), axis.title=element_text(size=16))
    normalization_plots$pca <- plotPCA(sce, pca_data_input="pdata") + fontsize
  }

  ave.counts <- rowMeans(counts(sce_cell_filtered))
  keep <- ave.counts >= 1
  sum(keep)

  if (verbose) {
    hist(log10(ave.counts), breaks=100, main="", col="grey80",
         xlab=expression(Log[10]~"average count"))
    abline(v=log10(1), col="blue", lwd=2, lty=2)
    normalization_plots$initial_gene_filter <- recordPlot()

    print(plotQC(sce_cell_filtered, type = "highest-expression", n=50) + fontsize)

    normalization_plots$top_genes_qc <- recordPlot()
  }



  numcells <- nexprs(sce_cell_filtered, byrow=TRUE)
  alt.keep <- numcells >= 10
  sum(alt.keep)


  if (verbose) {
    smoothScatter(log10(ave.counts), numcells, xlab=expression(Log[10]~"average count"), ylab="Number of expressing cells")
    if (has_spike) is.ercc <- isSpike(sce_cell_filtered, type="ERCC")
    if (has_spike) points(log10(ave.counts[is.ercc]), numcells[is.ercc], col="red", pch=16, cex=0.5)

    normalization_plots$cell_filtering <- recordPlot()
  }

  sce_cellgene_filtered <- sce_cell_filtered[keep,]
  ########################################

  sce_cellgene_filtered <- computeSumFactors(sce_cellgene_filtered, sizes=c(20, 40, 60, 80))
  summary(sizeFactors(sce_cellgene_filtered))

  if (verbose) {
    plot(sizeFactors(sce_cellgene_filtered), sce_cellgene_filtered$total_counts/1e6, log="xy",
         ylab="Library size (millions)", xlab="Size factor")
    normalization_plots$size_factor <- recordPlot()
  }

  if(has_spike) {
    sce_cellgene_filtered <- computeSpikeFactors(sce_cellgene_filtered, type="ERCC", general.use=FALSE)
  }

  sce_normalized <- scater::normalize(sce_cellgene_filtered)

  if (verbose) {
    if(has_spike) {
      normalization_plots$ercc <- plotExplanatoryVariables(sce_normalized, variables=c("counts_feature_controls_ERCC",
                                                           "log10_counts_feature_controls_ERCC")) + fontsize
    }
  }



  var.fit <- trendVar(sce_normalized, trend="loess", use.spikes=FALSE, span=0.2)
  var.out <- decomposeVar(sce_normalized, var.fit)

  if (verbose) {
    plot(var.out$mean, var.out$total, pch=16, cex=0.6, xlab="Mean log-expression",
         ylab="Variance of log-expression")
    o <- order(var.out$mean)
    lines(var.out$mean[o], var.out$tech[o], col="dodgerblue", lwd=2)
    if (has_spike) cur.spike <- isSpike(sce)
    if (has_spike) points(var.out$mean[cur.spike], var.out$total[cur.spike], col="red", pch=16)

    normalization_plots$gene_variance <- recordPlot()
  }

  hvg.out <- var.out[which(var.out$FDR <= 0.05 & var.out$bio >= 0.5),]
  hvg.out <- hvg.out[order(hvg.out$bio, decreasing=TRUE),]
  nrow(hvg.out)
  head(hvg.out)

  if (verbose) {
    normalization_plots$top_genes <- plotExpression(sce, rownames(hvg.out)[1:10]) + fontsize
  }

  expression_normalized_filtered <- exprs(sce[rownames(hvg.out),]) %>% t()
  counts_filtered <- counts[rownames(expression_normalized_filtered),colnames(expression_normalized_filtered)]

  if (verbose) {
    exprs(sce)[rownames(hvg.out), ] %>% pheatmap::pheatmap()
    normalization_plots$top_heatmap <- recordPlot()
  }

  graphics.off()

  lst(
    expression,
    counts,
    normalization_plots
  )
}
