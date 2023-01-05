# Hitters-Dataset-Analysis
Analysis of the Hitters Dataset, in which baseball players' statistics are used to predict their salary.</br>
<ul>
  <li>Determined the most important features in predicting baseball players' salaries using:
     <ul>
        <li>Linear Regression
        <li>Best Subsets
        <li>Step-wise approaches (forward and backward)
        <li>Lasso
        <li>Elastic Net
        <li>Adaptive Lasso
     </ul>
   <img src="https://github.com/hhuang5163/Hitters-Dataset-Analysis/raw/main/ImportantFeatures.png">
   For best subsets and stepwise forward and backward, I tuned how many features to select based on minimizing the Bayesian Information Criterion value.
   <li>Fit and visualized regularization paths for:
      <ul>
        <li>Lasso
        <li>Elastic Net at &#593 = 0.33, 0.66
        <li>Adaptive Lasso
      </ul>
    The regularization paths for each model can be found in the RegularizationPaths folder.
   <li>Determined the average prediction mean squared error (MSE) for:
      <ul>
        <li>Least Squares
        <li>Ridge Regression
        <li>Best Subsets
        <li>Step-wise approaches (forward and backward)
        <li>Lasso
        <li>Elastic Net
        <li>Adaptive Lasso
      </ul>
    The visualization for the average MSE can be found at AvgPredictionMSE.png.
</ul>

# To run
Simply open the correct file and run to replicate the results as described above.</br>
<b>hittersfeatureselection.R</b> performs feature selection on the Hitters dataset.</br>
<b>mse_analysis.R</b> determines the average prediction MSE for each model on the Hitters dataset.
