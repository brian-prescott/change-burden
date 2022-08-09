scripts=(
    "baseline-analysis" "counterfactual-analysis" "counterfactual-extensions" "figure-generation" "figure-extension" "stylized-figure-gen" "analysis-paragraph-values"
)
for s in ${scripts[@]};
do
    Rscript $s.R
done