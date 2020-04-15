set root=C:/Users/huang/workspace1/PDFReport_FI/2020_02_16_11_12_56_AM
cd /D %root%
echo "Generate PDF report with LaTeX!"
for %%a in (
FixedIncome
	Equity
	Commodity
) do (
	pdflatex -interaction=nonstopmode "\def\ac{%%a} \input{ProductPackage_%%a.tex}"
	pdflatex -interaction=nonstopmode "\def\ac{%%a} \input{ProductPackage_%%a.tex}"
)
