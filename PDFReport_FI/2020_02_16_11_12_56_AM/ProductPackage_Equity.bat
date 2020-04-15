set root=C:/Users/huang/workspace1/PDFReport_FI/2020_02_16_11_12_56_AM
cd /D %root%
echo "Generate PDF report with LaTeX!"
for %%a in (
	Equity
) do (
	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a "\def\ac{%%a} \input{ProductPackage.tex}"
	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a "\def\ac{%%a} \input{ProductPackage.tex}"
)
