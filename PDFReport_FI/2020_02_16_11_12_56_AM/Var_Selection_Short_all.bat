echo "Generate PDF report with LaTeX!"
set root=C:/Users/huang/workspace1/PDFReport_FI/2020_02_16_11_12_56_AM
cd /D %root%
for %%x in (
FI_treas_short
	FI_treas_long
	FI_IG_short
	FI_IG_long
	FI_IG_mid
	FI_HY
	FI_deved_intl
	FI_deving_intl
	EQ_large_cap
	EQ_mid_cap
	EQ_small_cap
	EQ_deved_intl
	EQ_deving_intl
	CO_oil1
	CO_oil2
	CO_nat_gas
	CO_gasoline
	CO_gold
	CO_silver
	CO_grains
) do (
	pdflatex -interaction=nonstopmode "\def\CLUSTER{%%x} \input{Var_Selection_ShortVersion_%%x.tex}"
	pdflatex -interaction=nonstopmode "\def\CLUSTER{%%x} \input{Var_Selection_ShortVersion_%%x.tex}"
)
