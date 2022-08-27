default:
	@echo targets: 101

101:
	Rscript readYaml.R df-101

102:
	Rscript readYaml.R df-102

all:
	for f in df*.yaml; do Rscript readYaml.R $$f; done

#             echo $$f; \
           #
