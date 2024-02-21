GEN_JSON    = gen_json
OUT_DIR     = source/generated
GNATPP_OPTS = -P gnat/sarif.gpr
SCHEMA_URL  = https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json

all:
	gprbuild -P gnat/sarif.gpr
	gprbuild -P gnat/sarif_examples.gpr

generate: create pretty

create: source/header.adt
	curl -o sarif-schema.json ${SCHEMA_URL}
	${GEN_JSON} --root-package SARIF.Types \
	  --enum-package Enum --root-type Root \
	  --header-file $^ sarif-schema.json > ada.txt
	gnatchop -gnatyN -gnat2022 -w ada.txt ${OUT_DIR}
	rm -f ada.txt

pretty:
	gnatpp ${GNATPP_OPTS} ${OUT_DIR}/*.ad[sb]
