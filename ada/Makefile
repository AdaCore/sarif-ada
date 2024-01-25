GEN_JSON    = gen_json
OUT_DIR     = ./generated
GNATPP_OPTS = -P sarif.gpr

all: create pretty

create: header.adt ../sarif-schema-2.1.0.json
	${GEN_JSON} --root-package SARIF.Types \
	  --enum-package Enum --root-type Root \
	  --header-file $^ > ada.txt
	gnatchop -gnatyN -gnat2022 -w ada.txt ${OUT_DIR}
	rm -f ada.txt

pretty:
	gnatpp ${GNATPP_OPTS} ${OUT_DIR}/*.ad[sb]
