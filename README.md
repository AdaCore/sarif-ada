# SARIF Ada

An Ada library generated with the [JSON Schema for Ada](https://github.com/AdaCore/VSS/tree/master/tools/json_schema) tool for parsing and producing [SARIF](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=sarif) reports.

## VS Code Workspace

A VS Code workspace is provided to work with this repository.
It is recommended to check out the [VSS](https://github.com/AdaCore/VSS) repository relative to this one as `../VSS`.

Alternatively you can change the `GPR_PROJECT_PATH` variable in the `.vscode/settings.json` workspace settings file to include your custom location of `VSS` or set it in the environment before invoking VS Code.

## SARIF types and I/O subprograms

The program `gen_json` generates the necessary types and procedures to
process SARIF files.

It generates all the necessary code from the official SARIF JSON Schema file
`sarif-schema-2.1.0.json` downloaded from the [official
source](https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/).

## Generating the code

Have a working Ada environment, with `gnatchop` and `gnatpp` binaries (present
by default in GNAT toolchains) and `alr` available in your `PATH`
(you can download Alire [here](https://alire.ada.dev/)).

Then you will need to build the `gen_json` tool from the
[`VSS` repository](https://github.com/AdaCore/VSS/tree/master/tools/json_schema),
and make it available in your `PATH` environment variable.
Here are the instructions to achieve that:

```shell
git clone https://github.com/AdaCore/VSS.git
cd VSS/tools/json_schema
alr build
export PATH=`pwd`/../../.objs/tools:$PATH
```

Now you can generate the code with a single `make generate` command.
