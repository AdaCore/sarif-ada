# SARIF types and I/O subprograms

The program `gen_json` generates the necessary types and procedures to
process SARIF files.

It generates all the necessary code from a single JSON Schema file
(sarif-schema-2.1.0.json) that can be found on
https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/ site.

## Generating the code

Have a working Ada environment, with `gnatchop` and `gnatpp` binaries (present
by default in GNAT toolchains) and `alr` available in your `PATH`
(you can download Alire [here](https://alire.ada.dev/)).

Then you will need to build the `gen_json` tool from the `VSS` repository, and
make it available in your `PATH` environment variable.
Here are the instructions to achieve that:

```shell
git clone https://github.com/AdaCore/VSS.git
cd VSS/tools/json_schema
alr build
export PATH=`pwd`/../../.objs/tools:$PATH
```

Now you can generate the code with a single `make` command.
