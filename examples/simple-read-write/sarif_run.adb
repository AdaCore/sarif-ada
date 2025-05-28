--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Push_Writers;
with VSS.Text_Streams.File_Input;
with VSS.Text_Streams.File_Output;
with VSS.Text_Streams.Standards;

with SARIF.Types.Inputs;
with SARIF.Types.Outputs;

procedure SARIF_Run is

   Console : VSS.Text_Streams.Output_Text_Stream'Class :=
     VSS.Text_Streams.Standards.Standard_Output;

   procedure Read_File;
   procedure Write_File;

   procedure Read_File is
      Input   : aliased VSS.Text_Streams.File_Input.File_Input_Text_Stream;
      Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Success : Boolean := True;
      Root    : SARIF.Types.Root;
   begin
      Input.Open ("simple-example.sarif", "utf-8");
      Reader.Set_Stream (Input'Unchecked_Access);
      Reader.Read_Next;
      pragma Assert (Reader.Is_Start_Document);
      Reader.Read_Next;
      SARIF.Types.Inputs.Input_Root (Reader, Root, Success);
      pragma Assert (Success);

      for J in 1 .. Root.runs.Length loop
         declare
            Run : SARIF.Types.run renames Root.runs (J);
         begin
            for R in 1 .. Run.results.Length loop
               declare
                  Result : SARIF.Types.result renames Run.results (R);
               begin
                  Console.Put_Line (Result.message.text, Success);
               end;
            end loop;
         end;
      end loop;
   end Read_File;

   procedure Write_File is
      Output  : aliased VSS.Text_Streams.File_Output.File_Output_Text_Stream;
      Writer  : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Success : Boolean := True;
      Root    : SARIF.Types.Root;
      Run     : SARIF.Types.run;
      Result  : SARIF.Types.result;
   begin
      Result :=
        (message =>
           (text => "Something went good! O_o",
            others => <>),
         others => <>);

      Run.results.Clear (Is_Null => False);
      --  This Clear call is necessary if no further results will be added.
      --  It makes the output routine generate `results: []`.

      Run.results.Append (Result);
      Root.runs.Append (Run);

      Console.Put_Line ("Writing example-output.sarif...", Success);
      Writer.Set_Stream (Output'Unchecked_Access);
      Output.Create ("example-output.sarif", "utf-8");
      Writer.Start_Document;
      SARIF.Types.Outputs.Output_Root (Writer, Root);
      Writer.End_Document;
   end Write_File;

begin
   Read_File;

   Write_File;
end SARIF_Run;
