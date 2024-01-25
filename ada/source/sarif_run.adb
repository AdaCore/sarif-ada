--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.Text_Streams.File_Input;
with VSS.JSON.Pull_Readers.Simple;
with SARIF.Types.Inputs;

procedure SARIF_Run is
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

end SARIF_Run;
