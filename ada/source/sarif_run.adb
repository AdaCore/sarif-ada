with VSS.Text_Streams.File_Input;
with VSS.JSON.Pull_Readers.Simple;
with SARIF.Inputs;

procedure SARIF_Run is
   Input   : aliased VSS.Text_Streams.File_Input.File_Input_Text_Stream;
   Reader  : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   Success : Boolean := True;
   Root    : SARIF.Root;
begin
   Input.Open ("simple-example.sarif", "utf-8");
   Reader.Set_Stream (Input'Unchecked_Access);
   Reader.Read_Next;
   pragma Assert (Reader.Is_Start_Document);
   Reader.Read_Next;
   SARIF.Inputs.Input_Root (Reader, Root, Success);
   pragma Assert (Success);

end SARIF_Run;
