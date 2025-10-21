--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;
with Minimal_Perfect_Hash;

package body SARIF.Types.Inputs is
   pragma Style_Checks (Off);
   use type VSS.JSON.JSON_Number_Kind;
   use type VSS.Strings.Virtual_String;

   procedure Input_Any_Value
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Any_Value'Class;
      Success : in out Boolean) is
      use type VSS.JSON.Streams.JSON_Stream_Element_Kind;
   begin
      case Reader.Element_Kind is
         when VSS.JSON.Streams.Start_Array =>
            Value.Append ((Kind => VSS.JSON.Streams.Start_Array));
            Reader.Read_Next;
            while Success and Reader.Element_Kind /= VSS.JSON.Streams.End_Array
            loop
               Input_Any_Value (Reader, Value, Success);
            end loop;
            Value.Append ((Kind => VSS.JSON.Streams.End_Array));
         when VSS.JSON.Streams.Start_Object =>
            Value.Append ((Kind => VSS.JSON.Streams.Start_Object));
            Reader.Read_Next;
            while Success and Reader.Element_Kind = VSS.JSON.Streams.Key_Name
            loop
               Value.Append (Reader.Element);
               Reader.Read_Next;
               Input_Any_Value (Reader, Value, Success);
            end loop;
            Value.Append ((Kind => VSS.JSON.Streams.End_Object));
         when VSS.JSON.Streams.String_Value | VSS.JSON.Streams.Number_Value
           | VSS.JSON.Streams.Boolean_Value | VSS.JSON.Streams.Null_Value =>
            Value.Append (Reader.Element);
         when others =>
            Success := False;
      end case;
      if Success then
         Reader.Read_Next;
      end if;
   end Input_Any_Value;

   package notification_level_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["none",
      "note",
      "warning",
      "error"]);

   procedure Input_notification_level
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.notification_level;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           notification_level_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.notification_level'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_notification_level;

   package suppression_kind_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["inSource",
      "external"]);

   procedure Input_suppression_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.suppression_kind;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           suppression_kind_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.suppression_kind'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_suppression_kind;

   package suppression_status_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["accepted",
      "underReview",
      "rejected"]);

   procedure Input_suppression_status
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.suppression_status;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           suppression_status_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.suppression_status'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_suppression_status;

   package toolComponent_contents_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["localizedData",
      "nonLocalizedData"]);

   procedure Input_toolComponent_contents
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.toolComponent_contents;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           toolComponent_contents_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.toolComponent_contents'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_toolComponent_contents;

   package artifact_roles_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["analysisTarget",
      "attachment",
      "responseFile",
      "resultFile",
      "standardStream",
      "tracedFile",
      "unmodified",
      "modified",
      "added",
      "deleted",
      "renamed",
      "uncontrolled",
      "driver",
      "extension",
      "translation",
      "taxonomy",
      "policy",
      "referencedOnCommandLine",
      "memoryContents",
      "directory",
      "userSpecifiedConfiguration",
      "toolSpecifiedConfiguration",
      "debugOutputFile"]);

   procedure Input_artifact_roles
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.artifact_roles;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           artifact_roles_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.artifact_roles'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_artifact_roles;

   package reportingConfiguration_level_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["none",
      "note",
      "warning",
      "error"]);

   procedure Input_reportingConfiguration_level
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.reportingConfiguration_level;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           reportingConfiguration_level_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.reportingConfiguration_level'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_reportingConfiguration_level;

   package result_kind_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["notApplicable",
      "pass",
      "fail",
      "review",
      "open",
      "informational"]);

   procedure Input_result_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.result_kind;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           result_kind_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.result_kind'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_result_kind;

   package result_level_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["none",
      "note",
      "warning",
      "error"]);

   procedure Input_result_level
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.result_level;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           result_level_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.result_level'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_result_level;

   package result_baselineState_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["new",
      "unchanged",
      "updated",
      "absent"]);

   procedure Input_result_baselineState
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.result_baselineState;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           result_baselineState_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.result_baselineState'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_result_baselineState;

   package run_columnKind_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["utf16CodeUnits",
      "unicodeCodePoints"]);

   procedure Input_run_columnKind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.run_columnKind;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           run_columnKind_Minimal_Perfect_Hash.Get_Index (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.run_columnKind'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_run_columnKind;

   package threadFlowLocation_importance_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["important",
      "essential",
      "unimportant"]);

   procedure Input_threadFlowLocation_importance
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.threadFlowLocation_importance;
      Success : in out Boolean) is
      Index : constant Integer :=
        (if Reader.Is_String_Value then
           threadFlowLocation_importance_Minimal_Perfect_Hash.Get_Index
             (Reader.String_Value)
         else -1);
   begin
      if Index > 0 then
         Value := Enum.threadFlowLocation_importance'Val (Index - 1);
         Reader.Read_Next;
      else
         Success := False;
      end if;
   end Input_threadFlowLocation_importance;

   package node_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id",
      "label",
      "location",
      "children",
      "properties"]);

   procedure Input_node
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out node;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 node_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  label
                     Reader.Read_Next;

                     Value.label :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.label.Value, Success);
                  when 3 =>  --  location
                     Reader.Read_Next;

                     Value.location :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_location (Reader, Value.location.Value, Success);
                  when 4 =>  --  children
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.children.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : node;
                           begin
                              Input_node (Reader, Item, Success);
                              Value.children.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_node;

   package edgeTraversal_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["edgeId",
      "message",
      "finalState",
      "stepOverEdgeCount",
      "properties"]);

   procedure Input_edgeTraversal
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out edgeTraversal;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 edgeTraversal_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  edgeId
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.edgeId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 3 =>  --  finalState
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.finalState, Success);
                  when 4 =>  --  stepOverEdgeCount
                     Reader.Read_Next;

                     Value.stepOverEdgeCount :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.stepOverEdgeCount.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_edgeTraversal;

   package stackFrame_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["location",
      "module",
      "threadId",
      "parameters",
      "properties"]);

   procedure Input_stackFrame
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out stackFrame;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 stackFrame_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  location
                     Reader.Read_Next;

                     Value.location :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_location (Reader, Value.location.Value, Success);
                  when 2 =>  --  module
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.module := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  threadId
                     Reader.Read_Next;

                     Value.threadId :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  parameters
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.parameters.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.parameters.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_stackFrame;

   package reportingDescriptorRelationship_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["target",
      "kinds",
      "description",
      "properties"]);

   procedure Input_reportingDescriptorRelationship
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingDescriptorRelationship;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 reportingDescriptorRelationship_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  target
                     Reader.Read_Next;

                     Input_reportingDescriptorReference
                       (Reader, Value.target, Success);
                  when 2 =>  --  kinds
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.kinds.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.kinds.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_reportingDescriptorRelationship;

   package propertyBag_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["tags"]);

   procedure Input_propertyBag
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out propertyBag;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 propertyBag_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  tags
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.tags.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.tags.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when others =>
                     Value.Additional_Properties.Append
                       ((VSS.JSON.Streams.Key_Name,
                         Reader.Key_Name));
                     Reader.Read_Next;
                     Input_Any_Value
                       (Reader, Value.Additional_Properties, Success);
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_propertyBag;

   package logicalLocation_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name",
      "index",
      "fullyQualifiedName",
      "decoratedName",
      "parentIndex",
      "kind",
      "properties"]);

   procedure Input_logicalLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out logicalLocation;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 logicalLocation_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  name
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  fullyQualifiedName
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.fullyQualifiedName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  decoratedName
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.decoratedName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  parentIndex
                     Reader.Read_Next;

                     Value.parentIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.parentIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  kind
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.kind := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_logicalLocation;

   package resultProvenance_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["firstDetectionTimeUtc",
      "lastDetectionTimeUtc",
      "firstDetectionRunGuid",
      "lastDetectionRunGuid",
      "invocationIndex",
      "conversionSources",
      "properties"]);

   procedure Input_resultProvenance
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out resultProvenance;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 resultProvenance_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  firstDetectionTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.firstDetectionTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  lastDetectionTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.lastDetectionTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  firstDetectionRunGuid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.firstDetectionRunGuid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  lastDetectionRunGuid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.lastDetectionRunGuid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  invocationIndex
                     Reader.Read_Next;

                     Value.invocationIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.invocationIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  conversionSources
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.conversionSources.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : physicalLocation;
                           begin
                              Input_physicalLocation (Reader, Item, Success);
                              Value.conversionSources.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_resultProvenance;

   package message_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["text",
      "markdown",
      "id",
      "arguments",
      "properties"]);

   procedure Input_message
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out message;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 message_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  text
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.text := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  markdown
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.markdown := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  arguments
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.arguments.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.arguments.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_message;

   package artifactChange_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["artifactLocation",
      "replacements",
      "properties"]);

   procedure Input_artifactChange
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifactChange;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 artifactChange_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  artifactLocation
                     Reader.Read_Next;

                     Input_artifactLocation
                       (Reader, Value.artifactLocation, Success);
                  when 2 =>  --  replacements
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.replacements.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : replacement;
                           begin
                              Input_replacement (Reader, Item, Success);
                              Value.replacements.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_artifactChange;

   package address_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["absoluteAddress",
      "relativeAddress",
      "length",
      "kind",
      "name",
      "fullyQualifiedName",
      "offsetFromParent",
      "index",
      "parentIndex",
      "properties"]);

   procedure Input_address
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out address;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 address_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  absoluteAddress
                     Reader.Read_Next;

                     Value.absoluteAddress :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.absoluteAddress.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  relativeAddress
                     Reader.Read_Next;

                     Value.relativeAddress :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.relativeAddress.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  length
                     Reader.Read_Next;

                     Value.length :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.length.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  kind
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.kind := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  name
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  fullyQualifiedName
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.fullyQualifiedName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  offsetFromParent
                     Reader.Read_Next;

                     Value.offsetFromParent :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offsetFromParent.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  parentIndex
                     Reader.Read_Next;

                     Value.parentIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.parentIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_address;

   package reportingDescriptorReference_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id",
      "index",
      "guid",
      "toolComponent",
      "properties"]);

   procedure Input_reportingDescriptorReference
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingDescriptorReference;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 reportingDescriptorReference_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  toolComponent
                     Reader.Read_Next;

                     Value.toolComponent :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_toolComponentReference
                       (Reader, Value.toolComponent.Value, Success);
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_reportingDescriptorReference;

   package translationMetadata_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name",
      "fullName",
      "shortDescription",
      "fullDescription",
      "downloadUri",
      "informationUri",
      "properties"]);

   procedure Input_translationMetadata
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out translationMetadata;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 translationMetadata_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  name
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  fullName
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.fullName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  shortDescription
                     Reader.Read_Next;

                     Value.shortDescription :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.shortDescription.Value, Success);
                  when 4 =>  --  fullDescription
                     Reader.Read_Next;

                     Value.fullDescription :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.fullDescription.Value, Success);
                  when 5 =>  --  downloadUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.downloadUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  informationUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.informationUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_translationMetadata;

   package notification_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["locations",
      "message",
      "level",
      "threadId",
      "timeUtc",
      "exception",
      "descriptor",
      "associatedRule",
      "properties"]);

   procedure Input_notification
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out notification;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 notification_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  locations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.locations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : location;
                           begin
                              Input_location (Reader, Item, Success);
                              Value.locations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  message
                     Reader.Read_Next;

                     Input_message (Reader, Value.message, Success);
                  when 3 =>  --  level
                     Reader.Read_Next;

                     Value.level :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_notification_level
                       (Reader, Value.level.Value, Success);
                  when 4 =>  --  threadId
                     Reader.Read_Next;

                     Value.threadId :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.threadId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  timeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.timeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  exception
                     Reader.Read_Next;

                     Value.a_exception :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_a_exception
                       (Reader, Value.a_exception.Value, Success);
                  when 7 =>  --  descriptor
                     Reader.Read_Next;

                     Value.descriptor :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_reportingDescriptorReference
                       (Reader, Value.descriptor.Value, Success);
                  when 8 =>  --  associatedRule
                     Reader.Read_Next;

                     Value.associatedRule :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_reportingDescriptorReference
                       (Reader, Value.associatedRule.Value, Success);
                  when 9 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_notification;

   package physicalLocation_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["address",
      "artifactLocation",
      "region",
      "contextRegion",
      "properties"]);

   procedure Input_physicalLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out physicalLocation;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 physicalLocation_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  address
                     Reader.Read_Next;

                     Value.address :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_address (Reader, Value.address.Value, Success);
                  when 2 =>  --  artifactLocation
                     Reader.Read_Next;

                     Value.artifactLocation :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.artifactLocation.Value, Success);
                  when 3 =>  --  region
                     Reader.Read_Next;

                     Value.region :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_region (Reader, Value.region.Value, Success);
                  when 4 =>  --  contextRegion
                     Reader.Read_Next;

                     Value.contextRegion :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_region (Reader, Value.contextRegion.Value, Success);
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_physicalLocation;

   package versionControlDetails_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["repositoryUri",
      "revisionId",
      "branch",
      "revisionTag",
      "asOfTimeUtc",
      "mappedTo",
      "properties"]);

   procedure Input_versionControlDetails
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out versionControlDetails;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 versionControlDetails_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  repositoryUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.repositoryUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  revisionId
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.revisionId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  branch
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.branch := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  revisionTag
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.revisionTag := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  asOfTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.asOfTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  mappedTo
                     Reader.Read_Next;

                     Value.mappedTo :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.mappedTo.Value, Success);
                  when 7 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_versionControlDetails;

   package location_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id",
      "physicalLocation",
      "logicalLocations",
      "message",
      "annotations",
      "relationships",
      "properties"]);

   procedure Input_location
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out location;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 location_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  id
                     Reader.Read_Next;

                     Value.id :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.id.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  physicalLocation
                     Reader.Read_Next;

                     Value.physicalLocation :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_physicalLocation
                       (Reader, Value.physicalLocation.Value, Success);
                  when 3 =>  --  logicalLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.logicalLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : logicalLocation;
                           begin
                              Input_logicalLocation (Reader, Item, Success);
                              Value.logicalLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 5 =>  --  annotations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.annotations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : region;
                           begin
                              Input_region (Reader, Item, Success);
                              Value.annotations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  relationships
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.relationships.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : locationRelationship;
                           begin
                              Input_locationRelationship
                                (Reader, Item, Success);
                              Value.relationships.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_location;

   package suppression_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["guid",
      "kind",
      "status",
      "justification",
      "location",
      "properties"]);

   procedure Input_suppression
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out suppression;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 suppression_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  kind
                     Reader.Read_Next;

                     Input_suppression_kind (Reader, Value.kind, Success);
                  when 3 =>  --  status
                     Reader.Read_Next;

                     Value.status :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_suppression_status
                       (Reader, Value.status.Value, Success);
                  when 4 =>  --  justification
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.justification := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  location
                     Reader.Read_Next;

                     Value.location :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_location (Reader, Value.location.Value, Success);
                  when 6 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_suppression;

   package externalPropertyFileReference_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["location",
      "guid",
      "itemCount",
      "properties"]);

   procedure Input_externalPropertyFileReference
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out externalPropertyFileReference;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 externalPropertyFileReference_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  location
                     Reader.Read_Next;

                     Value.location :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.location.Value, Success);
                  when 2 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  itemCount
                     Reader.Read_Next;

                     Value.itemCount :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.itemCount.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_externalPropertyFileReference;

   package locationRelationship_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["target",
      "kinds",
      "description",
      "properties"]);

   procedure Input_locationRelationship
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out locationRelationship;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 locationRelationship_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  target
                     Reader.Read_Next;

                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.target :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  kinds
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.kinds.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.kinds.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_locationRelationship;

   package edge_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id",
      "label",
      "sourceNodeId",
      "targetNodeId",
      "properties"]);

   procedure Input_edge
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out edge;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 edge_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  label
                     Reader.Read_Next;

                     Value.label :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.label.Value, Success);
                  when 3 =>  --  sourceNodeId
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.sourceNodeId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  targetNodeId
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.targetNodeId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_edge;

   package Root_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["$schema",
      "version",
      "runs",
      "inlineExternalProperties",
      "properties"]);

   procedure Input_Root
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Root;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 Root_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  $schema
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.schema := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  version
                     Reader.Read_Next;

                     if Reader.Is_String_Value
                       and then Reader.String_Value = "2.1.0"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  runs
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.runs.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : run;
                           begin
                              Input_run (Reader, Item, Success);
                              Value.runs.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  inlineExternalProperties
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.inlineExternalProperties.Clear
                          (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalProperties;
                           begin
                              Input_externalProperties (Reader, Item, Success);
                              Value.inlineExternalProperties.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_Root;

   package specialLocations_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["displayBase",
      "properties"]);

   procedure Input_specialLocations
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out specialLocations;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 specialLocations_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  displayBase
                     Reader.Read_Next;

                     Value.displayBase :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.displayBase.Value, Success);
                  when 2 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_specialLocations;

   package toolComponent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["guid",
      "name",
      "organization",
      "product",
      "productSuite",
      "shortDescription",
      "fullDescription",
      "fullName",
      "version",
      "semanticVersion",
      "dottedQuadFileVersion",
      "releaseDateUtc",
      "downloadUri",
      "informationUri",
      "globalMessageStrings",
      "notifications",
      "rules",
      "taxa",
      "locations",
      "language",
      "contents",
      "isComprehensive",
      "localizedDataSemanticVersion",
      "minimumRequiredLocalizedDataSemanticVersion",
      "associatedComponent",
      "translationMetadata",
      "supportedTaxonomies",
      "properties"]);

   procedure Input_toolComponent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out toolComponent;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 toolComponent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  name
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  organization
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.organization := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  product
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.product := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  productSuite
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.productSuite := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  shortDescription
                     Reader.Read_Next;

                     Value.shortDescription :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.shortDescription.Value, Success);
                  when 7 =>  --  fullDescription
                     Reader.Read_Next;

                     Value.fullDescription :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.fullDescription.Value, Success);
                  when 8 =>  --  fullName
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.fullName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  version
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.version := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  semanticVersion
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.semanticVersion := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  dottedQuadFileVersion
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.dottedQuadFileVersion := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  releaseDateUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.releaseDateUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  downloadUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.downloadUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  informationUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.informationUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  globalMessageStrings
                     Reader.Read_Next;

                     Input_Any_Value
                       (Reader, Value.globalMessageStrings, Success);
                  when 16 =>  --  notifications
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.notifications.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : reportingDescriptor;
                           begin
                              Input_reportingDescriptor
                                (Reader, Item, Success);
                              Value.notifications.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  rules
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.rules.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : reportingDescriptor;
                           begin
                              Input_reportingDescriptor
                                (Reader, Item, Success);
                              Value.rules.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 18 =>  --  taxa
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.taxa.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : reportingDescriptor;
                           begin
                              Input_reportingDescriptor
                                (Reader, Item, Success);
                              Value.taxa.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 19 =>  --  locations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.locations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : artifactLocation;
                           begin
                              Input_artifactLocation (Reader, Item, Success);
                              Value.locations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 20 =>  --  language
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.language := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 21 =>  --  contents
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.contents.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Enum.toolComponent_contents;
                           begin
                              Input_toolComponent_contents
                                (Reader, Item, Success);
                              Value.contents.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 22 =>  --  isComprehensive
                     Reader.Read_Next;

                     if Reader.Is_Boolean_Value then
                        Value.isComprehensive := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 23 =>  --  localizedDataSemanticVersion
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.localizedDataSemanticVersion :=
                          Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 24 =>  --  minimumRequiredLocalizedDataSemanticVersion
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.minimumRequiredLocalizedDataSemanticVersion :=
                          Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 25 =>  --  associatedComponent
                     Reader.Read_Next;

                     Value.associatedComponent :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_toolComponentReference
                       (Reader, Value.associatedComponent.Value, Success);
                  when 26 =>  --  translationMetadata
                     Reader.Read_Next;

                     Value.translationMetadata :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_translationMetadata
                       (Reader, Value.translationMetadata.Value, Success);
                  when 27 =>  --  supportedTaxonomies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.supportedTaxonomies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponentReference;
                           begin
                              Input_toolComponentReference
                                (Reader, Item, Success);
                              Value.supportedTaxonomies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 28 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_toolComponent;

   package invocation_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["commandLine",
      "arguments",
      "responseFiles",
      "startTimeUtc",
      "endTimeUtc",
      "exitCode",
      "ruleConfigurationOverrides",
      "notificationConfigurationOverrides",
      "toolExecutionNotifications",
      "toolConfigurationNotifications",
      "exitCodeDescription",
      "exitSignalName",
      "exitSignalNumber",
      "processStartFailureMessage",
      "executionSuccessful",
      "machine",
      "account",
      "processId",
      "executableLocation",
      "workingDirectory",
      "environmentVariables",
      "stdin",
      "stdout",
      "stderr",
      "stdoutStderr",
      "properties"]);

   procedure Input_invocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out invocation;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 invocation_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  commandLine
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.commandLine := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  arguments
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.arguments.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.arguments.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  responseFiles
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.responseFiles.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : artifactLocation;
                           begin
                              Input_artifactLocation (Reader, Item, Success);
                              Value.responseFiles.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  startTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.startTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  endTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.endTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  exitCode
                     Reader.Read_Next;

                     Value.exitCode :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.exitCode.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  ruleConfigurationOverrides
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.ruleConfigurationOverrides.Clear
                          (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : configurationOverride;
                           begin
                              Input_configurationOverride
                                (Reader, Item, Success);
                              Value.ruleConfigurationOverrides.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  notificationConfigurationOverrides
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.notificationConfigurationOverrides.Clear
                          (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : configurationOverride;
                           begin
                              Input_configurationOverride
                                (Reader, Item, Success);
                              Value.notificationConfigurationOverrides.Append
                                (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  toolExecutionNotifications
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.toolExecutionNotifications.Clear
                          (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : notification;
                           begin
                              Input_notification (Reader, Item, Success);
                              Value.toolExecutionNotifications.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  toolConfigurationNotifications
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.toolConfigurationNotifications.Clear
                          (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : notification;
                           begin
                              Input_notification (Reader, Item, Success);
                              Value.toolConfigurationNotifications.Append
                                (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  exitCodeDescription
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.exitCodeDescription := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  exitSignalName
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.exitSignalName := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  exitSignalNumber
                     Reader.Read_Next;

                     Value.exitSignalNumber :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.exitSignalNumber.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  processStartFailureMessage
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.processStartFailureMessage :=
                          Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  executionSuccessful
                     Reader.Read_Next;

                     if Reader.Is_Boolean_Value then
                        Value.executionSuccessful := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  machine
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.machine := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  account
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.account := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 18 =>  --  processId
                     Reader.Read_Next;

                     Value.processId :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.processId.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 19 =>  --  executableLocation
                     Reader.Read_Next;

                     Value.executableLocation :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.executableLocation.Value, Success);
                  when 20 =>  --  workingDirectory
                     Reader.Read_Next;

                     Value.workingDirectory :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.workingDirectory.Value, Success);
                  when 21 =>  --  environmentVariables
                     Reader.Read_Next;

                     Input_Any_Value
                       (Reader, Value.environmentVariables, Success);
                  when 22 =>  --  stdin
                     Reader.Read_Next;

                     Value.stdin :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.stdin.Value, Success);
                  when 23 =>  --  stdout
                     Reader.Read_Next;

                     Value.stdout :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.stdout.Value, Success);
                  when 24 =>  --  stderr
                     Reader.Read_Next;

                     Value.stderr :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.stderr.Value, Success);
                  when 25 =>  --  stdoutStderr
                     Reader.Read_Next;

                     Value.stdoutStderr :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.stdoutStderr.Value, Success);
                  when 26 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_invocation;

   package conversion_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["tool",
      "invocation",
      "analysisToolLogFiles",
      "properties"]);

   procedure Input_conversion
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out conversion;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 conversion_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  tool
                     Reader.Read_Next;

                     Input_tool (Reader, Value.tool, Success);
                  when 2 =>  --  invocation
                     Reader.Read_Next;

                     Value.invocation :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_invocation
                       (Reader, Value.invocation.Value, Success);
                  when 3 =>  --  analysisToolLogFiles
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.analysisToolLogFiles.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : artifactLocation;
                           begin
                              Input_artifactLocation (Reader, Item, Success);
                              Value.analysisToolLogFiles.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_conversion;

   package runAutomationDetails_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["description",
      "id",
      "guid",
      "correlationGuid",
      "properties"]);

   procedure Input_runAutomationDetails
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out runAutomationDetails;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 runAutomationDetails_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 2 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  correlationGuid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.correlationGuid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_runAutomationDetails;

   package graph_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["description",
      "nodes",
      "edges",
      "properties"]);

   procedure Input_graph
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out graph;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 graph_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 2 =>  --  nodes
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.nodes.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : node;
                           begin
                              Input_node (Reader, Item, Success);
                              Value.nodes.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  edges
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.edges.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : edge;
                           begin
                              Input_edge (Reader, Item, Success);
                              Value.edges.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_graph;

   package externalProperties_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["schema",
      "version",
      "guid",
      "runGuid",
      "conversion",
      "graphs",
      "externalizedProperties",
      "artifacts",
      "invocations",
      "logicalLocations",
      "threadFlowLocations",
      "results",
      "taxonomies",
      "driver",
      "extensions",
      "policies",
      "translations",
      "addresses",
      "webRequests",
      "webResponses",
      "properties"]);

   procedure Input_externalProperties
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out externalProperties;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 externalProperties_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  schema
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.schema := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  version
                     Reader.Read_Next;

                     if Reader.Is_String_Value
                       and then Reader.String_Value = "2.1.0"
                     then
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  runGuid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.runGuid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  conversion
                     Reader.Read_Next;

                     Value.conversion :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_conversion
                       (Reader, Value.conversion.Value, Success);
                  when 6 =>  --  graphs
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.graphs.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : graph;
                           begin
                              Input_graph (Reader, Item, Success);
                              Value.graphs.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  externalizedProperties
                     Reader.Read_Next;

                     Value.externalizedProperties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.externalizedProperties.Value, Success);
                  when 8 =>  --  artifacts
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.artifacts.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : artifact;
                           begin
                              Input_artifact (Reader, Item, Success);
                              Value.artifacts.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  invocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.invocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : invocation;
                           begin
                              Input_invocation (Reader, Item, Success);
                              Value.invocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  logicalLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.logicalLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : logicalLocation;
                           begin
                              Input_logicalLocation (Reader, Item, Success);
                              Value.logicalLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  threadFlowLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.threadFlowLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : threadFlowLocation;
                           begin
                              Input_threadFlowLocation (Reader, Item, Success);
                              Value.threadFlowLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  results
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.results.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : result;
                           begin
                              Input_result (Reader, Item, Success);
                              Value.results.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  taxonomies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.taxonomies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.taxonomies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  driver
                     Reader.Read_Next;

                     Value.driver :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_toolComponent (Reader, Value.driver.Value, Success);
                  when 15 =>  --  extensions
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.extensions.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.extensions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  policies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.policies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.policies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  translations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.translations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.translations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 18 =>  --  addresses
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.addresses.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : address;
                           begin
                              Input_address (Reader, Item, Success);
                              Value.addresses.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 19 =>  --  webRequests
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.webRequests.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : webRequest;
                           begin
                              Input_webRequest (Reader, Item, Success);
                              Value.webRequests.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 20 =>  --  webResponses
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.webResponses.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : webResponse;
                           begin
                              Input_webResponse (Reader, Item, Success);
                              Value.webResponses.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 21 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_externalProperties;

   package rectangle_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["top",
      "left",
      "bottom",
      "right",
      "message",
      "properties"]);

   procedure Input_rectangle
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out rectangle;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 rectangle_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  top
                     Reader.Read_Next;

                     Value.top :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.top.Value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.top.Value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  left
                     Reader.Read_Next;

                     Value.left :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.left.Value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.left.Value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  bottom
                     Reader.Read_Next;

                     Value.bottom :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.bottom.Value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.bottom.Value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  right
                     Reader.Read_Next;

                     Value.right :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.right.Value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.right.Value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 6 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_rectangle;

   package reportingDescriptor_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id",
      "deprecatedIds",
      "guid",
      "deprecatedGuids",
      "name",
      "deprecatedNames",
      "shortDescription",
      "fullDescription",
      "messageStrings",
      "defaultConfiguration",
      "helpUri",
      "help",
      "relationships",
      "properties"]);

   procedure Input_reportingDescriptor
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingDescriptor;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 reportingDescriptor_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  deprecatedIds
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.deprecatedIds.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.deprecatedIds.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  deprecatedGuids
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.deprecatedGuids.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.deprecatedGuids.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  name
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  deprecatedNames
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.deprecatedNames.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.deprecatedNames.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  shortDescription
                     Reader.Read_Next;

                     Value.shortDescription :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.shortDescription.Value, Success);
                  when 8 =>  --  fullDescription
                     Reader.Read_Next;

                     Value.fullDescription :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.fullDescription.Value, Success);
                  when 9 =>  --  messageStrings
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.messageStrings, Success);
                  when 10 =>  --  defaultConfiguration
                     Reader.Read_Next;

                     Value.defaultConfiguration :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_reportingConfiguration
                       (Reader, Value.defaultConfiguration.Value, Success);
                  when 11 =>  --  helpUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.helpUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  help
                     Reader.Read_Next;

                     Value.help :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.help.Value, Success);
                  when 13 =>  --  relationships
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.relationships.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : reportingDescriptorRelationship;
                           begin
                              Input_reportingDescriptorRelationship
                                (Reader, Item, Success);
                              Value.relationships.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_reportingDescriptor;

   package artifact_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["description",
      "location",
      "parentIndex",
      "offset",
      "length",
      "roles",
      "mimeType",
      "contents",
      "encoding",
      "sourceLanguage",
      "hashes",
      "lastModifiedTimeUtc",
      "properties"]);

   procedure Input_artifact
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifact;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 artifact_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 2 =>  --  location
                     Reader.Read_Next;

                     Value.location :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.location.Value, Success);
                  when 3 =>  --  parentIndex
                     Reader.Read_Next;

                     Value.parentIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.parentIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  offset
                     Reader.Read_Next;

                     Value.offset :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.offset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  length
                     Reader.Read_Next;

                     Value.length :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.length.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  roles
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.roles.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : Enum.artifact_roles;
                           begin
                              Input_artifact_roles (Reader, Item, Success);
                              Value.roles.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  mimeType
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.mimeType := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  contents
                     Reader.Read_Next;

                     Value.contents :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactContent
                       (Reader, Value.contents.Value, Success);
                  when 9 =>  --  encoding
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.encoding := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  sourceLanguage
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.sourceLanguage := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  hashes
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.hashes, Success);
                  when 12 =>  --  lastModifiedTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.lastModifiedTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_artifact;

   package reportingConfiguration_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["enabled",
      "level",
      "rank",
      "parameters",
      "properties"]);

   procedure Input_reportingConfiguration
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingConfiguration;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 reportingConfiguration_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  enabled
                     Reader.Read_Next;

                     if Reader.Is_Boolean_Value then
                        Value.enabled := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  level
                     Reader.Read_Next;

                     Value.level :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_reportingConfiguration_level
                       (Reader, Value.level.Value, Success);
                  when 3 =>  --  rank
                     Reader.Read_Next;

                     Value.rank :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.rank.Value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.rank.Value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  parameters
                     Reader.Read_Next;

                     Value.parameters :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.parameters.Value, Success);
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_reportingConfiguration;

   package toolComponentReference_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["name",
      "index",
      "guid",
      "properties"]);

   procedure Input_toolComponentReference
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out toolComponentReference;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 toolComponentReference_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  name
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.name := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_toolComponentReference;

   package tool_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["driver",
      "extensions",
      "properties"]);

   procedure Input_tool
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out tool;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 tool_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  driver
                     Reader.Read_Next;

                     Input_toolComponent (Reader, Value.driver, Success);
                  when 2 =>  --  extensions
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.extensions.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.extensions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_tool;

   package webRequest_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["index",
      "protocol",
      "version",
      "target",
      "method",
      "headers",
      "parameters",
      "body",
      "properties"]);

   procedure Input_webRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out webRequest;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 webRequest_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  protocol
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.protocol := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  version
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.version := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  target
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.target := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  method
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.method := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  headers
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.headers, Success);
                  when 7 =>  --  parameters
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.parameters, Success);
                  when 8 =>  --  body
                     Reader.Read_Next;

                     Value.a_body :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactContent
                       (Reader, Value.a_body.Value, Success);
                  when 9 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_webRequest;

   package fix_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["description",
      "artifactChanges",
      "properties"]);

   procedure Input_fix
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out fix;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 fix_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 2 =>  --  artifactChanges
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.artifactChanges.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : artifactChange;
                           begin
                              Input_artifactChange (Reader, Item, Success);
                              Value.artifactChanges.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_fix;

   package result_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["ruleId",
      "ruleIndex",
      "rule",
      "kind",
      "level",
      "message",
      "analysisTarget",
      "locations",
      "guid",
      "correlationGuid",
      "occurrenceCount",
      "partialFingerprints",
      "fingerprints",
      "stacks",
      "codeFlows",
      "graphs",
      "graphTraversals",
      "relatedLocations",
      "suppressions",
      "baselineState",
      "rank",
      "attachments",
      "hostedViewerUri",
      "workItemUris",
      "provenance",
      "fixes",
      "taxa",
      "webRequest",
      "webResponse",
      "properties"]);

   procedure Input_result
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out result;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 result_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  ruleId
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.ruleId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  ruleIndex
                     Reader.Read_Next;

                     Value.ruleIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.ruleIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  rule
                     Reader.Read_Next;

                     Value.rule :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_reportingDescriptorReference
                       (Reader, Value.rule.Value, Success);
                  when 4 =>  --  kind
                     Reader.Read_Next;

                     Value.kind :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_result_kind (Reader, Value.kind.Value, Success);
                  when 5 =>  --  level
                     Reader.Read_Next;

                     Value.level :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_result_level (Reader, Value.level.Value, Success);
                  when 6 =>  --  message
                     Reader.Read_Next;

                     Input_message (Reader, Value.message, Success);
                  when 7 =>  --  analysisTarget
                     Reader.Read_Next;

                     Value.analysisTarget :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactLocation
                       (Reader, Value.analysisTarget.Value, Success);
                  when 8 =>  --  locations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.locations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : location;
                           begin
                              Input_location (Reader, Item, Success);
                              Value.locations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  guid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.guid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  correlationGuid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.correlationGuid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  occurrenceCount
                     Reader.Read_Next;

                     Value.occurrenceCount :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.occurrenceCount.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  partialFingerprints
                     Reader.Read_Next;

                     Input_Any_Value
                       (Reader, Value.partialFingerprints, Success);
                  when 13 =>  --  fingerprints
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.fingerprints, Success);
                  when 14 =>  --  stacks
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.stacks.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : stack;
                           begin
                              Input_stack (Reader, Item, Success);
                              Value.stacks.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  codeFlows
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.codeFlows.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : codeFlow;
                           begin
                              Input_codeFlow (Reader, Item, Success);
                              Value.codeFlows.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  graphs
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.graphs.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : graph;
                           begin
                              Input_graph (Reader, Item, Success);
                              Value.graphs.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  graphTraversals
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.graphTraversals.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : graphTraversal;
                           begin
                              Input_graphTraversal (Reader, Item, Success);
                              Value.graphTraversals.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 18 =>  --  relatedLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.relatedLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : location;
                           begin
                              Input_location (Reader, Item, Success);
                              Value.relatedLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 19 =>  --  suppressions
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.suppressions.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : suppression;
                           begin
                              Input_suppression (Reader, Item, Success);
                              Value.suppressions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 20 =>  --  baselineState
                     Reader.Read_Next;

                     Value.baselineState :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_result_baselineState
                       (Reader, Value.baselineState.Value, Success);
                  when 21 =>  --  rank
                     Reader.Read_Next;

                     Value.rank :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value then
                        if Reader.Number_Value.Kind = VSS.JSON.JSON_Integer
                        then
                           Value.rank.Value :=
                             Float (Reader.Number_Value.Integer_Value);
                        elsif Reader.Number_Value.Kind = VSS.JSON.JSON_Float
                        then
                           Value.rank.Value :=
                             Float (Reader.Number_Value.Float_Value);
                        else
                           Success := False;
                        end if;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 22 =>  --  attachments
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.attachments.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : attachment;
                           begin
                              Input_attachment (Reader, Item, Success);
                              Value.attachments.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 23 =>  --  hostedViewerUri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.hostedViewerUri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 24 =>  --  workItemUris
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.workItemUris.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.workItemUris.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 25 =>  --  provenance
                     Reader.Read_Next;

                     Value.provenance :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_resultProvenance
                       (Reader, Value.provenance.Value, Success);
                  when 26 =>  --  fixes
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.fixes.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : fix;
                           begin
                              Input_fix (Reader, Item, Success);
                              Value.fixes.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 27 =>  --  taxa
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.taxa.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : reportingDescriptorReference;
                           begin
                              Input_reportingDescriptorReference
                                (Reader, Item, Success);
                              Value.taxa.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 28 =>  --  webRequest
                     Reader.Read_Next;

                     Value.webRequest :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_webRequest
                       (Reader, Value.webRequest.Value, Success);
                  when 29 =>  --  webResponse
                     Reader.Read_Next;

                     Value.webResponse :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_webResponse
                       (Reader, Value.webResponse.Value, Success);
                  when 30 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_result;

   package region_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["startLine",
      "startColumn",
      "endLine",
      "endColumn",
      "charOffset",
      "charLength",
      "byteOffset",
      "byteLength",
      "snippet",
      "message",
      "sourceLanguage",
      "properties"]);

   procedure Input_region
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out region;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 region_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  startLine
                     Reader.Read_Next;

                     Value.startLine :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.startLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  startColumn
                     Reader.Read_Next;

                     Value.startColumn :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.startColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  endLine
                     Reader.Read_Next;

                     Value.endLine :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endLine.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  endColumn
                     Reader.Read_Next;

                     Value.endColumn :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.endColumn.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  charOffset
                     Reader.Read_Next;

                     Value.charOffset :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.charOffset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  charLength
                     Reader.Read_Next;

                     Value.charLength :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.charLength.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  byteOffset
                     Reader.Read_Next;

                     Value.byteOffset :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.byteOffset.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  byteLength
                     Reader.Read_Next;

                     Value.byteLength :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.byteLength.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  snippet
                     Reader.Read_Next;

                     Value.snippet :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactContent
                       (Reader, Value.snippet.Value, Success);
                  when 10 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 11 =>  --  sourceLanguage
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.sourceLanguage := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 12 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_region;

   package artifactLocation_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["uri",
      "uriBaseId",
      "index",
      "description",
      "properties"]);

   procedure Input_artifactLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifactLocation;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 artifactLocation_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  uri
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.uri := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  uriBaseId
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.uriBaseId := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_artifactLocation;

   package graphTraversal_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["runGraphIndex",
      "resultGraphIndex",
      "description",
      "initialState",
      "immutableState",
      "edgeTraversals",
      "properties"]);

   procedure Input_graphTraversal
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out graphTraversal;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 graphTraversal_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  runGraphIndex
                     Reader.Read_Next;

                     Value.runGraphIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.runGraphIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  resultGraphIndex
                     Reader.Read_Next;

                     Value.resultGraphIndex :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.resultGraphIndex.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 4 =>  --  initialState
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.initialState, Success);
                  when 5 =>  --  immutableState
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.immutableState, Success);
                  when 6 =>  --  edgeTraversals
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.edgeTraversals.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : edgeTraversal;
                           begin
                              Input_edgeTraversal (Reader, Item, Success);
                              Value.edgeTraversals.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_graphTraversal;

   package attachment_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["description",
      "artifactLocation",
      "regions",
      "rectangles",
      "properties"]);

   procedure Input_attachment
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out attachment;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 attachment_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  description
                     Reader.Read_Next;

                     Value.description :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.description.Value, Success);
                  when 2 =>  --  artifactLocation
                     Reader.Read_Next;

                     Input_artifactLocation
                       (Reader, Value.artifactLocation, Success);
                  when 3 =>  --  regions
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.regions.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : region;
                           begin
                              Input_region (Reader, Item, Success);
                              Value.regions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  rectangles
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.rectangles.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : rectangle;
                           begin
                              Input_rectangle (Reader, Item, Success);
                              Value.rectangles.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_attachment;

   package stack_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["message",
      "frames",
      "properties"]);

   procedure Input_stack
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out stack;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 stack_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 2 =>  --  frames
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.frames.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : stackFrame;
                           begin
                              Input_stackFrame (Reader, Item, Success);
                              Value.frames.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_stack;

   package replacement_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["deletedRegion",
      "insertedContent",
      "properties"]);

   procedure Input_replacement
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out replacement;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 replacement_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  deletedRegion
                     Reader.Read_Next;

                     Input_region (Reader, Value.deletedRegion, Success);
                  when 2 =>  --  insertedContent
                     Reader.Read_Next;

                     Value.insertedContent :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactContent
                       (Reader, Value.insertedContent.Value, Success);
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_replacement;

   package run_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["tool",
      "invocations",
      "conversion",
      "language",
      "versionControlProvenance",
      "originalUriBaseIds",
      "artifacts",
      "logicalLocations",
      "graphs",
      "results",
      "automationDetails",
      "runAggregates",
      "baselineGuid",
      "redactionTokens",
      "defaultEncoding",
      "defaultSourceLanguage",
      "newlineSequences",
      "columnKind",
      "externalPropertyFileReferences",
      "threadFlowLocations",
      "taxonomies",
      "addresses",
      "translations",
      "policies",
      "webRequests",
      "webResponses",
      "specialLocations",
      "properties"]);

   procedure Input_run
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out run;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 run_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  tool
                     Reader.Read_Next;

                     Input_tool (Reader, Value.tool, Success);
                  when 2 =>  --  invocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.invocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : invocation;
                           begin
                              Input_invocation (Reader, Item, Success);
                              Value.invocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  conversion
                     Reader.Read_Next;

                     Value.conversion :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_conversion
                       (Reader, Value.conversion.Value, Success);
                  when 4 =>  --  language
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.language := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  versionControlProvenance
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.versionControlProvenance.Clear
                          (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : versionControlDetails;
                           begin
                              Input_versionControlDetails
                                (Reader, Item, Success);
                              Value.versionControlProvenance.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  originalUriBaseIds
                     Reader.Read_Next;

                     Input_Any_Value
                       (Reader, Value.originalUriBaseIds, Success);
                  when 7 =>  --  artifacts
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.artifacts.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : artifact;
                           begin
                              Input_artifact (Reader, Item, Success);
                              Value.artifacts.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  logicalLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.logicalLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : logicalLocation;
                           begin
                              Input_logicalLocation (Reader, Item, Success);
                              Value.logicalLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  graphs
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.graphs.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : graph;
                           begin
                              Input_graph (Reader, Item, Success);
                              Value.graphs.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  results
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.results.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : result;
                           begin
                              Input_result (Reader, Item, Success);
                              Value.results.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  automationDetails
                     Reader.Read_Next;

                     Value.automationDetails :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_runAutomationDetails
                       (Reader, Value.automationDetails.Value, Success);
                  when 12 =>  --  runAggregates
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.runAggregates.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : runAutomationDetails;
                           begin
                              Input_runAutomationDetails
                                (Reader, Item, Success);
                              Value.runAggregates.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  baselineGuid
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.baselineGuid := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  redactionTokens
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.redactionTokens.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.redactionTokens.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  defaultEncoding
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.defaultEncoding := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  defaultSourceLanguage
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.defaultSourceLanguage := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  newlineSequences
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.newlineSequences.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.newlineSequences.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 18 =>  --  columnKind
                     Reader.Read_Next;

                     Value.columnKind :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_run_columnKind
                       (Reader, Value.columnKind.Value, Success);
                  when 19 =>  --  externalPropertyFileReferences
                     Reader.Read_Next;

                     Value.externalPropertyFileReferences :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_externalPropertyFileReferences
                       (Reader, Value.externalPropertyFileReferences.Value,
                        Success);
                  when 20 =>  --  threadFlowLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.threadFlowLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : threadFlowLocation;
                           begin
                              Input_threadFlowLocation (Reader, Item, Success);
                              Value.threadFlowLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 21 =>  --  taxonomies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.taxonomies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.taxonomies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 22 =>  --  addresses
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.addresses.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : address;
                           begin
                              Input_address (Reader, Item, Success);
                              Value.addresses.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 23 =>  --  translations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.translations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.translations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 24 =>  --  policies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.policies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : toolComponent;
                           begin
                              Input_toolComponent (Reader, Item, Success);
                              Value.policies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 25 =>  --  webRequests
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.webRequests.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : webRequest;
                           begin
                              Input_webRequest (Reader, Item, Success);
                              Value.webRequests.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 26 =>  --  webResponses
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.webResponses.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : webResponse;
                           begin
                              Input_webResponse (Reader, Item, Success);
                              Value.webResponses.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 27 =>  --  specialLocations
                     Reader.Read_Next;

                     Value.specialLocations :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_specialLocations
                       (Reader, Value.specialLocations.Value, Success);
                  when 28 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_run;

   package externalPropertyFileReferences_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["conversion",
      "graphs",
      "externalizedProperties",
      "artifacts",
      "invocations",
      "logicalLocations",
      "threadFlowLocations",
      "results",
      "taxonomies",
      "addresses",
      "driver",
      "extensions",
      "policies",
      "translations",
      "webRequests",
      "webResponses",
      "properties"]);

   procedure Input_externalPropertyFileReferences
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out externalPropertyFileReferences;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 externalPropertyFileReferences_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  conversion
                     Reader.Read_Next;

                     Value.conversion :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_externalPropertyFileReference
                       (Reader, Value.conversion.Value, Success);
                  when 2 =>  --  graphs
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.graphs.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.graphs.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  externalizedProperties
                     Reader.Read_Next;

                     Value.externalizedProperties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_externalPropertyFileReference
                       (Reader, Value.externalizedProperties.Value, Success);
                  when 4 =>  --  artifacts
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.artifacts.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.artifacts.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  invocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.invocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.invocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  logicalLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.logicalLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.logicalLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  threadFlowLocations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.threadFlowLocations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.threadFlowLocations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 8 =>  --  results
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.results.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.results.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  taxonomies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.taxonomies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.taxonomies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  addresses
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.addresses.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.addresses.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  driver
                     Reader.Read_Next;

                     Value.driver :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_externalPropertyFileReference
                       (Reader, Value.driver.Value, Success);
                  when 12 =>  --  extensions
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.extensions.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.extensions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 13 =>  --  policies
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.policies.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.policies.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 14 =>  --  translations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.translations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.translations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 15 =>  --  webRequests
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.webRequests.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.webRequests.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 16 =>  --  webResponses
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.webResponses.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : externalPropertyFileReference;
                           begin
                              Input_externalPropertyFileReference
                                (Reader, Item, Success);
                              Value.webResponses.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 17 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_externalPropertyFileReferences;

   package a_exception_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["kind",
      "message",
      "stack",
      "innerExceptions",
      "properties"]);

   procedure Input_a_exception
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out a_exception;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 a_exception_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  kind
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.kind := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  message
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.message := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  stack
                     Reader.Read_Next;

                     Value.stack :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_stack (Reader, Value.stack.Value, Success);
                  when 4 =>  --  innerExceptions
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.innerExceptions.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : a_exception;
                           begin
                              Input_a_exception (Reader, Item, Success);
                              Value.innerExceptions.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_a_exception;

   package threadFlowLocation_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["index",
      "location",
      "stack",
      "kinds",
      "taxa",
      "module",
      "state",
      "nestingLevel",
      "executionOrder",
      "executionTimeUtc",
      "importance",
      "webRequest",
      "webResponse",
      "properties"]);

   procedure Input_threadFlowLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out threadFlowLocation;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 threadFlowLocation_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  location
                     Reader.Read_Next;

                     Value.location :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_location (Reader, Value.location.Value, Success);
                  when 3 =>  --  stack
                     Reader.Read_Next;

                     Value.stack :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_stack (Reader, Value.stack.Value, Success);
                  when 4 =>  --  kinds
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.kinds.Clear;
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : VSS.Strings.Virtual_String;
                           begin
                              if Reader.Is_String_Value then
                                 Item := Reader.String_Value;
                                 Reader.Read_Next;
                              else
                                 Success := False;
                              end if;
                              Value.kinds.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  taxa
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.taxa.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : reportingDescriptorReference;
                           begin
                              Input_reportingDescriptorReference
                                (Reader, Item, Success);
                              Value.taxa.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  module
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.module := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 7 =>  --  state
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.state, Success);
                  when 8 =>  --  nestingLevel
                     Reader.Read_Next;

                     Value.nestingLevel :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.nestingLevel.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  executionOrder
                     Reader.Read_Next;

                     Value.executionOrder :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.executionOrder.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 10 =>  --  executionTimeUtc
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.executionTimeUtc := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 11 =>  --  importance
                     Reader.Read_Next;

                     Value.importance :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_threadFlowLocation_importance
                       (Reader, Value.importance.Value, Success);
                  when 12 =>  --  webRequest
                     Reader.Read_Next;

                     Value.webRequest :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_webRequest
                       (Reader, Value.webRequest.Value, Success);
                  when 13 =>  --  webResponse
                     Reader.Read_Next;

                     Value.webResponse :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_webResponse
                       (Reader, Value.webResponse.Value, Success);
                  when 14 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_threadFlowLocation;

   package codeFlow_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["message",
      "threadFlows",
      "properties"]);

   procedure Input_codeFlow
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out codeFlow;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 codeFlow_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 2 =>  --  threadFlows
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.threadFlows.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : threadFlow;
                           begin
                              Input_threadFlow (Reader, Item, Success);
                              Value.threadFlows.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_codeFlow;

   package multiformatMessageString_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["text",
      "markdown",
      "properties"]);

   procedure Input_multiformatMessageString
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out multiformatMessageString;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 multiformatMessageString_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  text
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.text := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  markdown
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.markdown := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_multiformatMessageString;

   package artifactContent_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["text",
      "binary",
      "rendered",
      "properties"]);

   procedure Input_artifactContent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifactContent;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 artifactContent_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  text
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.text := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  binary
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.binary := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  rendered
                     Reader.Read_Next;

                     Value.rendered :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_multiformatMessageString
                       (Reader, Value.rendered.Value, Success);
                  when 4 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_artifactContent;

   package webResponse_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["index",
      "protocol",
      "version",
      "statusCode",
      "reasonPhrase",
      "headers",
      "body",
      "noResponseReceived",
      "properties"]);

   procedure Input_webResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out webResponse;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 webResponse_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  index
                     Reader.Read_Next;

                     Value.index :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.index.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  protocol
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.protocol := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 3 =>  --  version
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.version := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 4 =>  --  statusCode
                     Reader.Read_Next;

                     Value.statusCode :=
                       (Is_Set => True,
                        Value  => <>);
                     if Reader.Is_Number_Value
                       and then Reader.Number_Value.Kind =
                         VSS.JSON.JSON_Integer
                     then
                        Value.statusCode.Value :=
                          Integer (Reader.Number_Value.Integer_Value);
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 5 =>  --  reasonPhrase
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.reasonPhrase := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  headers
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.headers, Success);
                  when 7 =>  --  body
                     Reader.Read_Next;

                     Value.a_body :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_artifactContent
                       (Reader, Value.a_body.Value, Success);
                  when 8 =>  --  noResponseReceived
                     Reader.Read_Next;

                     if Reader.Is_Boolean_Value then
                        Value.noResponseReceived := Reader.Boolean_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 9 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_webResponse;

   package threadFlow_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["id",
      "message",
      "initialState",
      "immutableState",
      "locations",
      "properties"]);

   procedure Input_threadFlow
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out threadFlow;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 threadFlow_Minimal_Perfect_Hash.Get_Index (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  id
                     Reader.Read_Next;

                     if Reader.Is_String_Value then
                        Value.id := Reader.String_Value;
                        Reader.Read_Next;
                     else
                        Success := False;
                     end if;
                  when 2 =>  --  message
                     Reader.Read_Next;

                     Value.message :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_message (Reader, Value.message.Value, Success);
                  when 3 =>  --  initialState
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.initialState, Success);
                  when 4 =>  --  immutableState
                     Reader.Read_Next;

                     Input_Any_Value (Reader, Value.immutableState, Success);
                  when 5 =>  --  locations
                     Reader.Read_Next;

                     if Success and Reader.Is_Start_Array then
                        Reader.Read_Next;
                        Value.locations.Clear (Is_Null => False);
                        while Success and not Reader.Is_End_Array loop
                           declare
                              Item : threadFlowLocation;
                           begin
                              Input_threadFlowLocation (Reader, Item, Success);
                              Value.locations.Append (Item);
                           end;
                        end loop;
                        if Success then
                           Reader.Read_Next;  --  skip End_Array
                        end if;
                     else
                        Success := False;
                     end if;
                  when 6 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_threadFlow;

   package configurationOverride_Minimal_Perfect_Hash is new Minimal_Perfect_Hash
     (["configuration",
      "descriptor",
      "properties"]);

   procedure Input_configurationOverride
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out configurationOverride;
      Success : in out Boolean) is
   begin
      if Success and Reader.Is_Start_Object then
         Reader.Read_Next;
      else
         Success := False;
      end if;

      while Success and not Reader.Is_End_Object loop
         if Reader.Is_Key_Name then
            declare
               Index : constant Natural :=
                 configurationOverride_Minimal_Perfect_Hash.Get_Index
                   (Reader.Key_Name);
            begin

               case Index is
                  when 1 =>  --  configuration
                     Reader.Read_Next;

                     Input_reportingConfiguration
                       (Reader, Value.configuration, Success);
                  when 2 =>  --  descriptor
                     Reader.Read_Next;

                     Input_reportingDescriptorReference
                       (Reader, Value.descriptor, Success);
                  when 3 =>  --  properties
                     Reader.Read_Next;

                     Value.properties :=
                       (Is_Set => True,
                        Value  => <>);
                     Input_propertyBag
                       (Reader, Value.properties.Value, Success);
                  when others =>
                     Reader.Read_Next;
                     Reader.Skip_Current_Value;
               end case;
            end;
         else
            Success := False;
         end if;
      end loop;

      if Success then
         Reader.Read_Next;  --  skip End_Object
      end if;
   end Input_configurationOverride;

end SARIF.Types.Inputs;
