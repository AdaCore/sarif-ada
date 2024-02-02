--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Style_Checks ("M99");  --  suppress style warning unitl gnatpp is fixed
with Ada.Unchecked_Deallocation;

package body SARIF.Types is
   procedure Free is new Ada.Unchecked_Deallocation
     (configurationOverride_Array, configurationOverride_Array_Access);

   overriding procedure Adjust (Self : in out configurationOverride_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new configurationOverride_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out configurationOverride_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : configurationOverride_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out configurationOverride_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out configurationOverride_Vector;
      Value : configurationOverride) is
      Init_Length     : constant Positive                  :=
        Positive'Max (1, 256 / configurationOverride'Size);
      Self_Data_Saved : configurationOverride_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new configurationOverride_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new configurationOverride_Array'
             (Self.Data.all &
              configurationOverride_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_configurationOverride_Variable_Reference
     (Self  : aliased in out configurationOverride_Vector;
      Index : Positive)
      return configurationOverride_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_configurationOverride_Constant_Reference
     (Self  : aliased configurationOverride_Vector;
      Index : Positive)
      return configurationOverride_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (locationRelationship_Array, locationRelationship_Array_Access);

   overriding procedure Adjust (Self : in out locationRelationship_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new locationRelationship_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out locationRelationship_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : locationRelationship_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out locationRelationship_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out locationRelationship_Vector;
      Value : locationRelationship) is
      Init_Length     : constant Positive                 :=
        Positive'Max (1, 256 / locationRelationship'Size);
      Self_Data_Saved : locationRelationship_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new locationRelationship_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new locationRelationship_Array'
             (Self.Data.all &
              locationRelationship_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_locationRelationship_Variable_Reference
     (Self  : aliased in out locationRelationship_Vector;
      Index : Positive)
      return locationRelationship_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_locationRelationship_Constant_Reference
     (Self  : aliased locationRelationship_Vector;
      Index : Positive)
      return locationRelationship_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (threadFlowLocation_Array, threadFlowLocation_Array_Access);

   overriding procedure Adjust (Self : in out threadFlowLocation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new threadFlowLocation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out threadFlowLocation_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : threadFlowLocation_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out threadFlowLocation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out threadFlowLocation_Vector; Value : threadFlowLocation) is
      Init_Length     : constant Positive               :=
        Positive'Max (1, 256 / threadFlowLocation'Size);
      Self_Data_Saved : threadFlowLocation_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new threadFlowLocation_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new threadFlowLocation_Array'
             (Self.Data.all &
              threadFlowLocation_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_threadFlowLocation_Variable_Reference
     (Self  : aliased in out threadFlowLocation_Vector;
      Index : Positive)
      return threadFlowLocation_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_threadFlowLocation_Constant_Reference
     (Self  : aliased threadFlowLocation_Vector;
      Index : Positive)
      return threadFlowLocation_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (result_Array, result_Array_Access);

   overriding procedure Adjust (Self : in out result_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new result_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out result_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : result_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out result_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out result_Vector; Value : result) is
      Init_Length : constant Positive   := Positive'Max (1, 256 / result'Size);
      Self_Data_Saved : result_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new result_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new result_Array'
             (Self.Data.all & result_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_result_Variable_Reference
     (Self  : aliased in out result_Vector;
      Index : Positive)
      return result_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_result_Constant_Reference
     (Self  : aliased result_Vector;
      Index : Positive)
      return result_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (runAutomationDetails_Array, runAutomationDetails_Array_Access);

   overriding procedure Adjust (Self : in out runAutomationDetails_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new runAutomationDetails_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out runAutomationDetails_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : runAutomationDetails_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out runAutomationDetails_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out runAutomationDetails_Vector;
      Value : runAutomationDetails) is
      Init_Length     : constant Positive                 :=
        Positive'Max (1, 256 / runAutomationDetails'Size);
      Self_Data_Saved : runAutomationDetails_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new runAutomationDetails_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new runAutomationDetails_Array'
             (Self.Data.all &
              runAutomationDetails_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_runAutomationDetails_Variable_Reference
     (Self  : aliased in out runAutomationDetails_Vector;
      Index : Positive)
      return runAutomationDetails_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_runAutomationDetails_Constant_Reference
     (Self  : aliased runAutomationDetails_Vector;
      Index : Positive)
      return runAutomationDetails_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (replacement_Array, replacement_Array_Access);

   overriding procedure Adjust (Self : in out replacement_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new replacement_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out replacement_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : replacement_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out replacement_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out replacement_Vector; Value : replacement) is
      Init_Length     : constant Positive        :=
        Positive'Max (1, 256 / replacement'Size);
      Self_Data_Saved : replacement_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new replacement_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new replacement_Array'
             (Self.Data.all & replacement_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_replacement_Variable_Reference
     (Self  : aliased in out replacement_Vector;
      Index : Positive)
      return replacement_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_replacement_Constant_Reference
     (Self  : aliased replacement_Vector;
      Index : Positive)
      return replacement_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (webResponse_Array, webResponse_Array_Access);

   overriding procedure Adjust (Self : in out webResponse_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new webResponse_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out webResponse_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : webResponse_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out webResponse_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out webResponse_Vector; Value : webResponse) is
      Init_Length     : constant Positive        :=
        Positive'Max (1, 256 / webResponse'Size);
      Self_Data_Saved : webResponse_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new webResponse_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new webResponse_Array'
             (Self.Data.all & webResponse_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_webResponse_Variable_Reference
     (Self  : aliased in out webResponse_Vector;
      Index : Positive)
      return webResponse_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_webResponse_Constant_Reference
     (Self  : aliased webResponse_Vector;
      Index : Positive)
      return webResponse_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (address_Array, address_Array_Access);

   overriding procedure Adjust (Self : in out address_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new address_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out address_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : address_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out address_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out address_Vector; Value : address) is
      Init_Length : constant Positive := Positive'Max (1, 256 / address'Size);
      Self_Data_Saved : address_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new address_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new address_Array'
             (Self.Data.all & address_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_address_Variable_Reference
     (Self  : aliased in out address_Vector;
      Index : Positive)
      return address_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_address_Constant_Reference
     (Self  : aliased address_Vector;
      Index : Positive)
      return address_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (physicalLocation_Array, physicalLocation_Array_Access);

   overriding procedure Adjust (Self : in out physicalLocation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new physicalLocation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out physicalLocation_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : physicalLocation_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out physicalLocation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out physicalLocation_Vector; Value : physicalLocation) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / physicalLocation'Size);
      Self_Data_Saved : physicalLocation_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new physicalLocation_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new physicalLocation_Array'
             (Self.Data.all & physicalLocation_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_physicalLocation_Variable_Reference
     (Self  : aliased in out physicalLocation_Vector;
      Index : Positive)
      return physicalLocation_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_physicalLocation_Constant_Reference
     (Self  : aliased physicalLocation_Vector;
      Index : Positive)
      return physicalLocation_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (stackFrame_Array, stackFrame_Array_Access);

   overriding procedure Adjust (Self : in out stackFrame_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new stackFrame_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out stackFrame_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : stackFrame_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out stackFrame_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out stackFrame_Vector; Value : stackFrame) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / stackFrame'Size);
      Self_Data_Saved : stackFrame_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new stackFrame_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new stackFrame_Array'
             (Self.Data.all & stackFrame_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_stackFrame_Variable_Reference
     (Self  : aliased in out stackFrame_Vector;
      Index : Positive)
      return stackFrame_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_stackFrame_Constant_Reference
     (Self  : aliased stackFrame_Vector;
      Index : Positive)
      return stackFrame_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (toolComponent_Array, toolComponent_Array_Access);

   overriding procedure Adjust (Self : in out toolComponent_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new toolComponent_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out toolComponent_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : toolComponent_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out toolComponent_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out toolComponent_Vector; Value : toolComponent) is
      Init_Length     : constant Positive          :=
        Positive'Max (1, 256 / toolComponent'Size);
      Self_Data_Saved : toolComponent_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new toolComponent_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new toolComponent_Array'
             (Self.Data.all & toolComponent_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_toolComponent_Variable_Reference
     (Self  : aliased in out toolComponent_Vector;
      Index : Positive)
      return toolComponent_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_toolComponent_Constant_Reference
     (Self  : aliased toolComponent_Vector;
      Index : Positive)
      return toolComponent_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (externalPropertyFileReference_Array,
      externalPropertyFileReference_Array_Access);

   overriding procedure Adjust
     (Self : in out externalPropertyFileReference_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new externalPropertyFileReference_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out externalPropertyFileReference_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length
     (Self : externalPropertyFileReference_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out externalPropertyFileReference_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out externalPropertyFileReference_Vector;
      Value : externalPropertyFileReference) is
      Init_Length     : constant Positive                          :=
        Positive'Max (1, 256 / externalPropertyFileReference'Size);
      Self_Data_Saved : externalPropertyFileReference_Array_Access :=
        Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data :=
           new externalPropertyFileReference_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new externalPropertyFileReference_Array'
             (Self.Data.all &
              externalPropertyFileReference_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_externalPropertyFileReference_Variable_Reference
     (Self  : aliased in out externalPropertyFileReference_Vector;
      Index : Positive)
      return externalPropertyFileReference_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_externalPropertyFileReference_Constant_Reference
     (Self  : aliased externalPropertyFileReference_Vector;
      Index : Positive)
      return externalPropertyFileReference_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (stack_Array, stack_Array_Access);

   overriding procedure Adjust (Self : in out stack_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new stack_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out stack_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : stack_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out stack_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out stack_Vector; Value : stack) is
      Init_Length : constant Positive  := Positive'Max (1, 256 / stack'Size);
      Self_Data_Saved : stack_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new stack_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new stack_Array'
             (Self.Data.all & stack_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_stack_Variable_Reference
     (Self  : aliased in out stack_Vector;
      Index : Positive)
      return stack_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_stack_Constant_Reference
     (Self  : aliased stack_Vector;
      Index : Positive)
      return stack_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Integer_Array, Integer_Array_Access);

   overriding procedure Adjust (Self : in out Integer_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new Integer_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out Integer_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : Integer_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out Integer_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out Integer_Vector; Value : Integer) is
      Init_Length : constant Positive := Positive'Max (1, 256 / Integer'Size);
      Self_Data_Saved : Integer_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new Integer_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new Integer_Array'
             (Self.Data.all & Integer_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_Integer_Variable_Reference
     (Self  : aliased in out Integer_Vector;
      Index : Positive)
      return Integer_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_Integer_Constant_Reference
     (Self  : aliased Integer_Vector;
      Index : Positive)
      return Integer_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (notification_Array, notification_Array_Access);

   overriding procedure Adjust (Self : in out notification_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new notification_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out notification_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : notification_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out notification_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out notification_Vector; Value : notification) is
      Init_Length     : constant Positive         :=
        Positive'Max (1, 256 / notification'Size);
      Self_Data_Saved : notification_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new notification_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new notification_Array'
             (Self.Data.all & notification_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_notification_Variable_Reference
     (Self  : aliased in out notification_Vector;
      Index : Positive)
      return notification_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_notification_Constant_Reference
     (Self  : aliased notification_Vector;
      Index : Positive)
      return notification_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (attachment_Array, attachment_Array_Access);

   overriding procedure Adjust (Self : in out attachment_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new attachment_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out attachment_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : attachment_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out attachment_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out attachment_Vector; Value : attachment) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / attachment'Size);
      Self_Data_Saved : attachment_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new attachment_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new attachment_Array'
             (Self.Data.all & attachment_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_attachment_Variable_Reference
     (Self  : aliased in out attachment_Vector;
      Index : Positive)
      return attachment_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_attachment_Constant_Reference
     (Self  : aliased attachment_Vector;
      Index : Positive)
      return attachment_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (suppression_Array, suppression_Array_Access);

   overriding procedure Adjust (Self : in out suppression_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new suppression_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out suppression_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : suppression_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out suppression_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out suppression_Vector; Value : suppression) is
      Init_Length     : constant Positive        :=
        Positive'Max (1, 256 / suppression'Size);
      Self_Data_Saved : suppression_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new suppression_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new suppression_Array'
             (Self.Data.all & suppression_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_suppression_Variable_Reference
     (Self  : aliased in out suppression_Vector;
      Index : Positive)
      return suppression_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_suppression_Constant_Reference
     (Self  : aliased suppression_Vector;
      Index : Positive)
      return suppression_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (edgeTraversal_Array, edgeTraversal_Array_Access);

   overriding procedure Adjust (Self : in out edgeTraversal_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new edgeTraversal_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out edgeTraversal_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : edgeTraversal_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out edgeTraversal_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out edgeTraversal_Vector; Value : edgeTraversal) is
      Init_Length     : constant Positive          :=
        Positive'Max (1, 256 / edgeTraversal'Size);
      Self_Data_Saved : edgeTraversal_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new edgeTraversal_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new edgeTraversal_Array'
             (Self.Data.all & edgeTraversal_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_edgeTraversal_Variable_Reference
     (Self  : aliased in out edgeTraversal_Vector;
      Index : Positive)
      return edgeTraversal_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_edgeTraversal_Constant_Reference
     (Self  : aliased edgeTraversal_Vector;
      Index : Positive)
      return edgeTraversal_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (graphTraversal_Array, graphTraversal_Array_Access);

   overriding procedure Adjust (Self : in out graphTraversal_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new graphTraversal_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out graphTraversal_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : graphTraversal_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out graphTraversal_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out graphTraversal_Vector; Value : graphTraversal) is
      Init_Length     : constant Positive           :=
        Positive'Max (1, 256 / graphTraversal'Size);
      Self_Data_Saved : graphTraversal_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new graphTraversal_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new graphTraversal_Array'
             (Self.Data.all & graphTraversal_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_graphTraversal_Variable_Reference
     (Self  : aliased in out graphTraversal_Vector;
      Index : Positive)
      return graphTraversal_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_graphTraversal_Constant_Reference
     (Self  : aliased graphTraversal_Vector;
      Index : Positive)
      return graphTraversal_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (location_Array, location_Array_Access);

   overriding procedure Adjust (Self : in out location_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new location_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out location_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : location_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out location_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out location_Vector; Value : location) is
      Init_Length : constant Positive := Positive'Max (1, 256 / location'Size);
      Self_Data_Saved : location_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new location_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new location_Array'
             (Self.Data.all & location_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_location_Variable_Reference
     (Self  : aliased in out location_Vector;
      Index : Positive)
      return location_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_location_Constant_Reference
     (Self  : aliased location_Vector;
      Index : Positive)
      return location_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (graph_Array, graph_Array_Access);

   overriding procedure Adjust (Self : in out graph_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new graph_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out graph_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : graph_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out graph_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out graph_Vector; Value : graph) is
      Init_Length : constant Positive  := Positive'Max (1, 256 / graph'Size);
      Self_Data_Saved : graph_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new graph_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new graph_Array'
             (Self.Data.all & graph_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_graph_Variable_Reference
     (Self  : aliased in out graph_Vector;
      Index : Positive)
      return graph_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_graph_Constant_Reference
     (Self  : aliased graph_Vector;
      Index : Positive)
      return graph_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (reportingDescriptorReference_Array,
      reportingDescriptorReference_Array_Access);

   overriding procedure Adjust
     (Self : in out reportingDescriptorReference_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new reportingDescriptorReference_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out reportingDescriptorReference_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length
     (Self : reportingDescriptorReference_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out reportingDescriptorReference_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out reportingDescriptorReference_Vector;
      Value : reportingDescriptorReference) is
      Init_Length     : constant Positive                         :=
        Positive'Max (1, 256 / reportingDescriptorReference'Size);
      Self_Data_Saved : reportingDescriptorReference_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data :=
           new reportingDescriptorReference_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new reportingDescriptorReference_Array'
             (Self.Data.all &
              reportingDescriptorReference_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_reportingDescriptorReference_Variable_Reference
     (Self  : aliased in out reportingDescriptorReference_Vector;
      Index : Positive)
      return reportingDescriptorReference_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_reportingDescriptorReference_Constant_Reference
     (Self  : aliased reportingDescriptorReference_Vector;
      Index : Positive)
      return reportingDescriptorReference_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (reportingDescriptorRelationship_Array,
      reportingDescriptorRelationship_Array_Access);

   overriding procedure Adjust
     (Self : in out reportingDescriptorRelationship_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new reportingDescriptorRelationship_Array'
             (Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out reportingDescriptorRelationship_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length
     (Self : reportingDescriptorRelationship_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out reportingDescriptorRelationship_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out reportingDescriptorRelationship_Vector;
      Value : reportingDescriptorRelationship) is
      Init_Length     : constant Positive                            :=
        Positive'Max (1, 256 / reportingDescriptorRelationship'Size);
      Self_Data_Saved : reportingDescriptorRelationship_Array_Access :=
        Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data :=
           new reportingDescriptorRelationship_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new reportingDescriptorRelationship_Array'
             (Self.Data.all &
              reportingDescriptorRelationship_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_reportingDescriptorRelationship_Variable_Reference
     (Self  : aliased in out reportingDescriptorRelationship_Vector;
      Index : Positive)
      return reportingDescriptorRelationship_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_reportingDescriptorRelationship_Constant_Reference
     (Self  : aliased reportingDescriptorRelationship_Vector;
      Index : Positive)
      return reportingDescriptorRelationship_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (region_Array, region_Array_Access);

   overriding procedure Adjust (Self : in out region_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new region_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out region_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : region_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out region_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out region_Vector; Value : region) is
      Init_Length : constant Positive   := Positive'Max (1, 256 / region'Size);
      Self_Data_Saved : region_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new region_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new region_Array'
             (Self.Data.all & region_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_region_Variable_Reference
     (Self  : aliased in out region_Vector;
      Index : Positive)
      return region_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_region_Constant_Reference
     (Self  : aliased region_Vector;
      Index : Positive)
      return region_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (versionControlDetails_Array, versionControlDetails_Array_Access);

   overriding procedure Adjust (Self : in out versionControlDetails_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new versionControlDetails_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out versionControlDetails_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : versionControlDetails_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out versionControlDetails_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out versionControlDetails_Vector;
      Value : versionControlDetails) is
      Init_Length     : constant Positive                  :=
        Positive'Max (1, 256 / versionControlDetails'Size);
      Self_Data_Saved : versionControlDetails_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new versionControlDetails_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new versionControlDetails_Array'
             (Self.Data.all &
              versionControlDetails_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_versionControlDetails_Variable_Reference
     (Self  : aliased in out versionControlDetails_Vector;
      Index : Positive)
      return versionControlDetails_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_versionControlDetails_Constant_Reference
     (Self  : aliased versionControlDetails_Vector;
      Index : Positive)
      return versionControlDetails_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (rectangle_Array, rectangle_Array_Access);

   overriding procedure Adjust (Self : in out rectangle_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new rectangle_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out rectangle_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : rectangle_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out rectangle_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out rectangle_Vector; Value : rectangle) is
      Init_Length     : constant Positive      :=
        Positive'Max (1, 256 / rectangle'Size);
      Self_Data_Saved : rectangle_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new rectangle_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new rectangle_Array'
             (Self.Data.all & rectangle_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_rectangle_Variable_Reference
     (Self  : aliased in out rectangle_Vector;
      Index : Positive)
      return rectangle_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_rectangle_Constant_Reference
     (Self  : aliased rectangle_Vector;
      Index : Positive)
      return rectangle_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (codeFlow_Array, codeFlow_Array_Access);

   overriding procedure Adjust (Self : in out codeFlow_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new codeFlow_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out codeFlow_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : codeFlow_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out codeFlow_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out codeFlow_Vector; Value : codeFlow) is
      Init_Length : constant Positive := Positive'Max (1, 256 / codeFlow'Size);
      Self_Data_Saved : codeFlow_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new codeFlow_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new codeFlow_Array'
             (Self.Data.all & codeFlow_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_codeFlow_Variable_Reference
     (Self  : aliased in out codeFlow_Vector;
      Index : Positive)
      return codeFlow_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_codeFlow_Constant_Reference
     (Self  : aliased codeFlow_Vector;
      Index : Positive)
      return codeFlow_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (toolComponentReference_Array, toolComponentReference_Array_Access);

   overriding procedure Adjust (Self : in out toolComponentReference_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new toolComponentReference_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out toolComponentReference_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : toolComponentReference_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out toolComponentReference_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out toolComponentReference_Vector;
      Value : toolComponentReference) is
      Init_Length     : constant Positive                   :=
        Positive'Max (1, 256 / toolComponentReference'Size);
      Self_Data_Saved : toolComponentReference_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new toolComponentReference_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new toolComponentReference_Array'
             (Self.Data.all &
              toolComponentReference_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_toolComponentReference_Variable_Reference
     (Self  : aliased in out toolComponentReference_Vector;
      Index : Positive)
      return toolComponentReference_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_toolComponentReference_Constant_Reference
     (Self  : aliased toolComponentReference_Vector;
      Index : Positive)
      return toolComponentReference_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (edge_Array, edge_Array_Access);

   overriding procedure Adjust (Self : in out edge_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new edge_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out edge_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : edge_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out edge_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out edge_Vector; Value : edge) is
      Init_Length     : constant Positive := Positive'Max (1, 256 / edge'Size);
      Self_Data_Saved : edge_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new edge_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new edge_Array'
             (Self.Data.all & edge_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_edge_Variable_Reference
     (Self  : aliased in out edge_Vector;
      Index : Positive)
      return edge_Variable_Reference is (Element => Self.Data (Index)'Access);

   not overriding function Get_edge_Constant_Reference
     (Self  : aliased edge_Vector;
      Index : Positive)
      return edge_Constant_Reference is (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (toolComponent_contents_Array, toolComponent_contents_Array_Access);

   overriding procedure Adjust (Self : in out toolComponent_contents_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new toolComponent_contents_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize
     (Self : in out toolComponent_contents_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : toolComponent_contents_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out toolComponent_contents_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self  : in out toolComponent_contents_Vector;
      Value : Enum.toolComponent_contents) is
      Init_Length     : constant Positive                   :=
        Positive'Max (1, 256 / Enum.toolComponent_contents'Size);
      Self_Data_Saved : toolComponent_contents_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new toolComponent_contents_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new toolComponent_contents_Array'
             (Self.Data.all &
              toolComponent_contents_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_toolComponent_contents_Variable_Reference
     (Self  : aliased in out toolComponent_contents_Vector;
      Index : Positive)
      return toolComponent_contents_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_toolComponent_contents_Constant_Reference
     (Self  : aliased toolComponent_contents_Vector;
      Index : Positive)
      return toolComponent_contents_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (node_Array, node_Array_Access);

   overriding procedure Adjust (Self : in out node_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new node_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out node_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : node_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out node_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out node_Vector; Value : node) is
      Init_Length     : constant Positive := Positive'Max (1, 256 / node'Size);
      Self_Data_Saved : node_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new node_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new node_Array'
             (Self.Data.all & node_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_node_Variable_Reference
     (Self  : aliased in out node_Vector;
      Index : Positive)
      return node_Variable_Reference is (Element => Self.Data (Index)'Access);

   not overriding function Get_node_Constant_Reference
     (Self  : aliased node_Vector;
      Index : Positive)
      return node_Constant_Reference is (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (threadFlow_Array, threadFlow_Array_Access);

   overriding procedure Adjust (Self : in out threadFlow_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new threadFlow_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out threadFlow_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : threadFlow_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out threadFlow_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out threadFlow_Vector; Value : threadFlow) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / threadFlow'Size);
      Self_Data_Saved : threadFlow_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new threadFlow_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new threadFlow_Array'
             (Self.Data.all & threadFlow_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_threadFlow_Variable_Reference
     (Self  : aliased in out threadFlow_Vector;
      Index : Positive)
      return threadFlow_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_threadFlow_Constant_Reference
     (Self  : aliased threadFlow_Vector;
      Index : Positive)
      return threadFlow_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (fix_Array, fix_Array_Access);

   overriding procedure Adjust (Self : in out fix_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new fix_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out fix_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : fix_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out fix_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out fix_Vector; Value : fix) is
      Init_Length     : constant Positive := Positive'Max (1, 256 / fix'Size);
      Self_Data_Saved : fix_Array_Access  := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new fix_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new fix_Array'(Self.Data.all & fix_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_fix_Variable_Reference
     (Self  : aliased in out fix_Vector;
      Index : Positive)
      return fix_Variable_Reference is (Element => Self.Data (Index)'Access);

   not overriding function Get_fix_Constant_Reference
     (Self  : aliased fix_Vector;
      Index : Positive)
      return fix_Constant_Reference is (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (invocation_Array, invocation_Array_Access);

   overriding procedure Adjust (Self : in out invocation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new invocation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out invocation_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : invocation_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out invocation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out invocation_Vector; Value : invocation) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / invocation'Size);
      Self_Data_Saved : invocation_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new invocation_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new invocation_Array'
             (Self.Data.all & invocation_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_invocation_Variable_Reference
     (Self  : aliased in out invocation_Vector;
      Index : Positive)
      return invocation_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_invocation_Constant_Reference
     (Self  : aliased invocation_Vector;
      Index : Positive)
      return invocation_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (artifactChange_Array, artifactChange_Array_Access);

   overriding procedure Adjust (Self : in out artifactChange_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new artifactChange_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out artifactChange_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : artifactChange_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out artifactChange_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out artifactChange_Vector; Value : artifactChange) is
      Init_Length     : constant Positive           :=
        Positive'Max (1, 256 / artifactChange'Size);
      Self_Data_Saved : artifactChange_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new artifactChange_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new artifactChange_Array'
             (Self.Data.all & artifactChange_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_artifactChange_Variable_Reference
     (Self  : aliased in out artifactChange_Vector;
      Index : Positive)
      return artifactChange_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_artifactChange_Constant_Reference
     (Self  : aliased artifactChange_Vector;
      Index : Positive)
      return artifactChange_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (artifact_roles_Array, artifact_roles_Array_Access);

   overriding procedure Adjust (Self : in out artifact_roles_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new artifact_roles_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out artifact_roles_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : artifact_roles_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out artifact_roles_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out artifact_roles_Vector; Value : Enum.artifact_roles) is
      Init_Length     : constant Positive           :=
        Positive'Max (1, 256 / Enum.artifact_roles'Size);
      Self_Data_Saved : artifact_roles_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new artifact_roles_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new artifact_roles_Array'
             (Self.Data.all & artifact_roles_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_artifact_roles_Variable_Reference
     (Self  : aliased in out artifact_roles_Vector;
      Index : Positive)
      return artifact_roles_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_artifact_roles_Constant_Reference
     (Self  : aliased artifact_roles_Vector;
      Index : Positive)
      return artifact_roles_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (artifact_Array, artifact_Array_Access);

   overriding procedure Adjust (Self : in out artifact_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new artifact_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out artifact_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : artifact_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out artifact_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out artifact_Vector; Value : artifact) is
      Init_Length : constant Positive := Positive'Max (1, 256 / artifact'Size);
      Self_Data_Saved : artifact_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new artifact_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new artifact_Array'
             (Self.Data.all & artifact_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_artifact_Variable_Reference
     (Self  : aliased in out artifact_Vector;
      Index : Positive)
      return artifact_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_artifact_Constant_Reference
     (Self  : aliased artifact_Vector;
      Index : Positive)
      return artifact_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (logicalLocation_Array, logicalLocation_Array_Access);

   overriding procedure Adjust (Self : in out logicalLocation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new logicalLocation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out logicalLocation_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : logicalLocation_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out logicalLocation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out logicalLocation_Vector; Value : logicalLocation) is
      Init_Length     : constant Positive            :=
        Positive'Max (1, 256 / logicalLocation'Size);
      Self_Data_Saved : logicalLocation_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new logicalLocation_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new logicalLocation_Array'
             (Self.Data.all & logicalLocation_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_logicalLocation_Variable_Reference
     (Self  : aliased in out logicalLocation_Vector;
      Index : Positive)
      return logicalLocation_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_logicalLocation_Constant_Reference
     (Self  : aliased logicalLocation_Vector;
      Index : Positive)
      return logicalLocation_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (webRequest_Array, webRequest_Array_Access);

   overriding procedure Adjust (Self : in out webRequest_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new webRequest_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out webRequest_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : webRequest_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out webRequest_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out webRequest_Vector; Value : webRequest) is
      Init_Length     : constant Positive       :=
        Positive'Max (1, 256 / webRequest'Size);
      Self_Data_Saved : webRequest_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new webRequest_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new webRequest_Array'
             (Self.Data.all & webRequest_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_webRequest_Variable_Reference
     (Self  : aliased in out webRequest_Vector;
      Index : Positive)
      return webRequest_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_webRequest_Constant_Reference
     (Self  : aliased webRequest_Vector;
      Index : Positive)
      return webRequest_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (externalProperties_Array, externalProperties_Array_Access);

   overriding procedure Adjust (Self : in out externalProperties_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new externalProperties_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out externalProperties_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : externalProperties_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out externalProperties_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out externalProperties_Vector; Value : externalProperties) is
      Init_Length     : constant Positive               :=
        Positive'Max (1, 256 / externalProperties'Size);
      Self_Data_Saved : externalProperties_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new externalProperties_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new externalProperties_Array'
             (Self.Data.all &
              externalProperties_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_externalProperties_Variable_Reference
     (Self  : aliased in out externalProperties_Vector;
      Index : Positive)
      return externalProperties_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_externalProperties_Constant_Reference
     (Self  : aliased externalProperties_Vector;
      Index : Positive)
      return externalProperties_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (run_Array, run_Array_Access);

   overriding procedure Adjust (Self : in out run_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new run_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out run_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : run_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out run_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out run_Vector; Value : run) is
      Init_Length     : constant Positive := Positive'Max (1, 256 / run'Size);
      Self_Data_Saved : run_Array_Access  := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new run_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new run_Array'(Self.Data.all & run_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_run_Variable_Reference
     (Self  : aliased in out run_Vector;
      Index : Positive)
      return run_Variable_Reference is (Element => Self.Data (Index)'Access);

   not overriding function Get_run_Constant_Reference
     (Self  : aliased run_Vector;
      Index : Positive)
      return run_Constant_Reference is (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (a_exception_Array, a_exception_Array_Access);

   overriding procedure Adjust (Self : in out a_exception_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data := new a_exception_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out a_exception_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : a_exception_Vector) return Natural is (Self.Length);

   procedure Clear (Self : in out a_exception_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append (Self : in out a_exception_Vector; Value : a_exception) is
      Init_Length     : constant Positive        :=
        Positive'Max (1, 256 / a_exception'Size);
      Self_Data_Saved : a_exception_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new a_exception_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new a_exception_Array'
             (Self.Data.all & a_exception_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_a_exception_Variable_Reference
     (Self  : aliased in out a_exception_Vector;
      Index : Positive)
      return a_exception_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_a_exception_Constant_Reference
     (Self  : aliased a_exception_Vector;
      Index : Positive)
      return a_exception_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (reportingDescriptor_Array, reportingDescriptor_Array_Access);

   overriding procedure Adjust (Self : in out reportingDescriptor_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new reportingDescriptor_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out reportingDescriptor_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : reportingDescriptor_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out reportingDescriptor_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out reportingDescriptor_Vector; Value : reportingDescriptor) is
      Init_Length     : constant Positive                :=
        Positive'Max (1, 256 / reportingDescriptor'Size);
      Self_Data_Saved : reportingDescriptor_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new reportingDescriptor_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new reportingDescriptor_Array'
             (Self.Data.all &
              reportingDescriptor_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_reportingDescriptor_Variable_Reference
     (Self  : aliased in out reportingDescriptor_Vector;
      Index : Positive)
      return reportingDescriptor_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_reportingDescriptor_Constant_Reference
     (Self  : aliased reportingDescriptor_Vector;
      Index : Positive)
      return reportingDescriptor_Constant_Reference is
     (Element => Self.Data (Index)'Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (artifactLocation_Array, artifactLocation_Array_Access);

   overriding procedure Adjust (Self : in out artifactLocation_Vector) is
   begin
      if Self.Length > 0 then
         Self.Data :=
           new artifactLocation_Array'(Self.Data (1 .. Self.Length));
      end if;
   end Adjust;

   overriding procedure Finalize (Self : in out artifactLocation_Vector) is
   begin
      Free (Self.Data);
      Self.Length := 0;
   end Finalize;

   function Length (Self : artifactLocation_Vector) return Natural is
     (Self.Length);

   procedure Clear (Self : in out artifactLocation_Vector) is
   begin
      Self.Length := 0;
   end Clear;

   procedure Append
     (Self : in out artifactLocation_Vector; Value : artifactLocation) is
      Init_Length     : constant Positive             :=
        Positive'Max (1, 256 / artifactLocation'Size);
      Self_Data_Saved : artifactLocation_Array_Access := Self.Data;
   begin
      if Self.Length = 0 then
         Self.Data := new artifactLocation_Array (1 .. Init_Length);
      elsif Self.Length = Self.Data'Last then
         Self.Data :=
           new artifactLocation_Array'
             (Self.Data.all & artifactLocation_Array'(1 .. Self.Length => <>));
         Free (Self_Data_Saved);
      end if;
      Self.Length             := Self.Length + 1;
      Self.Data (Self.Length) := Value;
   end Append;

   not overriding function Get_artifactLocation_Variable_Reference
     (Self  : aliased in out artifactLocation_Vector;
      Index : Positive)
      return artifactLocation_Variable_Reference is
     (Element => Self.Data (Index)'Access);

   not overriding function Get_artifactLocation_Constant_Reference
     (Self  : aliased artifactLocation_Vector;
      Index : Positive)
      return artifactLocation_Constant_Reference is
     (Element => Self.Data (Index)'Access);

end SARIF.Types;
