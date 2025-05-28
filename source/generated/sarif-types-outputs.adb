--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces;
package body SARIF.Types.Outputs is
   pragma Style_Checks (Off);
   procedure Output_Any_Value
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Any_Value'Class) is
   begin
      for Item of Value loop
         case Item.Kind is
            when VSS.JSON.Streams.Start_Array =>
               Handler.Start_Array;
            when VSS.JSON.Streams.End_Array =>
               Handler.End_Array;
            when VSS.JSON.Streams.Start_Object =>
               Handler.Start_Object;
            when VSS.JSON.Streams.End_Object =>
               Handler.End_Object;
            when VSS.JSON.Streams.Key_Name =>
               Handler.Key_Name (Item.Key_Name);
            when VSS.JSON.Streams.String_Value =>
               Handler.String_Value (Item.String_Value);
            when VSS.JSON.Streams.Number_Value =>
               Handler.Number_Value (Item.Number_Value);
            when VSS.JSON.Streams.Boolean_Value =>
               Handler.Boolean_Value (Item.Boolean_Value);
            when VSS.JSON.Streams.Null_Value =>
               Handler.Null_Value;
            when VSS.JSON.Streams.None =>
               null;
            when VSS.JSON.Streams.Invalid =>
               raise Program_Error;
            when VSS.JSON.Streams.Start_Document =>
               raise Program_Error;
            when VSS.JSON.Streams.End_Document =>
               raise Program_Error;
            when VSS.JSON.Streams.Comment =>
               raise Program_Error;
         end case;
      end loop;
   end Output_Any_Value;

   procedure Output_notification_level
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.notification_level) is
   begin
      case Value is
         when Enum.none =>
            Handler.String_Value ("none");
         when Enum.note =>
            Handler.String_Value ("note");
         when Enum.warning =>
            Handler.String_Value ("warning");
         when Enum.error =>
            Handler.String_Value ("error");
      end case;
   end Output_notification_level;

   procedure Output_suppression_kind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.suppression_kind) is
   begin
      case Value is
         when Enum.inSource =>
            Handler.String_Value ("inSource");
         when Enum.external =>
            Handler.String_Value ("external");
      end case;
   end Output_suppression_kind;

   procedure Output_suppression_status
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.suppression_status) is
   begin
      case Value is
         when Enum.accepted =>
            Handler.String_Value ("accepted");
         when Enum.underReview =>
            Handler.String_Value ("underReview");
         when Enum.rejected =>
            Handler.String_Value ("rejected");
      end case;
   end Output_suppression_status;

   procedure Output_toolComponent_contents
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.toolComponent_contents) is
   begin
      case Value is
         when Enum.localizedData =>
            Handler.String_Value ("localizedData");
         when Enum.nonLocalizedData =>
            Handler.String_Value ("nonLocalizedData");
      end case;
   end Output_toolComponent_contents;

   procedure Output_artifact_roles
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.artifact_roles) is
   begin
      case Value is
         when Enum.analysisTarget =>
            Handler.String_Value ("analysisTarget");
         when Enum.attachment =>
            Handler.String_Value ("attachment");
         when Enum.responseFile =>
            Handler.String_Value ("responseFile");
         when Enum.resultFile =>
            Handler.String_Value ("resultFile");
         when Enum.standardStream =>
            Handler.String_Value ("standardStream");
         when Enum.tracedFile =>
            Handler.String_Value ("tracedFile");
         when Enum.unmodified =>
            Handler.String_Value ("unmodified");
         when Enum.modified =>
            Handler.String_Value ("modified");
         when Enum.added =>
            Handler.String_Value ("added");
         when Enum.deleted =>
            Handler.String_Value ("deleted");
         when Enum.renamed =>
            Handler.String_Value ("renamed");
         when Enum.uncontrolled =>
            Handler.String_Value ("uncontrolled");
         when Enum.driver =>
            Handler.String_Value ("driver");
         when Enum.extension =>
            Handler.String_Value ("extension");
         when Enum.translation =>
            Handler.String_Value ("translation");
         when Enum.taxonomy =>
            Handler.String_Value ("taxonomy");
         when Enum.policy =>
            Handler.String_Value ("policy");
         when Enum.referencedOnCommandLine =>
            Handler.String_Value ("referencedOnCommandLine");
         when Enum.memoryContents =>
            Handler.String_Value ("memoryContents");
         when Enum.directory =>
            Handler.String_Value ("directory");
         when Enum.userSpecifiedConfiguration =>
            Handler.String_Value ("userSpecifiedConfiguration");
         when Enum.toolSpecifiedConfiguration =>
            Handler.String_Value ("toolSpecifiedConfiguration");
         when Enum.debugOutputFile =>
            Handler.String_Value ("debugOutputFile");
      end case;
   end Output_artifact_roles;

   procedure Output_reportingConfiguration_level
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.reportingConfiguration_level) is
   begin
      case Value is
         when Enum.none =>
            Handler.String_Value ("none");
         when Enum.note =>
            Handler.String_Value ("note");
         when Enum.warning =>
            Handler.String_Value ("warning");
         when Enum.error =>
            Handler.String_Value ("error");
      end case;
   end Output_reportingConfiguration_level;

   procedure Output_result_kind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.result_kind) is
   begin
      case Value is
         when Enum.notApplicable =>
            Handler.String_Value ("notApplicable");
         when Enum.pass =>
            Handler.String_Value ("pass");
         when Enum.fail =>
            Handler.String_Value ("fail");
         when Enum.review =>
            Handler.String_Value ("review");
         when Enum.open =>
            Handler.String_Value ("open");
         when Enum.informational =>
            Handler.String_Value ("informational");
      end case;
   end Output_result_kind;

   procedure Output_result_level
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.result_level) is
   begin
      case Value is
         when Enum.none =>
            Handler.String_Value ("none");
         when Enum.note =>
            Handler.String_Value ("note");
         when Enum.warning =>
            Handler.String_Value ("warning");
         when Enum.error =>
            Handler.String_Value ("error");
      end case;
   end Output_result_level;

   procedure Output_result_baselineState
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.result_baselineState) is
   begin
      case Value is
         when Enum.a_new =>
            Handler.String_Value ("new");
         when Enum.unchanged =>
            Handler.String_Value ("unchanged");
         when Enum.updated =>
            Handler.String_Value ("updated");
         when Enum.absent =>
            Handler.String_Value ("absent");
      end case;
   end Output_result_baselineState;

   procedure Output_run_columnKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.run_columnKind) is
   begin
      case Value is
         when Enum.utf16CodeUnits =>
            Handler.String_Value ("utf16CodeUnits");
         when Enum.unicodeCodePoints =>
            Handler.String_Value ("unicodeCodePoints");
      end case;
   end Output_run_columnKind;

   procedure Output_threadFlowLocation_importance
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Enum.threadFlowLocation_importance) is
   begin
      case Value is
         when Enum.important =>
            Handler.String_Value ("important");
         when Enum.essential =>
            Handler.String_Value ("essential");
         when Enum.unimportant =>
            Handler.String_Value ("unimportant");
      end case;
   end Output_threadFlowLocation_importance;

   procedure Output_node
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : node) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.String_Value (Value.id);
      if Value.label.Is_Set then
         Handler.Key_Name ("label");
         Output_message (Handler, Value.label.Value);
      end if;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_location (Handler, Value.location.Value);
      end if;
      if not Value.children.Is_Null then
         Handler.Key_Name ("children");
         Handler.Start_Array;
         for J in 1 .. Value.children.Length loop
            Output_node (Handler, Value.children (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_node;

   procedure Output_edgeTraversal
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : edgeTraversal) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("edgeId");
      Handler.String_Value (Value.edgeId);
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      if not Value.finalState.Is_Empty then
         Handler.Key_Name ("finalState");
         Output_Any_Value (Handler, Value.finalState);
      end if;
      if Value.stepOverEdgeCount.Is_Set then
         Handler.Key_Name ("stepOverEdgeCount");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.stepOverEdgeCount.Value)));
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_edgeTraversal;

   procedure Output_stackFrame
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : stackFrame) is
   begin
      Handler.Start_Object;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_location (Handler, Value.location.Value);
      end if;
      if not Value.module.Is_Null then
         Handler.Key_Name ("module");
         Handler.String_Value (Value.module);
      end if;
      if Value.threadId.Is_Set then
         Handler.Key_Name ("threadId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.threadId.Value)));
      end if;
      if not Value.parameters.Is_Empty then
         Handler.Key_Name ("parameters");
         Handler.Start_Array;
         for J in 1 .. Value.parameters.Length loop
            Handler.String_Value (Value.parameters (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_stackFrame;

   procedure Output_reportingDescriptorRelationship
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : reportingDescriptorRelationship) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("target");
      Output_reportingDescriptorReference (Handler, Value.target);
      if not Value.kinds.Is_Empty then
         Handler.Key_Name ("kinds");
         Handler.Start_Array;
         for J in 1 .. Value.kinds.Length loop
            Handler.String_Value (Value.kinds (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_reportingDescriptorRelationship;

   procedure Output_propertyBag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : propertyBag) is
   begin
      Handler.Start_Object;
      if not Value.tags.Is_Empty then
         Handler.Key_Name ("tags");
         Handler.Start_Array;
         for J in 1 .. Value.tags.Length loop
            Handler.String_Value (Value.tags (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.End_Object;
   end Output_propertyBag;

   procedure Output_logicalLocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : logicalLocation) is
   begin
      Handler.Start_Object;
      if not Value.name.Is_Null then
         Handler.Key_Name ("name");
         Handler.String_Value (Value.name);
      end if;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if not Value.fullyQualifiedName.Is_Null then
         Handler.Key_Name ("fullyQualifiedName");
         Handler.String_Value (Value.fullyQualifiedName);
      end if;
      if not Value.decoratedName.Is_Null then
         Handler.Key_Name ("decoratedName");
         Handler.String_Value (Value.decoratedName);
      end if;
      if Value.parentIndex.Is_Set then
         Handler.Key_Name ("parentIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.parentIndex.Value)));
      end if;
      if not Value.kind.Is_Null then
         Handler.Key_Name ("kind");
         Handler.String_Value (Value.kind);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_logicalLocation;

   procedure Output_resultProvenance
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : resultProvenance) is
   begin
      Handler.Start_Object;
      if not Value.firstDetectionTimeUtc.Is_Null then
         Handler.Key_Name ("firstDetectionTimeUtc");
         Handler.String_Value (Value.firstDetectionTimeUtc);
      end if;
      if not Value.lastDetectionTimeUtc.Is_Null then
         Handler.Key_Name ("lastDetectionTimeUtc");
         Handler.String_Value (Value.lastDetectionTimeUtc);
      end if;
      if not Value.firstDetectionRunGuid.Is_Null then
         Handler.Key_Name ("firstDetectionRunGuid");
         Handler.String_Value (Value.firstDetectionRunGuid);
      end if;
      if not Value.lastDetectionRunGuid.Is_Null then
         Handler.Key_Name ("lastDetectionRunGuid");
         Handler.String_Value (Value.lastDetectionRunGuid);
      end if;
      if Value.invocationIndex.Is_Set then
         Handler.Key_Name ("invocationIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.invocationIndex.Value)));
      end if;
      if not Value.conversionSources.Is_Null then
         Handler.Key_Name ("conversionSources");
         Handler.Start_Array;
         for J in 1 .. Value.conversionSources.Length loop
            Output_physicalLocation (Handler, Value.conversionSources (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_resultProvenance;

   procedure Output_message
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : message) is
   begin
      Handler.Start_Object;
      if not Value.text.Is_Null then
         Handler.Key_Name ("text");
         Handler.String_Value (Value.text);
      end if;
      if not Value.markdown.Is_Null then
         Handler.Key_Name ("markdown");
         Handler.String_Value (Value.markdown);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      if not Value.arguments.Is_Empty then
         Handler.Key_Name ("arguments");
         Handler.Start_Array;
         for J in 1 .. Value.arguments.Length loop
            Handler.String_Value (Value.arguments (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_message;

   procedure Output_artifactChange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : artifactChange) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("artifactLocation");
      Output_artifactLocation (Handler, Value.artifactLocation);
      Handler.Key_Name ("replacements");
      Handler.Start_Array;
      for J in 1 .. Value.replacements.Length loop
         Output_replacement (Handler, Value.replacements (J));
      end loop;
      Handler.End_Array;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_artifactChange;

   procedure Output_address
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : address) is
   begin
      Handler.Start_Object;
      if Value.absoluteAddress.Is_Set then
         Handler.Key_Name ("absoluteAddress");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.absoluteAddress.Value)));
      end if;
      if Value.relativeAddress.Is_Set then
         Handler.Key_Name ("relativeAddress");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.relativeAddress.Value)));
      end if;
      if Value.length.Is_Set then
         Handler.Key_Name ("length");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.length.Value)));
      end if;
      if not Value.kind.Is_Null then
         Handler.Key_Name ("kind");
         Handler.String_Value (Value.kind);
      end if;
      if not Value.name.Is_Null then
         Handler.Key_Name ("name");
         Handler.String_Value (Value.name);
      end if;
      if not Value.fullyQualifiedName.Is_Null then
         Handler.Key_Name ("fullyQualifiedName");
         Handler.String_Value (Value.fullyQualifiedName);
      end if;
      if Value.offsetFromParent.Is_Set then
         Handler.Key_Name ("offsetFromParent");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offsetFromParent.Value)));
      end if;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if Value.parentIndex.Is_Set then
         Handler.Key_Name ("parentIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.parentIndex.Value)));
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_address;

   procedure Output_reportingDescriptorReference
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : reportingDescriptorReference) is
   begin
      Handler.Start_Object;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if Value.toolComponent.Is_Set then
         Handler.Key_Name ("toolComponent");
         Output_toolComponentReference (Handler, Value.toolComponent.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_reportingDescriptorReference;

   procedure Output_translationMetadata
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : translationMetadata) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if not Value.fullName.Is_Null then
         Handler.Key_Name ("fullName");
         Handler.String_Value (Value.fullName);
      end if;
      if Value.shortDescription.Is_Set then
         Handler.Key_Name ("shortDescription");
         Output_multiformatMessageString
           (Handler, Value.shortDescription.Value);
      end if;
      if Value.fullDescription.Is_Set then
         Handler.Key_Name ("fullDescription");
         Output_multiformatMessageString
           (Handler, Value.fullDescription.Value);
      end if;
      if not Value.downloadUri.Is_Null then
         Handler.Key_Name ("downloadUri");
         Handler.String_Value (Value.downloadUri);
      end if;
      if not Value.informationUri.Is_Null then
         Handler.Key_Name ("informationUri");
         Handler.String_Value (Value.informationUri);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_translationMetadata;

   procedure Output_notification
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : notification) is
   begin
      Handler.Start_Object;
      if not Value.locations.Is_Null then
         Handler.Key_Name ("locations");
         Handler.Start_Array;
         for J in 1 .. Value.locations.Length loop
            Output_location (Handler, Value.locations (J));
         end loop;
         Handler.End_Array;
      end if;
      Handler.Key_Name ("message");
      Output_message (Handler, Value.message);
      if Value.level.Is_Set then
         Handler.Key_Name ("level");
         Output_notification_level (Handler, Value.level.Value);
      end if;
      if Value.threadId.Is_Set then
         Handler.Key_Name ("threadId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.threadId.Value)));
      end if;
      if not Value.timeUtc.Is_Null then
         Handler.Key_Name ("timeUtc");
         Handler.String_Value (Value.timeUtc);
      end if;
      if Value.a_exception.Is_Set then
         Handler.Key_Name ("exception");
         Output_a_exception (Handler, Value.a_exception.Value);
      end if;
      if Value.descriptor.Is_Set then
         Handler.Key_Name ("descriptor");
         Output_reportingDescriptorReference (Handler, Value.descriptor.Value);
      end if;
      if Value.associatedRule.Is_Set then
         Handler.Key_Name ("associatedRule");
         Output_reportingDescriptorReference
           (Handler, Value.associatedRule.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_notification;

   procedure Output_physicalLocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : physicalLocation) is
   begin
      Handler.Start_Object;
      if Value.address.Is_Set then
         Handler.Key_Name ("address");
         Output_address (Handler, Value.address.Value);
      end if;
      if Value.artifactLocation.Is_Set then
         Handler.Key_Name ("artifactLocation");
         Output_artifactLocation (Handler, Value.artifactLocation.Value);
      end if;
      if Value.region.Is_Set then
         Handler.Key_Name ("region");
         Output_region (Handler, Value.region.Value);
      end if;
      if Value.contextRegion.Is_Set then
         Handler.Key_Name ("contextRegion");
         Output_region (Handler, Value.contextRegion.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_physicalLocation;

   procedure Output_versionControlDetails
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : versionControlDetails) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("repositoryUri");
      Handler.String_Value (Value.repositoryUri);
      if not Value.revisionId.Is_Null then
         Handler.Key_Name ("revisionId");
         Handler.String_Value (Value.revisionId);
      end if;
      if not Value.branch.Is_Null then
         Handler.Key_Name ("branch");
         Handler.String_Value (Value.branch);
      end if;
      if not Value.revisionTag.Is_Null then
         Handler.Key_Name ("revisionTag");
         Handler.String_Value (Value.revisionTag);
      end if;
      if not Value.asOfTimeUtc.Is_Null then
         Handler.Key_Name ("asOfTimeUtc");
         Handler.String_Value (Value.asOfTimeUtc);
      end if;
      if Value.mappedTo.Is_Set then
         Handler.Key_Name ("mappedTo");
         Output_artifactLocation (Handler, Value.mappedTo.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_versionControlDetails;

   procedure Output_location
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : location) is
   begin
      Handler.Start_Object;
      if Value.id.Is_Set then
         Handler.Key_Name ("id");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.id.Value)));
      end if;
      if Value.physicalLocation.Is_Set then
         Handler.Key_Name ("physicalLocation");
         Output_physicalLocation (Handler, Value.physicalLocation.Value);
      end if;
      if not Value.logicalLocations.Is_Null then
         Handler.Key_Name ("logicalLocations");
         Handler.Start_Array;
         for J in 1 .. Value.logicalLocations.Length loop
            Output_logicalLocation (Handler, Value.logicalLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      if not Value.annotations.Is_Null then
         Handler.Key_Name ("annotations");
         Handler.Start_Array;
         for J in 1 .. Value.annotations.Length loop
            Output_region (Handler, Value.annotations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.relationships.Is_Null then
         Handler.Key_Name ("relationships");
         Handler.Start_Array;
         for J in 1 .. Value.relationships.Length loop
            Output_locationRelationship (Handler, Value.relationships (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_location;

   procedure Output_suppression
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : suppression) is
   begin
      Handler.Start_Object;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      Handler.Key_Name ("kind");
      Output_suppression_kind (Handler, Value.kind);
      if Value.status.Is_Set then
         Handler.Key_Name ("status");
         Output_suppression_status (Handler, Value.status.Value);
      end if;
      if not Value.justification.Is_Null then
         Handler.Key_Name ("justification");
         Handler.String_Value (Value.justification);
      end if;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_location (Handler, Value.location.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_suppression;

   procedure Output_externalPropertyFileReference
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : externalPropertyFileReference) is
   begin
      Handler.Start_Object;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_artifactLocation (Handler, Value.location.Value);
      end if;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if Value.itemCount.Is_Set then
         Handler.Key_Name ("itemCount");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.itemCount.Value)));
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_externalPropertyFileReference;

   procedure Output_locationRelationship
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : locationRelationship) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("target");
      Handler.Integer_Value (Interfaces.Integer_64 (Integer'(Value.target)));
      if not Value.kinds.Is_Empty then
         Handler.Key_Name ("kinds");
         Handler.Start_Array;
         for J in 1 .. Value.kinds.Length loop
            Handler.String_Value (Value.kinds (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_locationRelationship;

   procedure Output_edge
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : edge) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.String_Value (Value.id);
      if Value.label.Is_Set then
         Handler.Key_Name ("label");
         Output_message (Handler, Value.label.Value);
      end if;
      Handler.Key_Name ("sourceNodeId");
      Handler.String_Value (Value.sourceNodeId);
      Handler.Key_Name ("targetNodeId");
      Handler.String_Value (Value.targetNodeId);
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_edge;

   procedure Output_Root
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : Root) is
   begin
      Handler.Start_Object;
      if not Value.schema.Is_Null then
         Handler.Key_Name ("$schema");
         Handler.String_Value (Value.schema);
      end if;
      Handler.Key_Name ("version");
      Handler.String_Value ("2.1.0");
      Handler.Key_Name ("runs");
      Handler.Start_Array;
      for J in 1 .. Value.runs.Length loop
         Output_run (Handler, Value.runs (J));
      end loop;
      Handler.End_Array;
      if not Value.inlineExternalProperties.Is_Null then
         Handler.Key_Name ("inlineExternalProperties");
         Handler.Start_Array;
         for J in 1 .. Value.inlineExternalProperties.Length loop
            Output_externalProperties
              (Handler, Value.inlineExternalProperties (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_Root;

   procedure Output_specialLocations
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : specialLocations) is
   begin
      Handler.Start_Object;
      if Value.displayBase.Is_Set then
         Handler.Key_Name ("displayBase");
         Output_artifactLocation (Handler, Value.displayBase.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_specialLocations;

   procedure Output_toolComponent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : toolComponent) is
   begin
      Handler.Start_Object;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if not Value.organization.Is_Null then
         Handler.Key_Name ("organization");
         Handler.String_Value (Value.organization);
      end if;
      if not Value.product.Is_Null then
         Handler.Key_Name ("product");
         Handler.String_Value (Value.product);
      end if;
      if not Value.productSuite.Is_Null then
         Handler.Key_Name ("productSuite");
         Handler.String_Value (Value.productSuite);
      end if;
      if Value.shortDescription.Is_Set then
         Handler.Key_Name ("shortDescription");
         Output_multiformatMessageString
           (Handler, Value.shortDescription.Value);
      end if;
      if Value.fullDescription.Is_Set then
         Handler.Key_Name ("fullDescription");
         Output_multiformatMessageString
           (Handler, Value.fullDescription.Value);
      end if;
      if not Value.fullName.Is_Null then
         Handler.Key_Name ("fullName");
         Handler.String_Value (Value.fullName);
      end if;
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      if not Value.semanticVersion.Is_Null then
         Handler.Key_Name ("semanticVersion");
         Handler.String_Value (Value.semanticVersion);
      end if;
      if not Value.dottedQuadFileVersion.Is_Null then
         Handler.Key_Name ("dottedQuadFileVersion");
         Handler.String_Value (Value.dottedQuadFileVersion);
      end if;
      if not Value.releaseDateUtc.Is_Null then
         Handler.Key_Name ("releaseDateUtc");
         Handler.String_Value (Value.releaseDateUtc);
      end if;
      if not Value.downloadUri.Is_Null then
         Handler.Key_Name ("downloadUri");
         Handler.String_Value (Value.downloadUri);
      end if;
      if not Value.informationUri.Is_Null then
         Handler.Key_Name ("informationUri");
         Handler.String_Value (Value.informationUri);
      end if;
      if not Value.globalMessageStrings.Is_Empty then
         Handler.Key_Name ("globalMessageStrings");
         Output_Any_Value (Handler, Value.globalMessageStrings);
      end if;
      if not Value.notifications.Is_Null then
         Handler.Key_Name ("notifications");
         Handler.Start_Array;
         for J in 1 .. Value.notifications.Length loop
            Output_reportingDescriptor (Handler, Value.notifications (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.rules.Is_Null then
         Handler.Key_Name ("rules");
         Handler.Start_Array;
         for J in 1 .. Value.rules.Length loop
            Output_reportingDescriptor (Handler, Value.rules (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.taxa.Is_Null then
         Handler.Key_Name ("taxa");
         Handler.Start_Array;
         for J in 1 .. Value.taxa.Length loop
            Output_reportingDescriptor (Handler, Value.taxa (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.locations.Is_Null then
         Handler.Key_Name ("locations");
         Handler.Start_Array;
         for J in 1 .. Value.locations.Length loop
            Output_artifactLocation (Handler, Value.locations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.language.Is_Null then
         Handler.Key_Name ("language");
         Handler.String_Value (Value.language);
      end if;
      if not Value.contents.Is_Null then
         Handler.Key_Name ("contents");
         Handler.Start_Array;
         for J in 1 .. Value.contents.Length loop
            Output_toolComponent_contents (Handler, Value.contents (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.isComprehensive then
         Handler.Key_Name ("isComprehensive");
         Handler.Boolean_Value (Value.isComprehensive);
      end if;
      if not Value.localizedDataSemanticVersion.Is_Null then
         Handler.Key_Name ("localizedDataSemanticVersion");
         Handler.String_Value (Value.localizedDataSemanticVersion);
      end if;
      if not Value.minimumRequiredLocalizedDataSemanticVersion.Is_Null then
         Handler.Key_Name ("minimumRequiredLocalizedDataSemanticVersion");
         Handler.String_Value
           (Value.minimumRequiredLocalizedDataSemanticVersion);
      end if;
      if Value.associatedComponent.Is_Set then
         Handler.Key_Name ("associatedComponent");
         Output_toolComponentReference
           (Handler, Value.associatedComponent.Value);
      end if;
      if Value.translationMetadata.Is_Set then
         Handler.Key_Name ("translationMetadata");
         Output_translationMetadata (Handler, Value.translationMetadata.Value);
      end if;
      if not Value.supportedTaxonomies.Is_Null then
         Handler.Key_Name ("supportedTaxonomies");
         Handler.Start_Array;
         for J in 1 .. Value.supportedTaxonomies.Length loop
            Output_toolComponentReference
              (Handler, Value.supportedTaxonomies (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_toolComponent;

   procedure Output_invocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : invocation) is
   begin
      Handler.Start_Object;
      if not Value.commandLine.Is_Null then
         Handler.Key_Name ("commandLine");
         Handler.String_Value (Value.commandLine);
      end if;
      if not Value.arguments.Is_Empty then
         Handler.Key_Name ("arguments");
         Handler.Start_Array;
         for J in 1 .. Value.arguments.Length loop
            Handler.String_Value (Value.arguments (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.responseFiles.Is_Null then
         Handler.Key_Name ("responseFiles");
         Handler.Start_Array;
         for J in 1 .. Value.responseFiles.Length loop
            Output_artifactLocation (Handler, Value.responseFiles (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.startTimeUtc.Is_Null then
         Handler.Key_Name ("startTimeUtc");
         Handler.String_Value (Value.startTimeUtc);
      end if;
      if not Value.endTimeUtc.Is_Null then
         Handler.Key_Name ("endTimeUtc");
         Handler.String_Value (Value.endTimeUtc);
      end if;
      if Value.exitCode.Is_Set then
         Handler.Key_Name ("exitCode");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.exitCode.Value)));
      end if;
      if not Value.ruleConfigurationOverrides.Is_Null then
         Handler.Key_Name ("ruleConfigurationOverrides");
         Handler.Start_Array;
         for J in 1 .. Value.ruleConfigurationOverrides.Length loop
            Output_configurationOverride
              (Handler, Value.ruleConfigurationOverrides (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.notificationConfigurationOverrides.Is_Null then
         Handler.Key_Name ("notificationConfigurationOverrides");
         Handler.Start_Array;
         for J in 1 .. Value.notificationConfigurationOverrides.Length loop
            Output_configurationOverride
              (Handler, Value.notificationConfigurationOverrides (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.toolExecutionNotifications.Is_Null then
         Handler.Key_Name ("toolExecutionNotifications");
         Handler.Start_Array;
         for J in 1 .. Value.toolExecutionNotifications.Length loop
            Output_notification
              (Handler, Value.toolExecutionNotifications (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.toolConfigurationNotifications.Is_Null then
         Handler.Key_Name ("toolConfigurationNotifications");
         Handler.Start_Array;
         for J in 1 .. Value.toolConfigurationNotifications.Length loop
            Output_notification
              (Handler, Value.toolConfigurationNotifications (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.exitCodeDescription.Is_Null then
         Handler.Key_Name ("exitCodeDescription");
         Handler.String_Value (Value.exitCodeDescription);
      end if;
      if not Value.exitSignalName.Is_Null then
         Handler.Key_Name ("exitSignalName");
         Handler.String_Value (Value.exitSignalName);
      end if;
      if Value.exitSignalNumber.Is_Set then
         Handler.Key_Name ("exitSignalNumber");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.exitSignalNumber.Value)));
      end if;
      if not Value.processStartFailureMessage.Is_Null then
         Handler.Key_Name ("processStartFailureMessage");
         Handler.String_Value (Value.processStartFailureMessage);
      end if;
      Handler.Key_Name ("executionSuccessful");
      Handler.Boolean_Value (Value.executionSuccessful);
      if not Value.machine.Is_Null then
         Handler.Key_Name ("machine");
         Handler.String_Value (Value.machine);
      end if;
      if not Value.account.Is_Null then
         Handler.Key_Name ("account");
         Handler.String_Value (Value.account);
      end if;
      if Value.processId.Is_Set then
         Handler.Key_Name ("processId");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.processId.Value)));
      end if;
      if Value.executableLocation.Is_Set then
         Handler.Key_Name ("executableLocation");
         Output_artifactLocation (Handler, Value.executableLocation.Value);
      end if;
      if Value.workingDirectory.Is_Set then
         Handler.Key_Name ("workingDirectory");
         Output_artifactLocation (Handler, Value.workingDirectory.Value);
      end if;
      if not Value.environmentVariables.Is_Empty then
         Handler.Key_Name ("environmentVariables");
         Output_Any_Value (Handler, Value.environmentVariables);
      end if;
      if Value.stdin.Is_Set then
         Handler.Key_Name ("stdin");
         Output_artifactLocation (Handler, Value.stdin.Value);
      end if;
      if Value.stdout.Is_Set then
         Handler.Key_Name ("stdout");
         Output_artifactLocation (Handler, Value.stdout.Value);
      end if;
      if Value.stderr.Is_Set then
         Handler.Key_Name ("stderr");
         Output_artifactLocation (Handler, Value.stderr.Value);
      end if;
      if Value.stdoutStderr.Is_Set then
         Handler.Key_Name ("stdoutStderr");
         Output_artifactLocation (Handler, Value.stdoutStderr.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_invocation;

   procedure Output_conversion
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : conversion) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("tool");
      Output_tool (Handler, Value.tool);
      if Value.invocation.Is_Set then
         Handler.Key_Name ("invocation");
         Output_invocation (Handler, Value.invocation.Value);
      end if;
      if not Value.analysisToolLogFiles.Is_Null then
         Handler.Key_Name ("analysisToolLogFiles");
         Handler.Start_Array;
         for J in 1 .. Value.analysisToolLogFiles.Length loop
            Output_artifactLocation (Handler, Value.analysisToolLogFiles (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_conversion;

   procedure Output_runAutomationDetails
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : runAutomationDetails) is
   begin
      Handler.Start_Object;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if not Value.correlationGuid.Is_Null then
         Handler.Key_Name ("correlationGuid");
         Handler.String_Value (Value.correlationGuid);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_runAutomationDetails;

   procedure Output_graph
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : graph) is
   begin
      Handler.Start_Object;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if not Value.nodes.Is_Null then
         Handler.Key_Name ("nodes");
         Handler.Start_Array;
         for J in 1 .. Value.nodes.Length loop
            Output_node (Handler, Value.nodes (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.edges.Is_Null then
         Handler.Key_Name ("edges");
         Handler.Start_Array;
         for J in 1 .. Value.edges.Length loop
            Output_edge (Handler, Value.edges (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_graph;

   procedure Output_externalProperties
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : externalProperties) is
   begin
      Handler.Start_Object;
      if not Value.schema.Is_Null then
         Handler.Key_Name ("schema");
         Handler.String_Value (Value.schema);
      end if;
      Handler.Key_Name ("version");
      Handler.String_Value ("2.1.0");
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if not Value.runGuid.Is_Null then
         Handler.Key_Name ("runGuid");
         Handler.String_Value (Value.runGuid);
      end if;
      if Value.conversion.Is_Set then
         Handler.Key_Name ("conversion");
         Output_conversion (Handler, Value.conversion.Value);
      end if;
      if not Value.graphs.Is_Null then
         Handler.Key_Name ("graphs");
         Handler.Start_Array;
         for J in 1 .. Value.graphs.Length loop
            Output_graph (Handler, Value.graphs (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.externalizedProperties.Is_Set then
         Handler.Key_Name ("externalizedProperties");
         Output_propertyBag (Handler, Value.externalizedProperties.Value);
      end if;
      if not Value.artifacts.Is_Null then
         Handler.Key_Name ("artifacts");
         Handler.Start_Array;
         for J in 1 .. Value.artifacts.Length loop
            Output_artifact (Handler, Value.artifacts (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.invocations.Is_Null then
         Handler.Key_Name ("invocations");
         Handler.Start_Array;
         for J in 1 .. Value.invocations.Length loop
            Output_invocation (Handler, Value.invocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.logicalLocations.Is_Null then
         Handler.Key_Name ("logicalLocations");
         Handler.Start_Array;
         for J in 1 .. Value.logicalLocations.Length loop
            Output_logicalLocation (Handler, Value.logicalLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.threadFlowLocations.Is_Null then
         Handler.Key_Name ("threadFlowLocations");
         Handler.Start_Array;
         for J in 1 .. Value.threadFlowLocations.Length loop
            Output_threadFlowLocation (Handler, Value.threadFlowLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.results.Is_Null then
         Handler.Key_Name ("results");
         Handler.Start_Array;
         for J in 1 .. Value.results.Length loop
            Output_result (Handler, Value.results (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.taxonomies.Is_Null then
         Handler.Key_Name ("taxonomies");
         Handler.Start_Array;
         for J in 1 .. Value.taxonomies.Length loop
            Output_toolComponent (Handler, Value.taxonomies (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.driver.Is_Set then
         Handler.Key_Name ("driver");
         Output_toolComponent (Handler, Value.driver.Value);
      end if;
      if not Value.extensions.Is_Null then
         Handler.Key_Name ("extensions");
         Handler.Start_Array;
         for J in 1 .. Value.extensions.Length loop
            Output_toolComponent (Handler, Value.extensions (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.policies.Is_Null then
         Handler.Key_Name ("policies");
         Handler.Start_Array;
         for J in 1 .. Value.policies.Length loop
            Output_toolComponent (Handler, Value.policies (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.translations.Is_Null then
         Handler.Key_Name ("translations");
         Handler.Start_Array;
         for J in 1 .. Value.translations.Length loop
            Output_toolComponent (Handler, Value.translations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.addresses.Is_Null then
         Handler.Key_Name ("addresses");
         Handler.Start_Array;
         for J in 1 .. Value.addresses.Length loop
            Output_address (Handler, Value.addresses (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.webRequests.Is_Null then
         Handler.Key_Name ("webRequests");
         Handler.Start_Array;
         for J in 1 .. Value.webRequests.Length loop
            Output_webRequest (Handler, Value.webRequests (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.webResponses.Is_Null then
         Handler.Key_Name ("webResponses");
         Handler.Start_Array;
         for J in 1 .. Value.webResponses.Length loop
            Output_webResponse (Handler, Value.webResponses (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_externalProperties;

   procedure Output_rectangle
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : rectangle) is
   begin
      Handler.Start_Object;
      if Value.top.Is_Set then
         Handler.Key_Name ("top");
         Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.top.Value));
      end if;
      if Value.left.Is_Set then
         Handler.Key_Name ("left");
         Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.left.Value));
      end if;
      if Value.bottom.Is_Set then
         Handler.Key_Name ("bottom");
         Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.bottom.Value));
      end if;
      if Value.right.Is_Set then
         Handler.Key_Name ("right");
         Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.right.Value));
      end if;
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_rectangle;

   procedure Output_reportingDescriptor
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : reportingDescriptor) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.String_Value (Value.id);
      if not Value.deprecatedIds.Is_Empty then
         Handler.Key_Name ("deprecatedIds");
         Handler.Start_Array;
         for J in 1 .. Value.deprecatedIds.Length loop
            Handler.String_Value (Value.deprecatedIds (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if not Value.deprecatedGuids.Is_Empty then
         Handler.Key_Name ("deprecatedGuids");
         Handler.Start_Array;
         for J in 1 .. Value.deprecatedGuids.Length loop
            Handler.String_Value (Value.deprecatedGuids (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.name.Is_Null then
         Handler.Key_Name ("name");
         Handler.String_Value (Value.name);
      end if;
      if not Value.deprecatedNames.Is_Empty then
         Handler.Key_Name ("deprecatedNames");
         Handler.Start_Array;
         for J in 1 .. Value.deprecatedNames.Length loop
            Handler.String_Value (Value.deprecatedNames (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.shortDescription.Is_Set then
         Handler.Key_Name ("shortDescription");
         Output_multiformatMessageString
           (Handler, Value.shortDescription.Value);
      end if;
      if Value.fullDescription.Is_Set then
         Handler.Key_Name ("fullDescription");
         Output_multiformatMessageString
           (Handler, Value.fullDescription.Value);
      end if;
      if not Value.messageStrings.Is_Empty then
         Handler.Key_Name ("messageStrings");
         Output_Any_Value (Handler, Value.messageStrings);
      end if;
      if Value.defaultConfiguration.Is_Set then
         Handler.Key_Name ("defaultConfiguration");
         Output_reportingConfiguration
           (Handler, Value.defaultConfiguration.Value);
      end if;
      if not Value.helpUri.Is_Null then
         Handler.Key_Name ("helpUri");
         Handler.String_Value (Value.helpUri);
      end if;
      if Value.help.Is_Set then
         Handler.Key_Name ("help");
         Output_multiformatMessageString (Handler, Value.help.Value);
      end if;
      if not Value.relationships.Is_Null then
         Handler.Key_Name ("relationships");
         Handler.Start_Array;
         for J in 1 .. Value.relationships.Length loop
            Output_reportingDescriptorRelationship
              (Handler, Value.relationships (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_reportingDescriptor;

   procedure Output_artifact
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : artifact) is
   begin
      Handler.Start_Object;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_artifactLocation (Handler, Value.location.Value);
      end if;
      if Value.parentIndex.Is_Set then
         Handler.Key_Name ("parentIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.parentIndex.Value)));
      end if;
      if Value.offset.Is_Set then
         Handler.Key_Name ("offset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.offset.Value)));
      end if;
      if Value.length.Is_Set then
         Handler.Key_Name ("length");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.length.Value)));
      end if;
      if not Value.roles.Is_Null then
         Handler.Key_Name ("roles");
         Handler.Start_Array;
         for J in 1 .. Value.roles.Length loop
            Output_artifact_roles (Handler, Value.roles (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.mimeType.Is_Null then
         Handler.Key_Name ("mimeType");
         Handler.String_Value (Value.mimeType);
      end if;
      if Value.contents.Is_Set then
         Handler.Key_Name ("contents");
         Output_artifactContent (Handler, Value.contents.Value);
      end if;
      if not Value.encoding.Is_Null then
         Handler.Key_Name ("encoding");
         Handler.String_Value (Value.encoding);
      end if;
      if not Value.sourceLanguage.Is_Null then
         Handler.Key_Name ("sourceLanguage");
         Handler.String_Value (Value.sourceLanguage);
      end if;
      if not Value.hashes.Is_Empty then
         Handler.Key_Name ("hashes");
         Output_Any_Value (Handler, Value.hashes);
      end if;
      if not Value.lastModifiedTimeUtc.Is_Null then
         Handler.Key_Name ("lastModifiedTimeUtc");
         Handler.String_Value (Value.lastModifiedTimeUtc);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_artifact;

   procedure Output_reportingConfiguration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : reportingConfiguration) is
   begin
      Handler.Start_Object;
      if Value.enabled then
         Handler.Key_Name ("enabled");
         Handler.Boolean_Value (Value.enabled);
      end if;
      if Value.level.Is_Set then
         Handler.Key_Name ("level");
         Output_reportingConfiguration_level (Handler, Value.level.Value);
      end if;
      if Value.rank.Is_Set then
         Handler.Key_Name ("rank");
         Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.rank.Value));
      end if;
      if Value.parameters.Is_Set then
         Handler.Key_Name ("parameters");
         Output_propertyBag (Handler, Value.parameters.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_reportingConfiguration;

   procedure Output_toolComponentReference
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : toolComponentReference) is
   begin
      Handler.Start_Object;
      if not Value.name.Is_Null then
         Handler.Key_Name ("name");
         Handler.String_Value (Value.name);
      end if;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_toolComponentReference;

   procedure Output_tool
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : tool) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("driver");
      Output_toolComponent (Handler, Value.driver);
      if not Value.extensions.Is_Null then
         Handler.Key_Name ("extensions");
         Handler.Start_Array;
         for J in 1 .. Value.extensions.Length loop
            Output_toolComponent (Handler, Value.extensions (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_tool;

   procedure Output_webRequest
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : webRequest) is
   begin
      Handler.Start_Object;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if not Value.protocol.Is_Null then
         Handler.Key_Name ("protocol");
         Handler.String_Value (Value.protocol);
      end if;
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      if not Value.target.Is_Null then
         Handler.Key_Name ("target");
         Handler.String_Value (Value.target);
      end if;
      if not Value.method.Is_Null then
         Handler.Key_Name ("method");
         Handler.String_Value (Value.method);
      end if;
      if not Value.headers.Is_Empty then
         Handler.Key_Name ("headers");
         Output_Any_Value (Handler, Value.headers);
      end if;
      if not Value.parameters.Is_Empty then
         Handler.Key_Name ("parameters");
         Output_Any_Value (Handler, Value.parameters);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_artifactContent (Handler, Value.a_body.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_webRequest;

   procedure Output_fix
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : fix) is
   begin
      Handler.Start_Object;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      Handler.Key_Name ("artifactChanges");
      Handler.Start_Array;
      for J in 1 .. Value.artifactChanges.Length loop
         Output_artifactChange (Handler, Value.artifactChanges (J));
      end loop;
      Handler.End_Array;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_fix;

   procedure Output_result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : result) is
   begin
      Handler.Start_Object;
      if not Value.ruleId.Is_Null then
         Handler.Key_Name ("ruleId");
         Handler.String_Value (Value.ruleId);
      end if;
      if Value.ruleIndex.Is_Set then
         Handler.Key_Name ("ruleIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.ruleIndex.Value)));
      end if;
      if Value.rule.Is_Set then
         Handler.Key_Name ("rule");
         Output_reportingDescriptorReference (Handler, Value.rule.Value);
      end if;
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Output_result_kind (Handler, Value.kind.Value);
      end if;
      if Value.level.Is_Set then
         Handler.Key_Name ("level");
         Output_result_level (Handler, Value.level.Value);
      end if;
      Handler.Key_Name ("message");
      Output_message (Handler, Value.message);
      if Value.analysisTarget.Is_Set then
         Handler.Key_Name ("analysisTarget");
         Output_artifactLocation (Handler, Value.analysisTarget.Value);
      end if;
      if not Value.locations.Is_Null then
         Handler.Key_Name ("locations");
         Handler.Start_Array;
         for J in 1 .. Value.locations.Length loop
            Output_location (Handler, Value.locations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.guid.Is_Null then
         Handler.Key_Name ("guid");
         Handler.String_Value (Value.guid);
      end if;
      if not Value.correlationGuid.Is_Null then
         Handler.Key_Name ("correlationGuid");
         Handler.String_Value (Value.correlationGuid);
      end if;
      if Value.occurrenceCount.Is_Set then
         Handler.Key_Name ("occurrenceCount");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.occurrenceCount.Value)));
      end if;
      if not Value.partialFingerprints.Is_Empty then
         Handler.Key_Name ("partialFingerprints");
         Output_Any_Value (Handler, Value.partialFingerprints);
      end if;
      if not Value.fingerprints.Is_Empty then
         Handler.Key_Name ("fingerprints");
         Output_Any_Value (Handler, Value.fingerprints);
      end if;
      if not Value.stacks.Is_Null then
         Handler.Key_Name ("stacks");
         Handler.Start_Array;
         for J in 1 .. Value.stacks.Length loop
            Output_stack (Handler, Value.stacks (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.codeFlows.Is_Null then
         Handler.Key_Name ("codeFlows");
         Handler.Start_Array;
         for J in 1 .. Value.codeFlows.Length loop
            Output_codeFlow (Handler, Value.codeFlows (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.graphs.Is_Null then
         Handler.Key_Name ("graphs");
         Handler.Start_Array;
         for J in 1 .. Value.graphs.Length loop
            Output_graph (Handler, Value.graphs (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.graphTraversals.Is_Null then
         Handler.Key_Name ("graphTraversals");
         Handler.Start_Array;
         for J in 1 .. Value.graphTraversals.Length loop
            Output_graphTraversal (Handler, Value.graphTraversals (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.relatedLocations.Is_Null then
         Handler.Key_Name ("relatedLocations");
         Handler.Start_Array;
         for J in 1 .. Value.relatedLocations.Length loop
            Output_location (Handler, Value.relatedLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.suppressions.Is_Null then
         Handler.Key_Name ("suppressions");
         Handler.Start_Array;
         for J in 1 .. Value.suppressions.Length loop
            Output_suppression (Handler, Value.suppressions (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.baselineState.Is_Set then
         Handler.Key_Name ("baselineState");
         Output_result_baselineState (Handler, Value.baselineState.Value);
      end if;
      if Value.rank.Is_Set then
         Handler.Key_Name ("rank");
         Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.rank.Value));
      end if;
      if not Value.attachments.Is_Null then
         Handler.Key_Name ("attachments");
         Handler.Start_Array;
         for J in 1 .. Value.attachments.Length loop
            Output_attachment (Handler, Value.attachments (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.hostedViewerUri.Is_Null then
         Handler.Key_Name ("hostedViewerUri");
         Handler.String_Value (Value.hostedViewerUri);
      end if;
      if not Value.workItemUris.Is_Empty then
         Handler.Key_Name ("workItemUris");
         Handler.Start_Array;
         for J in 1 .. Value.workItemUris.Length loop
            Handler.String_Value (Value.workItemUris (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.provenance.Is_Set then
         Handler.Key_Name ("provenance");
         Output_resultProvenance (Handler, Value.provenance.Value);
      end if;
      if not Value.fixes.Is_Null then
         Handler.Key_Name ("fixes");
         Handler.Start_Array;
         for J in 1 .. Value.fixes.Length loop
            Output_fix (Handler, Value.fixes (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.taxa.Is_Null then
         Handler.Key_Name ("taxa");
         Handler.Start_Array;
         for J in 1 .. Value.taxa.Length loop
            Output_reportingDescriptorReference (Handler, Value.taxa (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.webRequest.Is_Set then
         Handler.Key_Name ("webRequest");
         Output_webRequest (Handler, Value.webRequest.Value);
      end if;
      if Value.webResponse.Is_Set then
         Handler.Key_Name ("webResponse");
         Output_webResponse (Handler, Value.webResponse.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_result;

   procedure Output_region
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : region) is
   begin
      Handler.Start_Object;
      if Value.startLine.Is_Set then
         Handler.Key_Name ("startLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.startLine.Value)));
      end if;
      if Value.startColumn.Is_Set then
         Handler.Key_Name ("startColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.startColumn.Value)));
      end if;
      if Value.endLine.Is_Set then
         Handler.Key_Name ("endLine");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endLine.Value)));
      end if;
      if Value.endColumn.Is_Set then
         Handler.Key_Name ("endColumn");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.endColumn.Value)));
      end if;
      if Value.charOffset.Is_Set then
         Handler.Key_Name ("charOffset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.charOffset.Value)));
      end if;
      if Value.charLength.Is_Set then
         Handler.Key_Name ("charLength");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.charLength.Value)));
      end if;
      if Value.byteOffset.Is_Set then
         Handler.Key_Name ("byteOffset");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.byteOffset.Value)));
      end if;
      if Value.byteLength.Is_Set then
         Handler.Key_Name ("byteLength");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.byteLength.Value)));
      end if;
      if Value.snippet.Is_Set then
         Handler.Key_Name ("snippet");
         Output_artifactContent (Handler, Value.snippet.Value);
      end if;
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      if not Value.sourceLanguage.Is_Null then
         Handler.Key_Name ("sourceLanguage");
         Handler.String_Value (Value.sourceLanguage);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_region;

   procedure Output_artifactLocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : artifactLocation) is
   begin
      Handler.Start_Object;
      if not Value.uri.Is_Null then
         Handler.Key_Name ("uri");
         Handler.String_Value (Value.uri);
      end if;
      if not Value.uriBaseId.Is_Null then
         Handler.Key_Name ("uriBaseId");
         Handler.String_Value (Value.uriBaseId);
      end if;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_artifactLocation;

   procedure Output_graphTraversal
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : graphTraversal) is
   begin
      Handler.Start_Object;
      if Value.runGraphIndex.Is_Set then
         Handler.Key_Name ("runGraphIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.runGraphIndex.Value)));
      end if;
      if Value.resultGraphIndex.Is_Set then
         Handler.Key_Name ("resultGraphIndex");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.resultGraphIndex.Value)));
      end if;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      if not Value.initialState.Is_Empty then
         Handler.Key_Name ("initialState");
         Output_Any_Value (Handler, Value.initialState);
      end if;
      if not Value.immutableState.Is_Empty then
         Handler.Key_Name ("immutableState");
         Output_Any_Value (Handler, Value.immutableState);
      end if;
      if not Value.edgeTraversals.Is_Null then
         Handler.Key_Name ("edgeTraversals");
         Handler.Start_Array;
         for J in 1 .. Value.edgeTraversals.Length loop
            Output_edgeTraversal (Handler, Value.edgeTraversals (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_graphTraversal;

   procedure Output_attachment
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : attachment) is
   begin
      Handler.Start_Object;
      if Value.description.Is_Set then
         Handler.Key_Name ("description");
         Output_message (Handler, Value.description.Value);
      end if;
      Handler.Key_Name ("artifactLocation");
      Output_artifactLocation (Handler, Value.artifactLocation);
      if not Value.regions.Is_Null then
         Handler.Key_Name ("regions");
         Handler.Start_Array;
         for J in 1 .. Value.regions.Length loop
            Output_region (Handler, Value.regions (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.rectangles.Is_Null then
         Handler.Key_Name ("rectangles");
         Handler.Start_Array;
         for J in 1 .. Value.rectangles.Length loop
            Output_rectangle (Handler, Value.rectangles (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_attachment;

   procedure Output_stack
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : stack) is
   begin
      Handler.Start_Object;
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("frames");
      Handler.Start_Array;
      for J in 1 .. Value.frames.Length loop
         Output_stackFrame (Handler, Value.frames (J));
      end loop;
      Handler.End_Array;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_stack;

   procedure Output_replacement
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : replacement) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("deletedRegion");
      Output_region (Handler, Value.deletedRegion);
      if Value.insertedContent.Is_Set then
         Handler.Key_Name ("insertedContent");
         Output_artifactContent (Handler, Value.insertedContent.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_replacement;

   procedure Output_run
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : run) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("tool");
      Output_tool (Handler, Value.tool);
      if not Value.invocations.Is_Null then
         Handler.Key_Name ("invocations");
         Handler.Start_Array;
         for J in 1 .. Value.invocations.Length loop
            Output_invocation (Handler, Value.invocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.conversion.Is_Set then
         Handler.Key_Name ("conversion");
         Output_conversion (Handler, Value.conversion.Value);
      end if;
      if not Value.language.Is_Null then
         Handler.Key_Name ("language");
         Handler.String_Value (Value.language);
      end if;
      if not Value.versionControlProvenance.Is_Null then
         Handler.Key_Name ("versionControlProvenance");
         Handler.Start_Array;
         for J in 1 .. Value.versionControlProvenance.Length loop
            Output_versionControlDetails
              (Handler, Value.versionControlProvenance (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.originalUriBaseIds.Is_Empty then
         Handler.Key_Name ("originalUriBaseIds");
         Output_Any_Value (Handler, Value.originalUriBaseIds);
      end if;
      if not Value.artifacts.Is_Null then
         Handler.Key_Name ("artifacts");
         Handler.Start_Array;
         for J in 1 .. Value.artifacts.Length loop
            Output_artifact (Handler, Value.artifacts (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.logicalLocations.Is_Null then
         Handler.Key_Name ("logicalLocations");
         Handler.Start_Array;
         for J in 1 .. Value.logicalLocations.Length loop
            Output_logicalLocation (Handler, Value.logicalLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.graphs.Is_Null then
         Handler.Key_Name ("graphs");
         Handler.Start_Array;
         for J in 1 .. Value.graphs.Length loop
            Output_graph (Handler, Value.graphs (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.results.Is_Null then
         Handler.Key_Name ("results");
         Handler.Start_Array;
         for J in 1 .. Value.results.Length loop
            Output_result (Handler, Value.results (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.automationDetails.Is_Set then
         Handler.Key_Name ("automationDetails");
         Output_runAutomationDetails (Handler, Value.automationDetails.Value);
      end if;
      if not Value.runAggregates.Is_Null then
         Handler.Key_Name ("runAggregates");
         Handler.Start_Array;
         for J in 1 .. Value.runAggregates.Length loop
            Output_runAutomationDetails (Handler, Value.runAggregates (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.baselineGuid.Is_Null then
         Handler.Key_Name ("baselineGuid");
         Handler.String_Value (Value.baselineGuid);
      end if;
      if not Value.redactionTokens.Is_Empty then
         Handler.Key_Name ("redactionTokens");
         Handler.Start_Array;
         for J in 1 .. Value.redactionTokens.Length loop
            Handler.String_Value (Value.redactionTokens (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.defaultEncoding.Is_Null then
         Handler.Key_Name ("defaultEncoding");
         Handler.String_Value (Value.defaultEncoding);
      end if;
      if not Value.defaultSourceLanguage.Is_Null then
         Handler.Key_Name ("defaultSourceLanguage");
         Handler.String_Value (Value.defaultSourceLanguage);
      end if;
      if not Value.newlineSequences.Is_Empty then
         Handler.Key_Name ("newlineSequences");
         Handler.Start_Array;
         for J in 1 .. Value.newlineSequences.Length loop
            Handler.String_Value (Value.newlineSequences (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.columnKind.Is_Set then
         Handler.Key_Name ("columnKind");
         Output_run_columnKind (Handler, Value.columnKind.Value);
      end if;
      if Value.externalPropertyFileReferences.Is_Set then
         Handler.Key_Name ("externalPropertyFileReferences");
         Output_externalPropertyFileReferences
           (Handler, Value.externalPropertyFileReferences.Value);
      end if;
      if not Value.threadFlowLocations.Is_Null then
         Handler.Key_Name ("threadFlowLocations");
         Handler.Start_Array;
         for J in 1 .. Value.threadFlowLocations.Length loop
            Output_threadFlowLocation (Handler, Value.threadFlowLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.taxonomies.Is_Null then
         Handler.Key_Name ("taxonomies");
         Handler.Start_Array;
         for J in 1 .. Value.taxonomies.Length loop
            Output_toolComponent (Handler, Value.taxonomies (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.addresses.Is_Null then
         Handler.Key_Name ("addresses");
         Handler.Start_Array;
         for J in 1 .. Value.addresses.Length loop
            Output_address (Handler, Value.addresses (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.translations.Is_Null then
         Handler.Key_Name ("translations");
         Handler.Start_Array;
         for J in 1 .. Value.translations.Length loop
            Output_toolComponent (Handler, Value.translations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.policies.Is_Null then
         Handler.Key_Name ("policies");
         Handler.Start_Array;
         for J in 1 .. Value.policies.Length loop
            Output_toolComponent (Handler, Value.policies (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.webRequests.Is_Null then
         Handler.Key_Name ("webRequests");
         Handler.Start_Array;
         for J in 1 .. Value.webRequests.Length loop
            Output_webRequest (Handler, Value.webRequests (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.webResponses.Is_Null then
         Handler.Key_Name ("webResponses");
         Handler.Start_Array;
         for J in 1 .. Value.webResponses.Length loop
            Output_webResponse (Handler, Value.webResponses (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.specialLocations.Is_Set then
         Handler.Key_Name ("specialLocations");
         Output_specialLocations (Handler, Value.specialLocations.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_run;

   procedure Output_externalPropertyFileReferences
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : externalPropertyFileReferences) is
   begin
      Handler.Start_Object;
      if Value.conversion.Is_Set then
         Handler.Key_Name ("conversion");
         Output_externalPropertyFileReference
           (Handler, Value.conversion.Value);
      end if;
      if not Value.graphs.Is_Null then
         Handler.Key_Name ("graphs");
         Handler.Start_Array;
         for J in 1 .. Value.graphs.Length loop
            Output_externalPropertyFileReference (Handler, Value.graphs (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.externalizedProperties.Is_Set then
         Handler.Key_Name ("externalizedProperties");
         Output_externalPropertyFileReference
           (Handler, Value.externalizedProperties.Value);
      end if;
      if not Value.artifacts.Is_Null then
         Handler.Key_Name ("artifacts");
         Handler.Start_Array;
         for J in 1 .. Value.artifacts.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.artifacts (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.invocations.Is_Null then
         Handler.Key_Name ("invocations");
         Handler.Start_Array;
         for J in 1 .. Value.invocations.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.invocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.logicalLocations.Is_Null then
         Handler.Key_Name ("logicalLocations");
         Handler.Start_Array;
         for J in 1 .. Value.logicalLocations.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.logicalLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.threadFlowLocations.Is_Null then
         Handler.Key_Name ("threadFlowLocations");
         Handler.Start_Array;
         for J in 1 .. Value.threadFlowLocations.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.threadFlowLocations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.results.Is_Null then
         Handler.Key_Name ("results");
         Handler.Start_Array;
         for J in 1 .. Value.results.Length loop
            Output_externalPropertyFileReference (Handler, Value.results (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.taxonomies.Is_Null then
         Handler.Key_Name ("taxonomies");
         Handler.Start_Array;
         for J in 1 .. Value.taxonomies.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.taxonomies (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.addresses.Is_Null then
         Handler.Key_Name ("addresses");
         Handler.Start_Array;
         for J in 1 .. Value.addresses.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.addresses (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.driver.Is_Set then
         Handler.Key_Name ("driver");
         Output_externalPropertyFileReference (Handler, Value.driver.Value);
      end if;
      if not Value.extensions.Is_Null then
         Handler.Key_Name ("extensions");
         Handler.Start_Array;
         for J in 1 .. Value.extensions.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.extensions (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.policies.Is_Null then
         Handler.Key_Name ("policies");
         Handler.Start_Array;
         for J in 1 .. Value.policies.Length loop
            Output_externalPropertyFileReference (Handler, Value.policies (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.translations.Is_Null then
         Handler.Key_Name ("translations");
         Handler.Start_Array;
         for J in 1 .. Value.translations.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.translations (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.webRequests.Is_Null then
         Handler.Key_Name ("webRequests");
         Handler.Start_Array;
         for J in 1 .. Value.webRequests.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.webRequests (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.webResponses.Is_Null then
         Handler.Key_Name ("webResponses");
         Handler.Start_Array;
         for J in 1 .. Value.webResponses.Length loop
            Output_externalPropertyFileReference
              (Handler, Value.webResponses (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_externalPropertyFileReferences;

   procedure Output_a_exception
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : a_exception) is
   begin
      Handler.Start_Object;
      if not Value.kind.Is_Null then
         Handler.Key_Name ("kind");
         Handler.String_Value (Value.kind);
      end if;
      if not Value.message.Is_Null then
         Handler.Key_Name ("message");
         Handler.String_Value (Value.message);
      end if;
      if Value.stack.Is_Set then
         Handler.Key_Name ("stack");
         Output_stack (Handler, Value.stack.Value);
      end if;
      if not Value.innerExceptions.Is_Null then
         Handler.Key_Name ("innerExceptions");
         Handler.Start_Array;
         for J in 1 .. Value.innerExceptions.Length loop
            Output_a_exception (Handler, Value.innerExceptions (J));
         end loop;
         Handler.End_Array;
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_a_exception;

   procedure Output_threadFlowLocation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : threadFlowLocation) is
   begin
      Handler.Start_Object;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Output_location (Handler, Value.location.Value);
      end if;
      if Value.stack.Is_Set then
         Handler.Key_Name ("stack");
         Output_stack (Handler, Value.stack.Value);
      end if;
      if not Value.kinds.Is_Empty then
         Handler.Key_Name ("kinds");
         Handler.Start_Array;
         for J in 1 .. Value.kinds.Length loop
            Handler.String_Value (Value.kinds (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.taxa.Is_Null then
         Handler.Key_Name ("taxa");
         Handler.Start_Array;
         for J in 1 .. Value.taxa.Length loop
            Output_reportingDescriptorReference (Handler, Value.taxa (J));
         end loop;
         Handler.End_Array;
      end if;
      if not Value.module.Is_Null then
         Handler.Key_Name ("module");
         Handler.String_Value (Value.module);
      end if;
      if not Value.state.Is_Empty then
         Handler.Key_Name ("state");
         Output_Any_Value (Handler, Value.state);
      end if;
      if Value.nestingLevel.Is_Set then
         Handler.Key_Name ("nestingLevel");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.nestingLevel.Value)));
      end if;
      if Value.executionOrder.Is_Set then
         Handler.Key_Name ("executionOrder");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.executionOrder.Value)));
      end if;
      if not Value.executionTimeUtc.Is_Null then
         Handler.Key_Name ("executionTimeUtc");
         Handler.String_Value (Value.executionTimeUtc);
      end if;
      if Value.importance.Is_Set then
         Handler.Key_Name ("importance");
         Output_threadFlowLocation_importance
           (Handler, Value.importance.Value);
      end if;
      if Value.webRequest.Is_Set then
         Handler.Key_Name ("webRequest");
         Output_webRequest (Handler, Value.webRequest.Value);
      end if;
      if Value.webResponse.Is_Set then
         Handler.Key_Name ("webResponse");
         Output_webResponse (Handler, Value.webResponse.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_threadFlowLocation;

   procedure Output_codeFlow
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : codeFlow) is
   begin
      Handler.Start_Object;
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      Handler.Key_Name ("threadFlows");
      Handler.Start_Array;
      for J in 1 .. Value.threadFlows.Length loop
         Output_threadFlow (Handler, Value.threadFlows (J));
      end loop;
      Handler.End_Array;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_codeFlow;

   procedure Output_multiformatMessageString
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : multiformatMessageString) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("text");
      Handler.String_Value (Value.text);
      if not Value.markdown.Is_Null then
         Handler.Key_Name ("markdown");
         Handler.String_Value (Value.markdown);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_multiformatMessageString;

   procedure Output_artifactContent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : artifactContent) is
   begin
      Handler.Start_Object;
      if not Value.text.Is_Null then
         Handler.Key_Name ("text");
         Handler.String_Value (Value.text);
      end if;
      if not Value.binary.Is_Null then
         Handler.Key_Name ("binary");
         Handler.String_Value (Value.binary);
      end if;
      if Value.rendered.Is_Set then
         Handler.Key_Name ("rendered");
         Output_multiformatMessageString (Handler, Value.rendered.Value);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_artifactContent;

   procedure Output_webResponse
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : webResponse) is
   begin
      Handler.Start_Object;
      if Value.index.Is_Set then
         Handler.Key_Name ("index");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.index.Value)));
      end if;
      if not Value.protocol.Is_Null then
         Handler.Key_Name ("protocol");
         Handler.String_Value (Value.protocol);
      end if;
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      if Value.statusCode.Is_Set then
         Handler.Key_Name ("statusCode");
         Handler.Integer_Value
           (Interfaces.Integer_64 (Integer'(Value.statusCode.Value)));
      end if;
      if not Value.reasonPhrase.Is_Null then
         Handler.Key_Name ("reasonPhrase");
         Handler.String_Value (Value.reasonPhrase);
      end if;
      if not Value.headers.Is_Empty then
         Handler.Key_Name ("headers");
         Output_Any_Value (Handler, Value.headers);
      end if;
      if Value.a_body.Is_Set then
         Handler.Key_Name ("body");
         Output_artifactContent (Handler, Value.a_body.Value);
      end if;
      if Value.noResponseReceived then
         Handler.Key_Name ("noResponseReceived");
         Handler.Boolean_Value (Value.noResponseReceived);
      end if;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_webResponse;

   procedure Output_threadFlow
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : threadFlow) is
   begin
      Handler.Start_Object;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      if Value.message.Is_Set then
         Handler.Key_Name ("message");
         Output_message (Handler, Value.message.Value);
      end if;
      if not Value.initialState.Is_Empty then
         Handler.Key_Name ("initialState");
         Output_Any_Value (Handler, Value.initialState);
      end if;
      if not Value.immutableState.Is_Empty then
         Handler.Key_Name ("immutableState");
         Output_Any_Value (Handler, Value.immutableState);
      end if;
      Handler.Key_Name ("locations");
      Handler.Start_Array;
      for J in 1 .. Value.locations.Length loop
         Output_threadFlowLocation (Handler, Value.locations (J));
      end loop;
      Handler.End_Array;
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_threadFlow;

   procedure Output_configurationOverride
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : configurationOverride) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("configuration");
      Output_reportingConfiguration (Handler, Value.configuration);
      Handler.Key_Name ("descriptor");
      Output_reportingDescriptorReference (Handler, Value.descriptor);
      if Value.properties.Is_Set then
         Handler.Key_Name ("properties");
         Output_propertyBag (Handler, Value.properties.Value);
      end if;
      Handler.End_Object;
   end Output_configurationOverride;

end SARIF.Types.Outputs;
