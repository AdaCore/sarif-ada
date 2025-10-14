--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with VSS.JSON.Pull_Readers;

package SARIF.Types.Inputs is

   procedure Input_notification_level
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.notification_level;
      Success : in out Boolean);

   procedure Input_suppression_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.suppression_kind;
      Success : in out Boolean);

   procedure Input_suppression_status
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.suppression_status;
      Success : in out Boolean);

   procedure Input_toolComponent_contents
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.toolComponent_contents;
      Success : in out Boolean);

   procedure Input_artifact_roles
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.artifact_roles;
      Success : in out Boolean);

   procedure Input_reportingConfiguration_level
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.reportingConfiguration_level;
      Success : in out Boolean);

   procedure Input_result_kind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.result_kind;
      Success : in out Boolean);

   procedure Input_result_level
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.result_level;
      Success : in out Boolean);

   procedure Input_result_baselineState
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.result_baselineState;
      Success : in out Boolean);

   procedure Input_run_columnKind
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.run_columnKind;
      Success : in out Boolean);

   procedure Input_threadFlowLocation_importance
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Enum.threadFlowLocation_importance;
      Success : in out Boolean);

   procedure Input_node
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out node;
      Success : in out Boolean);

   procedure Input_edgeTraversal
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out edgeTraversal;
      Success : in out Boolean);

   procedure Input_stackFrame
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out stackFrame;
      Success : in out Boolean);

   procedure Input_reportingDescriptorRelationship
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingDescriptorRelationship;
      Success : in out Boolean);

   procedure Input_propertyBag
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out propertyBag;
      Success : in out Boolean);

   procedure Input_logicalLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out logicalLocation;
      Success : in out Boolean);

   procedure Input_resultProvenance
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out resultProvenance;
      Success : in out Boolean);

   procedure Input_message
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out message;
      Success : in out Boolean);

   procedure Input_artifactChange
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifactChange;
      Success : in out Boolean);

   procedure Input_address
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out address;
      Success : in out Boolean);

   procedure Input_reportingDescriptorReference
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingDescriptorReference;
      Success : in out Boolean);

   procedure Input_translationMetadata
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out translationMetadata;
      Success : in out Boolean);

   procedure Input_notification
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out notification;
      Success : in out Boolean);

   procedure Input_physicalLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out physicalLocation;
      Success : in out Boolean);

   procedure Input_versionControlDetails
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out versionControlDetails;
      Success : in out Boolean);

   procedure Input_location
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out location;
      Success : in out Boolean);

   procedure Input_suppression
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out suppression;
      Success : in out Boolean);

   procedure Input_externalPropertyFileReference
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out externalPropertyFileReference;
      Success : in out Boolean);

   procedure Input_locationRelationship
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out locationRelationship;
      Success : in out Boolean);

   procedure Input_edge
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out edge;
      Success : in out Boolean);

   procedure Input_Root
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out Root;
      Success : in out Boolean);

   procedure Input_specialLocations
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out specialLocations;
      Success : in out Boolean);

   procedure Input_toolComponent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out toolComponent;
      Success : in out Boolean);

   procedure Input_invocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out invocation;
      Success : in out Boolean);

   procedure Input_conversion
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out conversion;
      Success : in out Boolean);

   procedure Input_runAutomationDetails
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out runAutomationDetails;
      Success : in out Boolean);

   procedure Input_graph
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out graph;
      Success : in out Boolean);

   procedure Input_externalProperties
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out externalProperties;
      Success : in out Boolean);

   procedure Input_rectangle
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out rectangle;
      Success : in out Boolean);

   procedure Input_reportingDescriptor
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingDescriptor;
      Success : in out Boolean);

   procedure Input_artifact
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifact;
      Success : in out Boolean);

   procedure Input_reportingConfiguration
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out reportingConfiguration;
      Success : in out Boolean);

   procedure Input_toolComponentReference
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out toolComponentReference;
      Success : in out Boolean);

   procedure Input_tool
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out tool;
      Success : in out Boolean);

   procedure Input_webRequest
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out webRequest;
      Success : in out Boolean);

   procedure Input_fix
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out fix;
      Success : in out Boolean);

   procedure Input_result
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out result;
      Success : in out Boolean);

   procedure Input_region
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out region;
      Success : in out Boolean);

   procedure Input_artifactLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifactLocation;
      Success : in out Boolean);

   procedure Input_graphTraversal
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out graphTraversal;
      Success : in out Boolean);

   procedure Input_attachment
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out attachment;
      Success : in out Boolean);

   procedure Input_stack
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out stack;
      Success : in out Boolean);

   procedure Input_replacement
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out replacement;
      Success : in out Boolean);

   procedure Input_run
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out run;
      Success : in out Boolean);

   procedure Input_externalPropertyFileReferences
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out externalPropertyFileReferences;
      Success : in out Boolean);

   procedure Input_a_exception
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out a_exception;
      Success : in out Boolean);

   procedure Input_threadFlowLocation
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out threadFlowLocation;
      Success : in out Boolean);

   procedure Input_codeFlow
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out codeFlow;
      Success : in out Boolean);

   procedure Input_multiformatMessageString
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out multiformatMessageString;
      Success : in out Boolean);

   procedure Input_artifactContent
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out artifactContent;
      Success : in out Boolean);

   procedure Input_webResponse
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out webResponse;
      Success : in out Boolean);

   procedure Input_threadFlow
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out threadFlow;
      Success : in out Boolean);

   procedure Input_configurationOverride
     (Reader  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out configurationOverride;
      Success : in out Boolean);

end SARIF.Types.Inputs;
