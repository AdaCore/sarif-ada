--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Style_Checks ("M99");  --  suppress style warning unitl gnatpp is fixed
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with VSS.JSON.Streams;
with VSS.Strings;
with VSS.String_Vectors;

package SARIF.Types is
   package JSON_Event_Lists is new Ada.Containers.Doubly_Linked_Lists
     (VSS.JSON.Streams.JSON_Stream_Element, VSS.JSON.Streams."=");

   type Any_Value is new JSON_Event_Lists.List with null record;
   type Any_Object is new Any_Value with null record;

   type Optional_Integer (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer;
         when False =>
            null;
      end case;
   end record;

   type Optional_Float (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Float;
         when False =>
            null;
      end case;
   end record;

   type Integer_Or_String (Is_String : Boolean := False) is record
      case Is_String is
         when False =>
            Integer : Standard.Integer;
         when True =>
            String : VSS.Strings.Virtual_String;
      end case;
   end record;

   type Optional_Integer_Or_String (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Integer_Or_String;
         when False =>
            null;
      end case;
   end record;

   type configurationOverride_Vector is tagged private with
     Variable_Indexing => Get_configurationOverride_Variable_Reference,
     Constant_Indexing => Get_configurationOverride_Constant_Reference;

   type locationRelationship_Vector is tagged private with
     Variable_Indexing => Get_locationRelationship_Variable_Reference,
     Constant_Indexing => Get_locationRelationship_Constant_Reference;

   type threadFlowLocation_Vector is tagged private with
     Variable_Indexing => Get_threadFlowLocation_Variable_Reference,
     Constant_Indexing => Get_threadFlowLocation_Constant_Reference;

   type result_Vector is tagged private with
     Variable_Indexing => Get_result_Variable_Reference,
     Constant_Indexing => Get_result_Constant_Reference;

   type runAutomationDetails_Vector is tagged private with
     Variable_Indexing => Get_runAutomationDetails_Variable_Reference,
     Constant_Indexing => Get_runAutomationDetails_Constant_Reference;

   type replacement_Vector is tagged private with
     Variable_Indexing => Get_replacement_Variable_Reference,
     Constant_Indexing => Get_replacement_Constant_Reference;

   type webResponse_Vector is tagged private with
     Variable_Indexing => Get_webResponse_Variable_Reference,
     Constant_Indexing => Get_webResponse_Constant_Reference;

   type address_Vector is tagged private with
     Variable_Indexing => Get_address_Variable_Reference,
     Constant_Indexing => Get_address_Constant_Reference;

   type physicalLocation_Vector is tagged private with
     Variable_Indexing => Get_physicalLocation_Variable_Reference,
     Constant_Indexing => Get_physicalLocation_Constant_Reference;

   type stackFrame_Vector is tagged private with
     Variable_Indexing => Get_stackFrame_Variable_Reference,
     Constant_Indexing => Get_stackFrame_Constant_Reference;

   type toolComponent_Vector is tagged private with
     Variable_Indexing => Get_toolComponent_Variable_Reference,
     Constant_Indexing => Get_toolComponent_Constant_Reference;

   type externalPropertyFileReference_Vector is tagged private with
     Variable_Indexing => Get_externalPropertyFileReference_Variable_Reference,
     Constant_Indexing => Get_externalPropertyFileReference_Constant_Reference;

   type stack_Vector is tagged private with
     Variable_Indexing => Get_stack_Variable_Reference,
     Constant_Indexing => Get_stack_Constant_Reference;

   type Integer_Vector is tagged private with
     Variable_Indexing => Get_Integer_Variable_Reference,
     Constant_Indexing => Get_Integer_Constant_Reference;

   type notification_Vector is tagged private with
     Variable_Indexing => Get_notification_Variable_Reference,
     Constant_Indexing => Get_notification_Constant_Reference;

   type attachment_Vector is tagged private with
     Variable_Indexing => Get_attachment_Variable_Reference,
     Constant_Indexing => Get_attachment_Constant_Reference;

   type suppression_Vector is tagged private with
     Variable_Indexing => Get_suppression_Variable_Reference,
     Constant_Indexing => Get_suppression_Constant_Reference;

   type edgeTraversal_Vector is tagged private with
     Variable_Indexing => Get_edgeTraversal_Variable_Reference,
     Constant_Indexing => Get_edgeTraversal_Constant_Reference;

   type graphTraversal_Vector is tagged private with
     Variable_Indexing => Get_graphTraversal_Variable_Reference,
     Constant_Indexing => Get_graphTraversal_Constant_Reference;

   type location_Vector is tagged private with
     Variable_Indexing => Get_location_Variable_Reference,
     Constant_Indexing => Get_location_Constant_Reference;

   type graph_Vector is tagged private with
     Variable_Indexing => Get_graph_Variable_Reference,
     Constant_Indexing => Get_graph_Constant_Reference;

   type reportingDescriptorReference_Vector is tagged private with
     Variable_Indexing => Get_reportingDescriptorReference_Variable_Reference,
     Constant_Indexing => Get_reportingDescriptorReference_Constant_Reference;

   type reportingDescriptorRelationship_Vector is tagged private with
     Variable_Indexing =>
      Get_reportingDescriptorRelationship_Variable_Reference,
     Constant_Indexing =>
      Get_reportingDescriptorRelationship_Constant_Reference;

   type region_Vector is tagged private with
     Variable_Indexing => Get_region_Variable_Reference,
     Constant_Indexing => Get_region_Constant_Reference;

   type versionControlDetails_Vector is tagged private with
     Variable_Indexing => Get_versionControlDetails_Variable_Reference,
     Constant_Indexing => Get_versionControlDetails_Constant_Reference;

   type rectangle_Vector is tagged private with
     Variable_Indexing => Get_rectangle_Variable_Reference,
     Constant_Indexing => Get_rectangle_Constant_Reference;

   type codeFlow_Vector is tagged private with
     Variable_Indexing => Get_codeFlow_Variable_Reference,
     Constant_Indexing => Get_codeFlow_Constant_Reference;

   type toolComponentReference_Vector is tagged private with
     Variable_Indexing => Get_toolComponentReference_Variable_Reference,
     Constant_Indexing => Get_toolComponentReference_Constant_Reference;

   type edge_Vector is tagged private with
     Variable_Indexing => Get_edge_Variable_Reference,
     Constant_Indexing => Get_edge_Constant_Reference;

   type toolComponent_contents_Vector is tagged private with
     Variable_Indexing => Get_toolComponent_contents_Variable_Reference,
     Constant_Indexing => Get_toolComponent_contents_Constant_Reference;

   type node_Vector is tagged private with
     Variable_Indexing => Get_node_Variable_Reference,
     Constant_Indexing => Get_node_Constant_Reference;

   type threadFlow_Vector is tagged private with
     Variable_Indexing => Get_threadFlow_Variable_Reference,
     Constant_Indexing => Get_threadFlow_Constant_Reference;

   type fix_Vector is tagged private with
     Variable_Indexing => Get_fix_Variable_Reference,
     Constant_Indexing => Get_fix_Constant_Reference;

   type invocation_Vector is tagged private with
     Variable_Indexing => Get_invocation_Variable_Reference,
     Constant_Indexing => Get_invocation_Constant_Reference;

   type artifactChange_Vector is tagged private with
     Variable_Indexing => Get_artifactChange_Variable_Reference,
     Constant_Indexing => Get_artifactChange_Constant_Reference;

   type artifact_roles_Vector is tagged private with
     Variable_Indexing => Get_artifact_roles_Variable_Reference,
     Constant_Indexing => Get_artifact_roles_Constant_Reference;

   type artifact_Vector is tagged private with
     Variable_Indexing => Get_artifact_Variable_Reference,
     Constant_Indexing => Get_artifact_Constant_Reference;

   type logicalLocation_Vector is tagged private with
     Variable_Indexing => Get_logicalLocation_Variable_Reference,
     Constant_Indexing => Get_logicalLocation_Constant_Reference;

   type webRequest_Vector is tagged private with
     Variable_Indexing => Get_webRequest_Variable_Reference,
     Constant_Indexing => Get_webRequest_Constant_Reference;

   type externalProperties_Vector is tagged private with
     Variable_Indexing => Get_externalProperties_Variable_Reference,
     Constant_Indexing => Get_externalProperties_Constant_Reference;

   type run_Vector is tagged private with
     Variable_Indexing => Get_run_Variable_Reference,
     Constant_Indexing => Get_run_Constant_Reference;

   type a_exception_Vector is tagged private with
     Variable_Indexing => Get_a_exception_Variable_Reference,
     Constant_Indexing => Get_a_exception_Constant_Reference;

   type reportingDescriptor_Vector is tagged private with
     Variable_Indexing => Get_reportingDescriptor_Variable_Reference,
     Constant_Indexing => Get_reportingDescriptor_Constant_Reference;

   type artifactLocation_Vector is tagged private with
     Variable_Indexing => Get_artifactLocation_Variable_Reference,
     Constant_Indexing => Get_artifactLocation_Constant_Reference;

   package Enum is

      type notification_level is (none, note, warning, error);

      type Optional_notification_level (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : notification_level;
            when False =>
               null;
         end case;
      end record;

      type suppression_kind is (inSource, external);

      type suppression_status is (accepted, underReview, rejected);

      type Optional_suppression_status (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : suppression_status;
            when False =>
               null;
         end case;
      end record;

      type toolComponent_contents is (localizedData, nonLocalizedData);

      type artifact_roles is
        (analysisTarget, attachment, responseFile, resultFile, standardStream,
         tracedFile, unmodified, modified, added, deleted, renamed,
         uncontrolled, driver, extension, translation, taxonomy, policy,
         referencedOnCommandLine, memoryContents, directory,
         userSpecifiedConfiguration, toolSpecifiedConfiguration,
         debugOutputFile);

      type reportingConfiguration_level is (none, note, warning, error);

      type Optional_reportingConfiguration_level (Is_Set : Boolean := False) is
      record
         case Is_Set is
            when True =>
               Value : reportingConfiguration_level;
            when False =>
               null;
         end case;
      end record;

      type result_kind is
        (notApplicable, pass, fail, review, open, informational);

      type Optional_result_kind (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : result_kind;
            when False =>
               null;
         end case;
      end record;

      type result_level is (none, note, warning, error);

      type Optional_result_level (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : result_level;
            when False =>
               null;
         end case;
      end record;

      type result_baselineState is (a_new, unchanged, updated, absent);

      type Optional_result_baselineState (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : result_baselineState;
            when False =>
               null;
         end case;
      end record;

      type run_columnKind is (utf16CodeUnits, unicodeCodePoints);

      type Optional_run_columnKind (Is_Set : Boolean := False) is record
         case Is_Set is
            when True =>
               Value : run_columnKind;
            when False =>
               null;
         end case;
      end record;

      type threadFlowLocation_importance is
        (important, essential, unimportant);

      type Optional_threadFlowLocation_importance (Is_Set : Boolean := False)
      is
      record
         case Is_Set is
            when True =>
               Value : threadFlowLocation_importance;
            when False =>
               null;
         end case;
      end record;

   end Enum;

   type propertyBag is record
      tags : VSS.String_Vectors.Virtual_String_Vector;
      --  A set of distinct strings that provide additional information.
   end record;

   type Optional_propertyBag (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : propertyBag;
         when False =>
            null;
      end case;
   end record;

   type message is record
      text       : VSS.Strings.Virtual_String;
      --  A plain text message string.
      markdown   : VSS.Strings.Virtual_String;
      --  A Markdown message string.
      id         : VSS.Strings.Virtual_String;
      --  The identifier for this message.
      arguments  : VSS.String_Vectors.Virtual_String_Vector;
      --  An array of strings to substitute into the message string.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  message.
   end record;

   type Optional_message (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : message;
         when False =>
            null;
      end case;
   end record;

   type address is record
      absoluteAddress    : Optional_Integer;
      --  The address expressed as a byte offset from the start of the
      --  addressable region.
      relativeAddress    : Optional_Integer;
      --  The address expressed as a byte offset from the absolute address of
      --  the top-most parent object.
      length             : Optional_Integer;
      --  The number of bytes in this range of addresses.
      kind               : VSS.Strings.Virtual_String;
      --  An open-ended string that identifies the address kind. 'data',
      --  'function', 'header','instruction', 'module', 'page', 'section',
      --  'segment', 'stack', 'stackFrame', 'table' are well-known values.
      name               : VSS.Strings.Virtual_String;
      --  A name that is associated with the address, e.g., '.text'.
      fullyQualifiedName : VSS.Strings.Virtual_String;
      --  A human-readable fully qualified name that is associated with the
      --  address.
      offsetFromParent   : Optional_Integer;
      --  The byte offset of this address from the absolute or relative address
      --  of the parent object.
      index              : Optional_Integer;
      --  The index within run.addresses of the cached object for this address.
      parentIndex        : Optional_Integer;
      --  The index within run.addresses of the parent object.
      properties         : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  address.
   end record;

   type Optional_address (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : address;
         when False =>
            null;
      end case;
   end record;

   type artifactLocation is record
      uri         : VSS.Strings.Virtual_String;
      --  A string containing a valid relative or absolute URI.
      uriBaseId   : VSS.Strings.Virtual_String;
      --  A string which indirectly specifies the absolute URI with respect to
      --  which a relative URI in the "uri" property is interpreted.
      index       : Optional_Integer;
      --  The index within the run artifacts array of the artifact object
      --  associated with the artifact location.
      description : Optional_message;
      --  A short description of the artifact location.
      properties  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  artifact location.
   end record;

   type Optional_artifactLocation (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : artifactLocation;
         when False =>
            null;
      end case;
   end record;

   type multiformatMessageString is record
      text       : VSS.Strings.Virtual_String;
      --  A plain text message string or format string.
      markdown   : VSS.Strings.Virtual_String;
      --  A Markdown message string or format string.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  message.
   end record;

   type Optional_multiformatMessageString (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : multiformatMessageString;
         when False =>
            null;
      end case;
   end record;

   type artifactContent is record
      text       : VSS.Strings.Virtual_String;
      --  UTF-8-encoded content from a text artifact.
      binary     : VSS.Strings.Virtual_String;
      --  MIME Base64-encoded content from a binary artifact, or from a text
      --  artifact in its original encoding.
      rendered   : Optional_multiformatMessageString;
      --  An alternate rendered representation of the artifact (e.g., a
      --  decompiled representation of a binary region).
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  artifact content.
   end record;

   type Optional_artifactContent (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : artifactContent;
         when False =>
            null;
      end case;
   end record;

   type region is record
      startLine      : Optional_Integer;
      --  The line number of the first character in the region.
      startColumn    : Optional_Integer;
      --  The column number of the first character in the region.
      endLine        : Optional_Integer;
      --  The line number of the last character in the region.
      endColumn      : Optional_Integer;
      --  The column number of the character following the end of the region.
      charOffset     : Optional_Integer;
      --  The zero-based offset from the beginning of the artifact of the first
      --  character in the region.
      charLength     : Optional_Integer;
      --  The length of the region in characters.
      byteOffset     : Optional_Integer;
      --  The zero-based offset from the beginning of the artifact of the first
      --  byte in the region.
      byteLength     : Optional_Integer;
      --  The length of the region in bytes.
      snippet        : Optional_artifactContent;
      --  The portion of the artifact contents within the specified region.
      message        : Optional_message;
      --  A message relevant to the region.
      sourceLanguage : VSS.Strings.Virtual_String;
      --  Specifies the source language, if any, of the portion of the artifact
      --  specified by the region object.
      properties     : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the region.
   end record;

   type Optional_region (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : region;
         when False =>
            null;
      end case;
   end record;

   type physicalLocation is record
      address          : Optional_address;
      --  The address of the location.
      artifactLocation : Optional_artifactLocation;
      --  The location of the artifact.
      region           : Optional_region;
      --  Specifies a portion of the artifact.
      contextRegion    : Optional_region;
      --  Specifies a portion of the artifact that encloses the region. Allows
      --  a viewer to display additional context around the region.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  physical location.
   end record;

   type Optional_physicalLocation (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : physicalLocation;
         when False =>
            null;
      end case;
   end record;

   type location is record
      id               : Optional_Integer;
      --  Value that distinguishes this location from all other locations
      --  within a single result object.
      physicalLocation : Optional_physicalLocation;
      --  Identifies the artifact and region.
      logicalLocations : logicalLocation_Vector;
      --  The logical locations associated with the result.
      message          : Optional_message;
      --  A message relevant to the location.
      annotations      : region_Vector;
      --  A set of regions relevant to the location.
      relationships    : locationRelationship_Vector;
      --  An array of objects that describe relationships between this location
      --  and others.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  location.
   end record;

   type Optional_location (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : location;
         when False =>
            null;
      end case;
   end record;

   type node is record
      id         : VSS.Strings.Virtual_String;
      --  A string that uniquely identifies the node within its graph.
      label      : Optional_message;
      --  A short description of the node.
      location   : Optional_location;
      --  A code location associated with the node.
      children   : node_Vector;
      --  Array of child nodes.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the node.
   end record;

   type edgeTraversal is record
      edgeId            : VSS.Strings.Virtual_String;
      --  Identifies the edge being traversed.
      message           : Optional_message;
      --  A message to display to the user as the edge is traversed.
      finalState        : Any_Object;
      --  The values of relevant expressions after the edge has been traversed.
      stepOverEdgeCount : Optional_Integer;
      --  The number of edge traversals necessary to return from a nested
      --  graph.
      properties        : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the edge
      --  traversal.
   end record;

   type stackFrame is record
      location   : Optional_location;
      --  The location to which this stack frame refers.
      module     : VSS.Strings.Virtual_String;
      --  The name of the module that contains the code of this stack frame.
      threadId   : Optional_Integer;
      --  The thread identifier of the stack frame.
      parameters : VSS.String_Vectors.Virtual_String_Vector;
      --  The parameters of the call that is executing.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the stack
      --  frame.
   end record;

   type toolComponentReference is record
      name       : VSS.Strings.Virtual_String;
      --  The 'name' property of the referenced toolComponent.
      index      : Optional_Integer;
      --  An index into the referenced toolComponent in tool.extensions.
      guid       : VSS.Strings.Virtual_String;
      --  The 'guid' property of the referenced toolComponent.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  toolComponentReference.
   end record;

   type Optional_toolComponentReference (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : toolComponentReference;
         when False =>
            null;
      end case;
   end record;

   type reportingDescriptorReference is record
      id            : VSS.Strings.Virtual_String;
      --  The id of the descriptor.
      index         : Optional_Integer;
      --  The index into an array of descriptors in
      --  toolComponent.ruleDescriptors, toolComponent.notificationDescriptors,
      --  or toolComponent.taxonomyDescriptors, depending on context.
      guid          : VSS.Strings.Virtual_String;
      --  A guid that uniquely identifies the descriptor.
      toolComponent : Optional_toolComponentReference;
      --  A reference used to locate the toolComponent associated with the
      --  descriptor.
      properties    : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  reporting descriptor reference.
   end record;

   type Optional_reportingDescriptorReference (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : reportingDescriptorReference;
         when False =>
            null;
      end case;
   end record;

   type reportingDescriptorRelationship is record
      target      : reportingDescriptorReference;
      --  A reference to the related reporting descriptor.
      kinds       : VSS.String_Vectors.Virtual_String_Vector;
      --  A set of distinct strings that categorize the relationship.
      --  Well-known kinds include 'canPrecede', 'canFollow', 'willPrecede',
      --  'willFollow', 'superset', 'subset', 'equal', 'disjoint', 'relevant',
      --  and 'incomparable'.
      description : Optional_message;
      --  A description of the reporting descriptor relationship.
      properties  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  reporting descriptor reference.
   end record;

   type logicalLocation is record
      name               : VSS.Strings.Virtual_String;
      --  Identifies the construct in which the result occurred. For example,
      --  this property might contain the name of a class or a method.
      index              : Optional_Integer;
      --  The index within the logical locations array.
      fullyQualifiedName : VSS.Strings.Virtual_String;
      --  The human-readable fully qualified name of the logical location.
      decoratedName      : VSS.Strings.Virtual_String;
      --  The machine-readable name for the logical location, such as a
      --  mangled function name provided by a C++ compiler that encodes calling
      --  convention, return type and other details along with the function
      --  name.
      parentIndex        : Optional_Integer;
      --  Identifies the index of the immediate parent of the construct in
      --  which the result was detected. For example, this property might point
      --  to a logical location that represents the namespace that holds a
      --  type.
      kind               : VSS.Strings.Virtual_String;
      --  The type of construct this logical location component refers to.
      --  Should be one of 'function', 'member', 'module', 'namespace',
      --  'parameter', 'resource', 'returnType', 'type', 'variable', 'object',
      --  'array', 'property', 'value', 'element', 'text', 'attribute',
      --  'comment', 'declaration', 'dtd' or 'processingInstruction', if any
      --  of those accurately describe the construct.
      properties         : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the logical
      --  location.
   end record;

   type resultProvenance is record
      firstDetectionTimeUtc : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which the
      --  result was first detected. See "Date/time properties" in the SARIF
      --  spec for the required format.
      lastDetectionTimeUtc  : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which the
      --  result was most recently detected. See "Date/time properties" in
      --  the SARIF spec for the required format.
      firstDetectionRunGuid : VSS.Strings.Virtual_String;
      --  A GUID-valued string equal to the automationDetails.guid property of
      --  the run in which the result was first detected.
      lastDetectionRunGuid  : VSS.Strings.Virtual_String;
      --  A GUID-valued string equal to the automationDetails.guid property of
      --  the run in which the result was most recently detected.
      invocationIndex       : Optional_Integer;
      --  The index within the run.invocations array of the invocation object
      --  which describes the tool invocation that detected the result.
      conversionSources     : physicalLocation_Vector;
      --  An array of physicalLocation objects which specify the portions of an
      --  analysis tool's output that a converter transformed into the result.
      properties            : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the result.
   end record;

   type Optional_resultProvenance (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : resultProvenance;
         when False =>
            null;
      end case;
   end record;

   type artifactChange is record
      artifactLocation : SARIF.Types.artifactLocation;
      --  The location of the artifact to change.
      replacements     : replacement_Vector;
      --  An array of replacement objects, each of which represents the
      --  replacement of a single region in a single artifact specified
      --  by 'artifactLocation'.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the change.
   end record;

   type translationMetadata is record
      name             : VSS.Strings.Virtual_String;
      --  The name associated with the translation metadata.
      fullName         : VSS.Strings.Virtual_String;
      --  The full name associated with the translation metadata.
      shortDescription : Optional_multiformatMessageString;
      --  A brief description of the translation metadata.
      fullDescription  : Optional_multiformatMessageString;
      --  A comprehensive description of the translation metadata.
      downloadUri      : VSS.Strings.Virtual_String;
      --  The absolute URI from which the translation metadata can be
      --  downloaded.
      informationUri   : VSS.Strings.Virtual_String;
      --  The absolute URI from which information related to the translation
      --  metadata can be downloaded.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  translation metadata.
   end record;

   type Optional_translationMetadata (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : translationMetadata;
         when False =>
            null;
      end case;
   end record;

   type stack is record
      message    : Optional_message;
      --  A message relevant to this call stack.
      frames     : stackFrame_Vector;
      --  An array of stack frames that represents a sequence of calls,
      --  rendered in reverse chronological order, that comprise the call
      --  stack.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the stack.
   end record;

   type Optional_stack (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : stack;
         when False =>
            null;
      end case;
   end record;

   type a_exception is record
      kind            : VSS.Strings.Virtual_String;
      --  A string that identifies the kind of exception, for example, the
      --  fully qualified type name of an object that was thrown, or the
      --  symbolic name of a signal.
      message         : VSS.Strings.Virtual_String;
      --  A message that describes the exception.
      stack           : Optional_stack;
      --  The sequence of function calls leading to the exception.
      innerExceptions : a_exception_Vector;
      --  An array of exception objects each of which is considered a cause of
      --  this exception.
      properties      : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  exception.
   end record;

   type Optional_a_exception (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : a_exception;
         when False =>
            null;
      end case;
   end record;

   type notification is record
      locations      : location_Vector;
      --  The locations relevant to this notification.
      message        : SARIF.Types.message;
      --  A message that describes the condition that was encountered.
      level          : Enum.Optional_notification_level;
      --  A value specifying the severity level of the notification.
      threadId       : Optional_Integer;
      --  The thread identifier of the code that generated the notification.
      timeUtc        : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which the
      --  analysis tool generated the notification.
      a_exception    : Optional_a_exception;
      --  The runtime exception, if any, relevant to this notification.
      descriptor     : Optional_reportingDescriptorReference;
      --  A reference used to locate the descriptor relevant to this
      --  notification.
      associatedRule : Optional_reportingDescriptorReference;
      --  A reference used to locate the rule descriptor associated with this
      --  notification.
      properties     : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  notification.
   end record;

   type versionControlDetails is record
      repositoryUri : VSS.Strings.Virtual_String;
      --  The absolute URI of the repository.
      revisionId    : VSS.Strings.Virtual_String;
      --  A string that uniquely and permanently identifies the revision within
      --  the repository.
      branch        : VSS.Strings.Virtual_String;
      --  The name of a branch containing the revision.
      revisionTag   : VSS.Strings.Virtual_String;
      --  A tag that has been applied to the revision.
      asOfTimeUtc   : VSS.Strings.Virtual_String;
      --  A Coordinated Universal Time (UTC) date and time that can be used
      --  to synchronize an enlistment to the state of the repository at that
      --  time.
      mappedTo      : Optional_artifactLocation;
      --  The location in the local file system to which the root of the
      --  repository was mapped at the time of the analysis.
      properties    : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the version
      --  control details.
   end record;

   type suppression is record
      guid          : VSS.Strings.Virtual_String;
      --  A stable, unique identifer for the suprression in the form of a GUID.
      kind          : Enum.suppression_kind;
      --  A string that indicates where the suppression is persisted.
      status        : Enum.Optional_suppression_status;
      --  A string that indicates the review status of the suppression.
      justification : VSS.Strings.Virtual_String;
      --  A string representing the justification for the suppression.
      location      : Optional_location;
      --  Identifies the location associated with the suppression.
      properties    : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  suppression.
   end record;

   type externalPropertyFileReference is record
      location   : Optional_artifactLocation;
      --  The location of the external property file.
      guid       : VSS.Strings.Virtual_String;
      --  A stable, unique identifer for the external property file in the form
      --  of a GUID.
      itemCount  : Optional_Integer;
      --  A non-negative integer specifying the number of items contained in
      --  the external property file.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  external property file.
   end record;

   type Optional_externalPropertyFileReference (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : externalPropertyFileReference;
         when False =>
            null;
      end case;
   end record;

   type locationRelationship is record
      target      : Integer;
      --  A reference to the related location.
      kinds       : VSS.String_Vectors.Virtual_String_Vector;
      --  A set of distinct strings that categorize the relationship.
      --  Well-known kinds include 'includes', 'isIncludedBy' and 'relevant'.
      description : Optional_message;
      --  A description of the location relationship.
      properties  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  location relationship.
   end record;

   type edge is record
      id           : VSS.Strings.Virtual_String;
      --  A string that uniquely identifies the edge within its graph.
      label        : Optional_message;
      --  A short description of the edge.
      sourceNodeId : VSS.Strings.Virtual_String;
      --  Identifies the source node (the node at which the edge starts).
      targetNodeId : VSS.Strings.Virtual_String;
      --  Identifies the target node (the node at which the edge ends).
      properties   : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the edge.
   end record;

   type Root is record
      schema                   : VSS.Strings.Virtual_String;
      --  The URI of the JSON schema corresponding to the version.
      runs                     : run_Vector;
      --  The set of runs contained in this log file.
      inlineExternalProperties : externalProperties_Vector;
      --  References to external property files that share data between runs.
      properties               : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the log
      --  file.
   end record;

   type specialLocations is record
      displayBase : Optional_artifactLocation;
      --  Provides a suggestion to SARIF consumers to display file paths
      --  relative to the specified location.
      properties  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the special
      --  locations.
   end record;

   type Optional_specialLocations (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : specialLocations;
         when False =>
            null;
      end case;
   end record;

   type toolComponent is record
      guid                                        : VSS.Strings.Virtual_String;
      --  A unique identifer for the tool component in the form of a GUID.
      name                                        : VSS.Strings.Virtual_String;
      --  The name of the tool component.
      organization                                : VSS.Strings.Virtual_String;
      --  The organization or company that produced the tool component.
      product                                     : VSS.Strings.Virtual_String;
      --  A product suite to which the tool component belongs.
      productSuite                                : VSS.Strings.Virtual_String;
      --  A localizable string containing the name of the suite of products to
      --  which the tool component belongs.
      shortDescription : Optional_multiformatMessageString;
      --  A brief description of the tool component.
      fullDescription : Optional_multiformatMessageString;
      --  A comprehensive description of the tool component.
      fullName                                    : VSS.Strings.Virtual_String;
      --  The name of the tool component along with its version and any other
      --  useful identifying information, such as its locale.
      version                                     : VSS.Strings.Virtual_String;
      --  The tool component version, in whatever format the component natively
      --  provides.
      semanticVersion                             : VSS.Strings.Virtual_String;
      --  The tool component version in the format specified by Semantic
      --  Versioning 2.0.
      dottedQuadFileVersion                       : VSS.Strings.Virtual_String;
      --  The binary version of the tool component's primary executable file
      --  expressed as four non-negative integers separated by a period (for
      --  operating systems that express file versions in this way).
      releaseDateUtc                              : VSS.Strings.Virtual_String;
      --  A string specifying the UTC date (and optionally, the time) of the
      --  component's release.
      downloadUri                                 : VSS.Strings.Virtual_String;
      --  The absolute URI from which the tool component can be downloaded.
      informationUri                              : VSS.Strings.Virtual_String;
      --  The absolute URI at which information about this version of the tool
      --  component can be found.
      globalMessageStrings                        : Any_Object;
      --  A dictionary, each of whose keys is a resource identifier and each
      --  of whose values is a multiformatMessageString object, which holds
      --  message strings in plain text and (optionally) Markdown format. The
      --  strings can include placeholders, which can be used to construct a
      --  message in combination with an arbitrary number of additional string
      --  arguments.
      notifications                               : reportingDescriptor_Vector;
      --  An array of reportingDescriptor objects relevant to the notifications
      --  related to the configuration and runtime execution of the tool
      --  component.
      rules                                       : reportingDescriptor_Vector;
      --  An array of reportingDescriptor objects relevant to the analysis
      --  performed by the tool component.
      taxa                                        : reportingDescriptor_Vector;
      --  An array of reportingDescriptor objects relevant to the definitions
      --  of both standalone and tool-defined taxonomies.
      locations                                   : artifactLocation_Vector;
      --  An array of the artifactLocation objects associated with the tool
      --  component.
      language                                    : VSS.Strings.Virtual_String;
      --  The language of the messages emitted into the log file during this
      --  run (expressed as an ISO 639-1 two-letter lowercase language code)
      --  and an optional region (expressed as an ISO 3166-1 two-letter
      --  uppercase subculture code associated with a country or region). The
      --  casing is recommended but not required (in order for this data to
      --  conform to RFC5646).
      contents : toolComponent_contents_Vector;
      --  The kinds of data contained in this object.
      isComprehensive                             : Boolean := Boolean'First;
      --  Specifies whether this object contains a complete definition of
      --  the localizable and/or non-localizable data for this component,
      --  as opposed to including only data that is relevant to the results
      --  persisted to this log file.
      localizedDataSemanticVersion                : VSS.Strings.Virtual_String;
      --  The semantic version of the localized strings defined in this
      --  component; maintained by components that provide translations.
      minimumRequiredLocalizedDataSemanticVersion : VSS.Strings.Virtual_String;
      --  The minimum value of localizedDataSemanticVersion required in
      --  translations consumed by this component; used by components
      --  that consume translations.
      associatedComponent : Optional_toolComponentReference;
      --  The component which is strongly associated with this component. For a
      --  translation, this refers to the component which has been translated.
      --  For an extension, this is the driver that provides the extension's
      --  plugin model.
      translationMetadata : Optional_translationMetadata;
      --  Translation metadata, required for a translation, not populated by
      --  other component types.
      supportedTaxonomies : toolComponentReference_Vector;
      --  An array of toolComponentReference objects to declare the taxonomies
      --  supported by the tool component.
      properties                                  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the tool
      --  component.
   end record;

   type Optional_toolComponent (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : toolComponent;
         when False =>
            null;
      end case;
   end record;

   type invocation is record
      commandLine                        : VSS.Strings.Virtual_String;
      --  The command line used to invoke the tool.
      arguments : VSS.String_Vectors.Virtual_String_Vector;
      --  An array of strings, containing in order the command line arguments
      --  passed to the tool from the operating system.
      responseFiles                      : artifactLocation_Vector;
      --  The locations of any response files specified on the tool's command
      --  line.
      startTimeUtc                       : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which the
      --  invocation started. See "Date/time properties" in the SARIF spec
      --  for the required format.
      endTimeUtc                         : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which the
      --  invocation ended. See "Date/time properties" in the SARIF spec
      --  for the required format.
      exitCode                           : Optional_Integer;
      --  The process exit code.
      ruleConfigurationOverrides         : configurationOverride_Vector;
      --  An array of configurationOverride objects that describe rules related
      --  runtime overrides.
      notificationConfigurationOverrides : configurationOverride_Vector;
      --  An array of configurationOverride objects that describe notifications
      --  related runtime overrides.
      toolExecutionNotifications         : notification_Vector;
      --  A list of runtime conditions detected by the tool during the
      --  analysis.
      toolConfigurationNotifications     : notification_Vector;
      --  A list of conditions detected by the tool that are relevant to the
      --  tool's configuration.
      exitCodeDescription                : VSS.Strings.Virtual_String;
      --  The reason for the process exit.
      exitSignalName                     : VSS.Strings.Virtual_String;
      --  The name of the signal that caused the process to exit.
      exitSignalNumber                   : Optional_Integer;
      --  The numeric value of the signal that caused the process to exit.
      processStartFailureMessage         : VSS.Strings.Virtual_String;
      --  The reason given by the operating system that the process failed to
      --  start.
      executionSuccessful                : Boolean;
      --  Specifies whether the tool's execution completed successfully.
      machine                            : VSS.Strings.Virtual_String;
      --  The machine on which the invocation occurred.
      account                            : VSS.Strings.Virtual_String;
      --  The account under which the invocation occurred.
      processId                          : Optional_Integer;
      --  The id of the process in which the invocation occurred.
      executableLocation                 : Optional_artifactLocation;
      --  An absolute URI specifying the location of the executable that was
      --  invoked.
      workingDirectory                   : Optional_artifactLocation;
      --  The working directory for the invocation.
      environmentVariables               : Any_Object;
      --  The environment variables associated with the analysis tool process,
      --  expressed as key/value pairs.
      stdin                              : Optional_artifactLocation;
      --  A file containing the standard input stream to the process that was
      --  invoked.
      stdout                             : Optional_artifactLocation;
      --  A file containing the standard output stream from the process that
      --  was invoked.
      stderr                             : Optional_artifactLocation;
      --  A file containing the standard error stream from the process that was
      --  invoked.
      stdoutStderr                       : Optional_artifactLocation;
      --  A file containing the interleaved standard output and standard error
      --  stream from the process that was invoked.
      properties                         : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  invocation.
   end record;

   type Optional_invocation (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : invocation;
         when False =>
            null;
      end case;
   end record;

   type tool is record
      driver     : toolComponent;
      --  The analysis tool that was run.
      extensions : toolComponent_Vector;
      --  Tool extensions that contributed to or reconfigured the analysis tool
      --  that was run.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the tool.
   end record;

   type conversion is record
      tool                 : SARIF.Types.tool;
      --  A tool object that describes the converter.
      invocation           : Optional_invocation;
      --  An invocation object that describes the invocation of the converter.
      analysisToolLogFiles : artifactLocation_Vector;
      --  The locations of the analysis tool's per-run log files.
      properties           : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  conversion.
   end record;

   type Optional_conversion (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : conversion;
         when False =>
            null;
      end case;
   end record;

   type runAutomationDetails is record
      description     : Optional_message;
      --  A description of the identity and role played within the engineering
      --  system by this object's containing run object.
      id              : VSS.Strings.Virtual_String;
      --  A hierarchical string that uniquely identifies this object's
      --  containing run object.
      guid            : VSS.Strings.Virtual_String;
      --  A stable, unique identifer for this object's containing run object in
      --  the form of a GUID.
      correlationGuid : VSS.Strings.Virtual_String;
      --  A stable, unique identifier for the equivalence class of runs to
      --  which this object's containing run object belongs in the form of
      --  a GUID.
      properties      : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the run
      --  automation details.
   end record;

   type Optional_runAutomationDetails (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : runAutomationDetails;
         when False =>
            null;
      end case;
   end record;

   type graph is record
      description : Optional_message;
      --  A description of the graph.
      nodes       : node_Vector;
      --  An array of node objects representing the nodes of the graph.
      edges       : edge_Vector;
      --  An array of edge objects representing the edges of the graph.
      properties  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the graph.
   end record;

   type externalProperties is record
      schema                 : VSS.Strings.Virtual_String;
      --  The URI of the JSON schema corresponding to the version of the
      --  external property file format.
      guid                   : VSS.Strings.Virtual_String;
      --  A stable, unique identifer for this external properties object, in
      --  the form of a GUID.
      runGuid                : VSS.Strings.Virtual_String;
      --  A stable, unique identifer for the run associated with this external
      --  properties object, in the form of a GUID.
      conversion             : Optional_conversion;
      --  A conversion object that will be merged with a separate run.
      graphs                 : graph_Vector;
      --  An array of graph objects that will be merged with a separate run.
      externalizedProperties : Optional_propertyBag;
      --  Key/value pairs that provide additional information that will be
      --  merged with a separate run.
      artifacts              : artifact_Vector;
      --  An array of artifact objects that will be merged with a separate run.
      invocations            : invocation_Vector;
      --  Describes the invocation of the analysis tool that will be merged
      --  with a separate run.
      logicalLocations       : logicalLocation_Vector;
      --  An array of logical locations such as namespaces, types or functions
      --  that will be merged with a separate run.
      threadFlowLocations    : threadFlowLocation_Vector;
      --  An array of threadFlowLocation objects that will be merged with a
      --  separate run.
      results                : result_Vector;
      --  An array of result objects that will be merged with a separate run.
      taxonomies             : toolComponent_Vector;
      --  Tool taxonomies that will be merged with a separate run.
      driver                 : Optional_toolComponent;
      --  The analysis tool object that will be merged with a separate run.
      extensions             : toolComponent_Vector;
      --  Tool extensions that will be merged with a separate run.
      policies               : toolComponent_Vector;
      --  Tool policies that will be merged with a separate run.
      translations           : toolComponent_Vector;
      --  Tool translations that will be merged with a separate run.
      addresses              : address_Vector;
      --  Addresses that will be merged with a separate run.
      webRequests            : webRequest_Vector;
      --  Requests that will be merged with a separate run.
      webResponses           : webResponse_Vector;
      --  Responses that will be merged with a separate run.
      properties             : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  external properties.
   end record;

   type rectangle is record
      top        : Optional_Float;
      --  The Y coordinate of the top edge of the rectangle, measured in the
      --  image's natural units.
      left       : Optional_Float;
      --  The X coordinate of the left edge of the rectangle, measured in the
      --  image's natural units.
      bottom     : Optional_Float;
      --  The Y coordinate of the bottom edge of the rectangle, measured in the
      --  image's natural units.
      right      : Optional_Float;
      --  The X coordinate of the right edge of the rectangle, measured in the
      --  image's natural units.
      message    : Optional_message;
      --  A message relevant to the rectangle.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  rectangle.
   end record;

   type reportingConfiguration is record
      enabled    : Boolean := Boolean'First;
      --  Specifies whether the report may be produced during the scan.
      level      : Enum.Optional_reportingConfiguration_level;
      --  Specifies the failure level for the report.
      rank       : Optional_Float;
      --  Specifies the relative priority of the report. Used for analysis
      --  output only.
      parameters : Optional_propertyBag;
      --  Contains configuration information specific to a report.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  reporting configuration.
   end record;

   type Optional_reportingConfiguration (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : reportingConfiguration;
         when False =>
            null;
      end case;
   end record;

   type reportingDescriptor is record
      id                   : VSS.Strings.Virtual_String;
      --  A stable, opaque identifier for the report.
      deprecatedIds        : VSS.String_Vectors.Virtual_String_Vector;
      --  An array of stable, opaque identifiers by which this report was known
      --  in some previous version of the analysis tool.
      guid                 : VSS.Strings.Virtual_String;
      --  A unique identifer for the reporting descriptor in the form of a
      --  GUID.
      deprecatedGuids      : VSS.String_Vectors.Virtual_String_Vector;
      --  An array of unique identifies in the form of a GUID by which this
      --  report was known in some previous version of the analysis tool.
      name                 : VSS.Strings.Virtual_String;
      --  A report identifier that is understandable to an end user.
      deprecatedNames      : VSS.String_Vectors.Virtual_String_Vector;
      --  An array of readable identifiers by which this report was known in
      --  some previous version of the analysis tool.
      shortDescription     : Optional_multiformatMessageString;
      --  A concise description of the report. Should be a single sentence that
      --  is understandable when visible space is limited to a single line of
      --  text.
      fullDescription      : Optional_multiformatMessageString;
      --  A description of the report. Should, as far as possible, provide
      --  details sufficient to enable resolution of any problem indicated
      --  by the result.
      messageStrings       : Any_Object;
      --  A set of name/value pairs with arbitrary names. Each value is a
      --  multiformatMessageString object, which holds message strings in
      --  plain text and (optionally) Markdown format. The strings can include
      --  placeholders, which can be used to construct a message in combination
      --  with an arbitrary number of additional string arguments.
      defaultConfiguration : Optional_reportingConfiguration;
      --  Default reporting configuration information.
      helpUri              : VSS.Strings.Virtual_String;
      --  A URI where the primary documentation for the report can be found.
      help                 : Optional_multiformatMessageString;
      --  Provides the primary documentation for the report, useful when there
      --  is no online documentation.
      relationships        : reportingDescriptorRelationship_Vector;
      --  An array of objects that describe relationships between this
      --  reporting descriptor and others.
      properties           : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the report.
   end record;

   type artifact is record
      description         : Optional_message;
      --  A short description of the artifact.
      location            : Optional_artifactLocation;
      --  The location of the artifact.
      parentIndex         : Optional_Integer;
      --  Identifies the index of the immediate parent of the artifact, if this
      --  artifact is nested.
      offset              : Optional_Integer;
      --  The offset in bytes of the artifact within its containing artifact.
      length              : Optional_Integer;
      --  The length of the artifact in bytes.
      roles               : artifact_roles_Vector;
      --  The role or roles played by the artifact in the analysis.
      mimeType            : VSS.Strings.Virtual_String;
      --  The MIME type (RFC 2045) of the artifact.
      contents            : Optional_artifactContent;
      --  The contents of the artifact.
      encoding            : VSS.Strings.Virtual_String;
      --  Specifies the encoding for an artifact object that refers to a text
      --  file.
      sourceLanguage      : VSS.Strings.Virtual_String;
      --  Specifies the source language for any artifact object that refers to
      --  a text file that contains source code.
      hashes              : Any_Object;
      --  A dictionary, each of whose keys is the name of a hash function and
      --  each of whose values is the hashed value of the artifact produced by
      --  the specified hash function.
      lastModifiedTimeUtc : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which the
      --  artifact was most recently modified. See "Date/time properties"
      --  in the SARIF spec for the required format.
      properties          : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  artifact.
   end record;

   type webRequest is record
      index      : Optional_Integer;
      --  The index within the run.webRequests array of the request object
      --  associated with this result.
      protocol   : VSS.Strings.Virtual_String;
      --  The request protocol. Example: 'http'.
      version    : VSS.Strings.Virtual_String;
      --  The request version. Example: '1.1'.
      target     : VSS.Strings.Virtual_String;
      --  The target of the request.
      method     : VSS.Strings.Virtual_String;
      --  The HTTP method. Well-known values are 'GET', 'PUT', 'POST',
      --  'DELETE', 'PATCH', 'HEAD', 'OPTIONS', 'TRACE', 'CONNECT'.
      headers    : Any_Object;
      --  The request headers.
      parameters : Any_Object;
      --  The request parameters.
      a_body     : Optional_artifactContent;
      --  The body of the request.
      properties : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  request.
   end record;

   type Optional_webRequest (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : webRequest;
         when False =>
            null;
      end case;
   end record;

   type fix is record
      description     : Optional_message;
      --  A message that describes the proposed fix, enabling viewers to
      --  present the proposed change to an end user.
      artifactChanges : artifactChange_Vector;
      --  One or more artifact changes that comprise a fix for a result.
      properties      : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the fix.
   end record;

   type webResponse is record
      index              : Optional_Integer;
      --  The index within the run.webResponses array of the response object
      --  associated with this result.
      protocol           : VSS.Strings.Virtual_String;
      --  The response protocol. Example: 'http'.
      version            : VSS.Strings.Virtual_String;
      --  The response version. Example: '1.1'.
      statusCode         : Optional_Integer;
      --  The response status code. Example: 451.
      reasonPhrase       : VSS.Strings.Virtual_String;
      --  The response reason. Example: 'Not found'.
      headers            : Any_Object;
      --  The response headers.
      a_body             : Optional_artifactContent;
      --  The body of the response.
      noResponseReceived : Boolean := Boolean'First;
      --  Specifies whether a response was received from the server.
      properties         : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  response.
   end record;

   type Optional_webResponse (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : webResponse;
         when False =>
            null;
      end case;
   end record;

   type result is record
      ruleId              : VSS.Strings.Virtual_String;
      --  The stable, unique identifier of the rule, if any, to which this
      --  result is relevant.
      ruleIndex           : Optional_Integer;
      --  The index within the tool component rules array of the rule object
      --  associated with this result.
      rule                : Optional_reportingDescriptorReference;
      --  A reference used to locate the rule descriptor relevant to this
      --  result.
      kind                : Enum.Optional_result_kind;
      --  A value that categorizes results by evaluation state.
      level               : Enum.Optional_result_level;
      --  A value specifying the severity level of the result.
      message             : SARIF.Types.message;
      --  A message that describes the result. The first sentence of the
      --  message only will be displayed when visible space is limited.
      analysisTarget      : Optional_artifactLocation;
      --  Identifies the artifact that the analysis tool was instructed to
      --  scan. This need not be the same as the artifact where the result
      --  actually occurred.
      locations           : location_Vector;
      --  The set of locations where the result was detected. Specify only
      --  one location unless the problem indicated by the result can only
      --  be corrected by making a change at every specified location.
      guid                : VSS.Strings.Virtual_String;
      --  A stable, unique identifer for the result in the form of a GUID.
      correlationGuid     : VSS.Strings.Virtual_String;
      --  A stable, unique identifier for the equivalence class of logically
      --  identical results to which this result belongs, in the form of a
      --  GUID.
      occurrenceCount     : Optional_Integer;
      --  A positive integer specifying the number of times this logically
      --  unique result was observed in this run.
      partialFingerprints : Any_Object;
      --  A set of strings that contribute to the stable, unique identity of
      --  the result.
      fingerprints        : Any_Object;
      --  A set of strings each of which individually defines a stable, unique
      --  identity for the result.
      stacks              : stack_Vector;
      --  An array of 'stack' objects relevant to the result.
      codeFlows           : codeFlow_Vector;
      --  An array of 'codeFlow' objects relevant to the result.
      graphs              : graph_Vector;
      --  An array of zero or more unique graph objects associated with the
      --  result.
      graphTraversals     : graphTraversal_Vector;
      --  An array of one or more unique 'graphTraversal' objects.
      relatedLocations    : location_Vector;
      --  A set of locations relevant to this result.
      suppressions        : suppression_Vector;
      --  A set of suppressions relevant to this result.
      baselineState       : Enum.Optional_result_baselineState;
      --  The state of a result relative to a baseline of a previous run.
      rank                : Optional_Float;
      --  A number representing the priority or importance of the result.
      attachments         : attachment_Vector;
      --  A set of artifacts relevant to the result.
      hostedViewerUri     : VSS.Strings.Virtual_String;
      --  An absolute URI at which the result can be viewed.
      workItemUris        : VSS.String_Vectors.Virtual_String_Vector;
      --  The URIs of the work items associated with this result.
      provenance          : Optional_resultProvenance;
      --  Information about how and when the result was detected.
      fixes               : fix_Vector;
      --  An array of 'fix' objects, each of which represents a proposed fix to
      --  the problem indicated by the result.
      taxa                : reportingDescriptorReference_Vector;
      --  An array of references to taxonomy reporting descriptors that are
      --  applicable to the result.
      webRequest          : Optional_webRequest;
      --  A web request associated with this result.
      webResponse         : Optional_webResponse;
      --  A web response associated with this result.
      properties          : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the result.
   end record;

   type graphTraversal is record
      runGraphIndex    : Optional_Integer;
      --  The index within the run.graphs to be associated with the result.
      resultGraphIndex : Optional_Integer;
      --  The index within the result.graphs to be associated with the result.
      description      : Optional_message;
      --  A description of this graph traversal.
      initialState     : Any_Object;
      --  Values of relevant expressions at the start of the graph traversal
      --  that may change during graph traversal.
      immutableState   : Any_Object;
      --  Values of relevant expressions at the start of the graph traversal
      --  that remain constant for the graph traversal.
      edgeTraversals   : edgeTraversal_Vector;
      --  The sequences of edges traversed by this graph traversal.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the graph
      --  traversal.
   end record;

   type attachment is record
      description      : Optional_message;
      --  A message describing the role played by the attachment.
      artifactLocation : SARIF.Types.artifactLocation;
      --  The location of the attachment.
      regions          : region_Vector;
      --  An array of regions of interest within the attachment.
      rectangles       : rectangle_Vector;
      --  An array of rectangles specifying areas of interest within the image.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  attachment.
   end record;

   type replacement is record
      deletedRegion   : region;
      --  The region of the artifact to delete.
      insertedContent : Optional_artifactContent;
      --  The content to insert at the location specified by the
      --  'deletedRegion' property.
      properties      : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  replacement.
   end record;

   type externalPropertyFileReferences is record
      conversion             : Optional_externalPropertyFileReference;
      --  An external property file containing a run.conversion object to be
      --  merged with the root log file.
      graphs                 : externalPropertyFileReference_Vector;
      --  An array of external property files containing a run.graphs object to
      --  be merged with the root log file.
      externalizedProperties : Optional_externalPropertyFileReference;
      --  An external property file containing a run.properties object to be
      --  merged with the root log file.
      artifacts              : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.artifacts arrays
      --  to be merged with the root log file.
      invocations            : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.invocations arrays
      --  to be merged with the root log file.
      logicalLocations       : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.logicalLocations
      --  arrays to be merged with the root log file.
      threadFlowLocations    : externalPropertyFileReference_Vector;
      --  An array of external property files containing
      --  run.threadFlowLocations arrays to be merged with the root log file.
      results                : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.results arrays to
      --  be merged with the root log file.
      taxonomies             : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.taxonomies arrays
      --  to be merged with the root log file.
      addresses              : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.addresses arrays
      --  to be merged with the root log file.
      driver                 : Optional_externalPropertyFileReference;
      --  An external property file containing a run.driver object to be merged
      --  with the root log file.
      extensions             : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.extensions arrays
      --  to be merged with the root log file.
      policies               : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.policies arrays to
      --  be merged with the root log file.
      translations           : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.translations
      --  arrays to be merged with the root log file.
      webRequests            : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.requests arrays to
      --  be merged with the root log file.
      webResponses           : externalPropertyFileReference_Vector;
      --  An array of external property files containing run.responses arrays
      --  to be merged with the root log file.
      properties             : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  external property files.
   end record;

   type Optional_externalPropertyFileReferences (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when True =>
            Value : externalPropertyFileReferences;
         when False =>
            null;
      end case;
   end record;

   type run is record
      tool                           : SARIF.Types.tool;
      --  Information about the tool or tool pipeline that generated the
      --  results in this run. A run can only contain results produced by
      --  a single tool or tool pipeline. A run can aggregate results from
      --  multiple log files, as long as context around the tool run (tool
      --  command-line arguments and the like) is identical for all aggregated
      --  files.
      invocations                    : invocation_Vector;
      --  Describes the invocation of the analysis tool.
      conversion                     : Optional_conversion;
      --  A conversion object that describes how a converter transformed an
      --  analysis tool's native reporting format into the SARIF format.
      language                       : VSS.Strings.Virtual_String;
      --  The language of the messages emitted into the log file during this
      --  run (expressed as an ISO 639-1 two-letter lowercase culture code) and
      --  an optional region (expressed as an ISO 3166-1 two-letter uppercase
      --  subculture code associated with a country or region). The casing is
      --  recommended but not required (in order for this data to conform to
      --  RFC5646).
      versionControlProvenance       : versionControlDetails_Vector;
      --  Specifies the revision in version control of the artifacts that were
      --  scanned.
      originalUriBaseIds             : Any_Object;
      --  The artifact location specified by each uriBaseId symbol on the
      --  machine where the tool originally ran.
      artifacts                      : artifact_Vector;
      --  An array of artifact objects relevant to the run.
      logicalLocations               : logicalLocation_Vector;
      --  An array of logical locations such as namespaces, types or functions.
      graphs                         : graph_Vector;
      --  An array of zero or more unique graph objects associated with the
      --  run.
      results                        : result_Vector;
      --  The set of results contained in an SARIF log. The results array can
      --  be omitted when a run is solely exporting rules metadata. It must be
      --  present (but may be empty) if a log file represents an actual scan.
      automationDetails              : Optional_runAutomationDetails;
      --  Automation details that describe this run.
      runAggregates                  : runAutomationDetails_Vector;
      --  Automation details that describe the aggregate of runs to which this
      --  run belongs.
      baselineGuid                   : VSS.Strings.Virtual_String;
      --  The 'guid' property of a previous SARIF 'run' that comprises the
      --  baseline that was used to compute result 'baselineState' properties
      --  for the run.
      redactionTokens : VSS.String_Vectors.Virtual_String_Vector;
      --  An array of strings used to replace sensitive information in a
      --  redaction-aware property.
      defaultEncoding                : VSS.Strings.Virtual_String;
      --  Specifies the default encoding for any artifact object that refers to
      --  a text file.
      defaultSourceLanguage          : VSS.Strings.Virtual_String;
      --  Specifies the default source language for any artifact object that
      --  refers to a text file that contains source code.
      newlineSequences : VSS.String_Vectors.Virtual_String_Vector;
      --  An ordered list of character sequences that were treated as line
      --  breaks when computing region information for the run.
      columnKind                     : Enum.Optional_run_columnKind;
      --  Specifies the unit in which the tool measures columns.
      externalPropertyFileReferences : Optional_externalPropertyFileReferences;
      --  References to external property files that should be inlined with the
      --  content of a root log file.
      threadFlowLocations            : threadFlowLocation_Vector;
      --  An array of threadFlowLocation objects cached at run level.
      taxonomies                     : toolComponent_Vector;
      --  An array of toolComponent objects relevant to a taxonomy in which
      --  results are categorized.
      addresses                      : address_Vector;
      --  Addresses associated with this run instance, if any.
      translations                   : toolComponent_Vector;
      --  The set of available translations of the localized data provided by
      --  the tool.
      policies                       : toolComponent_Vector;
      --  Contains configurations that may potentially override both
      --  reportingDescriptor.defaultConfiguration (the tool's default
      --  severities) and invocation.configurationOverrides (severities
      --  established at run-time from the command line).
      webRequests                    : webRequest_Vector;
      --  An array of request objects cached at run level.
      webResponses                   : webResponse_Vector;
      --  An array of response objects cached at run level.
      specialLocations               : Optional_specialLocations;
      --  A specialLocations object that defines locations of special
      --  significance to SARIF consumers.
      properties                     : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the run.
   end record;

   type threadFlowLocation is record
      index            : Optional_Integer;
      --  The index within the run threadFlowLocations array.
      location         : Optional_location;
      --  The code location.
      stack            : Optional_stack;
      --  The call stack leading to this location.
      kinds            : VSS.String_Vectors.Virtual_String_Vector;
      --  A set of distinct strings that categorize the thread flow location.
      --  Well-known kinds include 'acquire', 'release', 'enter', 'exit',
      --  'call', 'return', 'branch', 'implicit', 'false', 'true', 'caution',
      --  'danger', 'unknown', 'unreachable', 'taint', 'function', 'handler',
      --  'lock', 'memory', 'resource', 'scope' and 'value'.
      taxa             : reportingDescriptorReference_Vector;
      --  An array of references to rule or taxonomy reporting descriptors that
      --  are applicable to the thread flow location.
      module           : VSS.Strings.Virtual_String;
      --  The name of the module that contains the code that is executing.
      state            : Any_Object;
      --  A dictionary, each of whose keys specifies a variable or expression,
      --  the associated value of which represents the variable or expression
      --  value. For an annotation of kind 'continuation', for example, this
      --  dictionary might hold the current assumed values of a set of global
      --  variables.
      nestingLevel     : Optional_Integer;
      --  An integer representing a containment hierarchy within the thread
      --  flow.
      executionOrder   : Optional_Integer;
      --  An integer representing the temporal order in which execution reached
      --  this location.
      executionTimeUtc : VSS.Strings.Virtual_String;
      --  The Coordinated Universal Time (UTC) date and time at which this
      --  location was executed.
      importance       : Enum.Optional_threadFlowLocation_importance;
      --  Specifies the importance of this location in understanding the code
      --  flow in which it occurs. The order from most to least important is
      --  "essential", "important", "unimportant". Default: "important".
      webRequest       : Optional_webRequest;
      --  A web request associated with this thread flow location.
      webResponse      : Optional_webResponse;
      --  A web response associated with this thread flow location.
      properties       : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  threadflow location.
   end record;

   type codeFlow is record
      message     : Optional_message;
      --  A message relevant to the code flow.
      threadFlows : threadFlow_Vector;
      --  An array of one or more unique threadFlow objects, each of which
      --  describes the progress of a program through a thread of execution.
      properties  : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the code
      --  flow.
   end record;

   type threadFlow is record
      id             : VSS.Strings.Virtual_String;
      --  An string that uniquely identifies the threadFlow within the codeFlow
      --  in which it occurs.
      message        : Optional_message;
      --  A message relevant to the thread flow.
      initialState   : Any_Object;
      --  Values of relevant expressions at the start of the thread flow that
      --  may change during thread flow execution.
      immutableState : Any_Object;
      --  Values of relevant expressions at the start of the thread flow that
      --  remain constant.
      locations      : threadFlowLocation_Vector;
      --  A temporally ordered array of 'threadFlowLocation' objects, each of
      --  which describes a location visited by the tool while producing the
      --  result.
      properties     : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the thread
      --  flow.
   end record;

   type configurationOverride is record
      configuration : reportingConfiguration;
      --  Specifies how the rule or notification was configured during the
      --  scan.
      descriptor    : reportingDescriptorReference;
      --  A reference used to locate the descriptor whose configuration was
      --  overridden.
      properties    : Optional_propertyBag;
      --  Key/value pairs that provide additional information about the
      --  configuration override.
   end record;

   function Length (Self : configurationOverride_Vector) return Natural;

   procedure Clear (Self : in out configurationOverride_Vector);

   procedure Append
     (Self  : in out configurationOverride_Vector;
      Value : configurationOverride);

   type configurationOverride_Variable_Reference
     (Element : not null access configurationOverride) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_configurationOverride_Variable_Reference
     (Self  : aliased in out configurationOverride_Vector;
      Index : Positive)
      return configurationOverride_Variable_Reference with
     Inline;

   type configurationOverride_Constant_Reference
     (Element : not null access constant configurationOverride) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_configurationOverride_Constant_Reference
     (Self  : aliased configurationOverride_Vector;
      Index : Positive)
      return configurationOverride_Constant_Reference with
     Inline;

   function Length (Self : locationRelationship_Vector) return Natural;

   procedure Clear (Self : in out locationRelationship_Vector);

   procedure Append
     (Self : in out locationRelationship_Vector; Value : locationRelationship);

   type locationRelationship_Variable_Reference
     (Element : not null access locationRelationship) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_locationRelationship_Variable_Reference
     (Self  : aliased in out locationRelationship_Vector;
      Index : Positive)
      return locationRelationship_Variable_Reference with
     Inline;

   type locationRelationship_Constant_Reference
     (Element : not null access constant locationRelationship) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_locationRelationship_Constant_Reference
     (Self  : aliased locationRelationship_Vector;
      Index : Positive)
      return locationRelationship_Constant_Reference with
     Inline;

   function Length (Self : threadFlowLocation_Vector) return Natural;

   procedure Clear (Self : in out threadFlowLocation_Vector);

   procedure Append
     (Self : in out threadFlowLocation_Vector; Value : threadFlowLocation);

   type threadFlowLocation_Variable_Reference
     (Element : not null access threadFlowLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_threadFlowLocation_Variable_Reference
     (Self  : aliased in out threadFlowLocation_Vector;
      Index : Positive)
      return threadFlowLocation_Variable_Reference with
     Inline;

   type threadFlowLocation_Constant_Reference
     (Element : not null access constant threadFlowLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_threadFlowLocation_Constant_Reference
     (Self  : aliased threadFlowLocation_Vector;
      Index : Positive)
      return threadFlowLocation_Constant_Reference with
     Inline;

   function Length (Self : result_Vector) return Natural;

   procedure Clear (Self : in out result_Vector);

   procedure Append (Self : in out result_Vector; Value : result);

   type result_Variable_Reference (Element : not null access result) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_result_Variable_Reference
     (Self  : aliased in out result_Vector;
      Index : Positive)
      return result_Variable_Reference with
     Inline;

   type result_Constant_Reference (Element : not null access constant result)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_result_Constant_Reference
     (Self  : aliased result_Vector;
      Index : Positive)
      return result_Constant_Reference with
     Inline;

   function Length (Self : runAutomationDetails_Vector) return Natural;

   procedure Clear (Self : in out runAutomationDetails_Vector);

   procedure Append
     (Self : in out runAutomationDetails_Vector; Value : runAutomationDetails);

   type runAutomationDetails_Variable_Reference
     (Element : not null access runAutomationDetails) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_runAutomationDetails_Variable_Reference
     (Self  : aliased in out runAutomationDetails_Vector;
      Index : Positive)
      return runAutomationDetails_Variable_Reference with
     Inline;

   type runAutomationDetails_Constant_Reference
     (Element : not null access constant runAutomationDetails) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_runAutomationDetails_Constant_Reference
     (Self  : aliased runAutomationDetails_Vector;
      Index : Positive)
      return runAutomationDetails_Constant_Reference with
     Inline;

   function Length (Self : replacement_Vector) return Natural;

   procedure Clear (Self : in out replacement_Vector);

   procedure Append (Self : in out replacement_Vector; Value : replacement);

   type replacement_Variable_Reference (Element : not null access replacement)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_replacement_Variable_Reference
     (Self  : aliased in out replacement_Vector;
      Index : Positive)
      return replacement_Variable_Reference with
     Inline;

   type replacement_Constant_Reference
     (Element : not null access constant replacement) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_replacement_Constant_Reference
     (Self  : aliased replacement_Vector;
      Index : Positive)
      return replacement_Constant_Reference with
     Inline;

   function Length (Self : webResponse_Vector) return Natural;

   procedure Clear (Self : in out webResponse_Vector);

   procedure Append (Self : in out webResponse_Vector; Value : webResponse);

   type webResponse_Variable_Reference (Element : not null access webResponse)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_webResponse_Variable_Reference
     (Self  : aliased in out webResponse_Vector;
      Index : Positive)
      return webResponse_Variable_Reference with
     Inline;

   type webResponse_Constant_Reference
     (Element : not null access constant webResponse) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_webResponse_Constant_Reference
     (Self  : aliased webResponse_Vector;
      Index : Positive)
      return webResponse_Constant_Reference with
     Inline;

   function Length (Self : address_Vector) return Natural;

   procedure Clear (Self : in out address_Vector);

   procedure Append (Self : in out address_Vector; Value : address);

   type address_Variable_Reference (Element : not null access address) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_address_Variable_Reference
     (Self  : aliased in out address_Vector;
      Index : Positive)
      return address_Variable_Reference with
     Inline;

   type address_Constant_Reference (Element : not null access constant address)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_address_Constant_Reference
     (Self  : aliased address_Vector;
      Index : Positive)
      return address_Constant_Reference with
     Inline;

   function Length (Self : physicalLocation_Vector) return Natural;

   procedure Clear (Self : in out physicalLocation_Vector);

   procedure Append
     (Self : in out physicalLocation_Vector; Value : physicalLocation);

   type physicalLocation_Variable_Reference
     (Element : not null access physicalLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_physicalLocation_Variable_Reference
     (Self  : aliased in out physicalLocation_Vector;
      Index : Positive)
      return physicalLocation_Variable_Reference with
     Inline;

   type physicalLocation_Constant_Reference
     (Element : not null access constant physicalLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_physicalLocation_Constant_Reference
     (Self  : aliased physicalLocation_Vector;
      Index : Positive)
      return physicalLocation_Constant_Reference with
     Inline;

   function Length (Self : stackFrame_Vector) return Natural;

   procedure Clear (Self : in out stackFrame_Vector);

   procedure Append (Self : in out stackFrame_Vector; Value : stackFrame);

   type stackFrame_Variable_Reference (Element : not null access stackFrame) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_stackFrame_Variable_Reference
     (Self  : aliased in out stackFrame_Vector;
      Index : Positive)
      return stackFrame_Variable_Reference with
     Inline;

   type stackFrame_Constant_Reference
     (Element : not null access constant stackFrame) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_stackFrame_Constant_Reference
     (Self  : aliased stackFrame_Vector;
      Index : Positive)
      return stackFrame_Constant_Reference with
     Inline;

   function Length (Self : toolComponent_Vector) return Natural;

   procedure Clear (Self : in out toolComponent_Vector);

   procedure Append
     (Self : in out toolComponent_Vector; Value : toolComponent);

   type toolComponent_Variable_Reference
     (Element : not null access toolComponent) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_toolComponent_Variable_Reference
     (Self  : aliased in out toolComponent_Vector;
      Index : Positive)
      return toolComponent_Variable_Reference with
     Inline;

   type toolComponent_Constant_Reference
     (Element : not null access constant toolComponent) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_toolComponent_Constant_Reference
     (Self  : aliased toolComponent_Vector;
      Index : Positive)
      return toolComponent_Constant_Reference with
     Inline;

   function Length
     (Self : externalPropertyFileReference_Vector) return Natural;

   procedure Clear (Self : in out externalPropertyFileReference_Vector);

   procedure Append
     (Self  : in out externalPropertyFileReference_Vector;
      Value : externalPropertyFileReference);

   type externalPropertyFileReference_Variable_Reference
     (Element : not null access externalPropertyFileReference) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_externalPropertyFileReference_Variable_Reference
     (Self  : aliased in out externalPropertyFileReference_Vector;
      Index : Positive)
      return externalPropertyFileReference_Variable_Reference with
     Inline;

   type externalPropertyFileReference_Constant_Reference
     (Element : not null access constant externalPropertyFileReference) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_externalPropertyFileReference_Constant_Reference
     (Self  : aliased externalPropertyFileReference_Vector;
      Index : Positive)
      return externalPropertyFileReference_Constant_Reference with
     Inline;

   function Length (Self : stack_Vector) return Natural;

   procedure Clear (Self : in out stack_Vector);

   procedure Append (Self : in out stack_Vector; Value : stack);

   type stack_Variable_Reference (Element : not null access stack) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_stack_Variable_Reference
     (Self  : aliased in out stack_Vector;
      Index : Positive)
      return stack_Variable_Reference with
     Inline;

   type stack_Constant_Reference (Element : not null access constant stack) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_stack_Constant_Reference
     (Self  : aliased stack_Vector;
      Index : Positive)
      return stack_Constant_Reference with
     Inline;

   function Length (Self : Integer_Vector) return Natural;

   procedure Clear (Self : in out Integer_Vector);

   procedure Append (Self : in out Integer_Vector; Value : Integer);

   type Integer_Variable_Reference (Element : not null access Integer) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Integer_Variable_Reference
     (Self  : aliased in out Integer_Vector;
      Index : Positive)
      return Integer_Variable_Reference with
     Inline;

   type Integer_Constant_Reference (Element : not null access constant Integer)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_Integer_Constant_Reference
     (Self  : aliased Integer_Vector;
      Index : Positive)
      return Integer_Constant_Reference with
     Inline;

   function Length (Self : notification_Vector) return Natural;

   procedure Clear (Self : in out notification_Vector);

   procedure Append (Self : in out notification_Vector; Value : notification);

   type notification_Variable_Reference
     (Element : not null access notification) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_notification_Variable_Reference
     (Self  : aliased in out notification_Vector;
      Index : Positive)
      return notification_Variable_Reference with
     Inline;

   type notification_Constant_Reference
     (Element : not null access constant notification) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_notification_Constant_Reference
     (Self  : aliased notification_Vector;
      Index : Positive)
      return notification_Constant_Reference with
     Inline;

   function Length (Self : attachment_Vector) return Natural;

   procedure Clear (Self : in out attachment_Vector);

   procedure Append (Self : in out attachment_Vector; Value : attachment);

   type attachment_Variable_Reference (Element : not null access attachment) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_attachment_Variable_Reference
     (Self  : aliased in out attachment_Vector;
      Index : Positive)
      return attachment_Variable_Reference with
     Inline;

   type attachment_Constant_Reference
     (Element : not null access constant attachment) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_attachment_Constant_Reference
     (Self  : aliased attachment_Vector;
      Index : Positive)
      return attachment_Constant_Reference with
     Inline;

   function Length (Self : suppression_Vector) return Natural;

   procedure Clear (Self : in out suppression_Vector);

   procedure Append (Self : in out suppression_Vector; Value : suppression);

   type suppression_Variable_Reference (Element : not null access suppression)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_suppression_Variable_Reference
     (Self  : aliased in out suppression_Vector;
      Index : Positive)
      return suppression_Variable_Reference with
     Inline;

   type suppression_Constant_Reference
     (Element : not null access constant suppression) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_suppression_Constant_Reference
     (Self  : aliased suppression_Vector;
      Index : Positive)
      return suppression_Constant_Reference with
     Inline;

   function Length (Self : edgeTraversal_Vector) return Natural;

   procedure Clear (Self : in out edgeTraversal_Vector);

   procedure Append
     (Self : in out edgeTraversal_Vector; Value : edgeTraversal);

   type edgeTraversal_Variable_Reference
     (Element : not null access edgeTraversal) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_edgeTraversal_Variable_Reference
     (Self  : aliased in out edgeTraversal_Vector;
      Index : Positive)
      return edgeTraversal_Variable_Reference with
     Inline;

   type edgeTraversal_Constant_Reference
     (Element : not null access constant edgeTraversal) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_edgeTraversal_Constant_Reference
     (Self  : aliased edgeTraversal_Vector;
      Index : Positive)
      return edgeTraversal_Constant_Reference with
     Inline;

   function Length (Self : graphTraversal_Vector) return Natural;

   procedure Clear (Self : in out graphTraversal_Vector);

   procedure Append
     (Self : in out graphTraversal_Vector; Value : graphTraversal);

   type graphTraversal_Variable_Reference
     (Element : not null access graphTraversal) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_graphTraversal_Variable_Reference
     (Self  : aliased in out graphTraversal_Vector;
      Index : Positive)
      return graphTraversal_Variable_Reference with
     Inline;

   type graphTraversal_Constant_Reference
     (Element : not null access constant graphTraversal) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_graphTraversal_Constant_Reference
     (Self  : aliased graphTraversal_Vector;
      Index : Positive)
      return graphTraversal_Constant_Reference with
     Inline;

   function Length (Self : location_Vector) return Natural;

   procedure Clear (Self : in out location_Vector);

   procedure Append (Self : in out location_Vector; Value : location);

   type location_Variable_Reference (Element : not null access location) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_location_Variable_Reference
     (Self  : aliased in out location_Vector;
      Index : Positive)
      return location_Variable_Reference with
     Inline;

   type location_Constant_Reference
     (Element : not null access constant location) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_location_Constant_Reference
     (Self  : aliased location_Vector;
      Index : Positive)
      return location_Constant_Reference with
     Inline;

   function Length (Self : graph_Vector) return Natural;

   procedure Clear (Self : in out graph_Vector);

   procedure Append (Self : in out graph_Vector; Value : graph);

   type graph_Variable_Reference (Element : not null access graph) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_graph_Variable_Reference
     (Self  : aliased in out graph_Vector;
      Index : Positive)
      return graph_Variable_Reference with
     Inline;

   type graph_Constant_Reference (Element : not null access constant graph) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_graph_Constant_Reference
     (Self  : aliased graph_Vector;
      Index : Positive)
      return graph_Constant_Reference with
     Inline;

   function Length (Self : reportingDescriptorReference_Vector) return Natural;

   procedure Clear (Self : in out reportingDescriptorReference_Vector);

   procedure Append
     (Self  : in out reportingDescriptorReference_Vector;
      Value : reportingDescriptorReference);

   type reportingDescriptorReference_Variable_Reference
     (Element : not null access reportingDescriptorReference) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_reportingDescriptorReference_Variable_Reference
     (Self  : aliased in out reportingDescriptorReference_Vector;
      Index : Positive)
      return reportingDescriptorReference_Variable_Reference with
     Inline;

   type reportingDescriptorReference_Constant_Reference
     (Element : not null access constant reportingDescriptorReference) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_reportingDescriptorReference_Constant_Reference
     (Self  : aliased reportingDescriptorReference_Vector;
      Index : Positive)
      return reportingDescriptorReference_Constant_Reference with
     Inline;

   function Length
     (Self : reportingDescriptorRelationship_Vector) return Natural;

   procedure Clear (Self : in out reportingDescriptorRelationship_Vector);

   procedure Append
     (Self  : in out reportingDescriptorRelationship_Vector;
      Value : reportingDescriptorRelationship);

   type reportingDescriptorRelationship_Variable_Reference
     (Element : not null access reportingDescriptorRelationship) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_reportingDescriptorRelationship_Variable_Reference
     (Self  : aliased in out reportingDescriptorRelationship_Vector;
      Index : Positive)
      return reportingDescriptorRelationship_Variable_Reference with
     Inline;

   type reportingDescriptorRelationship_Constant_Reference
     (Element : not null access constant reportingDescriptorRelationship) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_reportingDescriptorRelationship_Constant_Reference
     (Self  : aliased reportingDescriptorRelationship_Vector;
      Index : Positive)
      return reportingDescriptorRelationship_Constant_Reference with
     Inline;

   function Length (Self : region_Vector) return Natural;

   procedure Clear (Self : in out region_Vector);

   procedure Append (Self : in out region_Vector; Value : region);

   type region_Variable_Reference (Element : not null access region) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_region_Variable_Reference
     (Self  : aliased in out region_Vector;
      Index : Positive)
      return region_Variable_Reference with
     Inline;

   type region_Constant_Reference (Element : not null access constant region)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_region_Constant_Reference
     (Self  : aliased region_Vector;
      Index : Positive)
      return region_Constant_Reference with
     Inline;

   function Length (Self : versionControlDetails_Vector) return Natural;

   procedure Clear (Self : in out versionControlDetails_Vector);

   procedure Append
     (Self  : in out versionControlDetails_Vector;
      Value : versionControlDetails);

   type versionControlDetails_Variable_Reference
     (Element : not null access versionControlDetails) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_versionControlDetails_Variable_Reference
     (Self  : aliased in out versionControlDetails_Vector;
      Index : Positive)
      return versionControlDetails_Variable_Reference with
     Inline;

   type versionControlDetails_Constant_Reference
     (Element : not null access constant versionControlDetails) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_versionControlDetails_Constant_Reference
     (Self  : aliased versionControlDetails_Vector;
      Index : Positive)
      return versionControlDetails_Constant_Reference with
     Inline;

   function Length (Self : rectangle_Vector) return Natural;

   procedure Clear (Self : in out rectangle_Vector);

   procedure Append (Self : in out rectangle_Vector; Value : rectangle);

   type rectangle_Variable_Reference (Element : not null access rectangle) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_rectangle_Variable_Reference
     (Self  : aliased in out rectangle_Vector;
      Index : Positive)
      return rectangle_Variable_Reference with
     Inline;

   type rectangle_Constant_Reference
     (Element : not null access constant rectangle) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_rectangle_Constant_Reference
     (Self  : aliased rectangle_Vector;
      Index : Positive)
      return rectangle_Constant_Reference with
     Inline;

   function Length (Self : codeFlow_Vector) return Natural;

   procedure Clear (Self : in out codeFlow_Vector);

   procedure Append (Self : in out codeFlow_Vector; Value : codeFlow);

   type codeFlow_Variable_Reference (Element : not null access codeFlow) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_codeFlow_Variable_Reference
     (Self  : aliased in out codeFlow_Vector;
      Index : Positive)
      return codeFlow_Variable_Reference with
     Inline;

   type codeFlow_Constant_Reference
     (Element : not null access constant codeFlow) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_codeFlow_Constant_Reference
     (Self  : aliased codeFlow_Vector;
      Index : Positive)
      return codeFlow_Constant_Reference with
     Inline;

   function Length (Self : toolComponentReference_Vector) return Natural;

   procedure Clear (Self : in out toolComponentReference_Vector);

   procedure Append
     (Self  : in out toolComponentReference_Vector;
      Value : toolComponentReference);

   type toolComponentReference_Variable_Reference
     (Element : not null access toolComponentReference) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_toolComponentReference_Variable_Reference
     (Self  : aliased in out toolComponentReference_Vector;
      Index : Positive)
      return toolComponentReference_Variable_Reference with
     Inline;

   type toolComponentReference_Constant_Reference
     (Element : not null access constant toolComponentReference) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_toolComponentReference_Constant_Reference
     (Self  : aliased toolComponentReference_Vector;
      Index : Positive)
      return toolComponentReference_Constant_Reference with
     Inline;

   function Length (Self : edge_Vector) return Natural;

   procedure Clear (Self : in out edge_Vector);

   procedure Append (Self : in out edge_Vector; Value : edge);

   type edge_Variable_Reference (Element : not null access edge) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_edge_Variable_Reference
     (Self  : aliased in out edge_Vector;
      Index : Positive)
      return edge_Variable_Reference with
     Inline;

   type edge_Constant_Reference (Element : not null access constant edge) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_edge_Constant_Reference
     (Self  : aliased edge_Vector;
      Index : Positive)
      return edge_Constant_Reference with
     Inline;

   function Length (Self : toolComponent_contents_Vector) return Natural;

   procedure Clear (Self : in out toolComponent_contents_Vector);

   procedure Append
     (Self  : in out toolComponent_contents_Vector;
      Value : Enum.toolComponent_contents);

   type toolComponent_contents_Variable_Reference
     (Element : not null access Enum.toolComponent_contents) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_toolComponent_contents_Variable_Reference
     (Self  : aliased in out toolComponent_contents_Vector;
      Index : Positive)
      return toolComponent_contents_Variable_Reference with
     Inline;

   type toolComponent_contents_Constant_Reference
     (Element : not null access constant Enum.toolComponent_contents) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_toolComponent_contents_Constant_Reference
     (Self  : aliased toolComponent_contents_Vector;
      Index : Positive)
      return toolComponent_contents_Constant_Reference with
     Inline;

   function Length (Self : node_Vector) return Natural;

   procedure Clear (Self : in out node_Vector);

   procedure Append (Self : in out node_Vector; Value : node);

   type node_Variable_Reference (Element : not null access node) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_node_Variable_Reference
     (Self  : aliased in out node_Vector;
      Index : Positive)
      return node_Variable_Reference with
     Inline;

   type node_Constant_Reference (Element : not null access constant node) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_node_Constant_Reference
     (Self  : aliased node_Vector;
      Index : Positive)
      return node_Constant_Reference with
     Inline;

   function Length (Self : threadFlow_Vector) return Natural;

   procedure Clear (Self : in out threadFlow_Vector);

   procedure Append (Self : in out threadFlow_Vector; Value : threadFlow);

   type threadFlow_Variable_Reference (Element : not null access threadFlow) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_threadFlow_Variable_Reference
     (Self  : aliased in out threadFlow_Vector;
      Index : Positive)
      return threadFlow_Variable_Reference with
     Inline;

   type threadFlow_Constant_Reference
     (Element : not null access constant threadFlow) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_threadFlow_Constant_Reference
     (Self  : aliased threadFlow_Vector;
      Index : Positive)
      return threadFlow_Constant_Reference with
     Inline;

   function Length (Self : fix_Vector) return Natural;

   procedure Clear (Self : in out fix_Vector);

   procedure Append (Self : in out fix_Vector; Value : fix);

   type fix_Variable_Reference (Element : not null access fix) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_fix_Variable_Reference
     (Self  : aliased in out fix_Vector;
      Index : Positive)
      return fix_Variable_Reference with
     Inline;

   type fix_Constant_Reference (Element : not null access constant fix) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_fix_Constant_Reference
     (Self  : aliased fix_Vector;
      Index : Positive)
      return fix_Constant_Reference with
     Inline;

   function Length (Self : invocation_Vector) return Natural;

   procedure Clear (Self : in out invocation_Vector);

   procedure Append (Self : in out invocation_Vector; Value : invocation);

   type invocation_Variable_Reference (Element : not null access invocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_invocation_Variable_Reference
     (Self  : aliased in out invocation_Vector;
      Index : Positive)
      return invocation_Variable_Reference with
     Inline;

   type invocation_Constant_Reference
     (Element : not null access constant invocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_invocation_Constant_Reference
     (Self  : aliased invocation_Vector;
      Index : Positive)
      return invocation_Constant_Reference with
     Inline;

   function Length (Self : artifactChange_Vector) return Natural;

   procedure Clear (Self : in out artifactChange_Vector);

   procedure Append
     (Self : in out artifactChange_Vector; Value : artifactChange);

   type artifactChange_Variable_Reference
     (Element : not null access artifactChange) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifactChange_Variable_Reference
     (Self  : aliased in out artifactChange_Vector;
      Index : Positive)
      return artifactChange_Variable_Reference with
     Inline;

   type artifactChange_Constant_Reference
     (Element : not null access constant artifactChange) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifactChange_Constant_Reference
     (Self  : aliased artifactChange_Vector;
      Index : Positive)
      return artifactChange_Constant_Reference with
     Inline;

   function Length (Self : artifact_roles_Vector) return Natural;

   procedure Clear (Self : in out artifact_roles_Vector);

   procedure Append
     (Self : in out artifact_roles_Vector; Value : Enum.artifact_roles);

   type artifact_roles_Variable_Reference
     (Element : not null access Enum.artifact_roles) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifact_roles_Variable_Reference
     (Self  : aliased in out artifact_roles_Vector;
      Index : Positive)
      return artifact_roles_Variable_Reference with
     Inline;

   type artifact_roles_Constant_Reference
     (Element : not null access constant Enum.artifact_roles) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifact_roles_Constant_Reference
     (Self  : aliased artifact_roles_Vector;
      Index : Positive)
      return artifact_roles_Constant_Reference with
     Inline;

   function Length (Self : artifact_Vector) return Natural;

   procedure Clear (Self : in out artifact_Vector);

   procedure Append (Self : in out artifact_Vector; Value : artifact);

   type artifact_Variable_Reference (Element : not null access artifact) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifact_Variable_Reference
     (Self  : aliased in out artifact_Vector;
      Index : Positive)
      return artifact_Variable_Reference with
     Inline;

   type artifact_Constant_Reference
     (Element : not null access constant artifact) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifact_Constant_Reference
     (Self  : aliased artifact_Vector;
      Index : Positive)
      return artifact_Constant_Reference with
     Inline;

   function Length (Self : logicalLocation_Vector) return Natural;

   procedure Clear (Self : in out logicalLocation_Vector);

   procedure Append
     (Self : in out logicalLocation_Vector; Value : logicalLocation);

   type logicalLocation_Variable_Reference
     (Element : not null access logicalLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_logicalLocation_Variable_Reference
     (Self  : aliased in out logicalLocation_Vector;
      Index : Positive)
      return logicalLocation_Variable_Reference with
     Inline;

   type logicalLocation_Constant_Reference
     (Element : not null access constant logicalLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_logicalLocation_Constant_Reference
     (Self  : aliased logicalLocation_Vector;
      Index : Positive)
      return logicalLocation_Constant_Reference with
     Inline;

   function Length (Self : webRequest_Vector) return Natural;

   procedure Clear (Self : in out webRequest_Vector);

   procedure Append (Self : in out webRequest_Vector; Value : webRequest);

   type webRequest_Variable_Reference (Element : not null access webRequest) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_webRequest_Variable_Reference
     (Self  : aliased in out webRequest_Vector;
      Index : Positive)
      return webRequest_Variable_Reference with
     Inline;

   type webRequest_Constant_Reference
     (Element : not null access constant webRequest) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_webRequest_Constant_Reference
     (Self  : aliased webRequest_Vector;
      Index : Positive)
      return webRequest_Constant_Reference with
     Inline;

   function Length (Self : externalProperties_Vector) return Natural;

   procedure Clear (Self : in out externalProperties_Vector);

   procedure Append
     (Self : in out externalProperties_Vector; Value : externalProperties);

   type externalProperties_Variable_Reference
     (Element : not null access externalProperties) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_externalProperties_Variable_Reference
     (Self  : aliased in out externalProperties_Vector;
      Index : Positive)
      return externalProperties_Variable_Reference with
     Inline;

   type externalProperties_Constant_Reference
     (Element : not null access constant externalProperties) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_externalProperties_Constant_Reference
     (Self  : aliased externalProperties_Vector;
      Index : Positive)
      return externalProperties_Constant_Reference with
     Inline;

   function Length (Self : run_Vector) return Natural;

   procedure Clear (Self : in out run_Vector);

   procedure Append (Self : in out run_Vector; Value : run);

   type run_Variable_Reference (Element : not null access run) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_run_Variable_Reference
     (Self  : aliased in out run_Vector;
      Index : Positive)
      return run_Variable_Reference with
     Inline;

   type run_Constant_Reference (Element : not null access constant run) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_run_Constant_Reference
     (Self  : aliased run_Vector;
      Index : Positive)
      return run_Constant_Reference with
     Inline;

   function Length (Self : a_exception_Vector) return Natural;

   procedure Clear (Self : in out a_exception_Vector);

   procedure Append (Self : in out a_exception_Vector; Value : a_exception);

   type a_exception_Variable_Reference (Element : not null access a_exception)
   is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_a_exception_Variable_Reference
     (Self  : aliased in out a_exception_Vector;
      Index : Positive)
      return a_exception_Variable_Reference with
     Inline;

   type a_exception_Constant_Reference
     (Element : not null access constant a_exception) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_a_exception_Constant_Reference
     (Self  : aliased a_exception_Vector;
      Index : Positive)
      return a_exception_Constant_Reference with
     Inline;

   function Length (Self : reportingDescriptor_Vector) return Natural;

   procedure Clear (Self : in out reportingDescriptor_Vector);

   procedure Append
     (Self : in out reportingDescriptor_Vector; Value : reportingDescriptor);

   type reportingDescriptor_Variable_Reference
     (Element : not null access reportingDescriptor) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_reportingDescriptor_Variable_Reference
     (Self  : aliased in out reportingDescriptor_Vector;
      Index : Positive)
      return reportingDescriptor_Variable_Reference with
     Inline;

   type reportingDescriptor_Constant_Reference
     (Element : not null access constant reportingDescriptor) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_reportingDescriptor_Constant_Reference
     (Self  : aliased reportingDescriptor_Vector;
      Index : Positive)
      return reportingDescriptor_Constant_Reference with
     Inline;

   function Length (Self : artifactLocation_Vector) return Natural;

   procedure Clear (Self : in out artifactLocation_Vector);

   procedure Append
     (Self : in out artifactLocation_Vector; Value : artifactLocation);

   type artifactLocation_Variable_Reference
     (Element : not null access artifactLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifactLocation_Variable_Reference
     (Self  : aliased in out artifactLocation_Vector;
      Index : Positive)
      return artifactLocation_Variable_Reference with
     Inline;

   type artifactLocation_Constant_Reference
     (Element : not null access constant artifactLocation) is
   null record with
     Implicit_Dereference => Element;

   not overriding function Get_artifactLocation_Constant_Reference
     (Self  : aliased artifactLocation_Vector;
      Index : Positive)
      return artifactLocation_Constant_Reference with
     Inline;

private
   type configurationOverride_Array is
     array (Positive range <>) of aliased configurationOverride;
   type configurationOverride_Array_Access is
     access configurationOverride_Array;
   type configurationOverride_Vector is
   new Ada.Finalization.Controlled with record
      Data   : configurationOverride_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out configurationOverride_Vector);

   overriding procedure Finalize (Self : in out configurationOverride_Vector);

   type locationRelationship_Array is
     array (Positive range <>) of aliased locationRelationship;
   type locationRelationship_Array_Access is access locationRelationship_Array;
   type locationRelationship_Vector is
   new Ada.Finalization.Controlled with record
      Data   : locationRelationship_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out locationRelationship_Vector);

   overriding procedure Finalize (Self : in out locationRelationship_Vector);

   type threadFlowLocation_Array is
     array (Positive range <>) of aliased threadFlowLocation;
   type threadFlowLocation_Array_Access is access threadFlowLocation_Array;
   type threadFlowLocation_Vector is
   new Ada.Finalization.Controlled with record
      Data   : threadFlowLocation_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out threadFlowLocation_Vector);

   overriding procedure Finalize (Self : in out threadFlowLocation_Vector);

   type result_Array is array (Positive range <>) of aliased result;
   type result_Array_Access is access result_Array;
   type result_Vector is new Ada.Finalization.Controlled with record
      Data   : result_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out result_Vector);

   overriding procedure Finalize (Self : in out result_Vector);

   type runAutomationDetails_Array is
     array (Positive range <>) of aliased runAutomationDetails;
   type runAutomationDetails_Array_Access is access runAutomationDetails_Array;
   type runAutomationDetails_Vector is
   new Ada.Finalization.Controlled with record
      Data   : runAutomationDetails_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out runAutomationDetails_Vector);

   overriding procedure Finalize (Self : in out runAutomationDetails_Vector);

   type replacement_Array is array (Positive range <>) of aliased replacement;
   type replacement_Array_Access is access replacement_Array;
   type replacement_Vector is new Ada.Finalization.Controlled with record
      Data   : replacement_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out replacement_Vector);

   overriding procedure Finalize (Self : in out replacement_Vector);

   type webResponse_Array is array (Positive range <>) of aliased webResponse;
   type webResponse_Array_Access is access webResponse_Array;
   type webResponse_Vector is new Ada.Finalization.Controlled with record
      Data   : webResponse_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out webResponse_Vector);

   overriding procedure Finalize (Self : in out webResponse_Vector);

   type address_Array is array (Positive range <>) of aliased address;
   type address_Array_Access is access address_Array;
   type address_Vector is new Ada.Finalization.Controlled with record
      Data   : address_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out address_Vector);

   overriding procedure Finalize (Self : in out address_Vector);

   type physicalLocation_Array is
     array (Positive range <>) of aliased physicalLocation;
   type physicalLocation_Array_Access is access physicalLocation_Array;
   type physicalLocation_Vector is new Ada.Finalization.Controlled with record
      Data   : physicalLocation_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out physicalLocation_Vector);

   overriding procedure Finalize (Self : in out physicalLocation_Vector);

   type stackFrame_Array is array (Positive range <>) of aliased stackFrame;
   type stackFrame_Array_Access is access stackFrame_Array;
   type stackFrame_Vector is new Ada.Finalization.Controlled with record
      Data   : stackFrame_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out stackFrame_Vector);

   overriding procedure Finalize (Self : in out stackFrame_Vector);

   type toolComponent_Array is
     array (Positive range <>) of aliased toolComponent;
   type toolComponent_Array_Access is access toolComponent_Array;
   type toolComponent_Vector is new Ada.Finalization.Controlled with record
      Data   : toolComponent_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out toolComponent_Vector);

   overriding procedure Finalize (Self : in out toolComponent_Vector);

   type externalPropertyFileReference_Array is
     array (Positive range <>) of aliased externalPropertyFileReference;
   type externalPropertyFileReference_Array_Access is
     access externalPropertyFileReference_Array;
   type externalPropertyFileReference_Vector is
   new Ada.Finalization.Controlled with record
      Data   : externalPropertyFileReference_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust
     (Self : in out externalPropertyFileReference_Vector);

   overriding procedure Finalize
     (Self : in out externalPropertyFileReference_Vector);

   type stack_Array is array (Positive range <>) of aliased stack;
   type stack_Array_Access is access stack_Array;
   type stack_Vector is new Ada.Finalization.Controlled with record
      Data   : stack_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out stack_Vector);

   overriding procedure Finalize (Self : in out stack_Vector);

   type Integer_Array is array (Positive range <>) of aliased Integer;
   type Integer_Array_Access is access Integer_Array;
   type Integer_Vector is new Ada.Finalization.Controlled with record
      Data   : Integer_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out Integer_Vector);

   overriding procedure Finalize (Self : in out Integer_Vector);

   type notification_Array is
     array (Positive range <>) of aliased notification;
   type notification_Array_Access is access notification_Array;
   type notification_Vector is new Ada.Finalization.Controlled with record
      Data   : notification_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out notification_Vector);

   overriding procedure Finalize (Self : in out notification_Vector);

   type attachment_Array is array (Positive range <>) of aliased attachment;
   type attachment_Array_Access is access attachment_Array;
   type attachment_Vector is new Ada.Finalization.Controlled with record
      Data   : attachment_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out attachment_Vector);

   overriding procedure Finalize (Self : in out attachment_Vector);

   type suppression_Array is array (Positive range <>) of aliased suppression;
   type suppression_Array_Access is access suppression_Array;
   type suppression_Vector is new Ada.Finalization.Controlled with record
      Data   : suppression_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out suppression_Vector);

   overriding procedure Finalize (Self : in out suppression_Vector);

   type edgeTraversal_Array is
     array (Positive range <>) of aliased edgeTraversal;
   type edgeTraversal_Array_Access is access edgeTraversal_Array;
   type edgeTraversal_Vector is new Ada.Finalization.Controlled with record
      Data   : edgeTraversal_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out edgeTraversal_Vector);

   overriding procedure Finalize (Self : in out edgeTraversal_Vector);

   type graphTraversal_Array is
     array (Positive range <>) of aliased graphTraversal;
   type graphTraversal_Array_Access is access graphTraversal_Array;
   type graphTraversal_Vector is new Ada.Finalization.Controlled with record
      Data   : graphTraversal_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out graphTraversal_Vector);

   overriding procedure Finalize (Self : in out graphTraversal_Vector);

   type location_Array is array (Positive range <>) of aliased location;
   type location_Array_Access is access location_Array;
   type location_Vector is new Ada.Finalization.Controlled with record
      Data   : location_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out location_Vector);

   overriding procedure Finalize (Self : in out location_Vector);

   type graph_Array is array (Positive range <>) of aliased graph;
   type graph_Array_Access is access graph_Array;
   type graph_Vector is new Ada.Finalization.Controlled with record
      Data   : graph_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out graph_Vector);

   overriding procedure Finalize (Self : in out graph_Vector);

   type reportingDescriptorReference_Array is
     array (Positive range <>) of aliased reportingDescriptorReference;
   type reportingDescriptorReference_Array_Access is
     access reportingDescriptorReference_Array;
   type reportingDescriptorReference_Vector is
   new Ada.Finalization.Controlled with record
      Data   : reportingDescriptorReference_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust
     (Self : in out reportingDescriptorReference_Vector);

   overriding procedure Finalize
     (Self : in out reportingDescriptorReference_Vector);

   type reportingDescriptorRelationship_Array is
     array (Positive range <>) of aliased reportingDescriptorRelationship;
   type reportingDescriptorRelationship_Array_Access is
     access reportingDescriptorRelationship_Array;
   type reportingDescriptorRelationship_Vector is
   new Ada.Finalization.Controlled with record
      Data   : reportingDescriptorRelationship_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust
     (Self : in out reportingDescriptorRelationship_Vector);

   overriding procedure Finalize
     (Self : in out reportingDescriptorRelationship_Vector);

   type region_Array is array (Positive range <>) of aliased region;
   type region_Array_Access is access region_Array;
   type region_Vector is new Ada.Finalization.Controlled with record
      Data   : region_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out region_Vector);

   overriding procedure Finalize (Self : in out region_Vector);

   type versionControlDetails_Array is
     array (Positive range <>) of aliased versionControlDetails;
   type versionControlDetails_Array_Access is
     access versionControlDetails_Array;
   type versionControlDetails_Vector is
   new Ada.Finalization.Controlled with record
      Data   : versionControlDetails_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out versionControlDetails_Vector);

   overriding procedure Finalize (Self : in out versionControlDetails_Vector);

   type rectangle_Array is array (Positive range <>) of aliased rectangle;
   type rectangle_Array_Access is access rectangle_Array;
   type rectangle_Vector is new Ada.Finalization.Controlled with record
      Data   : rectangle_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out rectangle_Vector);

   overriding procedure Finalize (Self : in out rectangle_Vector);

   type codeFlow_Array is array (Positive range <>) of aliased codeFlow;
   type codeFlow_Array_Access is access codeFlow_Array;
   type codeFlow_Vector is new Ada.Finalization.Controlled with record
      Data   : codeFlow_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out codeFlow_Vector);

   overriding procedure Finalize (Self : in out codeFlow_Vector);

   type toolComponentReference_Array is
     array (Positive range <>) of aliased toolComponentReference;
   type toolComponentReference_Array_Access is
     access toolComponentReference_Array;
   type toolComponentReference_Vector is
   new Ada.Finalization.Controlled with record
      Data   : toolComponentReference_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out toolComponentReference_Vector);

   overriding procedure Finalize (Self : in out toolComponentReference_Vector);

   type edge_Array is array (Positive range <>) of aliased edge;
   type edge_Array_Access is access edge_Array;
   type edge_Vector is new Ada.Finalization.Controlled with record
      Data   : edge_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out edge_Vector);

   overriding procedure Finalize (Self : in out edge_Vector);

   type toolComponent_contents_Array is
     array (Positive range <>) of aliased Enum.toolComponent_contents;
   type toolComponent_contents_Array_Access is
     access toolComponent_contents_Array;
   type toolComponent_contents_Vector is
   new Ada.Finalization.Controlled with record
      Data   : toolComponent_contents_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out toolComponent_contents_Vector);

   overriding procedure Finalize (Self : in out toolComponent_contents_Vector);

   type node_Array is array (Positive range <>) of aliased node;
   type node_Array_Access is access node_Array;
   type node_Vector is new Ada.Finalization.Controlled with record
      Data   : node_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out node_Vector);

   overriding procedure Finalize (Self : in out node_Vector);

   type threadFlow_Array is array (Positive range <>) of aliased threadFlow;
   type threadFlow_Array_Access is access threadFlow_Array;
   type threadFlow_Vector is new Ada.Finalization.Controlled with record
      Data   : threadFlow_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out threadFlow_Vector);

   overriding procedure Finalize (Self : in out threadFlow_Vector);

   type fix_Array is array (Positive range <>) of aliased fix;
   type fix_Array_Access is access fix_Array;
   type fix_Vector is new Ada.Finalization.Controlled with record
      Data   : fix_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out fix_Vector);

   overriding procedure Finalize (Self : in out fix_Vector);

   type invocation_Array is array (Positive range <>) of aliased invocation;
   type invocation_Array_Access is access invocation_Array;
   type invocation_Vector is new Ada.Finalization.Controlled with record
      Data   : invocation_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out invocation_Vector);

   overriding procedure Finalize (Self : in out invocation_Vector);

   type artifactChange_Array is
     array (Positive range <>) of aliased artifactChange;
   type artifactChange_Array_Access is access artifactChange_Array;
   type artifactChange_Vector is new Ada.Finalization.Controlled with record
      Data   : artifactChange_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out artifactChange_Vector);

   overriding procedure Finalize (Self : in out artifactChange_Vector);

   type artifact_roles_Array is
     array (Positive range <>) of aliased Enum.artifact_roles;
   type artifact_roles_Array_Access is access artifact_roles_Array;
   type artifact_roles_Vector is new Ada.Finalization.Controlled with record
      Data   : artifact_roles_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out artifact_roles_Vector);

   overriding procedure Finalize (Self : in out artifact_roles_Vector);

   type artifact_Array is array (Positive range <>) of aliased artifact;
   type artifact_Array_Access is access artifact_Array;
   type artifact_Vector is new Ada.Finalization.Controlled with record
      Data   : artifact_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out artifact_Vector);

   overriding procedure Finalize (Self : in out artifact_Vector);

   type logicalLocation_Array is
     array (Positive range <>) of aliased logicalLocation;
   type logicalLocation_Array_Access is access logicalLocation_Array;
   type logicalLocation_Vector is new Ada.Finalization.Controlled with record
      Data   : logicalLocation_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out logicalLocation_Vector);

   overriding procedure Finalize (Self : in out logicalLocation_Vector);

   type webRequest_Array is array (Positive range <>) of aliased webRequest;
   type webRequest_Array_Access is access webRequest_Array;
   type webRequest_Vector is new Ada.Finalization.Controlled with record
      Data   : webRequest_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out webRequest_Vector);

   overriding procedure Finalize (Self : in out webRequest_Vector);

   type externalProperties_Array is
     array (Positive range <>) of aliased externalProperties;
   type externalProperties_Array_Access is access externalProperties_Array;
   type externalProperties_Vector is
   new Ada.Finalization.Controlled with record
      Data   : externalProperties_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out externalProperties_Vector);

   overriding procedure Finalize (Self : in out externalProperties_Vector);

   type run_Array is array (Positive range <>) of aliased run;
   type run_Array_Access is access run_Array;
   type run_Vector is new Ada.Finalization.Controlled with record
      Data   : run_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out run_Vector);

   overriding procedure Finalize (Self : in out run_Vector);

   type a_exception_Array is array (Positive range <>) of aliased a_exception;
   type a_exception_Array_Access is access a_exception_Array;
   type a_exception_Vector is new Ada.Finalization.Controlled with record
      Data   : a_exception_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out a_exception_Vector);

   overriding procedure Finalize (Self : in out a_exception_Vector);

   type reportingDescriptor_Array is
     array (Positive range <>) of aliased reportingDescriptor;
   type reportingDescriptor_Array_Access is access reportingDescriptor_Array;
   type reportingDescriptor_Vector is
   new Ada.Finalization.Controlled with record
      Data   : reportingDescriptor_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out reportingDescriptor_Vector);

   overriding procedure Finalize (Self : in out reportingDescriptor_Vector);

   type artifactLocation_Array is
     array (Positive range <>) of aliased artifactLocation;
   type artifactLocation_Array_Access is access artifactLocation_Array;
   type artifactLocation_Vector is new Ada.Finalization.Controlled with record
      Data   : artifactLocation_Array_Access;
      Length : Natural := 0;
   end record;

   overriding procedure Adjust (Self : in out artifactLocation_Vector);

   overriding procedure Finalize (Self : in out artifactLocation_Vector);

end SARIF.Types;
