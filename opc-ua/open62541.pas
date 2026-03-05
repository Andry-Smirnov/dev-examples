{$CODEPAGE UTF8}
unit open62541;


{$mode ObjFPC}{$H+}
{$DEFINE UA_ENABLE_TYPEDESCRIPTION}


interface


uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  DynLibs,
  {$ENDIF}
  Classes,
  SysUtils
  ;

const
  UA_EMPTY_ARRAY_SENTINEL = Pointer($01);


type
{$IFNDEF FPC}
  // Delphi XE compatibility
  // FixedUInt 32-bit
  DWord = LongWord;
{$ENDIF}
  // DWord on 32-bit platforms, QWord on 64-bit platforms
  size_t = NativeUInt;

  UA_Client = record
  end;
  PUA_Client = ^UA_Client;

// types.h

  UA_Boolean = ByteBool;
  PUA_Boolean = ^UA_Boolean;

  UA_Byte = Byte;
  PUA_Byte = ^UA_Byte;

  UA_Int16 = SmallInt;
  PUA_Int16 = ^UA_Int16;

  UA_UInt16 = Word;
  PUA_UInt16 = ^UA_UInt16;

  UA_Int32 = Integer;
  PUA_Int32 = ^UA_Int32;

  UA_UInt32 = DWord;
  PUA_UInt32 = ^UA_UInt32;

  UA_Int64 = Int64;
  PUA_Int64 = ^UA_Int64;

  UA_UInt64 = UInt64;
  PUA_UInt64 = ^UA_UInt64;

  UA_Float = Single;
  PUA_Float = ^UA_Float;

  UA_Double = Double;
  PUA_Double = ^UA_Double;


  // StatusCode (.. _statuscode:)
  //
  // A numeric identifier for a error or condition that is associated with a value
  // or an operation. See the section :ref:`statuscodes` for the meaning of a specific code.
  UA_StatusCode = DWord; // uint32_t
  PUA_StatusCode = ^UA_StatusCode;

  // String - A sequence of Unicode characters. Strings are just an array of UA_Byte.
  UA_String = record
    length: size_t; // The length of the string
    data: PUA_Byte; // The content (not null-terminated)
  end;
  PUA_String = ^UA_String;

  // DateTime (.. _datetime:)
  //
  // An instance in time. A DateTime value is encoded as a 64-bit signed integer
  // which represents the number of 100 nanosecond intervals since January 1, 1601 (UTC).
  //
  // The methods providing an interface to the system clock are architecture-
  // specific. Usually, they provide a UTC clock that includes leap seconds. The
  // OPC UA standard allows the use of International Atomic Time (TAI) for the
  // DateTime instead. But this is still unusual and not implemented for most
  // SDKs. Currently (2019), UTC and TAI are 37 seconds apart due to leap seconds.
  UA_DateTime = Int64;
  PUA_DateTime = ^UA_DateTime;

  UA_DateTimeStruct = record
    nanoSec: UA_UInt16;
    microSec: UA_UInt16;
    milliSec: UA_UInt16;
    sec: UA_UInt16;
    min: UA_UInt16;
    hour: UA_UInt16;
    day: UA_UInt16;
    month: UA_UInt16;
    year: UA_UInt16;
  end;

  // ByteString
  // A sequence of octets.
  UA_ByteString = UA_String;
  PUA_ByteString = ^UA_ByteString;

  // GUID
  // A 16 byte value that can be used as a globally unique identifier.
  UA_Guid = record
    data1: UA_UInt32;
    data2: UA_UInt16;
    data3: UA_UInt16 ;
    data4: array[0..7] of UA_Byte;
  end;

  UA_LogLevel = (
    UA_LOGLEVEL_TRACE,
    UA_LOGLEVEL_DEBUG,
    UA_LOGLEVEL_INFO,
    UA_LOGLEVEL_WARNING,
    UA_LOGLEVEL_ERROR,
    UA_LOGLEVEL_FATAL
  );

  UA_LogCategory = (
    UA_LOGCATEGORY_NETWORK,
    UA_LOGCATEGORY_SECURECHANNEL,
    UA_LOGCATEGORY_SESSION,
    UA_LOGCATEGORY_SERVER,
    UA_LOGCATEGORY_CLIENT,
    UA_LOGCATEGORY_USERLAND,
    UA_LOGCATEGORY_SECURITYPOLICY
  );

  UA_Logger = record
    // Log a message. The message string and following varargs are formatted
    // according to the rules of the printf command. Use the convenience macros
    // below that take the minimum log-level defined in ua_config.h into account.
    log: procedure(logContext: Pointer; level: UA_LogLevel; category: UA_LogCategory; msg: PAnsiChar; args: {va_list}array of const); cdecl;
    context: Pointer; // Logger state
    clear: procedure(context: Pointer); cdecl; // Clean up the logger plugin
  end;
  PUA_Logger  = ^UA_Logger;

  // NodeId (.. _nodeid:)
  //
  // An identifier for a node in the address space of an OPC UA Server.
  UA_NodeIdType = (
    // In the binary encoding, this can also
    // become 1 or 2 (two-byte and four-byte
    // encoding of small numeric nodeids)
    UA_NODEIDTYPE_NUMERIC    = 0,
    UA_NODEIDTYPE_STRING     = 3,
    UA_NODEIDTYPE_GUID       = 4,
    UA_NODEIDTYPE_BYTESTRING = 5
  );

  UA_NodeId = record
    namespaceIndex: UA_UInt16;
    identifierType: UA_NodeIdType;
    identifier: record
      case LongInt of
        0: ( numeric: UA_UInt32 );
        1: ( _string: UA_String );
        2: ( guid: UA_Guid );
        3: ( byteString: UA_ByteString );
      end;
  end;
  PUA_NodeId = ^UA_NodeId;

  // ExpandedNodeId
  //
  // A NodeId that allows the namespace URI to be specified instead of an index.
  UA_ExpandedNodeId = record
    nodeId: UA_NodeId;
    namespaceUri: UA_String;
    serverIndex: UA_UInt32;
  end;
  PUA_ExpandedNodeId = ^UA_ExpandedNodeId;

  // QualifiedName (.. _qualifiedname:)
  //
  // A name qualified by a namespace. *)
  UA_QualifiedName = record
    namespaceIndex: UA_UInt16;
    name: UA_String;
  end;
  PUA_QualifiedName = ^UA_QualifiedName;

  // LocalizedText
  //
  // Human readable text with an optional locale identifier.
  UA_LocalizedText = record
    locale: UA_String;
    text: UA_String;
  end;
  PUA_LocalizedText = ^UA_LocalizedText;

  // NumericRange
  //
  // NumericRanges are used to indicate subsets of a (multidimensional) array.
  // They no official data type in the OPC UA standard and are transmitted only
  // with a string encoding, such as "1:2,0:3,5". The colon separates min/max
  // index and the comma separates dimensions. A Single value indicates a range
  // with a Single element (min==max). *)
  UA_NumericRangeDimension = record
    min: UA_UInt32;
    max: UA_UInt32;
  end;

  UA_NumericRange = record
    dimensionsSize: size_t;
    dimensions: ^UA_NumericRangeDimension;
  end;
  PUA_NumericRange = ^UA_NumericRange;


  // Variant (.. _variant:)
  //
  // Variants may contain values of any type together with a description of the
  // content. See the section on :ref:`generic-types` on how types are described.
  // The standard mandates that variants contain built-in data types only. If the
  // value is not of a builtin type, it is wrapped into an :ref:`extensionobject`.
  // open62541 hides this wrapping transparently in the encoding layer. If the
  // data type is unknown to the receiver, the variant contains the original
  // ExtensionObject in binary or XML encoding.
  //
  // Variants may contain a scalar value or an array. For details on the handling
  // of arrays, see the section on :ref:`array-handling`. Array variants can have
  // an additional dimensionality (matrix, 3-tensor, ...) defined in an array of
  // dimension lengths. The actual values are kept in an array of dimensions one.
  // For users who work with higher-dimensions arrays directly, keep in mind that
  // dimensions of higher rank are serialized first (the highest rank dimension
  // has stride 1 and elements follow each other directly). Usually it is simplest
  // to interact with higher-dimensional arrays via ``UA_NumericRange``
  // descriptions (see :ref:`array-handling`).
  //
  // To differentiate between scalar / array variants, the following definition is
  // used. ``UA_Variant_isScalar`` provides simplified access to these checks.
  //
  // - ``arrayLength == 0 && data == NULL``: undefined array of length -1
  // - ``arrayLength == 0 && data == UA_EMPTY_ARRAY_SENTINEL``: array of length 0
  // - ``arrayLength == 0 && data > UA_EMPTY_ARRAY_SENTINEL``: scalar value
  // - ``arrayLength > 0``: array of the given length
  //
  // Variants can also be *empty*. Then, the pointer to the type description is ``NULL``.
  UA_VariantStorageType = (
    UA_VARIANT_DATA,         // The data has the same lifecycle as the variant
    UA_VARIANT_DATA_NODELETE // The data is "borrowed" by the variant and shall not be deleted at the end of the variant's lifecycle
  );

  PUA_DataType = ^UA_DataType;

  UA_Variant = record
    _type: PUA_DataType;          // The data type description
    storageType: UA_VariantStorageType;
    arrayLength: size_t;
    data: Pointer;                // Points to the scalar or array data
    arrayDimensionsSize: size_t;  // The number of dimensions
    arrayDimensions: ^UA_UInt32;  // The length of each dimension
  end;
  PUA_Variant = ^UA_Variant;

  // ExtensionObject (.. _extensionobject:)
  //
  // ExtensionObjects may contain scalars of any data type. Even those that are
  // unknown to the receiver. See the section on :ref:`generic-types` on how types
  // are described. If the received data type is unknown, the encoded string and
  // target NodeId is stored instead of the decoded value. *)
  UA_ExtensionObjectEncoding = (
    UA_EXTENSIONOBJECT_ENCODED_NOBODY     = 0,
    UA_EXTENSIONOBJECT_ENCODED_BYTESTRING = 1,
    UA_EXTENSIONOBJECT_ENCODED_XML        = 2,
    UA_EXTENSIONOBJECT_DECODED            = 3,
    UA_EXTENSIONOBJECT_DECODED_NODELETE   = 4 // Don't delete the content together with the ExtensionObject
  );


  UA_ExtensionObject = record
    encoding: UA_ExtensionObjectEncoding;
    content: record
      case LongInt of
        0:  ( encoded: record
              // The nodeid of the datatype
              typeId: UA_NodeId;
              // The bytestring of the encoded data
              body: UA_ByteString;
            end
            );
        1:  ( decoded: record
              _type: PUA_DataType;
              data: Pointer;
            end );
      end;
    end;
  PUA_ExtensionObject = ^UA_ExtensionObject;

  // DataValue
  //
  // A data value with an associated status code and timestamps.
  UA_DataValue = record
    value: UA_Variant;
    sourceTimestamp: UA_DateTime;
    serverTimestamp: UA_DateTime;
    sourcePicoseconds: UA_UInt16;
    serverPicoseconds: UA_UInt16;
    status: UA_StatusCode;
    flag: UA_Byte;
  end;
  PUA_DataValue = ^UA_DataValue;


  // DiagnosticInfo - A structure that contains detailed error and diagnostic information
  // associated with a StatusCode.
  PUA_DiagnosticInfo = ^UA_DiagnosticInfo;
  UA_DiagnosticInfo = bitpacked record
//    flag: UA_Boolean;
    hasSymbolicId: 0..1;
    hasNamespaceUri: 0..1;
    hasLocalizedText: 0..1;
    hasLocale: 0..1;
    hasAdditionalInfo: 0..1;
    hasInnerStatusCode: 0..1;
    hasInnerDiagnosticInfo: 0..1;

    symbolicId: UA_Int32;
    namespaceUri: UA_Int32;
    localizedText: UA_Int32;
    locale: UA_Int32;
    additionalInfo: UA_String;
    innerStatusCode: UA_StatusCode;
    innerDiagnosticInfo: PUA_DiagnosticInfo;
  end;


  T1Bit = 0..(2 shl (1-1))-1;
  T2Bit = 0..(2 shl (2-1))-1;
  T3Bit = 0..(2 shl (3-1))-1;
  T6Bit = 0..(2 shl (6-1))-1;

  UA_DataTypeMember = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    memberName: PAnsiChar;       // Human-readable member name
    {$ENDIF}
    memberType: PUA_DataType;    // The member data type description
    // How much padding is there before this member element? For arrays this is
    // the padding before the size_t length member.
    // (No padding between size_t and the following ptr.) For unions,
    // the padding includes the size of the switchfield (the offset from
    // the start of the union type).
    padding: T6Bit;
    isArray: T1Bit;              // The member is an array
    isOptional: T1Bit;           // The member is an optional field
  end;

  UA_DataType = bitpacked record
    {$IFDEF UA_ENABLE_TYPEDESCRIPTION}
    typeName: PAnsiChar;
    {$ENDIF}
    typeId: UA_NodeId;               // The nodeid of the type
    binaryEncodingId: UA_NodeId;     // NodeId of datatype when encoded as binary
    xmlEncodingId: UA_NodeId;        // NodeId of datatype when encoded as XML
    memSize: UA_UInt16;              // Size of the struct in memory
    typeKind: T6Bit;                 // Dispatch index for the handling routines
    pointerFree: T1Bit;              // The type (and its members) contains no pointers that need to be freed
    overlayable: T1Bit;              // The type has the identical memory layout in memory and on the binary stream.
    membersSize: UA_Byte;            // How many members does the type have?
    members: ^UA_DataTypeMember;
  end;


{$I generated_1_5.inc}


const
{$I statuscodes_1_5.inc}


(*
const
{$I nodeids_1_5.inc}
*)


var
  LibLoaded: Boolean;


procedure LoadOpen62541;
procedure UnloadOpen62541;


implementation


const
  LIB_NAME = 'libopen62541.1.5.dll';


type
  TUA_TYPES_Array = array[0..UA_TYPES_COUNT-1] of UA_DataType;
  PUA_TYPES_Array = ^TUA_TYPES_Array;



var
{$IFDEF MSWINDOWS}
  FOpen62541LibHandle: THandle;
{$ELSE}
  FOpen62541LibHandle: TLibHandle;
{$ENDIF}
  FRefCount: Integer;

  //
  UA_TYPES: PUA_TYPES_Array;


procedure LoadOpen62541;
begin
  Inc(FRefCount);
  if FRefCount <> 1 then
    Exit;

  FOpen62541LibHandle := LoadLibrary(LIB_NAME);
  if FOpen62541LibHandle = 0 then
    begin
      FRefCount := 0;
      raise EInOutError.CreateFmt('Can not load library "%s". Check your installation.' + sLineBreak + '%s',
        [LIB_NAME, GetLoadErrorStr()]);
    end;
  LibLoaded := True;

  // external variable name
  UA_TYPES := GetProcedureAddress(FOpen62541LibHandle, 'UA_TYPES');
(*
  UA_VariableAttributes_default := PUA_VariableAttributes(GetProcedureAddress(FOpen62541LibHandle, 'UA_VariableAttributes_default'))^;
  UA_MethodAttributes_default := PUA_MethodAttributes(GetProcedureAddress(FOpen62541LibHandle, 'UA_MethodAttributes_default'))^;
  UA_ObjectAttributes_default := PUA_ObjectAttributes(GetProcedureAddress(FOpen62541LibHandle, 'UA_ObjectAttributes_default'))^;
  UA_ObjectTypeAttributes_default := PUA_ObjectTypeAttributes(GetProcedureAddress(FOpen62541LibHandle, 'UA_ObjectTypeAttributes_default'))^;
  UA_ReferenceTypeAttributes_default := PUA_ReferenceTypeAttributes(GetProcedureAddress(FOpen62541LibHandle, 'UA_ReferenceTypeAttributes_default'))^;
  UA_DataTypeAttributes_default := PUA_DataTypeAttributes(GetProcedureAddress(FOpen62541LibHandle, 'UA_DataTypeAttributes_default'))^;
*)
end;


procedure UnloadOpen62541;
begin
  if FRefCount > 0 then
    begin
      Dec(FRefCount);
      if FRefCount = 0 then
        UnloadLibrary(FOpen62541LibHandle);
    end;
  LibLoaded := FRefCount = 0;
end;


end.

