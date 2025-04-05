unit SimplerXml;

{
  Class that simplifes working with XML file.

  Author:    Phil Hess.
  Copyright: Copyright 2016 Phil Hess.
  License:   Modified LGPL (see Free Pascal's rtl/COPYING.FPC).
             This means you can link your code to this compiled unit (statically 
             in a standalone executable or dynamically in a library) without 
             releasing your code. Only changes to this unit need to be made 
             publicly available.
}

{$mode delphi}

interface

uses
  SysUtils,
  DOM,
  XMLRead,
  XMLWrite;

{Note: AnsiString indicates where a UTF-8 string is expected or returned.}

type
  TSimplerXml = class
  private
    XmlDoc : TXMLDocument;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(const FileName : string) : Boolean;
    function SaveTo(const FileName : string) : Boolean;
    function FindNode(      ANode    : TDOMNode;
                      const NodeName : AnsiString) : TDOMNode;
    function GetNode(ANode   : TDOMNode;
                     NodeIdx : Integer) : TDOMNode;
    function GetNodeText(ANode : TDOMNode) : WideString;
    function GetNodeTextAsUTF8(ANode : TDOMNode) : AnsiString;
    function SetNodeText(      ANode    : TDOMNode;
                         const NodeText : WideString) : Boolean;
    function SetNodeTextAsUTF8(      ANode    : TDOMNode;
                               const NodeText : AnsiString) : Boolean;
    function GetAttrText(      ANode    : TDOMNode;
                         const AttrName : AnsiString) : WideString;
    function GetAttrTextAsUTF8(      ANode    : TDOMNode;
                               const AttrName : AnsiString) : AnsiString;
    function SetAttrText(      ANode    : TDOMNode;
                         const AttrName : AnsiString;
                         const AttrText : WideString) : Boolean;
    function SetAttrTextAsUTF8(      ANode    : TDOMNode;
                               const AttrName : AnsiString;
                               const AttrText : AnsiString) : Boolean;
    function AddNode(      ParentNode : TDOMNode;
                     const NodeName   : AnsiString) : TDOMNode;
  end;


implementation

constructor TSimplerXml.Create;
begin
  XmlDoc := TXMLDocument.Create;
end;


destructor TSimplerXml.Destroy;
begin
  XmlDoc.Free;
end;


function TSimplerXml.Load(const FileName : string) : Boolean;
begin
  Result := False;
  XmlDoc.Free;
  try
    XmlRead.ReadXmlFile(XmlDoc, FileName);
    Result := True;
  except
  end;
end;


function TSimplerXml.SaveTo(const FileName : string) : Boolean;
begin
  Result := False;
  try
    WriteXmlFile(XmlDoc, FileName);
    Result := True;
  except
  end;
end;


function TSimplerXml.FindNode(      ANode    : TDOMNode;
                              const NodeName : AnsiString) : TDOMNode;
begin
  if Assigned(ANode) then
    Result := ANode.FindNode(UTF8Decode(NodeName))
  else  {Search document}
    Result := XmlDoc.FindNode(UTF8Decode(NodeName));
end;


function TSimplerXml.GetNode(ANode   : TDOMNode;
                             NodeIdx : Integer) : TDOMNode;
begin
  Result := ANode.ChildNodes.Item[NodeIdx];
end;


function TSimplerXml.GetNodeText(ANode : TDOMNode) : WideString;
begin
  Result := '';
  if Assigned(ANode) then
    begin
    ANode := ANode.FindNode('#text');
    if Assigned(ANode) then
      Result := ANode.NodeValue;
    end;
end;


function TSimplerXml.GetNodeTextAsUTF8(ANode : TDOMNode) : AnsiString;
begin
  Result := UTF8Encode(GetNodeText(ANode));
end;


function TSimplerXml.SetNodeText(      ANode    : TDOMNode;
                                 const NodeText : WideString) : Boolean;
var
  TextNode : TDOMNode;
begin
  Result := False;
  if Assigned(ANode) then
    begin
    TextNode := ANode.FindNode('#text');
    if Assigned(TextNode) then
      TextNode.NodeValue := NodeText
    else  {Doesn't have text node for value}
      begin
      TextNode := XmlDoc.CreateTextNode(NodeText);
      ANode.AppendChild(TextNode);
      end;
    Result := True;
    end;
end;


function TSimplerXml.SetNodeTextAsUTF8(      ANode    : TDOMNode;
                                       const NodeText : AnsiString) : Boolean;
begin
  Result := SetNodeText(ANode, UTF8Decode(NodeText));
end;


function TSimplerXml.GetAttrText(      ANode    : TDOMNode;
                                 const AttrName : AnsiString) : WideString;
begin
  Result := '';
  if Assigned(ANode) then
    begin
    ANode := ANode.Attributes.GetNamedItem(UTF8Decode(AttrName));
    if Assigned(ANode) then
      Result := ANode.NodeValue;
    end;
end;


function TSimplerXml.GetAttrTextAsUTF8(      ANode    : TDOMNode;
                                       const AttrName : AnsiString) : AnsiString;
begin
  Result := UTF8Encode(GetAttrText(ANode, AttrName));
end;


function TSimplerXml.SetAttrText(      ANode    : TDOMNode;
                                 const AttrName : AnsiString;
                                 const AttrText : WideString) : Boolean;
var
  AttrNode : TDOMNode;
begin
  Result := False;
  if Assigned(ANode) then
    begin
    AttrNode := ANode.Attributes.GetNamedItem(UTF8Decode(AttrName));
    if Assigned(AttrNode) then
      AttrNode.NodeValue := AttrText
    else
      begin
      AttrNode := XmlDoc.CreateAttribute(UTF8Decode(AttrName));
      AttrNode.NodeValue := AttrText;
      ANode.Attributes.SetNamedItem(AttrNode);
      end;
    Result := True;
    end;
end;


function TSimplerXml.SetAttrTextAsUTF8(      ANode    : TDOMNode;
                                       const AttrName : AnsiString;
                                       const AttrText : AnsiString) : Boolean;
begin
  Result := SetAttrText(ANode, AttrName, UTF8Decode(AttrText));
end;


function TSimplerXml.AddNode(      ParentNode : TDOMNode;
                             const NodeName   : AnsiString) : TDOMNode;
begin
  Result := XmlDoc.CreateElement(UTF8Decode(NodeName));
  if Assigned(ParentNode) then
    ParentNode.AppendChild(Result)
  else  {Append to document}
    XmlDoc.AppendChild(Result);
end;


end.
