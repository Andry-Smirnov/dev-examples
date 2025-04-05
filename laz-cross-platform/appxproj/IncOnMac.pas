unit IncOnMac;

 {Dummy unit so that package compiles all Mac units.}

interface

{$IFDEF DARWIN}
uses
  NSFormat;
{$ENDIF}

implementation

end.
