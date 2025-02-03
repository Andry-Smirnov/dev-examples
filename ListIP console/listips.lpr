program listips;

{$MODE DELPHI}
{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Winsock,
  JwaIpHlpAPI,
  JwaIpRtrMib;

  procedure ListIPAddresses;
  var
    Ret: DWord;
    Table: PMib_IPAddrTable;
    TableSize: ULong;
    i: Integer;
    Row: PMib_IPAddrRow;
  begin
    // We begin by assuming there's just one entry, so we allocate space
    // for that one
    TableSize := SizeOf(Table^);
    GetMem(Table, TableSize);
    try
      // Request a list of IP addresses, unsorted
      Ret := GetIpAddrTable(Table, TableSize, False);
      case Ret of
        No_Error:                   ; // No error. Continue at the end of the case statement
        Error_Insufficient_Buffer:  begin
                                      // Oops. Space for just one entry wasn't enough. Allocate more.
                                      ReallocMem(Table, TableSize);
                                      Ret := GetIpAddrTable(Table, TableSize, False);
                                      if Ret <> No_Error then
                                        begin
                                          // Function expects signed value, but Ret is unsigned. Type
                                          // cast to avoid range-check error, however unlikely.
                                          RaiseLastOSError(Integer(Ret));
                                        end;
                                    end;
        else
          RaiseLastOSError(Integer(Ret));
      end;
      WriteLn(Table.dwNumEntries, ' entries:');
      if Table.dwNumEntries > 0 then
        begin
          Row := @Table.table[0];
          for i := 0 to Pred(Table.dwNumEntries) do
            begin
              WriteLn(inet_ntoa(in_addr(Row.dwAddr)));
              Inc(Row);
            end;
        end;
    finally
      FreeMem(Table);
    end;
  end;

begin
  try
    ListIPAddresses;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
