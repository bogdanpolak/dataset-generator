unit Helper.TField;

interface

uses
  System.SysUtils,
  System.Classes,
  System.NetEncoding,
  Data.DB;

type
  TFieldHelper = class helper for TField
  private
    function GetBase64(): string;
    procedure SetBase64(const aBase64Data: string);
  public
    property Base64: string read GetBase64 write SetBase64;
  end;

implementation

function TFieldHelper.GetBase64: string;
var
  fld: TBlobField;
  ds: TDataSet;
begin
  if not(self is TBlobField) then
    raise Exception.Create
      ('DataBase64 property is supported only for TBlobField');
  fld := self as TBlobField;
  Result := TNetEncoding.Base64.EncodeBytesToString(fld.Value);
end;

procedure TFieldHelper.SetBase64(const aBase64Data: string);
var
  fld: TBlobField;
  isBrowseMode: Boolean;
  streamBase64: TStream;
  blobStream: TStream;
  ds: TDataSet;
begin
  if not(self is TBlobField) then
    raise Exception.Create
      ('DataBase64 property is supported only for TBlobField');
  fld := self as TBlobField;
  isBrowseMode := fld.DataSet.State = dsBrowse;
  streamBase64 := TStringStream.Create(aBase64Data);
  ds := fld.DataSet;
  if isBrowseMode then
  begin
    fld.DataSet.Edit;
  end;
  try
    blobStream := ds.CreateBlobStream(fld, bmWrite);
    TNetEncoding.Base64.Decode(streamBase64, blobStream);
    blobStream.Free;
  finally
    if isBrowseMode then
    begin
      fld.DataSet.Post;
    end;
  end;
  streamBase64.Free;
end;

end.
