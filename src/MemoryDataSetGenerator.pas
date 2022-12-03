{ * ------------------------------------------------------------------------
  * ♥
  * ♥ Memory DataSet Generator
  * ♥ creates ClientDataSet or FDMemTable with the data
  * ♥
  * Component: TDSGenerator
  * Project: https://github.com/bogdanpolak/dataset-generator
  * ------------------------------------------------------------------------ }

unit MemoryDataSetGenerator;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  Data.DB,
  FireDAC.Comp.Client;

type
  TGeneratorMode = (genStructure, genAppend, genFunction, genUnit);
  TDataSetType = (dstFDMemTable, dstClientDataSet);
  TAppendMode = (amMultilineAppends, amSinglelineAppends, amAppendRows);

  TDSGenerator = class(TComponent)
  public const
    Version = '1.5';
  private
    fCode: TStrings;
    fDataSet: TDataSet;
    fIndentationText: String;
    fGeneratorMode: TGeneratorMode;
    fDataSetType: TDataSetType;
    fAppendMode: TAppendMode;
    fNameOfUnit: string;
    fMaxRows: integer;
    fRightMargin: integer;
    function GetDataFieldPrecision(fld: TField): integer;
    function GenerateOneAppend_Multiline: string;
    function GenerateSingleLine_ValuesArray: string;
    function GenerateOneAppend_Singleline: string;
  protected
    function GenerateLine_FieldDefAdd(fld: TField): string;
    function GenerateFieldByName(
      fld: TField;
      out line: string): boolean;
    function GenerateStructure: string;
    function GenerateOneAppend: string;
    function GenerateAppendsBlock: string;
    function FormatLongStringLiteral(
      const Literal: string;
      fistLineStartAt: integer): string;
    function GenerateUnitHeader: string;
    function GenerateUnitFooter: string;
    function GenerateFunction: string;
    function GenerateAll(aMode: TGeneratorMode): string;
    class function GenetateUnit(
      ds: TDataSet;
      const aUnitName: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    class function GenerateAsString(ds: TDataSet): string;
    class function GenerateAsArray(ds: TDataSet): TArray<String>;
    class procedure GenerateAndSaveToStream(
      ds: TDataSet;
      aStream: TStream);
    class procedure GenerateAndSaveToFile(
      ds: TDataSet;
      const aFileName: string);
    class procedure GenerateAndSaveClipboard(ds: TDataSet);
  published
    property DataSet: TDataSet read fDataSet write fDataSet;
    property Code: TStrings read fCode;
    property IndentationText: String read fIndentationText
      write fIndentationText;
    property GeneratorMode: TGeneratorMode read fGeneratorMode
      write fGeneratorMode;
    property DataSetType: TDataSetType read fDataSetType write fDataSetType;
    property AppendMode: TAppendMode read fAppendMode write fAppendMode;
    property NameOfUnit: string read fNameOfUnit write fNameOfUnit;
    property MaxRows: integer read fMaxRows write fMaxRows;
    property RightMargin: integer read fRightMargin write fRightMargin;
  end;

  TTextWrapper = class
    class function WrapTextWholeWords(
      const aText: string;
      aMaxWidth: integer): TArray<string>;
  end;

implementation

uses
  System.StrUtils,
  System.Rtti,
  Vcl.Clipbrd;

constructor TDSGenerator.Create(AOwner: TComponent);
begin
  inherited;
  // --------------------------------
  // Default options
  fGeneratorMode := genFunction;
  fDataSetType := dstFDMemTable;
  fAppendMode := amMultilineAppends;
  fIndentationText := '  ';
  fNameOfUnit := 'uSampleDataSet';
  fMaxRows := 100;
  fRightMargin := 76;
  // --------------------------------
  fCode := TStringList.Create;
end;

destructor TDSGenerator.Destroy;
begin
  fCode.Free;
  inherited;
end;

function FieldTypeToString(ft: TFieldType): string;
begin
  Result := System.Rtti.TRttiEnumerationType.GetName(ft);
end;

function TDSGenerator.GetDataFieldPrecision(fld: TField): integer;
begin
  System.Assert((fld is TBCDField) or (fld is TFMTBCDField) or
    (fld is TFloatField));
  if fld is TBCDField then
    Result := (fld as TBCDField).Precision
  else if fld is TFMTBCDField then
    Result := (fld as TFMTBCDField).Precision
  else
    Result := (fld as TFloatField).Precision
end;

function TDSGenerator.GenerateLine_FieldDefAdd(fld: TField): string;
begin
  (* -----------------------------------------------------------------------
    [Doc]
    TFieldType = ( ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd,
    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval,
    ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream,
    ftTimeStampOffset, ftObject, ftSingle);
    ------------------------------------------------------------------------- *)
  if fld.DataType in [ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint,
    ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime] then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ');'
  else if (fld.DataType in [ftBCD, ftFMTBcd]) then
    Result := 'with FieldDefs.AddFieldDef do begin' + sLineBreak +
      fIndentationText + '    ' +
      Format('Name := ''%s'';  DataType := %s;  Precision := %d;  Size := %d;',
      [fld.FieldName, FieldTypeToString(fld.DataType),
      GetDataFieldPrecision(fld), fld.Size]) + sLineBreak + fIndentationText
      + '  end;'
  else if (fld.DataType in [ftString, ftWideString]) and (fld.Size > 9999) then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', 100);'
  else if (fld.DataType in [ftString, ftWideString]) then
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', ' + fld.Size.ToString + ');'
  else
    Result := 'FieldDefs.Add(' + QuotedStr(fld.FieldName) + ', ' +
      FieldTypeToString(fld.DataType) + ', ' + fld.Size.ToString + ');';
end;

function FloatToCode(val: Extended): string;
begin
  Result := FloatToStr(val);
  Result := StringReplace(Result, ',', '.', []);
end;

function DateToCode(dt: TDateTime): string;
var
  y, m, d: word;
begin
  DecodeDate(dt, y, m, d);
  Result := Format('EncodeDate(%d,%d,%d)', [y, m, d]);
end;

function TimeToCode(dt: TDateTime): string;
var
  h, min, s, ms: word;
begin
  DecodeTime(dt, h, min, s, ms);
  Result := Format('EncodeTime(%d,%d,%d,%d)', [h, min, s, ms]);
end;

function DateTimeToCode(dt: TDateTime): string;
begin
  Result := DateToCode(dt);
  if Frac(dt) > 0 then
    Result := Result + '+' + TimeToCode(dt);
end;

function TDSGenerator.FormatLongStringLiteral(
  const Literal: string;
  fistLineStartAt: integer): string;
var
  s: string;
  lines: TArray<string>;
  i: integer;
begin
  s := QuotedStr(Literal);
  if fistLineStartAt + Length(s) <= RightMargin then
  begin
    Result := QuotedStr(Literal);
  end
  else
  begin
    lines := TTextWrapper.WrapTextWholeWords(s,
      RightMargin - 2 * Length(fIndentationText) - 1);
    Result := sLineBreak;
    for i := 0 to High(lines) do
      Result := Result + fIndentationText + fIndentationText +
        IfThen(i > 0, '''') + lines[i] + IfThen(i < High(lines),
        '''+' + sLineBreak, '');
  end
end;

function TDSGenerator.GenerateFieldByName(
  fld: TField;
  out line: string): boolean;
var
  linePattern: string;
  value: string;
begin
  Result := False;
  line := '';
  if fld.IsNull then
    exit;
  linePattern := fIndentationText + 'ds.FieldByName(' + QuotedStr(fld.FieldName)
    + ').Value := %s;';
  case fld.DataType of
    ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
      value := fld.AsString;
    ftBoolean:
      value := BoolToStr(fld.AsBoolean, true);
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      value := FloatToCode(fld.AsExtended);
    ftDate:
      value := DateToCode(fld.AsDateTime);
    ftTime:
      value := TimeToCode(fld.AsDateTime);
    ftDateTime:
      value := DateTimeToCode(fld.AsDateTime);
    ftString, ftWideString:
      value := FormatLongStringLiteral(fld.value, Length(linePattern) - 2);
  else
    value := '';
  end;
  if value = '' then
    exit;
  line := Format(linePattern, [value]);
  Result := true;
end;

function TDSGenerator.GenerateStructure: string;
var
  fld: TField;
  sDataSetCreate: string;
  sFieldDefinitions: string;
begin
  case fDataSetType of
    dstFDMemTable:
      sDataSetCreate := 'TFDMemTable.Create(AOwner)';
    dstClientDataSet:
      sDataSetCreate := 'TClientDataSet.Create(AOwner)';
  end;
  sFieldDefinitions := '';
  if fDataSet <> nil then
    for fld in fDataSet.Fields do
      sFieldDefinitions := sFieldDefinitions +
      { } fIndentationText + fIndentationText + GenerateLine_FieldDefAdd(fld) +
        sLineBreak;
  Result :=
  { } fIndentationText + 'ds := ' + sDataSetCreate + ';' + sLineBreak +
  { } fIndentationText + 'with ds do' + sLineBreak +
  { } fIndentationText + 'begin' + sLineBreak +
  { } sFieldDefinitions +
  { } fIndentationText + fIndentationText + 'CreateDataSet;' + sLineBreak +
  { } fIndentationText + 'end;' + sLineBreak
end;

function TDSGenerator.GenerateOneAppend_Multiline: string;
var
  fld: TField;
  line: string;
  sl: TStringList;
begin
  if (fDataSet = nil) or (fDataSet.Fields.Count = 0) then
    exit('');
  sl := TStringList.Create;
  try
    sl.Add(fIndentationText + 'ds.Append;');
    for fld in fDataSet.Fields do
    begin
      if GenerateFieldByName(fld, line) then
        sl.Add(line);
    end;
    sl.Add(fIndentationText + 'ds.Post;');
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TDSGenerator.GenerateSingleLine_ValuesArray: string;
var
  fld: TField;
  value: string;
begin
  Result := '';
  for fld in fDataSet.Fields do
  begin
    if fld.IsNull then
      value := 'Null'
    else
      case fld.DataType of
        ftAutoInc, ftInteger, ftWord, ftSmallint, ftLargeint:
          value := fld.AsString;
        ftBoolean:
          value := BoolToStr(fld.AsBoolean, true);
        ftFloat, ftCurrency, ftBCD, ftFMTBcd:
          value := FloatToCode(fld.AsExtended);
        ftDate:
          value := DateToCode(fld.AsDateTime);
        ftTime:
          value := TimeToCode(fld.AsDateTime);
        ftDateTime:
          value := DateTimeToCode(fld.AsDateTime);
        ftString, ftWideString:
          value := QuotedStr(fld.value);
      else
        value := 'Null'
      end;
    Result := IfThen(Result = '', value, Result + ', ' + value);
  end;
  Result := '[' + Result + ']';
end;

function TDSGenerator.GenerateOneAppend_Singleline: string;
begin
  if (fDataSet <> nil) and (fDataSet.Fields.Count > 0) then
    Result := fIndentationText + 'ds.AppendRecord(' +
      GenerateSingleLine_ValuesArray() + ');' + sLineBreak
  else
    Result := '';
end;

function TDSGenerator.GenerateOneAppend: string;
begin
  case fAppendMode of
    amMultilineAppends:
      Result := GenerateOneAppend_Multiline;
    amSinglelineAppends:
      Result := GenerateOneAppend_Singleline;
  else
    Result := '';
  end;
end;

function TDSGenerator.GenerateAppendsBlock: string;
var
  sDataAppend: string;
  aBookmark: TBookmark;
  aRowCounter: integer;
  sValuesArray: string;
begin
  if (fDataSet = nil) or (fDataSet.Fields.Count = 0) then
    exit('');
  if fMaxRows = 0 then
    aRowCounter := MaxInt
  else
    aRowCounter := fMaxRows;
  sDataAppend := '';
  if DataSet <> nil then
  begin
    DataSet.DisableControls;
    try
      DataSet.Active := true;
      aBookmark := DataSet.GetBookmark;
      try
        DataSet.First;
        if fAppendMode = amAppendRows then
        begin
          sDataAppend := fIndentationText + 'ds.AppendRows([' + sLineBreak;
          while not DataSet.Eof and (aRowCounter > 0) do
          begin
            sValuesArray := GenerateSingleLine_ValuesArray();
            DataSet.Next;
            sDataAppend := sDataAppend + fIndentationText + fIndentationText +
              sValuesArray + IfThen(not DataSet.Eof, ',') + sLineBreak;
            dec(aRowCounter);
          end;
          sDataAppend := sDataAppend + fIndentationText + ']);' + sLineBreak;
        end
        else
        begin
          while not DataSet.Eof and (aRowCounter > 0) do
          begin
            sDataAppend := sDataAppend + GenerateOneAppend;
            dec(aRowCounter);
            DataSet.Next;
          end;
        end;
      finally
        DataSet.GotoBookmark(aBookmark);
        DataSet.FreeBookmark(aBookmark);
      end;
    finally
      DataSet.EnableControls;
    end;
  end;

  Result :=
  { } sDataAppend +
  { } fIndentationText + 'ds.First;' + sLineBreak;
end;

function TDSGenerator.GenerateUnitHeader: string;
var
  sDataSetUnits: string;
begin
  case fDataSetType of
    dstFDMemTable:
      sDataSetUnits := fIndentationText + 'FireDAC.Comp.Client;';
    dstClientDataSet:
      sDataSetUnits :=
      { } fIndentationText + 'Datasnap.DBClient;'#13#10 +
      { } fIndentationText + 'MidasLib;';
  end;
  Result :=
  { } 'unit ' + fNameOfUnit + ';' + sLineBreak +
  { } sLineBreak +
  { } 'interface' + sLineBreak +
  { } sLineBreak +
  { } 'uses' + sLineBreak +
  { } fIndentationText + 'System.Classes,' + sLineBreak +
  { } fIndentationText + 'System.SysUtils,' + sLineBreak +
  { } fIndentationText + 'System.Variants,' + sLineBreak +
  { } fIndentationText + 'Data.DB,' + sLineBreak +
  { } sDataSetUnits + sLineBreak +
  { } sLineBreak +
  { } 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  { } sLineBreak +
  { } 'implementation' + sLineBreak +
  { } sLineBreak;
end;

function TDSGenerator.GenerateFunction: string;
var
  aClassName: string;
begin
  case fDataSetType of
    dstFDMemTable:
      aClassName := 'TFDMemTable';
    dstClientDataSet:
      aClassName := 'TClientDataSet';
  end;
  Result :=
  { } 'function GivenDataSet (aOwner: TComponent): TDataSet;' + sLineBreak +
  { } 'var' + sLineBreak +
  { } '  ds: ' + aClassName + ';' + sLineBreak +
  { } 'begin' + sLineBreak +
  { } GenerateStructure() +
  { } GenerateAppendsBlock() +
  { } '  Result := ds;' + sLineBreak +
  { } 'end;' + sLineBreak;
end;

function TDSGenerator.GenerateUnitFooter(): string;
begin
  Result := sLineBreak + 'end.' + sLineBreak;
end;

function TDSGenerator.GenerateAll(aMode: TGeneratorMode): string;
begin
  case aMode of
    genStructure:
      Result := GenerateStructure();
    genAppend:
      Result := GenerateAppendsBlock();
    genUnit:
      Result := GenerateUnitHeader + GenerateFunction + GenerateUnitFooter;
    genFunction:
      Result := GenerateFunction;
  else
    Result := '// Unsupported generator mode';
  end;
end;

procedure TDSGenerator.Execute;
begin
  fCode.Text := GenerateAll(fGeneratorMode);
end;

class function TDSGenerator.GenerateAsString(ds: TDataSet): string;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := gen.Code.Text;
  finally
    gen.Free;
  end;
end;

function TStringsToArray(sl: TStrings): TArray<String>;
var
  i: integer;
begin
  SetLength(Result, sl.Count);
  for i := 0 to sl.Count - 1 do
    Result[i] := sl[i];
end;

class function TDSGenerator.GenerateAsArray(ds: TDataSet): TArray<String>;
var
  gen: TDSGenerator;
begin
  gen := TDSGenerator.Create(nil);
  try
    gen.DataSet := ds;
    gen.Execute;
    Result := TStringsToArray(gen.Code);
  finally
    gen.Free;
  end;
end;

class function TDSGenerator.GenetateUnit(
  ds: TDataSet;
  const aUnitName: string): string;
var
  aGenerator: TDSGenerator;
begin
  aGenerator := TDSGenerator.Create(nil);
  try
    aGenerator.DataSet := ds;
    aGenerator.NameOfUnit := aUnitName;
    Result := aGenerator.GenerateAll(genUnit);
  finally
    aGenerator.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveToStream(
  ds: TDataSet;
  aStream: TStream);
var
  sCode: Utf8String;
begin
  sCode := Utf8String(GenetateUnit(ds, 'uSampleDataSet'));
  aStream.Write(sCode[1], Length(sCode));
end;

function GetUnitName_FromFilePath(aFilePath: string): string;
var
  aUnitName: string;
  aExtentionLen: integer;
begin
  aUnitName := ExtractFileName(aFilePath);
  aExtentionLen := Length(ExtractFileExt(aFilePath));
  if aExtentionLen > 0 then
    Result := aUnitName.Substring(0, aUnitName.Length - aExtentionLen)
  else
    Result := aUnitName;
end;

class procedure TDSGenerator.GenerateAndSaveToFile(
  ds: TDataSet;
  const aFileName: string);
var
  fs: TFileStream;
  sCode: Utf8String;
  aUnitName: string;
begin
  aUnitName := GetUnitName_FromFilePath(aFileName);
  sCode := Utf8String(GenetateUnit(ds, aUnitName));
  fs := TFileStream.Create(aFileName, fmCreate);
  try
    {
      aFilePreamble := TEncoding.UTF8.GetPreamble;
      aStream.Write(aFilePreamble[0], Length(aFilePreamble));
    }
    fs.Write(sCode[1], Length(sCode));
  finally
    fs.Free;
  end;
end;

class procedure TDSGenerator.GenerateAndSaveClipboard(ds: TDataSet);
var
  aGenerator: TDSGenerator;
begin
  aGenerator := TDSGenerator.Create(nil);
  try
    aGenerator.DataSet := ds;
    Clipboard.AsText := aGenerator.GenerateFunction;
  finally
    aGenerator.Free;
  end;
end;

{ TTextWrapper }

class function TTextWrapper.WrapTextWholeWords(
  const aText: string;
  aMaxWidth: integer): TArray<string>;
var
  i: integer;
  j: integer;
  Count: integer;
begin
  i := 0;
  j := aMaxWidth;
  Count := 0;
  Result := [];
  while j < aText.Length do
  begin
    while (j > i) and not(CharInSet(aText[j], [' ', '.', ',', '!', '?', ':',
      ';', '-'])) do
      dec(j);
    if j = i then
      j := i + aMaxWidth;
    SetLength(Result, Count + 1);
    Result[Count] := aText.Substring(i, j - i);
    i := j;
    j := j + aMaxWidth;
    Count := Count + 1;
  end;
  SetLength(Result, Count + 1);
  Result[Count] := aText.Substring(i);
end;

end.
