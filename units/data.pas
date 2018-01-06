{
Copyright © 2017-2018 Piotr Chlebek
http://sharktime.com/
}
unit data;

interface

uses defs;

function DataLoad(const path: string): TDataArr;
procedure DataSplit(var d: TDataArr; const testPerc: Single; const seed: Integer);
function DataNormalize(const d: TDataArr; const vmin,vmax: Single): TDataArr;
function DataSave(const d: TDataArr; const pathTrain, pathTest: string; const problem: Integer; const perm: Integer = 0): Boolean;

implementation

uses
  func,
  SysUtils; // for FileExists()

function DataLoad(const path: string): TDataArr;
var
  tf: TextFile;
  ndata,nin,nout,n,i: Integer;
begin
  SetLength(Result, 0);

  WriteLn('Loading data...');
  if not FileExists(path) then
    begin
      WriteLn('Can''t find data file: ' + path);
      Exit;
    end;
  AssignFile(tf, path);
  Reset(tf);
  if not EOF(tf) then
    begin
      ReadLn(tf, ndata, nin, nout);
      WriteLn(Format('ndata=%d  nin=%d  nout=%d', [ndata, nin, nout]));
      n := 0; // count data points
      if (ndata > 0) and (ndata <= MAX_DATA_POINTS) and (nin = NUMBER_OF_INPUTS) and (nout = 1) then
        begin
          SetLength(Result, ndata);
          for i:=0 to ndata-1 do
            FillChar(Result[i], SizeOf(TData), 0);
          while not EOF(tf) and (n < ndata) do
            begin
              ReadLn(tf, Result[n][0], Result[n][1], Result[n][2], Result[n][3], Result[n][4]);
              Inc(n);
            end;
        end;
    end;
  CloseFile(tf);
  if (n <> ndata) then
    WriteLn('Warning! Actual number of data points different than in the file header.');
  if (n <= 0) then
    begin
      WriteLn('Error! No data.');
      Exit;
    end;
end;

procedure DataSplit(var d: TDataArr; const testPerc: Single; const seed: Integer);
var
  n,k,i: Integer;
begin
  // mark test data with TData[-1] := SPLIT_TRAIN, and train data with TData[-1] := SPLIT_TEST
  // below method is slow and naive, feel free to implement better one
  WriteLn('Splitting data...');
  RandSeed := seed;
  n := Length(d);
  k := ForceInRange(Round( testPerc/100 * n ), 0, n); // number of test data points
  // set all to train
  for i:=0 to n-1 do
    d[i][-1] := SPLIT_TRAIN;
  // pick few for test
  while (k > 0) do
    begin
      i := Random(n);
      if (d[i][-1] = SPLIT_TRAIN) then
        begin
          d[i][-1] := SPLIT_TEST;
          Dec(k);
        end;
    end;
end;

function DataNormalize(const d: TDataArr; const vmin,vmax: Single): TDataArr;
var
  i,k: Integer;
  dmax,dmin: TData;
begin
  WriteLn('Normalizing data...');
  // search for max & min
  dmax := d[0];
  dmin := d[0];
  for i:=1 to High(d) do
    for k:=0 to High(TData) do
      begin
        if (dmax[k] < d[i][k]) then
          dmax[k] := d[i][k];
        if (dmin[k] > d[i][k]) then
          dmin[k] := d[i][k];
      end;
  // show data range and check for identical values
  for k:=0 to High(TData) do
    begin
      if (dmax[k] = dmin[k]) then
        begin
          WriteLn('Warning! All values are the same in column '+ i2s(k+1));
          dmax[k] := dmax[k] + 0.1;
          dmin[k] := dmin[k] - 0.1;
        end;
      WriteLn(Format('%.3f .. %.3f', [dmin[k], dmax[k]]));
    end;
  // normalize all inputs
  SetLength(Result, Length(d));
  for i:=0 to High(d) do
    begin
      Result[i] := d[i]; // copy first
      for k:=0 to High(TData)-1 do // normalize inputs, skip split and output
        Result[i][k] := vmin + (vmax-vmin) * (d[i][k] - dmin[k]) / (dmax[k] - dmin[k]);
    end;
end;

function DataSave(const d: TDataArr; const pathTrain, pathTest: string; const problem: Integer; const perm: Integer = 0): Boolean;
  function CountSplit(const v: Single): Integer;
  var i: Integer;
  begin
    Result := 0;
    for i:=0 to High(d) do
      if (d[i][-1] = v) then
        Inc( Result );
  end;
  procedure SaveSplit(const v: Single; const path: string; const perm: Integer = 0);
  var
    tf: TextFile;
    i,k,n,outputs: Integer;
    s: string;
  begin
    n := CountSplit( v );
      case problem of
        PROBLEM_CLASSIFICATION: outputs := NUMBER_OF_CLASSES;
        PROBLEM_REGRESSION    : outputs := 1;
      else
        WriteLn('Unknown kind of the problem!');
        Halt(1);
      end;
    AssignFile(tf, path);
    Rewrite(tf);
    WriteLn(tf, Format('%d %d %d', [n, NUMBER_OF_INPUTS, outputs]));
    for i:=0 to High(d) do
      if (d[i][-1] = v) then
        begin
          // inputs
          s := Format('%.3f %.3f %.3f %.3f', [d[i][0], d[i][1], d[i][2], d[i][3]]);
          // output
          k := Round(d[i][4]);
          Assert(NUMBER_OF_CLASSES = 3); // I know, this code not work for other number of classes
          Assert(k >= 1);
          Assert(k <= NUMBER_OF_CLASSES);
            case perm of
              0: ; // k := doPerm(k, 1, 2, 3); // default, do not permutate
              1: k := doPerm(k, 1, 3, 2);
              2: k := doPerm(k, 2, 1, 3);
              3: k := doPerm(k, 2, 3, 1);
              4: k := doPerm(k, 3, 1, 2);
              5: k := doPerm(k, 3, 2, 1);
            end;
            case problem of
              PROBLEM_CLASSIFICATION:
                case k of
                  1: s := s + ' 1 0 0';
                  2: s := s + ' 0 1 0';
                  3: s := s + ' 0 0 1';
                end;
              PROBLEM_REGRESSION:
                s := s +' '+ i2s(k);
            end;
          WriteLn(tf, s);
        end;
    CloseFile(tf);
  end;
begin
  try
    SaveSplit( SPLIT_TRAIN, pathTrain, perm );
    SaveSplit( SPLIT_TEST, pathTest, perm );
    Result := True; // OK
  except
    Result := False; // Error
  end;
end;

initialization

finalization

end.

