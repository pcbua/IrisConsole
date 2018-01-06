{
Copyright © 2017-2018 Piotr Chlebek
http://sharktime.com/

This software has been based on XorConsole (FANN's example).
It is available under MIT License, see LICENSE.

Links:
- this software home: https://github.com/pcbua/IrisConsole/
- FANN: http://leenissen.dk/fann/
- my other Machine Learning software: Sharky Neural Network - Classification neural network in action: http://sharktime.com/us_SharkyNeuralNetwork.html
}
program IrisConsole;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Math,
  fann,
  data in 'units/data.pas',
  defs in 'units/defs.pas',
  func in 'units/func.pas';

function NNetCreate(const hidden_layers, inputs, hidden_layer_size, outputs, activation_func_hidden, activation_func_output: Integer): PFann;
  procedure printMany(const str: string; const times: Integer);
  var i: Integer;
  begin
    if (times <= 10) then
      for i:=1 to times do Write(str)
    else
      Write(Format('%s .. %s (%d)',[Trim(str), Trim(str), times]));
    WriteLn;
  end;
var n,layers: Integer;
begin
  Result := nil;
    try
      n := hidden_layer_size; // just to have shorter lines below
      layers := hidden_layers + 2;
        case hidden_layers of
          0: Result := fann_create_standard(layers, inputs, outputs);
          1: Result := fann_create_standard(layers, inputs, n, outputs);
          2: Result := fann_create_standard(layers, inputs, n,n, outputs);
          3: Result := fann_create_standard(layers, inputs, n,n,n, outputs);
          4: Result := fann_create_standard(layers, inputs, n,n,n,n, outputs);
        end;
      if Assigned(Result) then
        begin
          // set training algorithm
          fann_set_training_algorithm(Result, FANN_TRAIN_RPROP); // FANN_TRAIN_RPROP, FANN_TRAIN_QUICKPROP, FANN_TRAIN_BATCH, FANN_TRAIN_INCREMENTAL
          // set error function
          fann_set_train_error_function(Result, FANN_ERRORFUNC_LINEAR); // FANN_ERRORFUNC_TANH, FANN_ERRORFUNC_LINEAR
          // set the activation function
          fann_set_activation_function_hidden(Result, activation_func_hidden); // for each neuron of the hidden layers
          fann_set_activation_function_output(Result, activation_func_output); // for each neuron of the output layer
          // set the activation steepness
          fann_set_activation_steepness_hidden(Result, 0.5); // for each neuron of all hidden layers
          fann_set_activation_steepness_output(Result, 0.5); // for each neuron of the output layer
          // set the stop function used during training
          fann_set_train_stop_function(Result, FANN_STOPFUNC_MSE);
          // set the bit fail limit used during training
          fann_set_bit_fail_limit(Result, 0.5);

          // 'draw' network
          WriteLn('--- Network Structure ---');
          printMany('OUT ', outputs);
          for layers:=1 to hidden_layers do
            printMany('O  ', hidden_layer_size);
          printMany('IN ', inputs);
        end;
    except
      Result := nil;
    end;
  if not Assigned(Result) then
    WriteLn('Error! Can''t create neural network.');
end;

function NNetManualTest(pnn: PFann; const path: string; const problem: Integer): Integer;
// returns number of misclassifed test examples
var
  inputs:  array [0..NUMBER_OF_INPUTS-1] of fann_type;
  outputs: array [0..NUMBER_OF_CLASSES-1] of fann_type;
  calc_out: PFann_Type_array; // network calculated output
  tf: TextFile;
  iorig,i1,i2,
  ndata,nin,nout,nerr,n: Integer;
  mserr: Single;
  s: string;
begin
  // yes I know, that simmilar test can be done with two lines, but look
  // fann_test_data() seems to have different misclassification heuristic criteria
  Result := 0;
  if not Assigned(pnn) or not FileExists(path) then
    Exit;

  AssignFile(tf, path);
  Reset(tf);
  ndata := 0; // number of input data
  nin   := 0; // number of inputs
  nout  := 0; // number of outputs
  nerr  := 0; // number of errors
  mserr := 0.0; // Mean squared error
  if not EOF(tf) then
    ReadLn(tf, ndata, nin, nout);
  WriteLn(Format('ndata=%d  nin=%d  nout=%d', [ndata, nin, nout]));
  n := 0; // count data points
  while not EOF(tf) and (n < ndata) do
    begin
      FillChar(inputs, SizeOf(inputs), 0);
      FillChar(outputs, SizeOf(outputs), 0);
        case problem of
          PROBLEM_CLASSIFICATION: ReadLn(tf, inputs[0], inputs[1], inputs[2], inputs[3], outputs[0], outputs[1], outputs[2]);
          PROBLEM_REGRESSION    : ReadLn(tf, inputs[0], inputs[1], inputs[2], inputs[3], outputs[0]);
        end;
      Inc(n);
      // compute result
      calc_out:= fann_run(pnn, @inputs[0]);
      s := ''; // label, for making log more readable (if not commented below)
        case problem of
          PROBLEM_CLASSIFICATION:
            begin
              mserr := mserr + (Sqr(outputs[0] - Calc_Out[0]) + Sqr(outputs[1] - Calc_Out[1]) + Sqr(outputs[2] - Calc_Out[2])) / 3.0;
              // check result (some parameters below are heuristic)
              iorig := IndexOfHighestOutput(outputs);
              i1 := IndexOfHighestOutput(calc_out^, NUMBER_OF_CLASSES);
              if (i1 <> iorig) then
                begin
                  s := s + 'Wrong:';
                  Inc(nerr);
                end;
              { // DEBUG:
              i2 := IndexOfSecondHighestOutput(calc_out^, NUMBER_OF_CLASSES);
              if (calc_out[i1] < 0.5) then s := s + 'Small:';
              if (calc_out[i2] > 0.25) then s := s + 'Big:';
              if (s = '') then s := 'OK:';
              WriteLn(Format('%s %f %f %f %f => %f %f %f',[s, inputs[0],inputs[1],inputs[2],inputs[3], Calc_Out[0],Calc_Out[1],Calc_Out[2]]));
              }
            end;
          PROBLEM_REGRESSION:
            begin
              mserr := mserr + Sqr(outputs[0] - Calc_Out[0]);
              if (outputs[0] <> Round(Calc_Out[0])) then
                begin
                  s := s + 'Wrong:';
                  Inc(nerr);
                end;
              { // DEBUG:
              if (Abs(outputs[0] - Calc_Out[0]) > 0.25) then s := s + 'Far:';
              if (s = '') then s := 'OK:';
              WriteLn(Format('%s %f %f %f %f => %f',[s, inputs[0],inputs[1],inputs[2],inputs[3], Calc_Out[0]]));
              }
            end;
        end;
    end;
  CloseFile(tf);
  if (n > 0) then
    mserr := mserr / n;
  WriteLn(Format('n=%d   Misclassified=%d  MSE=%.6f',[n,nerr,mserr]));
  Result := nerr;
end;

function NNetTrainAndTest(pnn: PFann; const pathTrain,pathTest: string; const problem: Integer): Integer;
// returns number of misclassifed test examples
var
  train_data,test_data: PFann_Train_Data;
  mserr: Single;
begin
  Result := 0;
  if not Assigned(pnn) or not FileExists(pathTrain) or not FileExists(pathTest) then
    Exit;

  // load data
  train_data := fann_read_train_from_file( PWideChar(AnsiString(pathTrain)) );
  test_data  := fann_read_train_from_file( PWideChar(AnsiString(pathTest)) );

  // train
  WriteLn('--- Train ---');
  fann_init_weights(pnn, train_data); // initialize the weights
  fann_train_on_data(pnn, train_data,
    50,   // max epochs
    10,   // epochs between reports
    0.005  // desired error
  );

  // test
  WriteLn('--- FANN Tests ---');
  // fann test on train data
  mserr := fann_test_data(pnn, train_data);
  WriteLn(Format('train: Misclassified=%d  MSE=%.6f',[fann_get_bit_fail(pnn), mserr]));
  // fann test on test data
  mserr := fann_test_data(pnn, test_data);
  WriteLn(Format('test:  Misclassified=%d  MSE=%.6f',[fann_get_bit_fail(pnn), mserr]));
  // Result := fann_get_bit_fail(pnn);
  // manual test on test data
  WriteLn('--- Manual Tests ---');
  Result := NNetManualTest(pnn, pathTest, problem);
end;

// for C/C++ masters: yes, code block below is kind of main() function
var
  nnet: PFann;
  rawData,nrmData: TDataArr;
  loop,perm,misclassified: Integer;
  sumCla,sumReg: array[0..5] of Integer;
  strPerm: string;
begin
  Randomize();

  // ========== data preparation ==========
  CreateDir( FOLDER_GENERATED );
  rawData := DataLoad( PATH_ORIG_DATA );
  nrmData := DataNormalize( rawData, -1.0, +1.0 );

  FillChar(sumCla, SizeOf(sumCla), 0);
  FillChar(sumReg, SizeOf(sumReg), 0);
  for perm:=0 to 5 do
    begin
      strPerm := '.perm' + i2s(perm);
      for loop:=1 to LOOP_TIMES do
        begin
          DataSplit( {var}nrmData, 33.3, Random(9999) );
          WriteLn('Saving train data...');
          DataSave( nrmData, PATH_NORM_TRAIN_CLA+strPerm, PATH_NORM_TEST_CLA+strPerm, PROBLEM_CLASSIFICATION, perm );
          DataSave( nrmData, PATH_NORM_TRAIN_REG+strPerm, PATH_NORM_TEST_REG+strPerm, PROBLEM_REGRESSION,     perm );

          // ========== classification ==========
          WriteLn('=== FANN Classification Example ===');
          nnet := NNetCreate(
            HIDDEN_LAYERS,
            NUMBER_OF_INPUTS,
            HIDDEN_LAYER_SIZE,
            NUMBER_OF_CLASSES, // number of outputs
            FANN_SIGMOID,
            FANN_SIGMOID );
            try
              misclassified := NNetTrainAndTest( nnet, PATH_NORM_TRAIN_CLA+strPerm, PATH_NORM_TEST_CLA+strPerm, PROBLEM_CLASSIFICATION );
              Inc(sumCla[perm], misclassified);
            finally
              fann_destroy( nnet );
            end;

          // ========== regression ==========
          WriteLn(EOL + '=== FANN Regression Example ===');
          // I know it is strange, but it is interesting, has less neurons than classification above
          // and for some permutations seems to work better than above classification
          nnet := NNetCreate(
            HIDDEN_LAYERS,
            NUMBER_OF_INPUTS,
            HIDDEN_LAYER_SIZE,
            1, // number of outputs
            FANN_SIGMOID,
            FANN_LINEAR );
            try
              misclassified := NNetTrainAndTest( nnet, PATH_NORM_TRAIN_REG+strPerm, PATH_NORM_TEST_REG+strPerm, PROBLEM_REGRESSION );
              Inc(sumReg[perm], misclassified);
            finally
              fann_destroy( nnet );
            end;
        end; // for loop
    end; // for perm

  WriteLn(EOL + '=== Summary (after '+ i2s(LOOP_TIMES) +' repetitions) ===');
  for perm:=0 to 5 do
    begin
      WriteLn('Permutation: ' + i2s(perm));
      WriteLn('Average Misclassified Cla.: ', f2s(sumCla[perm] / LOOP_TIMES));
      WriteLn('Average Misclassified Reg.: ', f2s(sumReg[perm] / LOOP_TIMES));
    end;
  ReadLn;
end.

