{
Copyright © 2017 Piotr Chlebek
This software has been based on XorConsole (FANN's example).
It is available under MIT License, see LICENSE.
}
program IrisConsole;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  fann;

var
  ann: PFann;
  inputs: array [0..3] of fann_type;
  output: fann_type;
  calc_out: PFann_Type_array;
  train_data,test_data: PFann_Train_Data;
  path50:  AnsiString = 'data/bezdekIris.fann-1to1.test50.data';
  path100: AnsiString = 'data/bezdekIris.fann-1to1.train100.data';
  path150: AnsiString = 'data/bezdekIris.fann-1to1.data';
  tf: TextFile;
  ndata,nin,nout,nerr,n : Integer;
  mserr : Single;
begin
  ann := fann_create_standard(3,4,2,1); { create NN,
    parameters:
    - number of layers (including input & outupt),
    - number of inputs
    - number of hidden neurons in 2nd layer (1st hidden)
    - number of outputs}

  train_data := fann_read_train_from_file(PChar(path100)); // read training data
  test_data  := fann_read_train_from_file(PChar(path50)); // read testing data

  fann_set_activation_steepness_hidden(ann, 0.5); // sets the steepness of the activation steepness for all neurons in the all hidden layers
  fann_set_activation_steepness_output(ann, 0.5); // sets the steepness of the activation steepness in the output layer

  fann_set_activation_function_hidden(ann, FANN_SIGMOID); // sets the activation function for all of the hidden layers
  fann_set_activation_function_output(ann, FANN_LINEAR); // sets the activation function for the output layer

  fann_set_train_stop_function(ann, FANN_STOPFUNC_MSE); // sets the stop function used during training
  fann_set_bit_fail_limit(ann, 0.5); // set the bit fail limit used during training

  fann_init_weights(ann, train_data); // initialize the weights

  fann_train_on_data(ann, train_data, 500, 50, 0.001); { trains on an entire dataset for a period of time,
    parameters:
    - NN
    - data
    - max epochs
    - epochs between reports
    - desired error}

  // my test
  AssignFile(tf, path50);
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
      output := 0.0;
      ReadLn(tf, inputs[0], inputs[1], inputs[2], inputs[3], output);
      Inc(n);

      calc_out:= fann_run(ann, @inputs[0]);
      mserr := mserr + Sqr(output - Calc_Out[0]);
      if (Round(Calc_Out[0]) <> output) then
        Inc(nerr);
      //WriteLn(Format('%f %f %f %f => %f / %f',[inputs[0],inputs[1],inputs[2],inputs[3],Calc_Out[0],output]));
    end;
  CloseFile(tf);
  if (n > 0) then
    mserr := mserr / n;
  WriteLn(Format('n=%d  Mistakes=%d  MSE=%.6f',[n,nerr,mserr]));

  // fann test
  mserr := fann_test_data(ann, test_data);
  WriteLn(Format('FANN: Mistakes=%d  MSE=%.6f',[fann_get_bit_fail(ann), mserr]));

  fann_destroy(ann);
  ReadLn;
end.
