{
Copyright © 2017-2018 Piotr Chlebek
http://sharktime.com/
}
unit defs;

interface

const
  EOL = #13+#10;
  TAB = #9;

  PATH_ORIG_DATA   = 'data/bezdekIris.fann.data';
  FOLDER_GENERATED = 'generated';
  // classification
  PROBLEM_CLASSIFICATION = 0;
  PATH_NORM_TRAIN_CLA    = FOLDER_GENERATED + '/bezdekIris.norm.train.cla.data';
  PATH_NORM_TEST_CLA     = FOLDER_GENERATED + '/bezdekIris.norm.test.cla.data';
  // regression (additionally)
  PROBLEM_REGRESSION     = 1;
  PATH_NORM_TRAIN_REG    = FOLDER_GENERATED + '/bezdekIris.norm.train.reg.data';
  PATH_NORM_TEST_REG     = FOLDER_GENERATED + '/bezdekIris.norm.test.reg.data';

  LOOP_TIMES        = 1; // try values 1-1000
  HIDDEN_LAYERS     = 1; // try values 0-4, this doesn't count input & outupt layer
  HIDDEN_LAYER_SIZE = 3; // try values 1-100, number of neurons in hidden layer
  NUMBER_OF_INPUTS  = 4; // unfortunately, chaning this will require some code parts to be rewritten
  NUMBER_OF_CLASSES = 3; // unfortunately, chaning this will require some code parts to be rewritten
  MAX_DATA_POINTS   = 10000;

  SPLIT_TRAIN = 0;
  SPLIT_TEST  = 1;

type
  TData = array[-1..4] of Single; // -1 - split, 0..3 - inputs, 4 - the output
  TDataArr = array of TData;

implementation

end.
