# IrisConsole
Neural Network example with Delphi + FANN (Fast Artificial Neural Network Library) under MS Windows.
Solves Iris flowers classification problem https://archive.ics.uci.edu/ml/datasets/iris (Fisher, 1936).
This has been solved with two methods
1. Neural network classification (softmax output layer with 3 neurons).
1. Neural network regression (output layer with 1 neuron).
Iris data set is included. Normalization has been implemented.
All 6 permutations of three output labels has been tried, because it may leads into interesting conclusion.

Requires:
* Delphi® 10.2 Tokyo Starter Edition https://www.embarcadero.com/products/delphi/starter/info
* Delphi FANN 2.1 (actually fann_delphi_2_0) http://leenissen.dk/fann/wp/language-bindings/

Properly compiled and run, gives similar output (very fast):
<pre>
Loading data...
ndata=150  nin=4  nout=1
Normalizing data...
4.300 .. 7.900
2.000 .. 4.400
1.000 .. 6.900
0.100 .. 2.500
1.000 .. 3.000
Splitting data...
Saving train data...
Saving test data...
Saving train data...
Saving test data...
=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3162247467. Bit fail 200.
Epochs           10. Current error: 0.1163726298. Bit fail 48.
Epochs           20. Current error: 0.0304054260. Bit fail 10.
Epochs           30. Current error: 0.0133458360. Bit fail 7.
Epochs           40. Current error: 0.0117748332. Bit fail 5.
Epochs           50. Current error: 0.0113211830. Bit fail 5.
--- FANN Tests ---
train: Misclassified=5  MSE=0.011267
test:  Misclassified=0  MSE=0.003530
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=0  MSE=0.003530

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 3.7706094360. Bit fail 100.
Epochs           10. Current error: 0.1516188526. Bit fail 33.
Epochs           20. Current error: 0.0717949104. Bit fail 6.
Epochs           30. Current error: 0.0490389156. Bit fail 5.
Epochs           40. Current error: 0.0405731583. Bit fail 3.
Epochs           50. Current error: 0.0370667815. Bit fail 3.
--- FANN Tests ---
train: Misclassified=3  MSE=0.036709
test:  Misclassified=0  MSE=0.026188
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=0  MSE=0.026188
Splitting data...
Saving train data...
Saving test data...
Saving train data...
Saving test data...
=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3379011790. Bit fail 233.
Epochs           10. Current error: 0.1204369990. Bit fail 50.
Epochs           20. Current error: 0.0289114920. Bit fail 5.
Epochs           28. Current error: 0.0047730533. Bit fail 1.
--- FANN Tests ---
train: Misclassified=1  MSE=0.003990
test:  Misclassified=10  MSE=0.049338
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=5  MSE=0.049338

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 4.1915731812. Bit fail 100.
Epochs           10. Current error: 0.2792019081. Bit fail 60.
Epochs           20. Current error: 0.2326420784. Bit fail 58.
Epochs           30. Current error: 0.1999127007. Bit fail 58.
Epochs           40. Current error: 0.1763159370. Bit fail 34.
Epochs           50. Current error: 0.1746868896. Bit fail 34.
--- FANN Tests ---
train: Misclassified=34  MSE=0.174678
test:  Misclassified=21  MSE=0.181352
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=21  MSE=0.181352
Splitting data...
Saving train data...
Saving test data...
Saving train data...
Saving test data...
=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3196280416. Bit fail 199.
Epochs           10. Current error: 0.1324040476. Bit fail 51.
Epochs           20. Current error: 0.0569751740. Bit fail 18.
Epochs           30. Current error: 0.0169747289. Bit fail 4.
Epochs           40. Current error: 0.0079245504. Bit fail 2.
Epochs           50. Current error: 0.0056044825. Bit fail 1.
--- FANN Tests ---
train: Misclassified=1  MSE=0.005440
test:  Misclassified=3  MSE=0.017434
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=2  MSE=0.017434

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 4.1788510132. Bit fail 100.
Epochs           10. Current error: 0.6880570984. Bit fail 67.
Epochs           20. Current error: 0.6039183044. Bit fail 67.
Epochs           30. Current error: 0.6060232925. Bit fail 67.
Epochs           40. Current error: 0.6048179245. Bit fail 67.
Epochs           50. Current error: 0.5993844604. Bit fail 67.
--- FANN Tests ---
train: Misclassified=67  MSE=0.599372
test:  Misclassified=33  MSE=0.570940
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=33  MSE=0.570940
Splitting data...
Saving train data...
Saving test data...
Saving train data...
Saving test data...
=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.2871732330. Bit fail 189.
Epochs           10. Current error: 0.1360199992. Bit fail 48.
Epochs           20. Current error: 0.0793880208. Bit fail 29.
Epochs           30. Current error: 0.0347217909. Bit fail 11.
Epochs           40. Current error: 0.0183730300. Bit fail 5.
Epochs           50. Current error: 0.0138545497. Bit fail 4.
--- FANN Tests ---
train: Misclassified=4  MSE=0.013491
test:  Misclassified=0  MSE=0.003047
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=0  MSE=0.003047

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 4.3670419312. Bit fail 100.
Epochs           10. Current error: 0.6605114746. Bit fail 74.
Epochs           20. Current error: 0.3216550446. Bit fail 56.
Epochs           30. Current error: 0.1685404968. Bit fail 24.
Epochs           40. Current error: 0.1070874310. Bit fail 13.
Epochs           50. Current error: 0.0625552416. Bit fail 6.
--- FANN Tests ---
train: Misclassified=8  MSE=0.063509
test:  Misclassified=2  MSE=0.036585
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=2  MSE=0.036585
Splitting data...
Saving train data...
Saving test data...
Saving train data...
Saving test data...
=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3175901286. Bit fail 142.
Epochs           10. Current error: 0.1529755910. Bit fail 47.
Epochs           20. Current error: 0.0697018305. Bit fail 24.
Epochs           30. Current error: 0.0255469481. Bit fail 6.
Epochs           40. Current error: 0.0111798628. Bit fail 7.
Epochs           50. Current error: 0.0098907240. Bit fail 5.
--- FANN Tests ---
train: Misclassified=4  MSE=0.010061
test:  Misclassified=6  MSE=0.025750
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=3  MSE=0.025750

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 4.0908453369. Bit fail 100.
Epochs           10. Current error: 0.6236494064. Bit fail 45.
Epochs           20. Current error: 0.5814595795. Bit fail 67.
Epochs           30. Current error: 0.4386654282. Bit fail 69.
Epochs           40. Current error: 0.2856497002. Bit fail 44.
Epochs           50. Current error: 0.1734809875. Bit fail 21.
--- FANN Tests ---
train: Misclassified=19  MSE=0.172083
test:  Misclassified=9  MSE=0.186759
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=9  MSE=0.186759
Splitting data...
Saving train data...
Saving test data...
Saving train data...
Saving test data...
=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.2835349019. Bit fail 225.
Epochs           10. Current error: 0.1107042185. Bit fail 51.
Epochs           20. Current error: 0.0423811436. Bit fail 16.
Epochs           30. Current error: 0.0160498492. Bit fail 7.
Epochs           40. Current error: 0.0107561262. Bit fail 4.
Epochs           50. Current error: 0.0087175600. Bit fail 4.
--- FANN Tests ---
train: Misclassified=4  MSE=0.008598
test:  Misclassified=7  MSE=0.035395
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=4  MSE=0.035395

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 3.2732696533. Bit fail 95.
Epochs           10. Current error: 0.1618586922. Bit fail 15.
Epochs           20. Current error: 0.0363282108. Bit fail 4.
Epochs           30. Current error: 0.0347124624. Bit fail 4.
Epochs           40. Current error: 0.0327482677. Bit fail 3.
Epochs           50. Current error: 0.0301110935. Bit fail 3.
--- FANN Tests ---
train: Misclassified=2  MSE=0.029778
test:  Misclassified=1  MSE=0.032981
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=1  MSE=0.032981

=== Summary (after 1 repetitions) ===
Permutation: 0
Average Misclassified Cla.: 0.000
Average Misclassified Reg.: 0.000
Permutation: 1
Average Misclassified Cla.: 5.000
Average Misclassified Reg.: 21.000
Permutation: 2
Average Misclassified Cla.: 2.000
Average Misclassified Reg.: 33.000
Permutation: 3
Average Misclassified Cla.: 0.000
Average Misclassified Reg.: 2.000
Permutation: 4
Average Misclassified Cla.: 3.000
Average Misclassified Reg.: 9.000
Permutation: 5
Average Misclassified Cla.: 4.000
Average Misclassified Reg.: 1.000
</pre>

Changing LOOP_TIMES into 1000 may give you better overview (very slow):
<pre>
=== Summary (after 1000 repetitions) ===
Permutation: 0
Average Misclassified Cla.: 2.556
Average Misclassified Reg.: 2.050
Permutation: 1
Average Misclassified Cla.: 2.525
Average Misclassified Reg.: 12.149
Permutation: 2
Average Misclassified Cla.: 2.502
Average Misclassified Reg.: 32.618
Permutation: 3
Average Misclassified Cla.: 2.506
Average Misclassified Reg.: 4.501
Permutation: 4
Average Misclassified Cla.: 2.492
Average Misclassified Reg.: 20.238
Permutation: 5
Average Misclassified Cla.: 2.440
Average Misclassified Reg.: 1.842
</pre>

Enjoy my other Machine Learning software:
<a href="http://sharktime.com/us_SharkyNeuralNetwork.html">Sharky Neural Network - Classification neural network in action</a>.

Copyright © 2017-2018 Piotr Chlebek
http://sharktime.com/
