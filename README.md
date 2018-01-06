# IrisConsole
Neural Network example with Delphi + FANN (Fast Artificial Neural Network Library) under MS Windows.
Solves Iris flowers classification problem https://archive.ics.uci.edu/ml/datasets/iris (Fisher, 1936).
This has been solved with two methods:
1. Neural network classification (output layer with three neurons, one neuron per class).
1. Neural network regression (output layer with single neuron).

Iris data set is included. Normalization has been implemented.
All 6 permutations of three output labels has been tried.

Requires:
* Delphi® 10.2 Tokyo Starter Edition https://www.embarcadero.com/products/delphi/starter/info
* Delphi FANN 2.1 (actually fann_delphi_2_0) http://leenissen.dk/fann/wp/language-bindings/

Properly compiled and run, gives similar output (very fast):
<pre>
Loading data...
ndata=150  nin=4  nout=1
Normalizing data...
 Input 1: 4.300 .. 7.900
 Input 2: 2.000 .. 4.400
 Input 3: 1.000 .. 6.900
 Input 4: 0.100 .. 2.500
Output 1: 1.000 .. 3.000

=== Data Split ===
Splitting data...
Saving data...

=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3182214101. Bit fail 202.
Epochs           10. Current error: 0.1164092636. Bit fail 48.
Epochs           20. Current error: 0.0244682805. Bit fail 7.
Epochs           30. Current error: 0.0109753736. Bit fail 4.
Epochs           40. Current error: 0.0076376963. Bit fail 3.
Epochs           50. Current error: 0.0068053555. Bit fail 4.
--- FANN Tests ---
train: Misclassified=3  MSE=0.006773
test:  Misclassified=4  MSE=0.025488
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=2  MSE=0.025488

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 3.7013278198. Bit fail 100.
Epochs           10. Current error: 0.2315687370. Bit fail 64.
Epochs           20. Current error: 0.0773226929. Bit fail 7.
Epochs           30. Current error: 0.0386998248. Bit fail 3.
Epochs           40. Current error: 0.0345529485. Bit fail 4.
Epochs           50. Current error: 0.0316405392. Bit fail 4.
--- FANN Tests ---
train: Misclassified=4  MSE=0.031393
test:  Misclassified=2  MSE=0.040391
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=2  MSE=0.040391

=== Data Split ===
Splitting data...
Saving data...

=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3138206736. Bit fail 200.
Epochs           10. Current error: 0.1091720835. Bit fail 48.
Epochs           20. Current error: 0.0251638699. Bit fail 9.
Epochs           30. Current error: 0.0103843268. Bit fail 3.
Epochs           40. Current error: 0.0067992187. Bit fail 2.
Epochs           50. Current error: 0.0052709802. Bit fail 2.
--- FANN Tests ---
train: Misclassified=2  MSE=0.005178
test:  Misclassified=4  MSE=0.021294
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=2  MSE=0.021294

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 4.6612078857. Bit fail 100.
Epochs           10. Current error: 0.6345777893. Bit fail 36.
Epochs           20. Current error: 0.2205088806. Bit fail 36.
Epochs           30. Current error: 0.1905712318. Bit fail 63.
Epochs           40. Current error: 0.1806548691. Bit fail 39.
Epochs           50. Current error: 0.1738353539. Bit fail 30.
--- FANN Tests ---
train: Misclassified=31  MSE=0.173196
test:  Misclassified=14  MSE=0.215188
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=14  MSE=0.215188

=== Data Split ===
Splitting data...
Saving data...

=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3424123637. Bit fail 200.
Epochs           10. Current error: 0.1151504517. Bit fail 43.
Epochs           20. Current error: 0.0296698221. Bit fail 8.
Epochs           30. Current error: 0.0137393252. Bit fail 7.
Epochs           40. Current error: 0.0090848899. Bit fail 2.
Epochs           50. Current error: 0.0085387603. Bit fail 2.
--- FANN Tests ---
train: Misclassified=2  MSE=0.008511
test:  Misclassified=4  MSE=0.018782
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=2  MSE=0.018782

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 2.8143246460. Bit fail 84.
Epochs           10. Current error: 0.6892807770. Bit fail 95.
Epochs           20. Current error: 0.2632955551. Bit fail 23.
Epochs           30. Current error: 0.1432660389. Bit fail 8.
Epochs           40. Current error: 0.0986700726. Bit fail 7.
Epochs           50. Current error: 0.0800056934. Bit fail 6.
--- FANN Tests ---
train: Misclassified=6  MSE=0.077363
test:  Misclassified=3  MSE=0.101357
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=3  MSE=0.101357

=== Data Split ===
Splitting data...
Saving data...

=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.2968744151. Bit fail 164.
Epochs           10. Current error: 0.1097097905. Bit fail 45.
Epochs           20. Current error: 0.0296566137. Bit fail 10.
Epochs           30. Current error: 0.0152365812. Bit fail 7.
Epochs           40. Current error: 0.0125950718. Bit fail 6.
Epochs           50. Current error: 0.0118781384. Bit fail 5.
--- FANN Tests ---
train: Misclassified=6  MSE=0.011844
test:  Misclassified=1  MSE=0.005552
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=0  MSE=0.005552

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 2.6822906494. Bit fail 67.
Epochs           10. Current error: 0.4370167542. Bit fail 75.
Epochs           20. Current error: 0.2288341713. Bit fail 38.
Epochs           30. Current error: 0.1037285423. Bit fail 15.
Epochs           40. Current error: 0.0445418310. Bit fail 5.
Epochs           50. Current error: 0.0334585929. Bit fail 5.
--- FANN Tests ---
train: Misclassified=5  MSE=0.031501
test:  Misclassified=2  MSE=0.024009
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=2  MSE=0.024009

=== Data Split ===
Splitting data...
Saving data...

=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3472680918. Bit fail 200.
Epochs           10. Current error: 0.1211695608. Bit fail 53.
Epochs           20. Current error: 0.0672357750. Bit fail 38.
Epochs           30. Current error: 0.0231496334. Bit fail 7.
Epochs           40. Current error: 0.0294903723. Bit fail 10.
Epochs           50. Current error: 0.0097531859. Bit fail 2.
--- FANN Tests ---
train: Misclassified=3  MSE=0.009641
test:  Misclassified=3  MSE=0.014496
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=1  MSE=0.014496

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 3.3083148193. Bit fail 100.
Epochs           10. Current error: 0.5109729767. Bit fail 52.
Epochs           20. Current error: 0.2347096825. Bit fail 25.
Epochs           30. Current error: 0.1299253178. Bit fail 9.
Epochs           40. Current error: 0.0870728970. Bit fail 9.
Epochs           50. Current error: 0.0782231045. Bit fail 7.
--- FANN Tests ---
train: Misclassified=6  MSE=0.077097
test:  Misclassified=5  MSE=0.115788
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=5  MSE=0.115788

=== Data Split ===
Splitting data...
Saving data...

=== FANN Classification Example ===
--- Network Structure ---
OUT OUT OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 0.3202335358. Bit fail 200.
Epochs           10. Current error: 0.1350699488. Bit fail 51.
Epochs           20. Current error: 0.0643438339. Bit fail 26.
Epochs           30. Current error: 0.0218221378. Bit fail 8.
Epochs           40. Current error: 0.0135991271. Bit fail 6.
Epochs           50. Current error: 0.0122640149. Bit fail 4.
--- FANN Tests ---
train: Misclassified=4  MSE=0.012168
test:  Misclassified=1  MSE=0.010876
--- Manual Tests ---
ndata=50  nin=4  nout=3
n=50   Misclassified=1  MSE=0.010876

=== FANN Regression Example ===
--- Network Structure ---
OUT
O  O  O
IN IN IN IN
--- Train ---
Max epochs       50. Desired error: 0.0049999999.
Epochs            1. Current error: 4.8251232910. Bit fail 100.
Epochs           10. Current error: 0.1181191254. Bit fail 17.
Epochs           20. Current error: 0.0390455508. Bit fail 3.
Epochs           30. Current error: 0.0339746451. Bit fail 4.
Epochs           40. Current error: 0.0334150338. Bit fail 4.
Epochs           50. Current error: 0.0327314758. Bit fail 2.
--- FANN Tests ---
train: Misclassified=2  MSE=0.032745
test:  Misclassified=0  MSE=0.030156
--- Manual Tests ---
ndata=50  nin=4  nout=1
n=50   Misclassified=0  MSE=0.030156

=== Summary (after 1 repetitions) ===
Permutation: 0
Average Misclassified Cla.: 2.000
Average Misclassified Reg.: 2.000
Permutation: 1
Average Misclassified Cla.: 2.000
Average Misclassified Reg.: 14.000
Permutation: 2
Average Misclassified Cla.: 2.000
Average Misclassified Reg.: 3.000
Permutation: 3
Average Misclassified Cla.: 0.000
Average Misclassified Reg.: 2.000
Permutation: 4
Average Misclassified Cla.: 1.000
Average Misclassified Reg.: 5.000
Permutation: 5
Average Misclassified Cla.: 1.000
Average Misclassified Reg.: 0.000
</pre>

Changing LOOP_TIMES into 1000 may give you better overview (very slow):
<pre>
=== Summary (after 1000 repetitions) ===
Permutation: 0
Average Misclassified Cla.: 2.475
Average Misclassified Reg.: 2.090
Permutation: 1
Average Misclassified Cla.: 2.456
Average Misclassified Reg.: 15.402
Permutation: 2
Average Misclassified Cla.: 2.476
Average Misclassified Reg.: 5.998
Permutation: 3
Average Misclassified Cla.: 2.432
Average Misclassified Reg.: 2.973
Permutation: 4
Average Misclassified Cla.: 2.457
Average Misclassified Reg.: 12.522
Permutation: 5
Average Misclassified Cla.: 2.438
Average Misclassified Reg.: 2.093
</pre>

Enjoy my other Machine Learning software:
<a href="http://sharktime.com/us_SharkyNeuralNetwork.html">Sharky Neural Network - Classification neural network in action</a>.

Copyright © 2017-2018 Piotr Chlebek
http://sharktime.com/
