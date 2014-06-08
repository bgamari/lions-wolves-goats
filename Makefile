all : MagicForest magic_forest Benchmark

% : %.hs
	ghc -O2 $<

% : %.cpp
	g++ -O2 -std=c++11 $< -o $@
