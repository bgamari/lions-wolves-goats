all : MagicForest magic_forest Benchmark

% : %.hs
	ghc -O2 $<

% : %.cpp
	g++ -std=c++11 $< -o $@
