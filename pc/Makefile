#makefile
#captureWC
LIBS = `pkg-config --cflags --libs opencv4`
CC = g++ -std=c++11 -O3

all: laserPrint

laserPrint: main.cpp LaserInstruction.o
	$(CC) -o laserPrint main.cpp LaserInstruction.o $(LIBS)

LaserInstruction.o: LaserInstruction.cpp LaserInstruction.hpp
	$(CC) -c LaserInstruction.cpp $(LIBS)
