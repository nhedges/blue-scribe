#makefile
#captureWC
LIBS = `pkg-config --cflags --libs opencv4`
CC = g++ -std=c++11 -O3

all: laserPrint

laserPrint: main.cpp LaserInstruction.o serial.o
	$(CC) -o laserPrint main.cpp LaserInstruction.o serial.o $(LIBS)

LaserInstruction.o: LaserInstruction.cpp LaserInstruction.hpp
	$(CC) -c LaserInstruction.cpp $(LIBS)
serial.o: serial.cpp serial.hpp
	$(CC) -c serial.cpp $(LIBS)
