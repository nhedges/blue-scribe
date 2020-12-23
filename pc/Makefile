#makefile
#captureWC
LIBS = `pkg-config --cflags --libs opencv`
CC = g++ -std=c++11 -O3

all: laserPrint

laserPrint: main.cpp LaserInstruction.o serial.o LaserOperation.o
	$(CC) -o laserPrint main.cpp LaserInstruction.o serial.o LaserOperation.o $(LIBS)

LaserInstruction.o: LaserInstruction.cpp LaserInstruction.hpp
	$(CC) -c LaserInstruction.cpp $(LIBS)
LaserOperation.o: LaserOperation.cpp LaserOperation.hpp
	$(CC) -c LaserOperation.cpp $(LIBS)
serial.o: serial.cpp serial.hpp
	$(CC) -c serial.cpp $(LIBS)
