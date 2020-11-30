#ifndef SERIAL_HPP
#define SERIAL_HPP
#include <fcntl.h>
#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>
#include <iostream>
#include <fstream>
#include <string>


class Serial
{
  public:
    Serial(std::string); // constructor
    int sendLine(std::string); // send
    std::string getLine(); // read a line
    int closePort(); // close the serial connection
  private:
    int m_serial_port; // file handle
};
#endif /* SERIAL_HPP */
