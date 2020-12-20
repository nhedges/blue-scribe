#include "serial.hpp"
#include <iostream>
Serial::Serial(std::string path)
{
  m_serial_port = open(path.c_str(), O_RDWR); // open the serial port file for send and receive
  if (m_serial_port < 0)
  {
    std::cout << "Error " << errno << ", " << std::endl;
    exit(1);
  }

  struct termios tty;
  if(tcgetattr(m_serial_port, &tty) != 0)
  { // read in the current serial settings, handle errors
    std::cout << "Error " << errno << std::endl;
    exit(1);
  }

  tty.c_cflag &= ~PARENB; // disable parity bit
  tty.c_cflag &= ~CSTOPB; // disable second stop bit
  tty.c_cflag &= ~CSIZE;
  tty.c_cflag |= CS8; // set 8 bits per byte
  tty.c_cflag &= ~CRTSCTS; // disable hardware flow control
  tty.c_cflag |= CREAD | CLOCAL; // disable modem signals
  tty.c_lflag &= ~ICANON; // disable line-by-line input
  tty.c_lflag &= ~ECHO; // disable echo
  tty.c_lflag |= ECHOE; //enable erasure
  tty.c_lflag &= ~ECHONL; // disable new-line echo
  tty.c_lflag &= ~ISIG; //disable interpretation if interrupt signals
  tty.c_iflag &= ~(IXON | IXOFF | IXANY); // turn of flow control
  tty.c_iflag &= ~(IGNBRK | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL); // disable special handling of inputs
  tty.c_oflag &= ~OPOST; // prevent interpretation of output bytes
  tty.c_oflag &= ~ONLCR; // prevent newline conversion to CR/LF
  // this configuration allows a blocking read,
  // and it will continue to block until something
  // is received.
  tty.c_cc[VTIME] = 2;
  tty.c_cc[VMIN] = 100;

  cfsetispeed(&tty, B9600); // set the input baud rate
  cfsetospeed(&tty, B9600); // set the output baud rate

  // save the tty settings and handle errors
  if (tcsetattr(m_serial_port, TCSANOW, &tty) != 0)
  {
    std::cout << "Error " << errno << std::endl;
    exit(1);
  }
}

int Serial::sendLine(std::string str)
{
  std::cout << str << " length " << str.length() << std::endl;
  std::cout << "file handle" << std::hex << m_serial_port << std::endl;
  return write(m_serial_port, str.c_str(), str.length());
}

std::string Serial::getLine()
{
  std::string output = "";
  output.reserve(256);
  char tempBuffer[256] = "";
  int n = 0;
  while (output.find("\n") == std::string::npos)
  {
    // loop and read until we get a newline
    n += read(m_serial_port, tempBuffer + n, 256);
    output = tempBuffer;
    //std::cout << " read " << n << "bytes" << std::endl;
    //std::cout << "output:" << output << std::endl;
  }
  return output;
}

int Serial::closePort()
{
  return close(m_serial_port);
}
