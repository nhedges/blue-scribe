#include "serial.hpp"
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
  tty.c_lflag |= ICANON; // enable line-by-line input(**NOT TYPICAL!**)
  tty.c_lflag &= ~ECHO; // disable echo
  tty.c_lflag &= ~ECHOE; //disable erasure
  tty.c_lflag &= ~ECHONL; // disable new-line echo
  tty.c_lflag &= ~ISIG; //disable interpretation if interrupt signals
  tty.c_iflag &= ~(IXON | IXOFF | IXANY); // turn of flow control
  tty.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP |INLCR |IGNCR | ICRNL); // disable special handling of inputs
  tty.c_oflag &= ~OPOST; // prevent interpretation of output bytes
  tty.c_oflag &= ~ONLCR; // prevent newline conversion to CR/LF
  // this configuration allows a blocking read,
  // and it will continue to block until something
  // is received.
  tty.c_cc[VTIME] = 0;
  tty.c_cc[VMIN] = 0;

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
  return write(m_serial_port, str.c_str(), sizeof(str.c_str()));
}

std::string Serial::getLine()
{
  std::string output;
  output.reserve(256);
  int n = read(m_serial_port, &output, 256);
  return output;
}

int Serial::closePort()
{
  return close(m_serial_port);
}
