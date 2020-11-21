#include "LaserInstruction.hpp"

LaserInstruction::LaserInstruction(std::function<void(std::string*)> dispCallback)
{
  m_class = CmdClass::HM; // home is the default
  m_displayCallback = dispCallback;
}

LaserInstruction::LaserInstruction(CmdClass commandType, uint32_t arg1, uint32_t arg2, std::function<void(std::string*)> dispCallback):
  m_displayCallback(dispCallback),
  m_class(commandType),
  m_arg1(arg1),
  m_arg2(arg2)
{
}

void LaserInstruction::send()
{
  std::string buf;
  buf = buf + cmdClassStrings[m_class];
  if (m_class != HM) // if it has two arguments
  {
    buf = buf + " " + std::to_string(m_arg1) + " " + std::to_string(m_arg2);
  }
  buf = buf + "\n";
  m_displayCallback(&buf);
}
