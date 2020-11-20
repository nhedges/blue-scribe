#include "LaserInstruction.hpp"

LaserInstruction::LaserInstruction(std::function<void(std::string*)> dispCallback):
{
  m_class = CmdClass::HM; // home is the default
}

LaserInstruction::LaserInstruction(CmdClass commandType, uint32_t arg1, uint32_t arg2, std::function<void(std::string*)> dispCallback):
  m_displayCallback(dispCallback),
  m_class(commandType),
  m_arg1(arg1),
  m_arg2(arg2)
{
}

void LaserInstruction::Send()
{
  std::string buf;
  buf << classStrings[m_class];
  if (m_class != HM) // if it has two arguments
  {
    buf << " " << arg1 << " " << arg2;
  }
  buf << std::endl;
  m_displayCallback(&buf);
}
