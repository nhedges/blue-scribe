#include "LaserInstruction.hpp"

LaserInstruction::LaserInstruction(std::function<void(std::string*)> dispCallback)
{
  m_class = CmdClass::HM; // home is the default
  m_displayCallback = dispCallback;
}

LaserInstruction::LaserInstruction(CmdClass commandType, int arg1, int arg2, std::function<void(std::string*)> dispCallback):
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
  buf = buf + "\n\r";
  m_displayCallback(&buf);
}

int LaserInstruction::getArg1()
{
  return m_arg1;
}

int LaserInstruction::getArg2()
{
  return m_arg2;
}

CmdClass LaserInstruction::getType()
{
  return m_class;
}

int LaserInstruction::getInternalCost()
{
  switch (m_class)
  {
    case GO:
    case HM:
      return 0;
      break;
    case SQ:
      return 4 * m_arg1;
      break;
    case BH:
    case BV:
      return m_arg1;
    default:
      return 0;
  }
}
