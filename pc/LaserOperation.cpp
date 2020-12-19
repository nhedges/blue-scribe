#include "LaserOperation.hpp"

int LaserOperation::getStartingXLoc()
{
  if (
      m_instructions.size() > 0 &&
      m_instructions[0] &&
      m_instructions[0]->getType() == GO)
  { // if we have a go instruction that is not null
    return m_instructions[0]->getArg1();
  }
  else
    return -1; // error otherwise
}

int LaserOperation::getStartingYLoc()
{
  if (
      m_instructions.size() > 0 &&
      m_instructions[0] &&
      m_instructions[0]->getType() == GO)
  { // if we have a go instruction that is not null
    return m_instructions[0]->getArg2();
  }
  else
    return -1; // error otherwise
}

int LaserOperation::getEndingXLoc()
{
  return -1;//TODO implement
}


int LaserOperation::getEndingYLoc()
{
  return -1;//TODO implement
}

int LaserOperation::getInternalCost()
{
  int total = 0;
  int x = getStartingXLoc();
  int y = getStartingYLoc();
  if (x == -1 || y == -1)
    return -1;
  for (int i = 0; i < m_instructions.size(); i++)
  {
    total += m_instructions[i]->getInternalCost();
  }
  return total;
}

int LaserOperation::appendInstruction(LaserInstruction* pNewInst)
{
  if (pNewInst != nullptr)
    m_instructions.push_back(std::shared_ptr<LaserInstruction>(pNewInst));
  return m_instructions.size();
}

void LaserOperation::run()
{
  for (int i = 0; i < m_instructions.size(); i++)
  {
    m_instructions.at(i)->send();
  }
}
