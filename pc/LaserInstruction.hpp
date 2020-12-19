#ifndef LASERINSTRUCTION_HPP
#define LASERINSTRUCTION_HPP

#include <functional>
#include <string>

enum CmdClass{GO, HM, SQ, BH, BV};
const std::string cmdClassStrings[] = {"GO", "HM", "SQ", "BH", "BV"};
// GOto, HoMe, SQare, Burn Horizontal, Burn Vertical

class LaserInstruction
{
  public:
    LaserInstruction(std::function<void(std::string*)> dispCallback);
    LaserInstruction(CmdClass commandType, int arg1, int arg2, std::function<void(std::string*)> dispCallback);
    void send();
    int getArg1();
    int getArg2();
    CmdClass getType();
    int getInternalCost();
  protected:
    std::function<void(std::string*)> m_displayCallback;
    CmdClass m_class;
    int m_arg1;
    int m_arg2;
};


#endif /* LASERINSTRUCTION_HPP */
