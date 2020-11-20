#ifndef LASERINSTRUCTION_HPP
#define LASERINSTRUCTION_HPP

#include <functional>
#include <string>

enum CmdClass{GO, HM, SQ, BH, BV};
const char* classStrings = {"GO", "HM", "SQ", "BH", "BV"};
// GOto, HoMe, SQare, Burn Horizontal, Burn Vertical

class LaserInstruction
{
  public:
    LaserInstruction(std::function<void(std::string*)> dispCallback);
    LaserInstruction(CmdClass commandType, uint32_t arg1, uint32_t arg2, std::function<void(std::string*)> dispCallback);
    void send();
  protected:
    std::function<void(std::string*)> m_displayCallback;
    CmdClass m_class;
    uint32_t m_arg1;
    uint32_t m_arg2;
};


#endif /* LASERINSTRUCTION_HPP */
