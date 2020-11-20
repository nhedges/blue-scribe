#ifndef LASERINSTRUCTION_HPP
#define LASERINSTRUCTION_HPP

#include <functional>

enum CmdClass{GO, HM, SQ, BH, BV};
// GOto, HoMe, SQare, Burn Horizontal, Burn Vertical

class LaserInstruction
{
  public:
    LaserInstruction(CmdClass commandType, std::function<void(std::string*)> dispCallback);
    LaserInstruction(CmdClass commandType, uint32_t arg1, uint32_t arg2, std::function<void(std::string*)> dispCallback);
    send();
  protected:
    std::function<void(std::string*)> m_displayCallback;
    uint32_t x; // x coordinate location
    uint32_t y; // y coordinate location
    uint32_t p; // power value
};


#endif /* LASERINSTRUCTION_HPP */
