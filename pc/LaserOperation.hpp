#ifndef LASEROPERATION_HPP
#define LASEROPERATION_HPP

#include "LaserInstruction.hpp"
#include <vector>
#include <memory>

class LaserOperation
{
  public:
    // these get the starting location of the op
    int getStartingXLoc();
    int getStartingYLoc();
    // these get the ending location of the op
    // (requires traversing all instructions)
    int getEndingXLoc();
    int getEndingYLoc();
    // calculates the cost, or number of steps,
    // required to traverse all instructions
    // in the op
    // (requires traversal)
    int getInternalCost();
    // adds a new instruction to the
    // operation.
    int appendInstruction(LaserInstruction* pNewInst);
    // runs all instructions in the operation,
    // one after the next.
    void run();
  protected:
    // main data structure for keeping track of the
    // instructions. vector of shared pointers
    std::vector<std::shared_ptr<LaserInstruction>> m_instructions;
};
#endif /* LASEROPERATION_HPP */
