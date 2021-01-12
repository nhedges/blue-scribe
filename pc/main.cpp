//******************************************************************
//
// main.cpp
//
// This is the main portion of the BlueScribe PC software,
// which communicates with the STM32 microcontroller over UART.
//
// It opens an image file, generates commands for the laser engraver,
// queues the list, and sends them, waiting for acknowledgement
// from the microcontroller.
// Nicholas Hedges, 2020
//
//******************************************************************
#include "LaserOperation.hpp"
#include <iostream>
#include <memory>
#include <vector>
#include <opencv4/opencv2/core/core.hpp>
#include <opencv4/opencv2/highgui/highgui.hpp>
#include <opencv4/opencv2/imgproc/imgproc.hpp>
#include "serial.hpp"

#define X_MAX 380
#define Y_MAX 380
#define MOTOR_SCALE 50
static Serial* pSerial;

int yNprompt(const char* message)
{
  std::cout << message << std::endl;
  std::string input = "";
  std::cin >> input;
  if (input.find("Y") == std::string::npos && input.find("y") == std::string::npos)
  {
    return 0;
  }
  return 1;
}

void uart_send(std::string* txt)
{
  std::cout << *txt;
  pSerial->sendLine(*txt);
  std::string input;
  while(input.compare("A\n\r") != 0)
  {
    input = pSerial->getLine();
    std::cout << "Debug: "<< input << std::endl;
  }
}

int effectiveDistance(int x1, int y1, int x2, int y2)
{
  int dx = abs(x2-x1);
  int dy = abs(y2-y1);
  int cost = 0;
  if (dx >= dy)
  {
    cost += dx;
  }
  else
  {
    cost += dy;
  }
  //std::cout << "distance from (" << x1 << ", " << y1 << ") -> (" << x2 << ", " << y2 << ") is " << cost << std::endl;
  return cost;
}

int totalCost(std::vector<LaserOperation> imgOps)
{
  int x = 0;
  int y = 0;
  int cost = 0;
  for (int i = 0; i < imgOps.size(); i++)
  {
    cost += effectiveDistance(x, y, imgOps[i].getStartingXLoc(), imgOps[i].getStartingYLoc());
    x = imgOps[i].getEndingXLoc();
    y = imgOps[i].getEndingYLoc();
    cost += imgOps[i].getInternalCost();
  }
  return cost;
}

void shortestPath(std::vector<LaserOperation>* imageOps)
{
  // this is the algorithm for trying to optimize the
  // time required to print out the picture.
  // the plan to try first
  //
  // (first op is always first)
  //
  // traverse the op, we have an x,y at the end
  // search nearby space for ops (or just forwards into the future)
  // If we find a closer starting point, swap it with the one
  // we were going to do next
  // do the op, repeat
  
  for( int i = 0; i < imageOps->size() - 1; i++)
  {
    int xStart = imageOps->at(i).getEndingXLoc();
    int yStart = imageOps->at(i).getEndingYLoc(); // find our starting point
    int nearestIndex = i+1;
    for (int j = i+2; (j < imageOps->size()-1) && (j-i < 4*X_MAX); j++)
    {
      if (
          effectiveDistance(
            xStart,
            yStart,
            imageOps->at(j).getStartingXLoc(),
            imageOps->at(j).getStartingYLoc())
          < effectiveDistance(
            xStart,
            yStart,
            imageOps->at(nearestIndex).getStartingXLoc(),
            imageOps->at(nearestIndex).getStartingYLoc())
          )
      {
        nearestIndex = j;
      }
    }
    if (nearestIndex != i+1)
    {
      /*std::cout << "swapped cost " << effectiveDistance(
            xStart,
            yStart,
            imageOps->at(i+1).getStartingXLoc(),
            imageOps->at(i+1).getStartingYLoc())
        << " for cost " << effectiveDistance(
            xStart,
            yStart,
            imageOps->at(nearestIndex).getStartingXLoc(),
            imageOps->at(nearestIndex).getStartingYLoc())
        << std::endl;*/
      std::swap(imageOps->at(i+1), imageOps->at(nearestIndex));
    }
  }
}

void alignCorners(int xmin, int ymin, int xmax, int ymax)
{
  std::vector<LaserOperation> tempOps;
  tempOps.push_back(*(new LaserOperation()));
  tempOps.back().appendInstruction(new LaserInstruction( // start from home
      [&](std::string* txt){ uart_send(txt);}
      ));

  tempOps.push_back(*(new LaserOperation()));
  tempOps.back().appendInstruction(
    new LaserInstruction( // move to this pixel
      GO,
      xmin * MOTOR_SCALE,
      ymin * MOTOR_SCALE,
      [&](std::string* txt){ uart_send(txt);}
    )
  );
  tempOps.push_back(*(new LaserOperation()));
  tempOps.back().appendInstruction(
    new LaserInstruction( // move to this pixel
      GO,
      xmax * MOTOR_SCALE,
      ymin * MOTOR_SCALE,
      [&](std::string* txt){ uart_send(txt);}
    )
  );
  tempOps.push_back(*(new LaserOperation()));
  tempOps.back().appendInstruction(
    new LaserInstruction( // move to this pixel
      GO,
      xmax * MOTOR_SCALE,
      ymax * MOTOR_SCALE,
      [&](std::string* txt){ uart_send(txt);}
    )
  );
  tempOps.push_back(*(new LaserOperation()));
  tempOps.back().appendInstruction(
    new LaserInstruction( // move to this pixel
      GO,
      xmin * MOTOR_SCALE,
      ymax * MOTOR_SCALE,
      [&](std::string* txt){ uart_send(txt);}
    )
  );
  tempOps.push_back(*(new LaserOperation()));
  tempOps.back().appendInstruction(
    new LaserInstruction( // move to this pixel
      GO,
      xmin * MOTOR_SCALE,
      ymin * MOTOR_SCALE,
      [&](std::string* txt){ uart_send(txt);}
    )
  );
  for (int i = 0; i < tempOps.size(); i++)
  {
    tempOps.at(i).run();
    if (yNprompt("Going to a corner. Continue? [Y/n]") == 0)
    {
      i = 0; // if the answer is no
    }
  }
}

int main(int argc, char **argv)
{
  std::string inPath = argv[1];
  cv::Mat image;
  // read in the image as a grayscale image
  image = cv::imread(inPath, cv::IMREAD_GRAYSCALE);
  cv::Size imageSize = image.size();
  uint32_t xLimit, yLimit;
  double laserScale = 2;
  if (imageSize.width < X_MAX)
  {
    xLimit = imageSize.width;
  }
  else
  {
    xLimit = X_MAX;
  }
  if (imageSize.height < Y_MAX)
  {
    yLimit = imageSize.height;
  }
  else
  {
    yLimit = Y_MAX;
  }
  //crop down the image to the print size
  image = image(cv::Rect(0,0,xLimit,yLimit));
  std::cout << "Loading image. At any time, you can use Ctrl + C to exit this program." << std::endl;
  if (yNprompt("Click Exit on the Preview. Continue? [Y/n]") == 0)
  {
    return 1;
  }


  std::string windName = "Preview";
  cv::namedWindow(windName);
  cv::imshow(windName, image);
  cv::waitKey(0);
  char temp; // wait for input before continuing

  std::cout << "Type the maximum output power out of 1000 (The old default used 512):";
  uint32_t maxPower = 0;
  std::cin >> maxPower;
  laserScale = (double)maxPower/(double)255;

  std::vector<LaserOperation> imageOps;
  imageOps.push_back(*(new LaserOperation()));
  imageOps.back().appendInstruction(new LaserInstruction( // start from home
      [&](std::string* txt){ uart_send(txt);}
      ));
  
  for (int r = 0; r < yLimit; r++)
  {
    int direction = 0;
    if ((r & 0x1) == 0) //even rows
      direction = 1;
    else
      direction = -1; // odd rows right to left
    if(direction == 1)
    { // left to right
    for (int c = 0; c < xLimit; c++)
    {
      auto pixHere = image.at<uchar>(r, c);
      if (pixHere != 0)
      {
        imageOps.push_back(*(new LaserOperation()));
        imageOps.back().appendInstruction(
          new LaserInstruction( // move to this pixel
            GO,
            c * MOTOR_SCALE,
            r * MOTOR_SCALE,
            [&](std::string* txt){ uart_send(txt);}
          )
        );
      }
      uint32_t extend = 0;
      if (pixHere == image.at<uchar>(r, c+1))
      {
        for (int c2 = c+1;
            (c2 < xLimit) && (image.at<uchar>(r,c2) == pixHere);
            c2++,extend++);
      }
      if (pixHere != 0) // we skip blank space
      {
        imageOps.back().appendInstruction(
          new LaserInstruction(
            BH,
            (extend+1) * MOTOR_SCALE,
            pixHere * laserScale,
            [&](std::string* txt){ uart_send(txt);}
          )
        );
      }
      c += extend;

    }
    }
    else
    {// right to left
    for (int c = xLimit; c >= 0; c--)
    {
      auto pixHere = image.at<uchar>(r, c);
      if (pixHere != 0)
      {
        imageOps.push_back(*(new LaserOperation()));
        imageOps.back().appendInstruction(
          new LaserInstruction( // move to this pixel
            GO,
            (c+1) * MOTOR_SCALE,
            r * MOTOR_SCALE,
            [&](std::string* txt){ uart_send(txt);}
          )
        );
      }
      uint32_t extend = 0;
      if (pixHere == image.at<uchar>(r, c-1))
      {
        for (int c2 = c-1;
            (c2 >= 0) && (image.at<uchar>(r,c2) == pixHere);
            c2--,extend--);
      }
      if (pixHere != 0) // we skip blank space
      {
        imageOps.back().appendInstruction(
          new LaserInstruction(
            BH,
            (extend-1) * MOTOR_SCALE,
            pixHere * laserScale,
            [&](std::string* txt){ uart_send(txt);}
          )
        );
      }
      c += extend;

    }
    }

  }
  imageOps.push_back(*(new LaserOperation()));
  imageOps.back().appendInstruction(new LaserInstruction( // go home at the end
      [&](std::string* txt){ uart_send(txt);}
      ));
  int tempCost = totalCost(imageOps);
  std::cout << "Generated " << imageOps.size() << " operations, costing " << tempCost << std::endl;
  std::cout << tempCost/400 << " seconds" << std::endl;
  if (yNprompt("Generate faster path? [Y/n]") == 1)
  {
    shortestPath(&imageOps);
  }
  tempCost = totalCost(imageOps);
  std::cout << "Generated " << imageOps.size() << " operations, costing " << tempCost << std::endl;
  std::cout << tempCost/400 << " seconds" << std::endl;
  pSerial = new Serial("/dev/ttyACM0");
  if(yNprompt("Corner Alignment? [y/N]") == 1)
  {
    alignCorners(0, 0, xLimit, yLimit);
  }
  if (yNprompt("Begin Burn? [Y/n]") == 1)
  {
    for (int i = 0; i < imageOps.size(); i++)
    {
      imageOps.at(i).run();
    }
  }
  return 0;
}
