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
#define LASER_SCALE 2 // max 510/1000 power
static Serial* pSerial;

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

int totalCost(std::vector<LaserOperation> imgOps)
{
  int x = 0;
  int y = 0;
  int cost = 0;
  for (int i = 0; i < imgOps.size(); i++)
  {
    int dx = abs(imgOps[i].getStartingXLoc() - x);
    int dy = abs(imgOps[i].getStartingYLoc() - y);
    std::cout << "x:" << x << std::endl;
    std::cout << "startX:" << imgOps[i].getStartingXLoc() << std::endl;
    std::cout << "endX:" << imgOps[i].getEndingXLoc() << std::endl;
    std::cout << "dx:" << dx << std::endl;
    x = imgOps[i].getEndingXLoc();
    y = imgOps[i].getEndingYLoc();
    if (dx >= dy)
    {
      cost += dx;
    }
    else
    {
      cost += dy;
    }
    cost += imgOps[i].getInternalCost();
    std::cout << "running cost " << cost << std::endl;
  }
  return cost;
}

int main(int argc, char **argv)
{
  std::string inPath = argv[1];
  cv::Mat image;
  // read in the image as a grayscale image
  image = cv::imread(inPath, cv::IMREAD_GRAYSCALE);
  cv::Size imageSize = image.size();
  uint32_t xLimit, yLimit;
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

  std::string windName = "Preview";
  cv::namedWindow(windName);
  cv::imshow(windName, image);
  cv::waitKey(0);
  std::string temp; // wait for input before continuing
  std::cin >> temp;
  pSerial = new Serial("/dev/ttyACM0");

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
            pixHere * LASER_SCALE,
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
            pixHere * LASER_SCALE,
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
  std::cin >> tempCost;

  for (int i = 0; i < imageOps.size(); i++)
  {
    imageOps.at(i).run();
  }
  
}
