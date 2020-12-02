#include "LaserInstruction.hpp"
#include <iostream>
#include <memory>
#include <vector>
#include <opencv4/opencv2/core/core.hpp>
#include <opencv4/opencv2/highgui/highgui.hpp>
#include <opencv4/opencv2/imgproc/imgproc.hpp>
#include "serial.hpp"

#define X_MAX 500
#define Y_MAX 500
static Serial* pSerial;

void uart_send(std::string* txt)
{
  std::cout << *txt;
  pSerial->sendLine(*txt);
  std::string input;
  while(input.compare("AK") != 0)
  {
    input = pSerial->getLine();
    if (input.compare(0,2,"CM") == 0)
    {
      std::cout << "Debug: "<< input << std::endl;
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
  pSerial = new Serial("/dev/ttyUSB0");

  std::vector<LaserInstruction> instructions;
  instructions.push_back(LaserInstruction( // start from home
      [&](std::string* txt){ uart_send(txt);}
      ));

  for (int r = 0; r < yLimit; r++)
  {
    for (int c = 0; c < xLimit; c++)
    {
      auto pixHere = image.at<uchar>(r, c);
      if (pixHere != 0)
      {
        instructions.push_back(
          LaserInstruction( // move to this pixel
            GO,
            r,
            c,
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
        instructions.push_back(
          LaserInstruction(
            BH,
            extend,
            pixHere,
            [&](std::string* txt){ uart_send(txt);}
          )
        );
      }
      c += extend;

    }

  }
  instructions.push_back(LaserInstruction( // go home at the end
      [&](std::string* txt){ uart_send(txt);}
      ));

  for (int i = 0; i < instructions.size(); i++)
  {
    instructions.at(i).send();
  }
  
}
