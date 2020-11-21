#include "LaserInstruction.hpp"
#include <iostream>
#include <memory>
#include <vector>
#include <opencv4/opencv2/core/core.hpp>
#include <opencv4/opencv2/highgui/highgui.hpp>
#include <opencv4/opencv2/imgproc/imgproc.hpp>

#define X_MAX 500
#define Y_MAX 500

void uart_send(std::string* txt)
{
  std::cout << *txt;
}

int main(int argc, char **argv)
{
  std::string inPath = argv[1];
  cv::Mat image;
  // read in the image as a grayscale image
  image = cv::imread(inPath, cv::IMREAD_GRAYSCALE);
  //crop down the image to the print size
  image = image(cv::Rect(0,0,X_MAX,Y_MAX));

  std::string windName = "Preview";
  cv::namedWindow(windName);
  cv::imshow(windName, image);
  cv::waitKey(0);
  std::string temp; // wait for input before continuing
  std::cin >> temp;

  std::vector<LaserInstruction> instructions;
  instructions.push_back(LaserInstruction(
      [&](std::string* txt){ uart_send(txt);}
      ));
  instructions.push_back(LaserInstruction(GO,20,30,
      [&](std::string* txt){ uart_send(txt);}
      ));
  for (int r = 0; r < Y_MAX; r++)
  {
    for (int c = 0; c < X_MAX; c++)
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
      uint32_t extend = 1;
      if (pixHere == image.at<uchar>(r, c+1))
      {
        for (int c2 = c;
            (c < X_MAX) && (image.at<uchar>(r,c2) == pixHere);
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

  for (int i = 0; i < instructions.size(); i++)
  {
    instructions.at(i).send();
  }
  
}
