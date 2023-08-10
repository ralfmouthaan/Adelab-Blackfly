// Ralf Mouthaan
// University of Adelaide
// March 2023
//
// Code to run Blackfly camera.
// Aim is to set basic parameters and to trigger image capture from software.

#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include "Spinnaker.h"
#include "SpinGenApi/SpinnakerGenApi.h"
#include <iostream>
#include <sstream>

using namespace Spinnaker;
using namespace Spinnaker::GenApi;
using namespace Spinnaker::GenICam;

class clsBlackfly {

protected:

    std::string SerialNo = "22421982";
    PixelFormatEnums PixelFormat = PixelFormatEnums::PixelFormat_Mono8;
    SystemPtr pSystem;
    CameraPtr pCam;
    ImageProcessor ImgProcessor;

    ImagePtr CaptureImage_SpinnakerPtr() {

        // Returns image as a Spinnaker pointer

        ImagePtr pRawImage;
        ImagePtr pImage;
        pRawImage = pCam->GetNextImage(1000);
        pImage = ImgProcessor.Convert(pRawImage, PixelFormat);
        pRawImage->Release(); // Raw image pointer obtained from camera capture call needs to be released.
                              // Spinnaker manual seems to hint that pointers obtained from conversions do not need to be released???
        return pImage;

    }
    cv::Mat CaptureImage_CVMat() {

        ImagePtr pImage = CaptureImage_SpinnakerPtr();
        unsigned int XPadding = pImage->GetXPadding();
        unsigned int YPadding = pImage->GetYPadding();
        unsigned int rowsize = pImage->GetWidth();
        unsigned int colsize = pImage->GetHeight();

        //image data contains padding. When allocating Mat container size, you need to account for the X,Y image data padding. 
        cv::Mat cvimg = cv::Mat(colsize + YPadding, rowsize + XPadding, CV_8UC1, pImage->GetData(), pImage->GetStride());
        cvimg = cvimg.clone(); // cvimg is corrupted when pImage is disposed. To avoid this, we deep-clone cvimg.
        return cvimg;

    }

public:

    int Width;
    int Height;

    clsBlackfly() {

        // Get instance of system
        pSystem = System::GetInstance();

        // Find camera + initialise
        CameraList camList = pSystem->GetCameras();
        unsigned int numCameras = camList.GetSize();
        pCam = camList.GetBySerial(SerialNo);
        camList.Clear();
        pCam->Init();

        // Retrieve GenICam nodemap
        INodeMap& NodeMap = pCam->GetNodeMap();

        // Turn off auto exposure
        CEnumerationPtr ptrExposureAuto = NodeMap.GetNode("ExposureAuto");
        ptrExposureAuto->SetIntValue(ptrExposureAuto->GetEntryByName("Off")->GetValue());

        // Turn off auto gain
        CEnumerationPtr ptrGainAuto = NodeMap.GetNode("GainAuto");
        ptrGainAuto->SetIntValue(ptrGainAuto->GetEntryByName("Off")->GetValue());

        // Set Gamma to 1
        CFloatPtr ptrGamma = NodeMap.GetNode("Gamma");
        ptrGamma->SetValue(1);

        // Get width and height
        CIntegerPtr ptrWidth = NodeMap.GetNode("Width");
        Width = ptrWidth->GetValue();
        CIntegerPtr ptrHeight = NodeMap.GetNode("Height");
        Height = ptrHeight->GetValue();

        // Set acquisition mode
        CEnumerationPtr ptrAcquisitionMode = NodeMap.GetNode("AcquisitionMode");
        ptrAcquisitionMode->SetIntValue(ptrAcquisitionMode->GetEntryByName("Continuous")->GetValue());

        // Set up image conversion tool
        ImgProcessor.SetColorProcessing(SPINNAKER_COLOR_PROCESSING_ALGORITHM_HQ_LINEAR);

        SetupTrigger("None");

        // Start acquisition
        pCam->BeginAcquisition();

    }
    ~clsBlackfly() {

        pCam->EndAcquisition();
        pSystem->ReleaseInstance();

    }

    double GetExposure() {

        INodeMap& NodeMap = pCam->GetNodeMap();
        CFloatPtr ptrExposureTime = NodeMap.GetNode("ExposureTime");
        return ptrExposureTime->GetValue();

    }
    void SetExposure(double Exposure) {

        INodeMap& NodeMap = pCam->GetNodeMap();
        CFloatPtr ptrExposureTime = NodeMap.GetNode("ExposureTime");
        const double MinExposure = ptrExposureTime->GetMin();
        const double MaxExposure = ptrExposureTime->GetMax();
        if (Exposure < MinExposure) { Exposure = MinExposure; }
        if (Exposure > MaxExposure) { Exposure = MaxExposure; }
        ptrExposureTime->SetValue(Exposure);

    }
    double GetGain() {

        INodeMap& NodeMap = pCam->GetNodeMap();
        CFloatPtr ptrGain = NodeMap.GetNode("Gain");
        return ptrGain->GetValue();

    }
    void SetGain(double Gain) {

        INodeMap& NodeMap = pCam->GetNodeMap();
        CFloatPtr ptrGain = NodeMap.GetNode("Gain");
        const double MinGain = ptrGain->GetMin();
        const double MaxGain = ptrGain->GetMax();
        if (Gain < MinGain) { Gain = MinGain; }
        if (Gain > MaxGain) { Gain = MaxGain; }
        ptrGain->SetValue(Gain);

    }
    double GetFrameRate() {

        INodeMap& NodeMap = pCam->GetNodeMap();
        CFloatPtr ptrFrameRate = NodeMap.GetNode("AcquisitionFrameRate");
        return ptrFrameRate->GetValue();

    }
    void SetupTrigger(const char* Trigger) {

        INodeMap& NodeMap = pCam->GetNodeMap();
        CEnumerationPtr ptrTriggerMode = NodeMap.GetNode("TriggerMode");
        CEnumerationPtr ptrTriggerSelector = NodeMap.GetNode("TriggerSelector");
        CEnumerationPtr ptrTriggerSource = NodeMap.GetNode("TriggerSource");

        if (strcmp(Trigger, "None") == 0) {
            ptrTriggerMode->SetIntValue(ptrTriggerMode->GetEntryByName("Off")->GetValue());
        }
        else if (strcmp(Trigger, "Software") == 0) {
            ptrTriggerMode->SetIntValue(ptrTriggerMode->GetEntryByName("Off")->GetValue());
            ptrTriggerSelector->SetIntValue(ptrTriggerSelector->GetEntryByName("FrameStart")->GetValue());
            ptrTriggerSource->SetIntValue(ptrTriggerSource->GetEntryByName("Software")->GetValue());
            ptrTriggerMode->SetIntValue(ptrTriggerMode->GetEntryByName("On")->GetValue());
        }
        else if (strcmp(Trigger, "Hardware") == 0) {
            ptrTriggerMode->SetIntValue(ptrTriggerMode->GetEntryByName("Off")->GetValue());
            ptrTriggerSelector->SetIntValue(ptrTriggerSelector->GetEntryByName("FrameStart")->GetValue());
            ptrTriggerSource->SetIntValue(ptrTriggerSource->GetEntryByName("Line0")->GetValue());
            ptrTriggerMode->SetIntValue(ptrTriggerMode->GetEntryByName("On")->GetValue());
        }

    }

    cv::Mat GetImage() {
        // Returns an image.
        // If not triggering, returns the most recent image.
        // If triggering, returns next image in sequence... I hope
        return CaptureImage_CVMat();
    }
    void ExecuteTrigger() {
        // Execute a software trigger
        INodeMap& NodeMap = pCam->GetNodeMap();
        CCommandPtr ptrSoftwareTriggerCommand = NodeMap.GetNode("TriggerSoftware");
        ptrSoftwareTriggerCommand->Execute();
    }

};

int main()
{
    clsBlackfly* Cam = new clsBlackfly;
    cv::Mat Img = Cam->GetImage();
    cv::imshow("", Img);
    cv::waitKey(0);

}