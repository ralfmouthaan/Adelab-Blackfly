' Ralf Mouthaan
' University of Adelaide
' August 2023
'
' Code to control Blacklfy camera

Imports SpinnakerNET
Imports SpinnakerNET.GenApi

Public Class clsBlackflyCam
    Inherits clsCamera

    Private SerialNumber As String
    Private Cam As IManagedCamera
    Private ImgProcessor As IManagedImageProcessor

    Private _Exposure As Double
    Private _Gain As Double

    Public Property intCameraID As Integer
    Public Property intTriggerMode As Integer
    Public Property portSLMClock As IO.Ports.SerialPort
    Public Property bolActive As Boolean

    Public Sub New(Optional ByVal _SerialNumber As String = "")
        MyBase.New

        _intCameraID = 1
        SerialNumber = _SerialNumber
        _strInstrumentName = "FLIR Blackfly"
        _Exposure = 50
        _Gain = 0

    End Sub

    Public Overrides Sub Startup(Optional bolShow As Boolean = False)

        If bolConnectionOpen = True Then Exit Sub
        Cam = Nothing

        Dim CameraSystem As ManagedSystem = New ManagedSystem()
        Dim InterfaceList As List(Of IManagedInterface) = CameraSystem.GetInterfaces()
        Dim CameraList As List(Of IManagedCamera) = CameraSystem.GetCameras()
        Dim nodemap As INodeMap = Nothing

        Dim idxInterface As Integer
        Dim idxCamera As Integer

        For idxInterface = 0 To InterfaceList.Count - 1

            Dim ManagedInterface As IManagedInterface = InterfaceList(idxInterface)
            ManagedInterface.UpdateCameras()

            For idxCamera = 0 To CameraList.Count - 1

                Dim _cam As IManagedCamera = CameraList(idxCamera)
                nodemap = _cam.GetTLDeviceNodeMap()
                Dim _SerialNumber As String = nodemap.GetNode(Of IString)("DeviceSerialNumber").Value

                If SerialNumber = SerialNumber Then
                    Cam = _cam
                    Exit For
                Else
                    Cam.Dispose()
                End If

            Next

            CameraList.Clear()
            If Cam IsNot Nothing Then Exit For

        Next

        InterfaceList.Clear()
        CameraSystem.Dispose()

        Cam.Init()
        nodemap = Cam.GetNodeMap

        Dim ExposureAutoNode As IEnum = nodemap.GetNode(Of IEnum)("ExposureAuto")
        Dim iExposureAutoOff As IEnumEntry = ExposureAutoNode.GetEntryByName("Off")
        ExposureAutoNode.Value = iExposureAutoOff.Value

        Dim GainAutoNode As IEnum = nodemap.GetNode(Of IEnum)("GainAuto")
        Dim GainAutoOff As IEnumEntry = GainAutoNode.GetEntryByName("Off")
        GainAutoNode.Value = GainAutoOff.Value

        Dim GammaNode As IFloat = nodemap.GetNode(Of IFloat)("Gamma")
        GammaNode.Value = 1

        Dim AcquisitionNode As IEnum = nodemap.GetNode(Of IEnum)("AcquisitionMode")
        Dim AcquisitionContinuous As IEnumEntry = AcquisitionNode.GetEntryByName("Continuous")
        AcquisitionNode.Value = AcquisitionContinuous.Value

        Dim TriggerModeNode As IEnum = nodemap.GetNode(Of IEnum)("TriggerMode")
        Dim TriggerModeSelectorNode As IEnum = nodemap.GetNode(Of IEnum)("TriggerSelector")
        Dim TriggerModeSourceNode As IEnum = nodemap.GetNode(Of IEnum)("TriggerSource")
        Dim TriggerModeOff As IEnumEntry = TriggerModeNode.GetEntryByName("Off")
        TriggerModeNode.Value = TriggerModeOff.Value
        Dim TriggerModeFrameStart As IEnumEntry = TriggerModeSelectorNode.GetEntryByName("FrameStart")
        TriggerModeSelectorNode.Value = TriggerModeFrameStart.Value
        Dim TriggerModeSourceSoftware As IEnumEntry = TriggerModeSourceNode.GetEntryByName("Software")
        TriggerModeSourceNode.Value = TriggerModeSourceSoftware.Value
        Dim TriggerModeOn As IEnumEntry = TriggerModeNode.GetEntryByName("On")
        TriggerModeNode.Value = TriggerModeOn.Value

        ImgProcessor = New ManagedImageProcessor()
        ImgProcessor.SetColorProcessing(ColorProcessingAlgorithm.HQ_LINEAR)

        MyBase.Startup(bolShow)
        Exposure = _Exposure
        Gain = _Gain

        Cam.BeginAcquisition()

    End Sub
    Public Overrides Sub Shutdown()
        MyBase.Shutdown()
        Cam.EndAcquisition()
        Cam.Dispose()
    End Sub

    Public Overrides ReadOnly Property ImageWidth As Integer
        Get
            Dim nodemap As INodeMap = Cam.GetNodeMap()
            Return CInt(nodemap.GetNode(Of IInteger)("Width").Value)
        End Get
    End Property
    Public Overrides ReadOnly Property ImageHeight As Integer
        Get
            Dim nodemap As INodeMap = Cam.GetNodeMap()
            Return CInt(nodemap.GetNode(Of IInteger)("Height").Value)
        End Get
    End Property

    Public Overrides Property Exposure As Double
        Get
            If bolConnectionOpen = True Then
                Dim nodemap As INodeMap = Cam.GetNodeMap()
                Return CDbl(nodemap.GetNode(Of IInteger)("ExposureTime").Value)
            Else
                Return _Exposure
            End If
        End Get
        Set(value As Double)

            _Exposure = value
            If bolConnectionOpen = False Then Exit Property

            Dim nodemap As INodeMap = Cam.GetNodeMap()
            Dim ExposureTimeNode As IFloat = nodemap.GetNode(Of IFloat)("ExposureTime")
            ExposureTimeNode.Value = value
        End Set
    End Property
    Public Property Gain As Double
        Get
            If bolConnectionOpen = True Then
                Dim nodemap As INodeMap = Cam.GetNodeMap()
                Return CDbl(nodemap.GetNode(Of IFloat)("Gain").Value)
            Else
                Return _Gain
            End If
        End Get
        Set(value As Double)

            _Gain = value
            If bolConnectionOpen = False Then Exit Property

            Dim nodemap As INodeMap = Cam.GetNodeMap()
            Dim GainNode As IFloat = nodemap.GetNode(Of IFloat)("Gain")
            GainNode.Value = value
        End Set
    End Property
    Public ReadOnly Property FrameRate As Double
        Get
            Dim nodemap As INodeMap = Cam.GetNodeMap()
            Return nodemap.GetNode(Of IFloat)("AcquisitionFrameRate").Value
        End Get
    End Property

    Public Overrides Sub SaveImage(Filename As String)

        Dim rawImage As IManagedImage = Cam.GetNextImage(1000)
        Dim convertedImage As IManagedImage = ImgProcessor.Convert(rawImage, PixelFormatEnums.Mono8)
        convertedImage.Save(Filename)

    End Sub

    Public Overrides Function GetIntegerImage(Optional NoFrames As Integer = -1) As Integer(,)
        ExecuteTrigger()
        Dim rawImage As IManagedImage = Cam.GetNextImage(1000)
        Dim convertedImage As IManagedImage = ImgProcessor.Convert(rawImage, PixelFormatEnums.Mono8)
        Dim Bmap As Bitmap = convertedImage.bitmap

        Dim RetVal(convertedImage.Height - 1, convertedImage.Width - 1) As Integer

        For i = 0 To convertedImage.Height - 1
            For j = 0 To convertedImage.Width - 1
                RetVal(i, j) = Bmap.GetPixel(j, i).R 'Assuming it's grayscale and R = G = B
            Next
        Next

        Return RetVal

    End Function
    Public Function GetBitmapImage() As Bitmap
        ExecuteTrigger()
        Dim rawImage As IManagedImage = Cam.GetNextImage(1000)
        Dim convertedImage As IManagedImage = ImgProcessor.Convert(rawImage, PixelFormatEnums.Mono8)
        Return convertedImage.bitmap
    End Function

    Private Sub ExecuteTrigger()
        If bolConnectionOpen = False Then Exit Sub
        Dim nodemap As INodeMap = Cam.GetNodeMap()
        Dim TriggerSoftwareNode As ICommand = nodemap.GetNode(Of ICommand)("TriggerSoftware")
        TriggerSoftwareNode.Execute()
    End Sub
End Class
