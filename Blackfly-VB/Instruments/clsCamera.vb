' Ralf Mouthaan
' University of Cambridge
' June 2019
'
' Camera parent class
' Performs a lot of the holography calculations too now.

Option Explicit On
Option Strict On

Public MustInherit Class clsCamera
    Inherits clsInstrument

    Public Sub New()

        fftwOffAxisFFTInput = Nothing
        fftwOffAxisFFTResult = Nothing
        fftwOffAxisIFFTInput = Nothing
        fftwOffAxisIFFTResult = Nothing

    End Sub

    Public Overridable Sub Startup(Optional ByVal bolShow As Boolean = False)

        'This is to be called after the connection to the camera has been established

        If OffAxisImageViewport.Width = 0 AndAlso OffAxisImageViewport.Height = 0 Then
            OffAxisImageViewport = New Rectangle
            OffAxisImageViewport.X = 0
            OffAxisImageViewport.Width = ImageWidth
            OffAxisImageViewport.Y = 0
            OffAxisImageViewport.Height = ImageHeight
        End If

        If OffAxisFFTViewport.Width = 0 AndAlso OffAxisFFTViewport.Height = 0 Then
            OffAxisFFTViewport = New Rectangle
            OffAxisFFTViewport.X = 0
            OffAxisFFTViewport.Y = 0
            OffAxisFFTViewport.Width = 1024
            OffAxisFFTViewport.Height = 1024
        End If

        bolConnectionOpen = True

    End Sub
    Public Overridable Sub Shutdown()

        If bolConnectionOpen = False Then Exit Sub

        'NOTE:
        ' Shutdown is called regularly.
        ' The code seems to randomly crash further down the line if I call dispose regularly.
        ' Suspect this is something to do with the FFTW.NET bindings not being very good.

        'If fftwOffAxisFFTInput IsNot Nothing AndAlso fftwOffAxisFFTInput.IsDisposed = False Then
        '    fftwOffAxisFFTInput.Dispose()
        'End If

        'If fftwOffAxisFFTResult IsNot Nothing AndAlso fftwOffAxisFFTResult.IsDisposed = False Then
        '    fftwOffAxisFFTResult.Dispose()
        'End If

        'If fftwOffAxisIFFTInput IsNot Nothing AndAlso fftwOffAxisIFFTInput.IsDisposed = False Then
        '    fftwOffAxisIFFTInput.Dispose()
        'End If

        'If fftwOffAxisIFFTResult IsNot Nothing AndAlso fftwOffAxisIFFTResult.IsDisposed = False Then
        '    fftwOffAxisIFFTResult.Dispose()
        'End If

        bolConnectionOpen = False

    End Sub

    Public Overridable Sub Save(ByVal strFilename As String)

        Dim writer As New IO.StreamWriter(strFilename)

        writer.WriteLine("Image Viewport x" + vbTab + OffAxisImageViewport.X.ToString)
        writer.WriteLine("Image Viewport y" + vbTab + OffAxisImageViewport.Y.ToString)
        writer.WriteLine("Image Viewport width" + vbTab + OffAxisImageViewport.Width.ToString)
        writer.WriteLine("Image Viewport height" + vbTab + OffAxisImageViewport.Height.ToString)
        writer.WriteLine("Image padded width" + vbTab + ImagePaddedWidth.ToString)

        writer.WriteLine("FFT Viewport x" + vbTab + OffAxisFFTViewport.X.ToString)
        writer.WriteLine("FFT Viewport y" + vbTab + OffAxisFFTViewport.Y.ToString)
        writer.WriteLine("FFT Viewport width" + vbTab + OffAxisFFTViewport.Width.ToString)
        writer.WriteLine("FFT Viewport height" + vbTab + OffAxisFFTViewport.Height.ToString)
        writer.WriteLine("FFT padded width" + vbTab + FFTPaddedWidth.ToString)

        writer.WriteLine("Exposure" + vbTab + Exposure.ToString)

        writer.Close()

    End Sub
    Public Overridable Sub Load(ByVal strFilename As String)

        Dim reader As New IO.StreamReader(strFilename)

        OffAxisImageViewport.X = CInt(Split(reader.ReadLine, vbTab)(1))
        OffAxisImageViewport.Y = CInt(Split(reader.ReadLine, vbTab)(1))
        OffAxisImageViewport.Width = CInt(Split(reader.ReadLine, vbTab)(1))
        OffAxisImageViewport.Height = CInt(Split(reader.ReadLine, vbTab)(1))
        ImagePaddedWidth = CInt(Split(reader.ReadLine, vbTab)(1))

        OffAxisFFTViewport.X = CInt(Split(reader.ReadLine, vbTab)(1))
        OffAxisFFTViewport.Y = CInt(Split(reader.ReadLine, vbTab)(1))
        OffAxisFFTViewport.Width = CInt(Split(reader.ReadLine, vbTab)(1))
        OffAxisFFTViewport.Height = CInt(Split(reader.ReadLine, vbTab)(1))
        FFTPaddedWidth = CInt(Split(reader.ReadLine, vbTab)(1))

        Exposure = CDbl(Split(reader.ReadLine, vbTab)(1))

        reader.Close()

    End Sub

#Region "Image Acquisition"
    Public MustOverride ReadOnly Property ImageWidth As Integer
    Public MustOverride ReadOnly Property ImageHeight As Integer
    Public MustOverride Property Exposure As Double
    Public MustOverride Function GetIntegerImage(Optional ByVal NoFrames As Integer = -1) As Integer(,)
    Public MustOverride Sub SaveImage(ByVal Filename As String)
    Public Function GetHDRImage() As Double(,)
        Call Err.Raise(-1, strInstrumentName, "Function has not yet been implemented")
        Return Nothing
    End Function
    Public Function GetDoubleImage() As Double(,)

        Dim Img(,) As Integer
        Dim RetVal(,) As Double

        Img = GetIntegerImage()
        ReDim RetVal(UBound(Img, 1), UBound(Img, 2))

        For i = 0 To UBound(Img, 1)
            For j = 0 To UBound(Img, 2)
                RetVal(i, j) = CDbl(Img(i, j))
            Next
        Next

        Return RetVal

    End Function
    Public Function GetPercentageSaturatedPixels() As Double

        Dim Img(,) As Integer
        Dim TotalCount As Long = 0
        Dim SaturatedCount As Long = 0

        Img = GetIntegerImage(3)

        For i = 0 To UBound(Img, 1)
            For j = 0 To UBound(Img, 2)
                If Img(i, j) > 230 Then
                    SaturatedCount += 1
                End If
                TotalCount += 1
            Next
        Next

        Return SaturatedCount / TotalCount * 100

    End Function
#End Region

#Region "Off-axis holography routines"

    Public OffAxisImageViewport As Rectangle
    Public OffAxisFFTViewport As Rectangle

    Public ImagePaddedWidth As Integer
    Public FFTPaddedWidth As Integer

    Public fftwOffAxisFFTInput As FFTW.NET.AlignedArrayComplex
    Public fftwOffAxisFFTResult As FFTW.NET.AlignedArrayComplex
    Public fftwOffAxisIFFTInput As FFTW.NET.AlignedArrayComplex
    Public fftwOffAxisIFFTResult As FFTW.NET.AlignedArrayComplex

    Public Function GetOffAxisCroppedImage() As Integer(,)

        Dim Img(,) As Integer = GetIntegerImage()
        ImageProcessing.Crop(Img, OffAxisImageViewport)
        Return Img

    End Function
    Public Function GetOffAxisFFTImage() As Numerics.Complex(,)

        Dim Img(,) As Integer = GetOffAxisCroppedImage()
        Dim RetVal(,) As Numerics.Complex = Nothing
        TypeConversions.RealToComplex(Img, RetVal)

        If ImagePaddedWidth > Img.GetLength(0) And ImagePaddedWidth > Img.GetLength(1) Then
            ImageProcessing.ZeroPad(RetVal, ImagePaddedWidth)  ' Note, this will only happen if the original image is smaller than the padded width for both axes.
        End If

        ImageProcessing.FFTShift(RetVal)
        ImageProcessing.FFT(RetVal, fftwOffAxisFFTInput, fftwOffAxisFFTResult, True)
        ImageProcessing.FFTShift(RetVal)
        Return RetVal

    End Function
    Public Function GetOffAxisCroppedFFTImage() As Numerics.Complex(,)

        Dim Img(,) As Numerics.Complex = GetOffAxisFFTImage()
        ImageProcessing.Crop(Img, OffAxisFFTViewport)
        Return Img

    End Function
    Public Function GetOffAxisComplexImage() As Numerics.Complex(,)

        Dim Img(,) As Numerics.Complex

        'Get FFT-IFFT image
        Img = GetOffAxisCroppedFFTImage()
        If FFTPaddedWidth > Img.GetLength(0) And FFTPaddedWidth > Img.GetLength(1) Then
            ImageProcessing.ZeroPad(Img, FFTPaddedWidth) ' Note, this will only happen if the original image is smaller than the padded width for both axes.
        End If
        ImageProcessing.FFTShift(Img)
        ImageProcessing.IFFT(Img, fftwOffAxisIFFTInput, fftwOffAxisIFFTResult, True)
        ImageProcessing.FFTShift(Img)

        'Outer rim seems to be junk when we do this. Remove it.
        Dim CropViewPort As Rectangle
        If FFTPaddedWidth = 0 Then
            CropViewPort = New Rectangle(1, 1, OffAxisFFTViewport.Width - 2, OffAxisFFTViewport.Height - 2)
        Else
            CropViewPort = New Rectangle(1, 1, FFTPaddedWidth - 2, FFTPaddedWidth - 2)
        End If
        ImageProcessing.Crop(Img, CropViewPort)

        Return Img

    End Function
    Public Function CalculateOffAxisComplexImage(ByVal Img(,) As Integer) As Numerics.Complex(,)

        Dim ComplexImg(,) As Numerics.Complex = Nothing

        ImageProcessing.Crop(Img, OffAxisImageViewport)
        TypeConversions.RealToComplex(Img, ComplexImg)
        ImageProcessing.FFTShift(ComplexImg)
        ImageProcessing.FFT(ComplexImg, fftwOffAxisFFTInput, fftwOffAxisFFTResult, True)
        ImageProcessing.FFTShift(ComplexImg)
        ImageProcessing.Crop(ComplexImg, OffAxisFFTViewport)
        If FFTPaddedWidth > ComplexImg.GetLength(0) And FFTPaddedWidth > ComplexImg.GetLength(1) Then
            ImageProcessing.ZeroPad(ComplexImg, FFTPaddedWidth) ' Note, this will only happen if the original image is smaller than the padded width for both axes.
        End If
        ImageProcessing.FFTShift(ComplexImg)
        ImageProcessing.IFFT(ComplexImg, fftwOffAxisIFFTInput, fftwOffAxisIFFTResult, True)
        ImageProcessing.FFTShift(ComplexImg)

        'Outer rim seems to be junk when we do this. Remove it.
        Dim CropViewPort As Rectangle
        If FFTPaddedWidth = 0 Then
            CropViewPort = New Rectangle(1, 1, OffAxisFFTViewport.Width - 2, OffAxisFFTViewport.Height - 2)
        Else
            CropViewPort = New Rectangle(1, 1, FFTPaddedWidth - 2, FFTPaddedWidth - 2)
        End If
        ImageProcessing.Crop(ComplexImg, CropViewPort)

        Return ComplexImg

    End Function
    Public Function CalculateOffAxisComplexImage(ByVal Img As List(Of Integer(,))) As Numerics.Complex(,)

        Dim ComplexImg(,) As Numerics.Complex = Nothing
        Dim DoubleImg(,) As Double = Nothing

        TypeConversions.ImageListToImage(Img, DoubleImg)
        TypeConversions.RealToComplex(DoubleImg, ComplexImg)
        ImageProcessing.Crop(ComplexImg, OffAxisImageViewport)
        ImageProcessing.FFTShift(ComplexImg)
        ImageProcessing.FFT(ComplexImg, fftwOffAxisFFTInput, fftwOffAxisFFTResult, True)
        ImageProcessing.FFTShift(ComplexImg)
        ImageProcessing.Crop(ComplexImg, OffAxisFFTViewport)
        ImageProcessing.FFTShift(ComplexImg)
        ImageProcessing.IFFT(ComplexImg, fftwOffAxisIFFTInput, fftwOffAxisIFFTResult, True)
        ImageProcessing.FFTShift(ComplexImg)

        'Outer rim seems to be junk when we do this. Remove it?
        Dim CropViewPort As New Rectangle(1, 1, OffAxisFFTViewport.Width - 2, OffAxisFFTViewport.Height - 2)
        ImageProcessing.Crop(ComplexImg, CropViewPort)

        Return ComplexImg

    End Function

#End Region

End Class
