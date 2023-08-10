' Ralf Mouthaan
' University of Cambridge
' January 2019
'
' Module to perform all FFTW calculations.
' Uses FFTW.NET bindings To FFTW library For this.
' All inputs/outputs are in terms of Numerics.Complex data structures
' Also does some cropping, zero padding and complex number operations

Option Explicit On
Option Strict On

Namespace ImageProcessing
    Module modImageProcessing
        Public Sub FFT(ByRef Img(,) As Numerics.Complex,
                       Optional ByRef fftwInput As FFTW.NET.AlignedArrayComplex = Nothing,
                       Optional ByRef fftwResult As FFTW.NET.AlignedArrayComplex = Nothing,
                       Optional ByVal bolOverwriteFFTW As Boolean = False)

            'bolOverwriteFFTW replaces the passed-in FFTW structures with new structures.

            Dim intFFTWidth As Integer = Img.GetLength(0)
            Dim intFFTHeight As Integer = Img.GetLength(1)
            Dim Divisor As Double = Math.Sqrt(intFFTWidth * intFFTHeight)

            'Set input and output arrays
            If fftwInput Is Nothing OrElse fftwInput.IsDisposed = True Then
                fftwInput = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If
            If fftwResult Is Nothing OrElse fftwResult.IsDisposed = True Then
                fftwResult = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If
            If fftwInput.GetLength(0) <> intFFTWidth OrElse fftwInput.GetLength(1) <> intFFTHeight Then
                fftwInput.Dispose()
                fftwInput = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If
            If fftwResult.GetLength(0) <> intFFTWidth OrElse fftwResult.GetLength(1) <> intFFTHeight Then
                fftwResult.Dispose()
                fftwResult = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If

            'Load data into arrays
            For i = 0 To intFFTWidth - 1
                For j = 0 To intFFTHeight - 1
                    fftwInput(i, j) = Img(i, j)
                Next
            Next

            ' Perform FFT
            FFTW.NET.DFT.FFT(fftwInput, fftwResult)

            ' Return data in appropriate format 
            For i = 0 To intFFTWidth - 1
                For j = 0 To intFFTHeight - 1
                    Img(i, j) = New Numerics.Complex(fftwResult(i, j).Real, fftwResult(i, j).Imaginary)
                    Img(i, j) = Img(i, j) / Divisor
                Next
            Next

            'If arrays are not kept, we need to dispose of them.
            If bolOverwriteFFTW = False Then
                fftwInput.Dispose()
                fftwResult.Dispose()
            End If

        End Sub
        Public Sub IFFT(ByRef Img(,) As Numerics.Complex,
                       Optional ByRef fftwInput As FFTW.NET.AlignedArrayComplex = Nothing,
                       Optional ByRef fftwResult As FFTW.NET.AlignedArrayComplex = Nothing,
                        Optional ByVal bolOverwriteFFTW As Boolean = False)

            Dim intFFTWidth As Integer = Img.GetLength(0)
            Dim intFFTHeight As Integer = Img.GetLength(1)
            Dim Divisor As Double = Math.Sqrt(intFFTWidth * intFFTHeight)

            'Set input and output arrays
            If fftwInput Is Nothing OrElse fftwInput.IsDisposed = True Then
                fftwInput = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If
            If fftwResult Is Nothing OrElse fftwResult.IsDisposed = True Then
                fftwResult = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If
            If fftwInput.GetLength(0) <> intFFTWidth OrElse fftwInput.GetLength(1) <> intFFTHeight Then
                fftwInput.Dispose()
                fftwInput = New FFTW.NET.AlignedArrayComplex(16, intFFTWidth, intFFTHeight)
            End If
            If fftwResult.GetLength(0) <> intFFTWidth OrElse fftwResult.GetLength(1) <> intFFTHeight Then
                fftwResult.Dispose()
                fftwResult = New FFTW.NET.AlignedArrayComplex(16, fftwInput.GetSize)
            End If

            For i = 0 To intFFTWidth - 1
                For j = 0 To intFFTHeight - 1
                    fftwInput(i, j) = Img(i, j)
                Next
            Next

            ' Perform FFT
            FFTW.NET.DFT.IFFT(fftwInput, fftwResult)

            ' Return data in appropriate format
            For i = 0 To intFFTWidth - 1
                For j = 0 To intFFTHeight - 1
                    Img(i, j) = New Numerics.Complex(fftwResult(i, j).Real, fftwResult(i, j).Imaginary)
                    Img(i, j) = Img(i, j) / Divisor
                Next
            Next

            'If arrays are not kept, we need to dispose of them.
            If bolOverwriteFFTW = False Then
                fftwInput.Dispose()
                fftwResult.Dispose()
            End If

        End Sub
        Public Sub FFTShift(ByRef Img(,) As Numerics.Complex)

            Dim width As Integer = Img.GetLength(0)
            Dim height As Integer = Img.GetLength(1)
            Dim Input(width - 1, height - 1) As Numerics.Complex
            Dim Output(width - 1, height - 1) As Numerics.Complex

            If width Mod 2 <> 0 Then Call Err.Raise(-1, "FFT Shift", "FFT Shift assumes even number of elements")
            If height Mod 2 <> 0 Then Call Err.Raise(-1, "FFT Shift", "FFT Shift assumes even number of elements")

            Array.Copy(Img, Input, Img.Length)

            'Perform FFT shift
            Parallel.For(0, CInt(width / 2),
                Sub(i)
                    For j = 0 To CInt(height / 2) - 1
                        Output(CInt(width / 2) + i, CInt(height / 2) + j) = Input(i, j)
                        Output(i, j) = Input(CInt(width / 2) + i, CInt(height / 2) + j)
                        Output(CInt(width / 2) + i, j) = Input(i, CInt(height / 2) + j)
                        Output(i, CInt(height / 2) + j) = Input(CInt(width / 2) + i, j)
                    Next
                End Sub)

            Array.Copy(Output, Img, Output.Length)

        End Sub

        Public Sub Conjugate(ByRef Img(,) As Numerics.Complex)

            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    Img(i, j) = Numerics.Complex.Conjugate(Img(i, j))
                Next
            Next

        End Sub
        Public Sub ZeroPad(ByRef Img(,) As Numerics.Complex, ByVal NewSize As Integer)

            Dim OldSizeX As Integer = Img.GetLength(0)
            Dim OldSizeY As Integer = Img.GetLength(1)
            Dim OffsetX As Integer = CInt(NewSize / 2) - CInt(OldSizeX / 2)
            Dim OffsetY As Integer = CInt(NewSize / 2) - CInt(OldSizeY / 2)

            Dim Input(Img.GetLength(0) - 1, Img.GetLength(1) - 1) As Numerics.Complex
            Dim Output(NewSize - 1, NewSize - 1) As Numerics.Complex

            Array.Copy(Img, Input, Img.Length)

            Parallel.For(0, OldSizeX - 1,
            Sub(i)
                For j = 0 To OldSizeY - 1
                    Output(OffsetX + i, OffsetY + j) = New Numerics.Complex(Input(i, j).Real, Input(i, j).Imaginary)
                Next
            End Sub)

            ReDim Img(NewSize - 1, NewSize - 1)
            Array.Copy(Output, Img, Output.Length)

        End Sub
        Public Sub ZeroPad(ByRef Img(,) As Numerics.Complex, ByVal NewSizeX As Integer, ByVal NewSizeY As Integer)

            Console.WriteLine("NEED TO PARALELLISE THIS ROUTINE")

            Dim OldSizeX As Integer = Img.GetLength(0)
            Dim OldSizeY As Integer = Img.GetLength(1)
            Dim OffsetX As Integer = CInt(NewSizeX / 2) - CInt(OldSizeX / 2)
            Dim OffsetY As Integer = CInt(NewSizeY / 2) - CInt(OldSizeY / 2)

            Dim Input(OldSizeX - 1, OldSizeY - 1) As Numerics.Complex
            For i = 0 To OldSizeX - 1
                For j = 0 To OldSizeY - 1
                    Input(i, j) = Img(i, j)
                Next
            Next

            ReDim Img(NewSizeX - 1, NewSizeY - 1)
            For i = 0 To OldSizeX - 1
                For j = 0 To OldSizeY - 1
                    Img(OffsetX + i, OffsetY + j) = New Numerics.Complex(Input(i, j).Real, Input(i, j).Imaginary)
                Next
            Next

        End Sub
        Public Sub Crop(ByRef Img(,) As Integer, ByVal Viewport As Rectangle)

            Dim Input(Img.GetLength(0) - 1, Img.GetLength(1) - 1) As Integer
            Dim Output(Viewport.Width - 1, Viewport.Height - 1) As Integer

            Array.Copy(Img, Input, Img.Length)

            ' Crop image based on viewport
            Parallel.For(Viewport.Left, Viewport.Right,
                Sub(i)
                    For j = Viewport.Top To Viewport.Bottom - 1
                        Output(i - Viewport.Left, j - Viewport.Top) = Input(i, j)
                    Next
                End Sub)

            ReDim Img(Viewport.Width - 1, Viewport.Height - 1)
            Array.Copy(Output, Img, Output.Length)

        End Sub
        Public Sub Crop(ByRef Img(,) As Double, ByVal Viewport As Rectangle)

            Dim Input(Img.GetLength(0) - 1, Img.GetLength(1) - 1) As Double
            Dim Output(Viewport.Width - 1, Viewport.Height - 1) As Double

            Array.Copy(Img, Input, Img.Length)

            ' Crop image based on viewport
            Parallel.For(Viewport.Left, Viewport.Right,
                Sub(i)
                    Parallel.For(Viewport.Top, Viewport.Bottom,
                        Sub(j)
                            Output(i - Viewport.Left, j - Viewport.Top) = Input(i, j)
                        End Sub)
                End Sub)

            ReDim Img(Viewport.Width - 1, Viewport.Height - 1)
            Array.Copy(Output, Img, Output.Length)

        End Sub
        Public Sub Crop(ByRef Img(,) As Numerics.Complex, ByVal Viewport As Rectangle)

            Dim Input(Img.GetLength(0) - 1, Img.GetLength(1) - 1) As Numerics.Complex
            Dim Output(Viewport.Width - 1, Viewport.Height - 1) As Numerics.Complex

            'Create a local input array
            Array.Copy(Img, Input, Img.Length)

            ' Crop image based on viewport
            Parallel.For(Viewport.Left, Viewport.Right,
                Sub(i)
                    For j = Viewport.Top To Viewport.Bottom - 1
                        Output(i - Viewport.Left, j - Viewport.Top) = Input(i, j)
                    Next
                End Sub)

            'Copy output array back to Img
            ReDim Img(Viewport.Width - 1, Viewport.Height - 1)
            Array.Copy(Output, Img, Output.Length)

        End Sub
        Public Sub Multiply(ByRef Img(,) As Numerics.Complex, ByVal Multiplier As Numerics.Complex)

            Dim Input(Img.GetLength(0) - 1, Img.GetLength(1) - 1) As Numerics.Complex
            Dim Output(Img.GetLength(0) - 1, Img.GetLength(1) - 1) As Numerics.Complex

            'Create a local input array
            Array.Copy(Img, Input, Img.Length)

            'Multiply
            Parallel.For(0, Input.GetLength(0),
                Sub(i)
                    For j = 0 To Input.GetLength(1) - 1
                        Output(i, j) = Input(i, j) * Multiplier
                    Next
                End Sub)

            'Copy output array back to Img
            Array.Copy(Output, Img, Output.Length)

        End Sub
        Public Sub Divide(ByRef Img(,) As Numerics.Complex, ByVal Divisor As Double(,))

            If UBound(Img, 1) <> UBound(Divisor, 1) Then
                Call Err.Raise(-1, "", "Images of different length")
            End If
            If UBound(Img, 2) <> UBound(Divisor, 2) Then
                Call Err.Raise(-1, "", "Images of different length")
            End If

            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    If Divisor(i, j) = 0 Then
                        Img(i, j) = 0 'False, but this will do for my purposes.
                    Else
                        Img(i, j) = Img(i, j) / Divisor(i, j)
                    End If
                Next
            Next

        End Sub
        Public Sub Divide(ByRef Img(,) As Numerics.Complex, ByVal Divisor As Numerics.Complex(,))

            If UBound(Img, 1) <> UBound(Divisor, 1) Then
                Call Err.Raise(-1, "", "Images of different length")
            End If
            If UBound(Img, 2) <> UBound(Divisor, 2) Then
                Call Err.Raise(-1, "", "Images of different length")
            End If

            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    If Divisor(i, j) = 0 Then
                        Img(i, j) = 0 'False, but this will do for my purposes.
                    Else
                        Img(i, j) = Img(i, j) / Divisor(i, j)
                    End If
                Next
            Next

        End Sub
        Public Sub Sum(ByVal Img1(,) As Numerics.Complex, ByVal Img2(,) As Numerics.Complex, ByRef Result(,) As Numerics.Complex)

            If UBound(Img1, 1) <> UBound(Img2, 1) Then
                Call Err.Raise(-1, "", "Images of different length")
            End If
            If UBound(Img2, 2) <> UBound(Img2, 2) Then
                Call Err.Raise(-1, "", "Images of different length")
            End If

            ReDim Result(UBound(Img1, 1), UBound(Img1, 2))

            For i = 0 To UBound(Img1, 1)
                For j = 0 To UBound(Img1, 2)
                    Result(i, j) = Img1(i, j) + Img2(i, j)
                Next
            Next

        End Sub
        Public Sub Sqrt(ByRef Input(,) As Integer, ByRef Output(,) As Double)

            ReDim Output(UBound(Input, 1), UBound(Input, 2))

            For i = 0 To UBound(Input, 1)
                For j = 0 To UBound(Input, 2)
                    Output(i, j) = Math.Sqrt(Input(i, j))
                Next
            Next

        End Sub
        Public Sub Magnitude(ByRef Img(,) As Numerics.Complex, ByRef Mag(,) As Double)

            ReDim Mag(Img.GetLength(0) - 1, Img.GetLength(1) - 1)

            For i = 0 To Img.GetLength(0) - 1
                For j = 0 To Img.GetLength(1) - 1
                    Mag(i, j) = Img(i, j).Magnitude
                Next
            Next

        End Sub
        Public Sub Subtract(ByRef Img1(,) As Integer, ByVal Img2(,) As Integer)

            If UBound(Img1, 1) <> UBound(Img2, 1) Then Call Err.Raise(-1, "Image Processing - Subtraction", "Images are different sizes")
            If UBound(Img1, 2) <> UBound(Img2, 2) Then Call Err.Raise(-1, "Image Processing - Subtraction", "Images are different sizes")

            For i = 0 To UBound(Img1, 1)
                For j = 0 To UBound(Img1, 2)
                    Img1(i, j) -= Img2(i, j)
                Next
            Next

        End Sub
        Public Sub Subtract(ByRef Img1(,) As Numerics.Complex, ByVal Img2(,) As Numerics.Complex)

            If UBound(Img1, 1) <> UBound(Img2, 1) Then Call Err.Raise(-1, "Image Processing - Subtraction", "Images are different sizes")
            If UBound(Img1, 2) <> UBound(Img2, 2) Then Call Err.Raise(-1, "Image Processing - Subtraction", "Images are different sizes")

            For i = 0 To UBound(Img1, 1)
                For j = 0 To UBound(Img1, 2)
                    Img1(i, j) -= Img2(i, j)
                Next
            Next

        End Sub
        Public Sub MagSquareSum(ByRef Img(,) As Numerics.Complex, ByRef RetVal As Double)

            RetVal = 0
            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    RetVal += Img(i, j).Magnitude ^ 2
                Next
            Next

        End Sub
        Public Sub MagSquareSum(ByRef Img(,) As Integer, ByRef RetVal As Double)

            RetVal = 0
            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    RetVal += Img(i, j) ^ 2
                Next
            Next

        End Sub
        Public Sub Maximum(ByRef Img(,) As Integer, ByRef RetVal As Integer)

            RetVal = 0
            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    If Img(i, j) > RetVal Then RetVal = Img(i, j)
                Next
            Next

        End Sub
        Public Sub Maximum(ByRef Img(,) As Numerics.Complex, ByRef RetVal As Double)

            RetVal = 0
            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    If Img(i, j).Magnitude > RetVal Then RetVal = Img(i, j).Magnitude
                Next
            Next

        End Sub
        Public Function CalculateAveragePhase(ByVal ComplexImg(,) As Numerics.Complex) As Double

            Dim RealSum As Double = 0, ImagSum As Double = 0
            Dim Input(ComplexImg.GetLength(0) - 1, ComplexImg.GetLength(1) - 1) As Numerics.Complex
            Dim max As Double

            'Copy Img to local array
            Array.Copy(ComplexImg, Input, ComplexImg.Length)

            Parallel.For(0, ComplexImg.GetLength(0),
                         Sub(i)
                             For j = 0 To ComplexImg.GetLength(1) - 1
                                 If ComplexImg(i, j).Magnitude > max Then
                                     max = ComplexImg(i, j).Magnitude
                                 End If
                             Next
                         End Sub)

            Parallel.For(0, ComplexImg.GetLength(0),
                Sub(i)
                    For j = 0 To ComplexImg.GetLength(1) - 1
                        If ComplexImg(i, j).Magnitude > 0.5 * max Then
                            RealSum += ComplexImg(i, j).Real
                            ImagSum += ComplexImg(i, j).Imaginary
                        End If
                    Next
                End Sub)

            Return Math.Atan2(ImagSum, RealSum)

        End Function
        Public Sub CircularFilter(ByRef Input(,) As Numerics.Complex)

            'Applies a circular aperture

            Dim imax As Integer = UBound(Input, 1)
            Dim jmax As Integer = UBound(Input, 2)

            For i = 0 To imax
                For j = 0 To jmax

                    If (2*i / imax - 1) ^ 2 + (2*j / jmax - 1) ^ 2 > 1 Then
                        Input(i, j) = 0
                    End If

                Next
            Next

        End Sub
        Public Sub SigmoidFilter(ByRef Input(,) As Numerics.Complex)

            'DOESN'T WORK AT THE MOMENT

            ' Fits a bespoke sigmoid filter. This filter is of the form
            ' 1/(1+exp(k*(r-r0))) where r = sqrt((x-x0)^2 + (y-y0)^2)
            ' k is pre-defined. x0 and y0 are correspond to the centroid.

            Dim r0 As Double
            Dim x0 As Double = -999, y0 As Double = -999
            Dim SumAX As Double, SumAY As Double, SumA As Double
            Dim r As Double
            Dim k As Double = 2

            'Calculate centroid
            For i = 0 To UBound(Input, 1)
                For j = 0 To UBound(Input, 2)
                    SumAX += i * Input(i, j).Magnitude
                    SumAY += j * Input(i, j).Magnitude
                    SumA += Input(i, j).Magnitude
                Next
            Next
            x0 = SumAX / SumA
            y0 = SumAY / SumA

            'Apply sigmoid filter
            For i = 0 To UBound(Input, 1)
                For j = 0 To UBound(Input, 2)
                    r = Math.Sqrt((i - x0) ^ 2 + (j - y0) ^ 2)
                    Input(i, j) = Input(i, j) / (1 + Math.Exp(k * (r - r0)))
                Next
            Next

        End Sub
        Public Sub HighPassFilter(ByRef Input(,) As Integer)

            For i = 0 To UBound(Input, 1)
                For j = 0 To UBound(Input, 2)
                    Input(i, j) = CInt(Input(i, j) / (1 + (50 / Input(i, j)) ^ 8))
                Next
            Next

        End Sub
        Public Function PowerInImage(ByRef Input(,) As Numerics.Complex) As Double

            Dim Sum As Double

            For i = 0 To UBound(Input, 1)
                For j = 0 To UBound(Input, 2)
                    Sum += Input(i, j).Magnitude ^ 2
                Next
            Next

            Return Sum

        End Function
        Public Sub SaveImgToFile(ByVal Img(,) As Integer, ByVal Filename As String)

            Dim writer As New System.IO.StreamWriter(Filename)

            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    writer.Write(Img(i, j).ToString)
                    If j < UBound(Img, 2) Then writer.Write(vbTab)
                Next
                If i < UBound(Img, 1) Then writer.WriteLine()
            Next

            writer.Close()

        End Sub
        Public Sub SaveImgToFile(ByVal Img(,) As Double, ByVal Filename As String)

            Dim writer As New System.IO.StreamWriter(Filename)

            For i = 0 To UBound(Img, 1)
                For j = 0 To UBound(Img, 2)
                    writer.Write(Img(i, j).ToString("F3"))
                    If j < UBound(Img, 2) Then writer.Write(vbTab)
                Next
                If i < UBound(Img, 1) Then writer.WriteLine()
            Next

            writer.Close()

        End Sub
        Public Sub SaveImgToFile(ByVal Img(,) As Numerics.Complex, ByVal Filename As String)

            Dim writer As New System.IO.StreamWriter(Filename)

            writer.WriteLine(TypeConversions.ImgToString(Img))

            writer.Close()

        End Sub

        Public Sub OverlapIntegral(ByVal Img1(,) As Numerics.Complex, ByVal Img2(,) As Numerics.Complex, ByRef Result As Double)

            If Img1.GetLength(0) <> Img2.GetLength(0) Then Call Err.Raise(-1, "Overlap Integral", "Images must have same dimensions")
            If Img1.GetLength(1) <> Img2.GetLength(1) Then Call Err.Raise(-1, "Overlap Integral", "Images must have same dimensions")

            Dim Numerator As Numerics.Complex = New Numerics.Complex(0, 0)
            Dim Sum1 As Double = 0
            Dim Sum2 As Double = 0

            For i = 1 To Img1.GetLength(0) - 2
                For j = 1 To Img1.GetLength(1) - 2

                    Numerator += Img1(i, j) * Numerics.Complex.Conjugate(Img2(i, j))
                    Sum1 += Img1(i, j).Magnitude ^ 2
                    Sum2 += Img2(i, j).Magnitude ^ 2

                Next
            Next

            Result = Numerator.Magnitude ^ 2 / Sum1 / Sum2

        End Sub
        Public Sub OverlapIntegral(ByVal Img1(,) As Numerics.Complex, ByVal Img2(,) As Double, ByRef Result As Double)

            If Img1.GetLength(0) <> Img2.GetLength(0) Then Call Err.Raise(-1, "Overlap Integral", "Images must have same dimensions")
            If Img1.GetLength(1) <> Img2.GetLength(1) Then Call Err.Raise(-1, "Overlap Integral", "Images must have same dimensions")

            Dim Numerator As Numerics.Complex = New Numerics.Complex(0, 0)
            Dim Sum1 As Double = 0
            Dim Sum2 As Double = 0

            For i = 0 To Img1.GetLength(0) - 1
                For j = 0 To Img1.GetLength(1) - 1

                    Numerator += Img1(i, j) * Img2(i, j) 'Conjugate of a real number is just the real number
                    Sum1 += Img1(i, j).Magnitude ^ 2
                    Sum2 += Img2(i, j) ^ 2

                Next
            Next

            Result = Numerator.Magnitude ^ 2 / Sum1 / Sum2

        End Sub
        Public Sub OverlapIntegral(ByVal Img1(,) As Numerics.Complex, ByVal Img2(,) As Integer, ByRef Result As Double)

            If Img1.GetLength(0) <> Img2.GetLength(0) Then Call Err.Raise(-1, "Overlap Integral", "Images must have same dimensions")
            If Img1.GetLength(1) <> Img2.GetLength(1) Then Call Err.Raise(-1, "Overlap Integral", "Images must have same dimensions")

            Dim Numerator As Numerics.Complex = New Numerics.Complex(0, 0)
            Dim Sum1 As Double = 0
            Dim Sum2 As Double = 0

            For i = 0 To Img1.GetLength(0) - 1
                For j = 0 To Img1.GetLength(1) - 1

                    Numerator += Img1(i, j) * Img2(i, j) 'Conjugate of a real number is just the real number
                    Sum1 += Img1(i, j).Magnitude ^ 2
                    Sum2 += Img2(i, j) ^ 2

                Next
            Next

            Result = Numerator.Magnitude ^ 2 / Sum1 / Sum2

        End Sub

    End Module
End Namespace