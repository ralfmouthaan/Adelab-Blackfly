Public Class frmMain
    Private Sub frmMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim Cam As New clsBlackflyCam("22421982")

        Cam.Startup()
        Cam.Gain = 0
        Cam.Exposure = 1000

        Cam.ExecuteTrigger()
        'Dim bmap As Bitmap = Cam.GetBitmapImage
        'PictureBox.Image = DirectCast(bmap, Image)

        'Dim blah(,) As Integer
        'blah = Cam.GetIntegerImage

        Cam.SaveImage("abc.png")

        Cam.Shutdown()

    End Sub
End Class
