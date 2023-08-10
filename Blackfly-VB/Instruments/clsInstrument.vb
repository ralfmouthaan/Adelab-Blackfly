'Ralf Mouthaan
'National Physical Laboratory
'
'Abstract class for all instruments

Option Explicit On
Option Strict On

Public MustInherit Class clsInstrument

    Protected Friend _strInstrumentName As String
    Protected Friend bolConnectionOpen As Boolean

    Public Sub New()
        _strInstrumentName = ""
    End Sub


    Public Overridable ReadOnly Property strInstrumentName As String
        Get
            Return _strInstrumentName
        End Get
    End Property

End Class
