' F0002.FRM
Option Explicit

Sub control2_Click ()
    ÿ™<.Tag = ""
  Hide
End Sub

Sub control1_Click ()
Dim l001E As String
Dim l0020 As String
  l001E = control4.FileName
  If  l001E = "" Then sub01CC: Exit Sub
  l0020 = control4.Path
  If  Right$(l0020, 1) <> "\" Then l0020 = l0020 & "\"
    ÿ™<.Tag = l0020 & l001E
  Hide
End Sub

Sub control3_Change ()
Dim l002A As Integer
  l002A = control3.ListIndex
  If  l002A < 0 Then Exit Sub
  control4.Pattern = control3.List(l002A)
End Sub

Sub control5_Change ()
  control4.Path = control5.List(control5.ListIndex)
End Sub

Sub control5_Click ()
  control4.Path = control5.List(control5.ListIndex)
End Sub

Sub control6_Change ()
  control5.Path = control6.Drive
End Sub

Sub control4_DblClick ()
  control1.Value = 1
End Sub

Sub Form_Activate ()
  Caption = gc00B6 & Tag
  control4.Path = control5.List(control5.ListIndex)
  control4.Pattern = Tag
End Sub
