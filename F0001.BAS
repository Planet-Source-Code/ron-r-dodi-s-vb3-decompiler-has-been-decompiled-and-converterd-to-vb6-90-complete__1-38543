' F0001.FRM
Option Explicit

Sub control1_Click ()
  gv7A8C = control4.Path
  If  Right$(gv7A8C, 1) <> "\" Then gv7A8C = gv7A8C & "\"
  Hide
End Sub

Sub control2_Click ()
Dim l0022 As String
Dim l0024 As String
  l0022 = control3
  If  Len(l0022) = 0 Then Beep: Exit Sub
  l0024 = control4.List(control4.ListIndex)
  If  Right$(l0024, 1) <> "\" Then l0024 = l0024 & "\"
  l0024 = l0024 & l0022
On Error Resume Next
  MkDir l0024
  control4.Path = l0024
  If  Err Then Beep: Exit Sub
  gv7A8C = l0024 & "\"
End Sub

Sub control6_Change ()
  control4.Path = control5.Drive
End Sub
