Attribute VB_Name = "MODULE91"
' Module9
' ' Option Explicit

Function fn088D(p0062 As Integer) As String
Dim l0064 As String
  l0064 = Space$(p0062)
  Get gv0882, , l0064
  fn088D = fn0350(l0064)
End Function

Sub sub03FF(p0050 As Integer)
Dim l0052 As Integer
  gv0D94 = 0
  For l0052 = gv0D80 To gv0D82
    If gv0D4C(l0052).M21FD = p0050 Then
      sub0452 l0052
      Exit Sub
    End If
  Next l0052
End Sub

Sub sub0452(p0054 As Integer)
Dim l0058 As Long
  If p0054 = gv0D94 Then Exit Sub
  gv0D86 = gv0D4C(p0054)
  If gv0D86.M15FE = 0 Then Exit Sub
  gv0D94 = p0054
  gv0D9A = 1 + fn02D0(gv0D86.M15FE, gv090C)
  l0058 = fn02D0(gv0D86.M21F6, gv090C)
  gv0D9E = gv0D9A + l0058
  Seek gv0882, gv0D9A
End Sub

