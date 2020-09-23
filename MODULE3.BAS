Attribute VB_Name = "MODULE3"
' Module3
' Option Explicit
Const mc0006 = 9 ' &H9%
Const mc0008 = 6 ' &H6%
Const mc00B6 = 229 ' &HE5%
Const mc00BE = 16384 ' &H4000%
Const mc00CC = 32 ' &H20%

Sub sub016B(p0038 As Integer)
  If gv0910 Then sub017A
  gv79A6 = gv7970(p0038)
  gv79B6 = p0038
  gv79B8 = gv79A6.M230D
  sub018B
End Sub

Sub sub017A()
Dim l00C0
Dim l00C2 As Integer
Dim l00C4 As Integer
' Const mc00CC = 32 ' &H20%
  If gv79B6 <= 0 Then gv0910 = False
  If Not gv0910 Then Exit Sub
  l00C2 = FreeFile
  Open fn0E8D("", Hex$(gv79B6)) For Binary As l00C2
  l00C4 = mc0008: Put l00C2, 1, l00C4
  l00C4 = gv09EE: Put l00C2, , l00C4
  Put l00C2, , gv7884
  For l00C0 = 1 To gv09EE
    l00C4 = Len(gv7888(l00C0))
    Put l00C2, , l00C4
    If l00C4 Then Put l00C2, , gv7888(l00C0)
  Next l00C0
  Put l00C2, , gv09F0
  l00C0 = 0: Put l00C2, , l00C0
  Close l00C2
  gv0910 = False
End Sub

Sub sub018B()
Dim l0096
Dim l0098 As String
Dim l009A As Integer
Dim l009C As Integer
Dim l009E
Dim l00A0 As Integer
' Const mc00B6 = 229 ' &HE5%
' Const mc00BE = 16384 ' &H4000%
  If gv79B6 <= 0 Then Exit Sub
  l0098 = fn0C3E(gv79B6)
  If gv792A Then
  gv0B68 = gv0B54 + gv79A6.M2329
  Get gv0882, gv0B68, l009C: gv09EA = fn032E(l009C)
  gv09EE = (gv09EA \ 2) - 1
  gv0B68 = gv0B68 + 2
  If gv09EE < 0 Then gv09EE = 0
  ReDim gv7888(gv09EE)
  gv7884 = String$(gv09EE, 0) & Chr$(mc00B6)
  gv09F0 = String$(gv09EE, 0)
  gv0910 = True
  If gv09EA < 6 Then Exit Sub
  sub01E6 l00A0, gc00BA & l0098
  ReDim gv09B8(gv09EE)
  For l0096 = 0 To gv09EE
    Get gv0882, , gv09B8(l0096)
  Next l0096
  l009A = FreeFile
  l0098 = fn0E8D("", Hex$(gv79B6))
On Error Resume Next
  If FileLen(l0098) = 0 Then Error 1000
  If Err Then
    On Error GoTo 0
    sub01F5
    sub0205 l00A0
    Exit Sub
  End If
On Error GoTo 0
  Open l0098 For Binary Access Read As l009A
  Get l009A, , l009E
  If l009E < mc0008 Then
    Close l009A
    gv7A4A = l0098
    sub0214 mc00BE
    sub0205 l00A0
    Exit Sub
  End If
  gv0910 = False
  Get l009A, , l009C
  If l009C Then
    If l009C <> gv09EE Then sub01CC
    Get l009A, , gv7884
  End If
  For l0096 = 1 To gv09EE
    Get l009A, , l009C
    If l009C Then
      gv7888(l0096) = Space$(l009C)
      Get l009A, , gv7888(l0096)
    End If
  Next l0096
  Get l009A, , gv09F0
  Close l009A
  sub0205 l00A0
  End If
End Sub

Sub sub019C(p004A As Integer)
  gv7A2A = p004A
  gv7A2C = gv79BC(p004A).M2341
  Get gv0882, fn0D74(gv7A2C), gv79F2
  If gv79BC(p004A).M1A97 Then
    sub009F gv79BC(p004A).M1A97
    sub01AB
  End If
End Sub

Sub sub01BB()
Dim l0060
Dim l0062 As Integer
Dim l0064
Dim l0066
Dim l0068 As String
  gv090E = True
  l0062 = FreeFile
  l0068 = fn0E8D("", "0")
On Error Resume Next
  If FileLen(l0068) = 0 Then Exit Sub
  If Err Then Exit Sub
On Error GoTo 0
  Open l0068 For Binary Access Read As l0062
  Get l0062, , l0066
  If l0066 > 7 Then
    Get l0062, , gv791C
    Screen.MousePointer = 11
    Get l0062, , l0064
    ReDim gv781C(l0064)
    For l0060 = 0 To l0064
      Get l0062, , l0064
      If l0064 Then
        gv781C(l0060) = Space$(l0064)
        Get l0062, , gv781C(l0060)
      End If
    Next l0060
    Get l0062, , l0064
    If l0064 <> gv09B6 + 1 Then sub01CC
    Get l0062, , gv7880
    For l0060 = 0 To gv09B6
      Get l0062, , gv784E(l0060)
    Next
    Get l0062, , l0064
    ReDim gv1002(l0064)
    For l0060 = 1 To l0064
      Get l0062, , gv1002(l0060)
    Next l0060
    Get l0062, , l0064
    ReDim gv1036(l0064)
    For l0060 = 1 To l0064
      Get l0062, , gv1036(l0060)
    Next
    For l0060 = 1 To gv79EE
      Get l0062, , gv79BC(l0060)
    Next
    gv090E = False
    Screen.MousePointer = 0
  End If
  Close l0062
End Sub

Sub sub01D5()
Dim l0090 As Integer
Dim l0092 As Integer
Dim l0094 As Integer
  Screen.MousePointer = 11
  l0092 = FreeFile
  Open fn0E8D("", "0") For Binary Access Write As l0092
  l0090 = mc0006: Put l0092, , l0090
  Put l0092, , gv791C
  l0094 = UBound(gv781C): Put l0092, , l0094
  For l0090 = 0 To l0094
    l0094 = Len(gv781C(l0090)): Put l0092, , l0094
    If l0094 Then Put l0092, , gv781C(l0090)
  Next l0090
  l0094 = Len(gv7880): Put l0092, , l0094
  Put l0092, , gv7880
  For l0090 = 0 To gv09B6
    Put l0092, , gv784E(l0090)
  Next
  l0094 = UBound(gv1002)
  Put l0092, , l0094
  For l0090 = 1 To l0094
    Put l0092, , gv1002(l0090)
  Next
  l0094 = UBound(gv1036)
  Put l0092, , l0094
  For l0090 = 1 To l0094
    Put l0092, , gv1036(l0090)
  Next
  For l0090 = 1 To gv79EE
    Put l0092, , gv79BC(l0090)
  Next
  l0090 = 0: Put l0092, , l0090
  Close l0092
  Screen.MousePointer = 0
  gv090E = False
End Sub

Sub sub0221()
  If gv090E Then sub01D5
  If gv0910 Then sub017A
End Sub

Sub sub022D()
  gv10E4 = 0
  Do Until fn09EE() And mc00CC
  Loop
End Sub

Sub sub023E(p00CE As Long, p00D0 As Long)
  If gv06A8 Then
    p00CE = gv06A8 + fn032E(gv79F2.M1752)
    p00D0 = gv06A8 + fn032E(gv79F2.M177B)
  Else
    p00CE = 0: p00D0 = 0
  End If
End Sub

Sub sub024C()
  gv7970(gv79B6) = gv79A6
  gv090E = True
End Sub

