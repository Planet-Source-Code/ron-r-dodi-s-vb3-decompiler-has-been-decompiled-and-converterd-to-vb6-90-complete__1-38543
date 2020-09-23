Attribute VB_Name = "MODULE111"
' Module11
' Option Explicit
Const mc0058 = 256 ' &H100%
Const mc0084 = 1 ' &H1%
Const mc00AA = 90 ' &H5A%
Const mc00B8 = 86 ' &H56%

Private Sub sub08D6()
Dim l00BA As Integer
Dim l00BC As Integer
Dim l00BE As String
Dim l00C2 As T17D2
  l00BA = gv0DA2
  Do
    l00BC = gv0DA6(l00BA + gc0DE0)
    gv7970(l00BC).M230D = l00BA
    Get gv0882, fn0D74(l00BA), l00C2
    gv7970(l00BC).M158E = l00C2.M158E
    l00BE = "Module" & Format$(l00BC)
    sub0433 l00BC, l00BE
    l00BA = gv0DA6(l00BA + gc0DE2)
  Loop Until l00BA <= 0
End Sub

Private Sub sub08E3(p0064 As Integer)
Dim l0066
Dim l0068 As Integer
Dim l006A As Integer
Dim l006E As T1B19
Dim l0070 As Integer
Dim l0072 As Integer
Dim l0074 As Long
Dim l0076 As Integer
' Const mc0084 = 1 ' &H1%
  sub0127 p0064
  l0076 = (p0064 - 3) * &H2000
  For l0066 = 1 To gv074C
    l006E = gv071A(l0066)
    If l006E.M1B05 <> 0 Or l006E.M1B23 <> 2 Then
      sub01CC
    Else
      l0074 = fn032E(l006E.M15FE)
      l0068 = l0076 + (l0074 \ 8) - 4
      If gv0DA6(l0068) <> gc0DDA Then sub01CC
      l0070 = gv0DA6(l0068 + gc0DE6)
      l006A = l006E.M1B0D
      If l0070 = 0 Or gv0DA6(l0070) <> gc0DD8 Then
        sub01CC
      Else
        Do
          If gv0DA6(l0068) <> gc0DDA Then sub0214 mc0084
          gv79BC(gv0DA6(l0068 + gc0DE4)).M1A97 = l006A
          Get gv0882, gv06A8 + l0074, l0072
          l0074 = fn032E(l0072)
          l0068 = l0076 + (l0074 \ 8) - 4
        Loop Until l0072 = &HFFFF
      End If
    End If
  Next l0066
End Sub

Sub sub08F1()
Dim l0088 As Integer
Dim l008A As Long
  gv06B0 = 0
  l008A = 0
  sub009F 3
  If gv06B2 <= 3 Then
    Exit Sub
  Else
    For l0088 = 4 To 6
      If (gv0778(l0088).M14F3 And mc0058) = 0 Then Exit For
      sub009F l0088
    Next
    l008A = &H10000 * (l0088 - 4) + gv06AC
  End If
  sub0442
  ReDim gv0DA6(l008A \ 8)
  ReDim gv781C(0)
  gv79A2 = 0
  gv79EE = 0
  gv06B0 = 0
  sub08FC
  ReDim gv7970(1 To gv79A2)
  sub08D6
  sub090B
  ReDim gv79BC(gv79EE)
  sub01BB
  If gv090E Then
    sub091A
  End If
  sub0427
  sub0927
  sub045E
End Sub

Sub sub08FC()
Dim l009A As Integer
Dim l009C
Dim l00A0 As T17D2
Dim l00A2 As Integer
' Const mc00AA = 90 ' &H5A%
  sub01E6 l00A2, gc00BE & gc00CE
  Get gv0882, gv0D9A + mc00AA, l009C
  gv0DA2 = fn0D6C(l009C)
  Get gv0882, fn0D74(gv0DA2), l00A0
  l009C = l00A0.M17FF
  gv0DA2 = fn0D6C(l009C)
  Do
    l009A = fn0D6C(l009C)
    Get gv0882, fn0D74(l009A), l00A0
    gv79A2 = gv79A2 + 1
    gv0DA6(l009A) = gc0DD8
    gv0DA6(l009A + gc0DE0) = gv79A2
    l009C = l00A0.M17FF
    If l009C = &HFFFF Then Exit Do
    gv0DA6(l009A + gc0DE2) = fn0D6C(l009C)
  Loop
  gv0DA6(l009A + gc0DE2) = l009C
  sub0205 l00A2
End Sub

Sub sub090B()
Dim l00AC As Integer
Dim l00AE
Dim l00B0 As Integer
Dim l00B6 As T16F5
' Const mc00B8 = 86 ' &H56%
  Get gv0882, gv0D9A + mc00B8, l00AE
  gv0DA4 = fn0D6C(l00AE)
  Do While l00AE <> &HFFFF
    l00AC = fn0D6C(l00AE)
    Get gv0882, fn0D74(l00AC), l00B6
    gv79EE = gv79EE + 1
    gv0DA6(l00AC + gc0DE4) = gv79EE
    gv0DA6(l00AC + gc0DEA) = (l00B6.M172B \ &H100) And 7
    If l00B6.M1747 And 1 Then
      gv0DA6(l00AC) = gc0DDA
      gv0DA6(l00AC + gc0DEC) = l00B6.M1752
    Else
      gv0DA6(l00AC) = gc0DDC
      gv0DA6(l00AC + gc0DEC) = -1
    End If
    l00B0 = fn0D6C(l00B6.M14FC)
    If gv0DA6(l00B0) <> gc0DD8 Then
      sub01CC
    End If
    gv0DA6(l00AC + gc0DE6) = l00B0
    l00AE = l00B6.M175B
    If l00AE = &HFFFF Then Exit Do
    gv0DA6(l00AC + gc0DE8) = fn0D6C(l00AE)
  Loop
  gv0DA6(l00AC + gc0DE8) = l00AE
End Sub

Private Sub sub091A()
Dim l002C As Integer
Dim l002E As Integer
Dim l0030 As Integer
Dim l0032 As String
Dim l0034 As Integer
Dim l0036 As Integer
Dim l003A As T16F5
' Const mc0058 = 256 ' &H100%
  If gv79EE = 0 Then Exit Sub
  l0030 = fn0D7F(gv0DA4)
  Do While l0030 <> &HFFFF
    l002E = fn0D6C(l0030)
    Get gv0882, fn0D74(l002E), l003A
    l002C = gv0DA6(l002E + gc0DE4)
    gv79BC(l002C).M2341 = l002E
    gv79BC(l002C).M158E = l003A.M158E
    gv79BC(l002C).M230D = gv0DA6(l002E + gc0DE6)
    If l003A.M172B And 2 Then
      l0032 = "fn"
      gv79BC(l002C).M235A = gv0EA2((l003A.M172B \ &H100) And 7)
    Else
      l0032 = "sub"
    End If
    If l003A.M1747 And 1 Then
      gv0DA6(l002E) = gc0DDA
      gv79BC(l002C).M15FE = l003A.M1752
      If l0030 = gv0F48 Then
        l0032 = "Main"
      Else
        l0032 = l0032 & Hex$(l0030)
      End If
    Else
      gv0DA6(l002E) = gc0DDC
      gv79BC(l002C).M15FE = -1
      l0032 = "ext" & l0032 & Hex$(l0030)
    End If
    sub0397 l002C, l0032
    l0030 = l003A.M175B
  Loop
  For l0034 = 3 To 6
    If (gv0778(l0034).M14F3 And mc0058) = 0 Then Exit For
    sub08E3 l0034
  Next
  For l0034 = 1 To gv79A2
    l002E = 0
    l0036 = gv7970(l0034).M230D
    For l002C = 1 To gv79EE
      If gv79BC(l002C).M230D = l0036 Then
        If gv79BC(l002C).M1A97 Then
          l002E = l002E + 1
          gv79BC(l002C).M2364 = l002E
        End If
      End If
    Next
  Next
End Sub

