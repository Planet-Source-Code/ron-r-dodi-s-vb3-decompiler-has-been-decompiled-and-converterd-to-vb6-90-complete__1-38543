Attribute VB_Name = "MODULE2"
' Module2
' Option Explicit
Const mc0084 = 1 ' &H1%
Const mc008C = 2 ' &H2%
Const mc008E = 3 ' &H3%
Const mc0090 = 4 ' &H4%
Const mc0092 = 5 ' &H5%
Const mc0094 = 6 ' &H6%
Const mc0096 = 7 ' &H7%
Const mc0098 = 8 ' &H8%
Const mc009A = 9 ' &H9%
Const mc009C = 10 ' &HA%
Const mc009E = 12 ' &HC%
Const mc00A0 = 14 ' &HE%
Const mc00A2 = 15 ' &HF%
Const mc00A4 = 16 ' &H10%

Function fn0142(p0130 As Integer) As Long
  fn0142 = fn02D0(gv0778(p0130).M1B49, gv090A)
End Function

Sub sub0093()
  gv0CC4(mc0084) = "CURSOR"
  gv0CC4(mc008C) = "BITMAP"
  gv0CC4(mc008E) = "ICON"
  gv0CC4(mc0090) = "MENU"
  gv0CC4(mc0092) = "DIALOG"
  gv0CC4(mc0094) = "STRING"
  gv0CC4(mc0096) = "FONTDIR"
  gv0CC4(mc0098) = "FONT"
  gv0CC4(mc009A) = "ACCELERATOR"
  gv0CC4(mc009C) = "RCDATA"
  gv0CC4(mc009E) = "GROUP CURSOR"
  gv0CC4(mc00A0) = "GROUP ICON"
  gv0CC4(mc00A2) = "NAMETABLE"
  gv0CC4(mc00A4) = "VERSIONINFO"
End Sub

Sub sub009F(p00A6 As Integer)
  gv06B0 = p00A6
  If gv06B0 Then
    gv06A8 = 1 + fn0142(p00A6)
    gv06AC = fn032E(gv0778(p00A6).M1B53)
    Seek gv0882, gv06A8
  Else
    gv06A8 = 0
    gv06AC = 0
  End If
End Sub

Sub sub00AE()
  gv08CA.M1836 = 0
  gv0884 = 0
  If gv0882 Then
    Close gv0882
  Else
    gv0882 = FreeFile
  End If
  Open gv0876 For Binary Access Read As gv0882
  Get gv0882, , gv0888
  If gv0888.M1836 <> gc05C6 Or gv0888.M18B0 < &H40 Then Exit Sub
  Get gv0882, gv0888.M18CB + 1, gv08CA
  If gv08CA.M1836 <> gc05C8 Then
    gv08CA.M1836 = 0
    Exit Sub
  End If
  gv090A = 2 ^ gv08CA.M19A3
  sub00BA
  sub00D0
  sub00E1
  sub00F3
  sub0104
  sub0115
End Sub

Sub sub00BA()
Dim l00B8 As Long
Dim l00BA As Integer
Dim l00BC As Integer
Dim l00BE As Integer
Dim l00C2 As T1A4F
Dim l00C6 As T1A3A
  l00B8 = gv0888.M18CB + gv08CA.M18F0 + 1
  gv0672 = 0
  Get gv0882, l00B8, gv09F6: l00BA = Asc(gv09F6)
  While l00BA
    gv0672 = gv0672 + l00BA
    Get gv0882, , gv09F6: l00BC = Asc(gv09F6)
    Select Case l00BC
    Case 0
    Case &HFF
      While l00BA
        Get gv0882, , l00C2
        l00BA = l00BA - 1
      Wend
    Case Else
      While l00BA
        Get gv0882, , l00C6
        l00BA = l00BA - 1
      Wend
    End Select
    Get gv0882, , gv09F6: l00BA = Asc(gv09F6)
  Wend
  ReDim gv0640(gv0672)
  Get gv0882, l00B8, gv09F6: l00BA = Asc(gv09F6)
  l00BE = 0
  While l00BA
    Get gv0882, , gv09F6: l00BC = Asc(gv09F6)
    Select Case l00BC
    Case 0
      l00BE = l00BE + l00BA
    Case &HFF
      While l00BA
        Get gv0882, , l00C2
        gv0640(l00BE).M14F3 = Asc(l00C2.M14F3)
        gv0640(l00BE).M1A97 = Asc(l00C2.M1A65)
        gv0640(l00BE).M15FE = l00C2.M1A47
        l00BA = l00BA - 1
        l00BE = l00BE + 1
      Wend
    Case Else
      While l00BA
        Get gv0882, , l00C6
        gv0640(l00BE).M14F3 = Asc(l00C6.M14F3)
        gv0640(l00BE).M1A97 = l00BC
        gv0640(l00BE).M15FE = l00C6.M1A47
        l00BA = l00BA - 1
        l00BE = l00BE + 1
      Wend
    End Select
    Get gv0882, , gv09F6: l00BA = Asc(gv09F6)
  Wend
End Sub

Sub sub00D0()
Dim l0110
  gv06B2 = gv08CA.M1949
  ReDim gv0778(gv06B2)
  Seek gv0882, gv0888.M18CB + gv08CA.M1964 + 1
  For l0110 = 1 To gv06B2
    Get gv0882, , gv0778(l0110)
  Next l0110
End Sub

Sub sub00E1()
Dim l00E6 As T20DB
Dim l00EA As T21ED
Dim l00EC As Long
Dim l00EE
Dim l00F0 As Integer
Dim l00F2 As Integer
Dim l00F4
Dim l00F6 As Integer
  If gv08CA.M196D = gv08CA.M1976 Then Exit Sub
  l00EC = gv0888.M18CB + gv08CA.M196D + 1
  Get gv0882, l00EC, gv090C
  gv090C = 2 ^ gv090C
  ReDim gv0C90(0): gv0CC2 = 0
  ReDim gv0D4C(0): gv0D7E = 0
  Do
    Get gv0882, , l00E6
    If l00E6.M15F7 = 0 Then Exit Do
    gv0CC2 = gv0CC2 + 1
    ReDim Preserve gv0C90(gv0CC2)
    gv0C90(gv0CC2).M15F7 = l00E6.M15F7
    gv0C90(gv0CC2).M20E4 = l00E6.M20E4
    gv0C90(gv0CC2).M20F6 = gv0D7E
    l00F0 = l00E6.M20E4
    If l00E6.M15F7 = (mc009C Or &H8000) Then
      gv0D80 = gv0D7E + 1
      gv0D82 = gv0D7E + l00F0
    End If
    ReDim Preserve gv0D4C(gv0D7E + l00F0)
    While l00F0 > 0
      gv0D7E = gv0D7E + 1
      Get gv0882, , l00EA
      gv0D4C(gv0D7E).M15F7 = l00E6.M15F7
      gv0D4C(gv0D7E).M21F6 = l00EA.M21F6
      gv0D4C(gv0D7E).M14F3 = l00EA.M14F3
      gv0D4C(gv0D7E).M21FD = l00EA.M21FD
      gv0D4C(gv0D7E).M15FE = l00EA.M15FE
      l00F0 = l00F0 - 1
    Wend
  Loop
  For l00EE = 1 To gv0CC2
    l00F6 = gv0C90(l00EE).M15F7
    If l00F6 < 0 Then
    Else
      Get gv0882, l00EC + l00F6, gv09F6
      gv0C90(l00EE).M1BD4 = Space$(Asc(gv09F6))
      Get gv0882, , gv0C90(l00EE).M1BD4
    End If
    l00F2 = gv0C90(l00EE).M20F6
    For l00F4 = l00F2 + 1 To l00F2 + gv0C90(l00EE).M20E4
      l00F6 = gv0D4C(l00F4).M21FD
      If l00F6 < 0 Then
        gv0D4C(l00F4).M1BD4 = Hex$(l00F6)
      Else
        Get gv0882, l00EC + l00F6, gv09F6
        gv0D4C(l00F4).M1BD4 = Space$(Asc(gv09F6))
        Get gv0882, , gv0D4C(l00F4).M1BD4
      End If
    Next l00F4
  Next l00EE
End Sub

Sub sub00F3()
Dim l00DC As Long
Dim l00DE As String
Dim l00E0
Dim l00E2
  gv080A = 0
  l00DC = gv0888.M18CB + gv08CA.M1976 + 1
  Get gv0882, l00DC, gv09F6
  While Asc(gv09F6)
    gv080A = gv080A + 1
    l00DE = Space$(Asc(gv09F6))
    Get gv0882, , l00DE
    Get gv0882, , l00E0
    Get gv0882, , gv09F6
  Wend
  ReDim gv07D8(gv080A)
  Seek gv0882, l00DC
  For l00E2 = 1 To gv080A
    Get gv0882, , gv09F6
    l00DE = Space$(Asc(gv09F6))
    Get gv0882, , l00DE
    l00E0 = InStr(l00DE, Chr$(0))
    If l00E0 Then l00DE = Left$(l00DE, l00E0 - 1)
    gv07D8(l00E2).M1BD4 = l00DE
    Get gv0882, , gv07D8(l00E2).M1BDC
  Next l00E2
End Sub

Sub sub0104()
Dim l00CC As Long
Dim l00CE As Long
Dim l00D0
Dim l00D2
  l00CE = gv0888.M18CB + gv08CA.M197E + 1
  l00CC = gv0888.M18CB + gv08CA.M1986 + 1
  ReDim gv0842(gv08CA.M1952)
  For l00D2 = 1 To gv08CA.M1952
    Get gv0882, l00CE, l00D0: l00CE = l00CE + 2
    Get gv0882, l00CC + l00D0, gv09F6
    gv0842(l00D2) = Space$(Asc(gv09F6))
    Get gv0882, , gv0842(l00D2)
    If Left$(gv0842(l00D2), 5) = "VBRUN" Then
      gv0884 = Val(Mid$(gv0842(l00D2), 6, 1))
    ElseIf Left$(gv0842(l00D2), 5) = "VB400" Then
      gv0884 = 4
    End If
  Next l00D2
End Sub

Sub sub0115()
Dim l00D4 As Long
Dim l00D6 As String
Dim l00D8
Dim l00DA As Integer
  l00D4 = gv08CA.M198E + 1
  gv0840 = 0
  Get gv0882, l00D4, gv09F6
  While Asc(gv09F6)
    gv0840 = gv0840 + 1
    l00D6 = Space$(Asc(gv09F6) + 2)
    Get gv0882, , l00D6
    Get gv0882, , gv09F6
  Wend
  ReDim gv080E(gv0840)
  Seek gv0882, l00D4
  For l00D8 = 1 To gv0840
    Get gv0882, , gv09F6
    l00D6 = Space$(Asc(gv09F6))
    Get gv0882, , l00D6
    l00DA = InStr(l00D6, Chr$(0))
    If l00DA Then l00D6 = Left$(l00D6, l00DA - 1)
    gv080E(l00D8).M1BD4 = l00D6
    Get gv0882, , gv080E(l00D8).M1BDC
  Next l00D8
End Sub

Sub sub0127(p00AA As Integer)
Dim l00AC
Dim l00B2 As T1AF2
Dim l00B6 As T1B19
  If p00AA = gv06B0 Then Exit Sub
  sub009F p00AA
  If gv0778(p00AA).M14F3 And gc07B2 Then
    Get gv0882, gv06A8 + gv06AC, gv074C
    If gv074C < 0 Then
      gv074C = 0
    End If
    ReDim gv071A(gv074C)
    For l00AC = 1 To gv074C
      Get gv0882, , l00B2
      l00B6.M15FE = l00B2.M15FE
      l00B6.M1B23 = Asc(l00B2.M1AFE)
      l00B6.M1B05 = Asc(l00B2.M1B05)
      l00B6.M1B0D = l00B2.M1B0D
      l00B6.M1B13 = l00B2.M1B13
      gv071A(l00AC) = l00B6
    Next l00AC
  Else
    gv074C = 0
  End If
End Sub

Sub sub0136(p0112 As Integer)
Dim l0114
Dim l0118 As Integer
Dim l011A As Long
Dim l011E As T1B19
Dim l0120 As Integer
Dim l0122 As Integer
Dim l0124 As String
Dim l0126() As Long
  gv06B0 = 0: sub0127 p0112
  Erase gv0674: gv06A6 = 0
  ReDim gv0674(gv06B2, 255)
  ReDim l0126(255) As Long
  For l0114 = 1 To gv074C
    l011E = gv071A(l0114)
    If l011E.M1B05 <> 0 Or l011E.M1B23 <> 2 Then
    Else
      l011A = fn032E(l011E.M15FE)
      gv06A6 = 1: l0124 = Chr$(1)
      Do
        gv06A6 = gv06A6 + 1
        Get gv0882, gv06A8 + l011A - 2, l0118: l011A = fn032E(l0118)
        l0126(gv06A6) = l011A
        For l0120 = 1 To Len(l0124)
          If l0126(Asc(Mid$(l0124, l0120, 1))) > l011A Then Exit For
        Next l0120
        l0124 = Left$(l0124, l0120 - 1) & Chr$(gv06A6) & Mid$(l0124, l0120)
      Get gv0882, , l0118
      l011A = fn032E(l0118)
      Loop Until l0118 = &HFFFF
      For l0120 = 1 To Len(l0124)
        l0122 = Asc(Mid$(l0124, l0120, 1))
        gv0674(l011E.M1B0D, l0120) = fn02D8(l0126(l0122))
      Next l0120
      gv0674(l011E.M1B0D, 0) = Len(l0124)
    End If
  Next l0114
End Sub

