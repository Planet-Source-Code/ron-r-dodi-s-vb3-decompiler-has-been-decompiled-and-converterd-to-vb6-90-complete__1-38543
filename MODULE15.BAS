Attribute VB_Name = "MODULE151"
' Module15
' Option Explicit
Const mc00A4 = 2048 ' &H800%
Const mc00F0 = 32 ' &H20%
Const mc00F4 = 8192 ' &H2000%
Const mc00F6 = 1024 ' &H400%
Const mc010C = 2 ' &H2%
Const mc010E = 1 ' &H1%
Const mc0124 = 8 ' &H8%
Const mc0126 = 9 ' &H9%
Const mc0128 = 10 ' &HA%
Const mc012A = 11 ' &HB%
Const mc012C = 12 ' &HC%
Const mc012E = 14 ' &HE%
Const mc0130 = 10 ' &HA%
Const mc0132 = 12 ' &HC%
Const mc0134 = 512 ' &H200%
Const mc0136 = 256 ' &H100%
Const mc0138 = 8 ' &H8%
Const mc013A = 9 ' &H9%
Const mc013C = 13 ' &HD%
Const mc013E = 11 ' &HB%
Const mc020E = 4 ' &H4%
Const mc0210 = 8 ' &H8%
Const mc0228 = 2 ' &H2%
Const mc022C = 1 ' &H1%
Const mc0232 = 1 ' &H1%
Const mc0234 = 2 ' &H2%
Const mc0246 = 3 ' &H3%
Const mc0248 = 1 ' &H1%
Const mc025A = 2 ' &H2%

Sub sub095F(p009A As Integer)
Dim l009C As Integer
' Const mc00A4 = 2048 ' &H800%
  l009C = gv0980(p009A + 2)
  If (l009C And &HFFF0) <> &HC100 Then
    sub0214 mc00A4
    Exit Sub
  End If
  l009C = l009C And &HF
  If l009C = 8 Then
  ElseIf l009C = 0 Then
    l009C = gc78CA
    If Not fn0C1A(gv0980(p009A - 1)) Then
      sub0214 mc00A4
      Exit Sub
    End If
  ElseIf l009C <= 9 Then
  Else
    sub01CC
    Exit Sub
  End If
  sub0BF7 p009A, gc78E0, l009C, gc78D2
End Sub

Sub sub0998(p00A6 As Integer)
Dim l00A8 As Integer
Dim l00AA As Integer
  l00A8 = gv09B8(p00A6 + 2) And &HF
  If l00A8 = 0 Then l00A8 = gc78CA
  Select Case gv09B8(p00A6 + 2) And &HF00
  Case 0: l00AA = gc78E0
    If gv09B8(p00A6) = 0 Then sub01CC
    sub095F fn0326(gv09B8(p00A6), 2)
  Case &H100: l00AA = gc78DC
    If gv09B8(p00A6) Then sub01CC
  Case &H200: l00AA = gc78DC
    If gv09B8(p00A6) Then sub01CC
  Case &H500: l00AA = gc78D8
    If gv09B8(p00A6) < 6 Then sub01CC
  Case &H600: l00AA = gc78DE
    If gv09B8(p00A6) >= 0 Then sub01CC
  Case Else
    sub01CC
  End Select
  sub0BBA p00A6, l00AA
  sub0970 p00A6, 0, l00AA, l00A8, gc78D2
End Sub

Sub sub0BF7(p00B4 As Integer, p00B6 As Integer, p00B8 As Integer, p00BA As Integer)
Dim l00BC As Integer
Dim l00BE As Integer
Dim l00C0 As Integer
  If p00B6 And &H1F Then
    sub01CC
    p00B8 = p00B6 Or p00B8
    p00B6 = p00B6 And gc78C0
    p00B8 = p00B8 Xor p00B6
  End If
  If p00B6 = 0 Then
    sub01CC
    Exit Sub
  End If
  l00BC = Asc(Mid$(gv7880, p00B4, 1))
  If l00BC = 0 And gv0980(p00B4) Then
    p00B6 = gc78E4
  End If
  l00BE = p00B8 Or (p00BA And gc78CE)
  If (p00B6 Or l00BE) = l00BC Then
    Exit Sub
  End If
  l00C0 = l00BC And gc78C0
  l00BC = l00BC Xor l00C0
  If p00B6 < l00C0 Then p00B6 = l00C0
  If l00BE = 0 Then
    l00BE = l00BC
  ElseIf l00BC And l00BC <> l00BE Then
    Exit Sub
  End If
  If (l00BE Or p00B6) = (l00BC Or l00C0) Then
    Exit Sub
  End If
  sub04A7 p00B4, 0, p00B6, p00B8, p00BA
  If p00B8 And gv784E(p00B4) = 0 Then
    sub04B5 fn0C02(p00B4), p00B4
  End If
End Sub

Sub sub04B5(p00C2 As String, p00C4 As Integer)
Dim l00C6 As Integer
  If p00C4 > UBound(gv784E) Then
    p00C2 = "???"
    Exit Sub
  End If
  l00C6 = gv784E(p00C4)
  If l00C6 = 0 Then
    For l00C6 = 3 To UBound(gv781C)
      If Len(gv781C(l00C6)) = 0 Then Exit For
    Next l00C6
    If l00C6 > UBound(gv781C) Then ReDim Preserve gv781C(l00C6 + 10)
    gv784E(p00C4) = l00C6
  End If
  If Len(p00C2) = 0 Then p00C2 = fn0C02(p00C4)
  sub0BC7 fn0336(l00C6, 4), p00C2
End Sub

Sub sub04A7(p00CA As Integer, p00CC As Integer, p00CE As Integer, p00D0 As Integer, p00D2 As Integer)
Dim l00D4 As Integer
Dim l00D6 As Integer
Dim l00DA As Integer
Dim l00DC As Integer
  If p00CA < 3 Or p00CA > gv09B6 Then sub01CC: Exit Sub
  l00D4 = p00CE Or p00D0 Or (p00D2 And gc78CE)
  l00D6 = Asc(Mid$(gv7880, p00CA, 1))
  If l00D6 >= gc78E6 Then
    If (l00D4 Xor l00D6) And &H1F Then
      p00D0 = l00D6 And &HF
      If l00D6 And gc78CE Then p00D2 = gc78D2 Else p00D2 = 0
    End If
    Exit Sub
  End If
  If p00CE = gc78E0 And p00D2 = 0 Then
    Select Case p00D0
    Case 1 To 6
      If p00CC = 0 Then p00CC = fn0C84(gv0980(), p00CA, p00D0, p00D2)
      If p00CC > 0 Then
        For l00D6 = p00CA To p00CA + p00CC - 1
          If gv0980(l00D6) Then
            p00CE = gc78E4
            Exit For
          End If
        Next
      End If
    End Select
  End If
  sub0BDA l00DA, l00DC, p00CE%, p00D0%, p00CA%, True
  If p00CE Then
    If p00CC = 0 Then p00CC = fn0C84(gv0980(), p00CA, p00D0, p00D2)
    If p00D0 And gc78C4 Then
      If gv2326 > 0 Then
        If Asc(Mid$(gv7880, p00CA - 1, 1)) Then
          If Asc(Mid$(gv7880, p00CA - 1, 1)) <> p00D0 Then sub01CC
        End If
      End If
      Mid$(gv7880, p00CA - 1, 1) = Chr$(p00D0)
    End If
    Mid$(gv7880, p00CA, 1) = Chr$(p00CE Or p00D0 Or (p00D2 And gc78CE))
    If p00CE > gc78D6 And p00D0 Then
      If gv784E(p00CA) = 0 Then
        sub04B5 fn0C02(p00CA), p00CA
      End If
    End If
  Else
    Mid$(gv7880, p00CA, 1) = Chr$(p00D0)
  End If
  If p00CC > 1 Then
    Mid$(gv7880, p00CA + 1, p00CC - 1) = String$(p00CC - 1, gc78D0)
    If p00D2 Then Mid$(gv7880, p00CA + 1, 1) = Chr$(p00D2)
  End If
  gv090E = True
End Sub

Sub sub098D(p00E2 As Integer, p00E4 As Integer, p00E6 As Integer, p00E8 As Integer)
Dim l00EA As Integer
Dim l00EC As Integer
Dim l00EE As Integer
' Const mc00F0 = 32 ' &H20%
' Const mc00F4 = 8192 ' &H2000%
' Const mc00F6 = 1024 ' &H400%
  If (p00E4 And &H1F) Then
    sub0214 mc00F0
    p00E6 = (p00E6 Or p00E4) And &HF
    p00E4 = p00E4 And gc78C0
  End If
  If gv792A Then
    Select Case p00E4
    Case gv78C2, gc78E6
      sub0214 mc00F4
      Exit Sub
    Case gc78D6
    Case gc78DC
      If gv09B8(p00E2) Then
        p00E4 = gc78E4
      End If
    Case gc78DE, gc78D8, gc78E4
      If gv09B8(p00E2) = 0 Then
        sub0214 mc00F6
        p00E4 = gc78DC
      End If
    Case gc78E2
      If fn032E(gv09B8(p00E2)) > gv09B2 Or (gv09B8(p00E2) And 1) Then
      Else
        sub0970 p00E2, 0, gc78E2, p00E6, p00E8
      End If
      Exit Sub
    Case Else
      sub0214 mc00F0
      Exit Sub
    End Select
  End If
  l00EA = Asc(Mid$(gv7884, p00E2, 1))
  l00EC = p00E6 Or (p00E8 And gc78CE)
  If (p00E4 Or l00EC) = l00EA Then
    Exit Sub
  End If
  l00EE = l00EA And gc78C0
  l00EA = l00EA Xor l00EE
  If p00E4 < l00EE Then p00E4 = l00EE
  If p00E4 = gc78E0 Then
    l00EA = Asc(Mid$(gv7880, fn0326(gv09B8(p00E2), 2), 1))
    l00EE = l00EA And gc78C0
    l00EA = l00EA Xor l00EE
    If p00E4 < l00EE Then p00E4 = l00EE
  End If
  If l00EC = 0 Then
    l00EC = l00EA
  ElseIf l00EA Then
    If l00EC <> l00EA Then
      sub0214 mc00A4
    End If
    Exit Sub
  End If
  If (l00EC Or p00E4) = (l00EA Or l00EE) Then
    Exit Sub
  End If
  sub0970 p00E2, 0, p00E4, l00EC, p00E8
End Sub

Sub sub097E(p00FC As String, p00FE As Integer)
Dim l0100 As Integer
Dim l0102 As Integer
' Const mc010C = 2 ' &H2%
' Const mc010E = 1 ' &H1%
  If p00FC = "" Then
    sub01CC
    Exit Sub
  End If
  l0102 = Asc(Mid$(gv7884, p00FE, 1))
  Select Case l0102 And gc78C0
  Case gv78C2, gc78E6
    Select Case l0102
    Case gc791A
      l0100 = fn0D6C(gv09B8(p00FE))
      If gv0DA6(l0100) = mc010C Then
        sub0397 gv0DA6(l0100 + mc010E), p00FC
        Exit Sub
      End If
    Case gc7914
      If gv2326 <> gc78BC Then
        sub01CC
      Else
        GoTo L16ADC
      End If
    Case Else
      sub01CC
    End Select
  Case gc78E2
    If gv09B8(p00FE) Then
      sub04B5 p00FC, fn0326(gv09B8(p00FE), 2)
      Exit Sub
    Else
      sub01CC
    End If
  Case gc78E4, gc78DC, gc78DE, gc78D8
L16ADC:
    gv7888(p00FE) = p00FC
  Case Else
    sub01CC
  End Select
End Sub

Sub sub0798(p0110 As Integer, p0112 As Integer, p0114 As Integer)
Dim l0116 As Integer
Dim l0118 As Integer
Dim l011A As Integer
Dim l011C As Integer
Dim l011E As Integer
Dim l0120 As Integer
 Const mc0124 = 8 ' &H8%
 Const mc0126 = 9 ' &H9%
 Const mc0128 = 10 ' &HA%
 Const mc012A = 11 ' &HB%
 Const mc012C = 12 ' &HC%
 Const mc012E = 14 ' &HE%
 Const mc0130 = 10 ' &HA%
 Const mc0132 = 12 ' &HC%
 Const mc0134 = 512 ' &H200%
 Const mc0136 = 256 ' &H100%
 Const mc0138 = 8 ' &H8%
 Const mc013A = 9 ' &H9%
 Const mc013C = 13 ' &HD%
 Const mc013E = 11 ' &HB%
  If p0112 < 8 Then Exit Sub
  l011A = fn0326(p0110, 2)
  If l011A > gv09EE Then
    Exit Sub
  End If
  l0116 = Asc(Mid$(gv7884, l011A, 1))
  l0118 = l0116 And gc78C0
  l0116 = l0116 Xor l0118
    If Len(gv7888(l011A)) Then
      sub0BBA l011A, l0118
      Exit Sub
    End If
  If l0116 And gc78CE Then Exit Sub
  If l0116 > gc78E6 Then Exit Sub
  If l0118 <= gc78D6 Then
    Select Case p0112
    Case mc0124
      l011C = gc78DE
    Case mc0126
      l011C = gc78DC
    Case mc0128
      l011C = gc78E2
    Case mc012A
      l011C = gc78D8
    Case mc012C
      If l0118 > gc78D6 Then Exit Sub
      l011C = gc78D6
    Case mc012E
      If p0114 = mc0130 Then
        sub0998 l011A
        Exit Sub
      ElseIf p0114 = mc0132 Then
        l011C = gc78E6
      End If
      If gv09B8(l011A) = 0 Then
        sub0214 mc0134
        l011C = gc78DC
      ElseIf gv09B8(l011A) And &H8001 Then
        l011C = gc78DE
      Else
        l011C = gc78D8
      End If
    Case Else
      sub01CC
      If l0118 Then Exit Sub
      l011C = gc78D6
    End Select
    sub0BBA l011A, l011C
  Else
    l011C = l0118
  End If
  Select Case p0114
  Case 0
    sub0214 mc0136
  Case mc0138
    l011E = 0
  Case mc013A
    l011E = gc78C8
  Case mc0130
    sub0998 l011A
    Exit Sub
  Case mc013C: l011E = gc78C6
  Case Is < 8
    l011E = p0114
  Case mc013E
    l011E = gc78CA
  Case mc0132
    sub0970 l011A, 1, gc78E6, gc78EC, 0
    Exit Sub
  Case Else
    sub01CC
    Exit Sub
  End Select
  sub098D l011A, l011C, l011E, l0120
End Sub

Sub sub0970(p0140 As Integer, p0142 As Integer, p0144 As Integer, p0146 As Integer, p0148 As Integer)
Dim l014A
Dim l014C As Integer
Dim l014E As Integer
  If p0144 = gc78DC And p0148 = 0 Then
    Select Case p0146
    Case 1 To 6
      If p0142 = 0 Then p0142 = fn0C33(p0140, p0144 Or p0146, p0148)
      If p0142 > 0 Then
        For l014A = p0140 To p0140 + p0142 - 1
          If gv09B8(l014A) Then
            p0144 = gc78E4
            Exit For
          End If
        Next
      End If
    End Select
  End If
  sub0BDA l014C, l014E, p0144, p0146, p0140, False
  If p0144 Then
    If p0142 = 0 Then p0142 = fn0C33(p0140, p0144 Or p0146, p0148)
    If p0146 And gc78C4 Then
      If Asc(Mid$(gv7884, p0140 - 1, 1)) Then
        If Asc(Mid$(gv7884, p0140 - 1, 1)) <> p0146 Then
          If Asc(Mid$(gv7884, p0140 - 1, 1)) <> gc78D0 Then
            If gv2326 > 0 Then
              sub01CC
              Exit Sub
            End If
          End If
        End If
      End If
      Mid$(gv7884, p0140 - 1, 1) = Chr$(p0146)
    End If
    If p0144 = gc78E2 Then sub04A7 fn0326(gv09B8(p0140), 2), 0, gc78E0, p0146, p0148
    Mid$(gv7884, p0140, 1) = Chr$(p0144 Or p0146 Or (p0148 And gc78CE))
    If p0144 > gc78D6 And p0146 Then
      If Len(gv7888(p0140)) = 0 Then
        sub097E fn0C27(p0140), p0140
      End If
    End If
  Else
    Mid$(gv7884, p0140, 1) = Chr$(p0146)
  End If
  If p0142 > 1 Then
    Mid$(gv7884, p0140 + 1, p0142 - 1) = String$(p0142 - 1, gc78D0)
    If p0148 Then Mid$(gv7884, p0140 + 1, 1) = Chr$(p0148)
  End If
  gv0910 = True
End Sub

Sub sub0433(p0152 As Integer, p0154)
Dim l0156 As Integer
  l0156 = gv7970(p0152).M158E
  sub0BC7 l0156, p0154
End Sub

Sub sub0BC7(p0160 As Integer, ByVal p0162 As String)
Dim l0164 As Integer
  l0164 = fn0326(p0160, 4)
  If l0164 > UBound(gv781C) Then ReDim Preserve gv781C(l0164)
  If gv781C(l0164) <> p0162 Then
    gv781C(l0164) = p0162
    gv090E = True
  End If
End Sub

Sub sub0397(p0166 As Integer, p0168)
  sub0BC7 gv79BC(p0166).M158E, p0168
End Sub

Private Sub sub0BDA(p0172 As Integer, p0174 As Integer, p0176 As Integer, p0178 As Integer, p017A As Integer, p017C As Integer)
Dim l017E As Integer
Dim l0180 As Integer
  If gv2326 = 0 Then
    If gv0B86 Then sub01CC
    Exit Sub
  ElseIf gv2326 = gc78BA Then
    If p017C Then p0172 = gv2326
    p0174 = gv2326
    Exit Sub
  End If
  If p017C Then
    l017E = Asc(Mid$(gv7880, p017A, 1))
  Else
    l017E = Asc(Mid$(gv7884, p017A, 1))
  End If
  l0180 = l017E And &H1F
  l017E = l017E Xor l0180
  l0180 = l0180 And &HF
  Select Case l017E
  Case gv78C2
    p0172 = gv2326
  Case gc78D6
    If p0176 > gc78D6 Then p0172 = gv2326
  Case gc78E4
    If p0176 <> l017E Then
      If p0172 <> gc78BC And p0176 <> gc78DC And p0176 <> gc78E0 Then
        GoSub L1759A
      End If
      p0176 = l017E
    End If
  Case gc78E6
      p0176 = l017E
      Exit Sub
    If p0176 <> l017E Then
      GoSub L1759A
      p0176 = l017E
      Exit Sub
    End If
  Case Else
    If p0176 <> l017E Then
      If p0176 <> gc78E4 Then
        GoSub L1759A
        p0176 = l017E
      End If
    End If
  End Select
  If p0178 Then
    If l0180 = 0 Then
      p0174 = gv2326
    ElseIf p0178 <> l0180 Then
      GoSub L1759A
    End If
  Else
    p0178 = l0180
  End If
Exit Sub

L1759A:
  sub0214 mc00A4
Return
End Sub

Sub sub049C(p0184 As Integer)
Dim l0186 As Integer
Dim l0188 As Integer
Dim l018A As Integer
Dim l018C As Integer
Dim l018E As Integer
Dim l0190 As Integer
Dim l0192 As Integer
Dim l0196 As T1583
Dim l019A As T15DC
Dim l019E As T1F4C
  l0188 = fn0326(p0184, 2)
  If Asc(Mid$(gv7880, l0188, 1)) Then
    p0184 = 0
    Exit Sub
  End If
  Get gv0882, gv0B5C + fn032E(p0184), l0196
  sub04A7 l0188, 4, gc790A, 0, 0
  gv784E(l0188) = fn0326(gv0980(l0188), 4)
  sub04B5 "T" & fn02BE(p0184), l0188
  l0186 = l0196.M15B1
  If l0196.M159A And 1 Then
    l018A = fn0326(l0186, 2)
    l0186 = gv0980(l018A - 1)
  End If
  Do While l0186
    l018A = fn0326(l0186, 2)
    Get gv0882, gv0B5C + fn032E(l0186), l019A
    l0192 = l018A + 4
    gv784E(l018A) = fn0326(gv0980(l018A), 4)
    l018C = l019A.M15F7
    If l018C > 9 Or l018C < 0 Then
      l018C = gc790C Or gc78CC
    Else
      l018C = gc790C Or l019A.M15F7
    End If
    If l019A.M15E9 And 1 Then
      l018C = l018C Or gc78CE
      l018E = 6 + (gv0980(l018A + 6) And &HFF) * 2
      sub04A7 l0192, l018E, 0, gc78D2, 0
      l0192 = l0192 + l018E
    End If
    If Asc(Mid$(gv7880, l018A - 1, 1)) = 0 Then
      l018C = l018C Or gc78C4
      If l019A.M15F7 = 8 Then
        l0190 = gc78C6
      ElseIf l019A.M15F7 = 0 Then
        sub01CC
      Else
        sub01CC
      End If
    Else
      l0190 = 0
    End If
    sub04A7 l018A, 4, l018C, l0190, 0
    sub04B5 "M" & fn02BE(l019A.M158E), l018A
    l0186 = l019A.M15E9 And -2
  Loop
  If l0196.M159A And 1 Then
    l0186 = l0196.M15B1
    l018A = fn0326(l0186, 2) - 1
    If l018A <> l0192 Then sub01CC
    Get gv0882, gv0B5C + fn032E(l0186), l019E
    l018E = (6 + l019E.M1D63 * 3) \ 2
    sub04A7 l018A, l018E, 0, gc78D0, 0
    l0192 = l018A + l018E
  End If
  Mid$(gv7880, l0192 - 1, 1) = Chr$(gc790E)
  p0184 = l0196.M159A And -2
End Sub

Function fn0BE7(p01A8 As Integer) As String
Dim l01AA As Integer
Dim l01AC As Integer
  If gv781A Then
    fn0BE7 = fn0C27(p01A8)
    Exit Function
  End If
  l01AA = Len(gv7888(p01A8))
  If l01AA Then
    fn0BE7 = gv7888(p01A8)
    Exit Function
  End If
  l01AC = Asc(Mid$(gv7884, p01A8, 1))
  If l01AC = gc791A Then
    fn0BE7 = fn0C8E(gv0DA6(fn0D6C(gv09B8(p01A8)) + mc010E))
  ElseIf (l01AC And gc78C0) = gc78E2 Then
    l01AC = fn0326(gv09B8(p01A8), 2)
    l01AC = gv784E(l01AC)
    If l01AC Then fn0BE7 = gv781C(l01AC)
  End If
End Function

Sub sub0BA1(p01B0 As Integer, p01B4 As ComboBox, p01B8 As ComboBox, p01BC As ComboBox)
  p01B4.AddItem "<special>"
  p01B4.AddItem "<refd>"
  If p01B0 Then
    p01B4.AddItem "?"
    p01B4.AddItem "?"
    p01B4.AddItem "Member"
  Else
    p01B4.AddItem "Param"
    p01B4.AddItem "Modul"
    p01B4.AddItem "Local"
  End If
  p01B4.AddItem "Global"
  p01B4.AddItem "Const"
  p01B4.AddItem "<fix>"
  p01B8.AddItem ""
  p01B8.AddItem "%"
  p01B8.AddItem "&"
  p01B8.AddItem "!"
  p01B8.AddItem "#"
  p01B8.AddItem "@"
  p01B8.AddItem "Variant"
  p01B8.AddItem "$"
  p01B8.AddItem "-$*"
  p01B8.AddItem "-Object"
  p01B8.AddItem "-Type"
  p01B8.AddItem "+MType"
  p01BC.AddItem ""
  p01BC.AddItem "+"
  p01BC.AddItem "+()"
End Sub

Function fn0BAE(p01C0 As Integer, p01C2 As Integer) As String
Dim l01C4 As Integer
  l01C4 = gv7970(p01C0).M14DF
  l01C4 = gv0F7C(l01C4).M2431
  If l01C4 = 0 Then sub01CC
  fn0BAE = gv0FB6(l01C4 + p01C2).M1BD4
End Function

Function fn0C02(p01D8 As Integer) As String
Dim l01DA As Integer
  If p01D8 <= 0 Or p01D8 > gv09B6 Then
    fn0C02 = "g?" & fn02BE(fn0336(p01D8, 2))
    Exit Function
  End If
  l01DA = gv784E(p01D8)
  If l01DA Then
    fn0C02 = gv781C(l01DA)
    Exit Function
  End If
  l01DA = Asc(Mid$(gv7880, p01D8, 1))
  If l01DA = 0 Or l01DA = gc7918 Then
    If gv0980(p01D8) Then
      l01DA = gc78E4
    Else
      l01DA = gc78E0
    End If
  End If
  Select Case l01DA And gc78C0
  Case gc78E0
    fn0C02 = "gv" & fn02BE(fn0336(p01D8, 2))
  Case gc78E4, gc78E6
    fn0C02 = "gc" & fn02BE(fn0336(p01D8, 2))
  Case Else
    fn0C02 = "g?" & fn02BE(fn0336(p01D8, 2))
  End Select
End Function

Function fn0C0E(p01DE As Integer) As Integer
Dim l01E0 As Integer
Dim l01E2 As Integer
On Error GoTo L183D0
  l01E2 = fn0326(gv09B8(p01DE), 2)
  If l01E2 = 0 Or l01E2 > gv09B6 Then Exit Function
  l01E0 = Asc(Mid$(gv7884, p01DE, 1))
  Select Case l01E0 And gc78C0
  Case gc78E2
    fn0C0E = l01E2
  Case gv78C2, gc78D6
    l01E0 = Asc(Mid$(gv7880, l01E2, 1))
    If l01E0 = 0 Then
      If Asc(Mid$(gv7880, l01E2 - 1, 1)) = 0 Then
        Exit Function
      End If
      fn0C0E = l01E2
    Else
      l01E0 = l01E0 And gc78C0
      If l01E0 = gc78E4 Or l01E0 = gc78E0 Then
        fn0C0E = l01E2
      End If
    End If
  End Select
L183D0:
Exit Function
End Function

Function fn0C1A(ByVal pv01E6 As Integer) As Integer
  pv01E6 = fn0326(pv01E6, 2)
  If pv01E6 > 0 And pv01E6 < gv09B6 Then fn0C1A = Asc(Mid$(gv7880, pv01E6, 1)) = gc790A
End Function

Function fn0C27(p01EA As Integer) As String
Dim l01EC As Integer
Dim l01EE As Integer
Dim l01F0 As Integer
Dim l01F2 As Integer
  l01EC = Len(gv7888(p01EA))
  If l01EC Then
    fn0C27 = gv7888(p01EA)
    Exit Function
  End If
  l01F0 = fn0336(p01EA, 2)
  l01F2 = gv09B8(p01EA)
  l01EE = Asc(Mid$(gv7884, p01EA, 1))
  Select Case l01EE
  Case 0, gc7918
    fn0C27 = "v" & fn02BE(l01F0)
    Exit Function
  Case gc791A
    l01EC = fn0D6C(l01F2)
    If l01EC > UBound(gv0DA6) Then
      Exit Function
    End If
    If gv0DA6(l01EC) < mc010C Then
      Exit Function
    End If
    l01EC = gv0DA6(l01EC + mc010E)
    fn0C27 = fn0C8E(l01EC)
    Exit Function
  Case gc7914
    fn0C27 = "obj?"
    Exit Function
  End Select
  Select Case l01EE And gc78C0
  Case gc78DC
    If l01F2 Then sub01CC
    fn0C27 = "m" & fn02BE(l01F0)
  Case gc78E2
    If l01F2 = 0 Then sub01CC
    fn0C27 = fn0C02(fn0326(l01F2, 2))
  Case gc78D8
    fn0C27 = "p" & fn02BE(l01F0)
  Case gc78DE
    If l01F2 <= 0 Or (l01F2 And 1) > 0 Then
      fn0C27 = "l" & fn02BE(l01F0)
    Else
      fn0C27 = "pv" & fn02BE(l01F0)
    End If
  Case gc78E4
    fn0C27 = "c" & fn02BE(l01F0)
  Case gc78D6
    fn0C27 = "v" & fn02BE(l01F0)
  Case Else
    fn0C27 = "v?" & fn02BE(l01F0)
  End Select
End Function

Function fn0C33(p01F6 As Integer, p01F8 As Integer, p01FA As Integer) As Integer
  Select Case p01FA
  Case 0
  Case gc78D0
    sub01CC
  Case gc78D2
    fn0C33 = fn0B94(gv09B8(), p01F6)
    Exit Function
  Case Else
    sub01CC
    fn0C33 = 2
    Exit Function
  End Select
  Select Case p01F8 And gc78C0
  Case gv78C2: sub01CC
  Case gc78D6
  Case gc78E2
    fn0C33 = 1
  Case gc78DC, gc78E4
    fn0C33 = fn0C84(gv09B8(), p01F6, p01F8 And &H1F, p01FA)
  Case gc78D8
    Select Case p01F8 And &HF
    Case 6
      fn0C33 = 2
    Case Else
      fn0C33 = 1
    End Select
  Case gc78DE
    Select Case p01F8 And &HF
    Case 6
      fn0C33 = 2
    Case 0 To gc78C8
      fn0C33 = 1
    Case gc78CA
      fn0C33 = 1
    Case Else
      sub01CC
      fn0C33 = 1
    End Select
  Case Else
    sub01CC
  End Select
End Function

Function fn0C3E(p01FE As Integer) As String
On Error Resume Next
  fn0C3E = fn0C49(gv7970(p01FE).M158E)
End Function

Function fn0C49(p0202 As Integer) As String
Dim l0204 As Integer
  l0204 = fn0326(p0202, 4)
  If l0204 <= UBound(gv781C) Then
    If Len(gv781C(l0204)) Then
      fn0C49 = gv781C(l0204)
      Exit Function
    End If
  End If
  sub0BC7 p0202, "gn" & Hex$(p0202)
  fn0C49 = gv781C(l0204)
End Function

Sub sub0C58()
Dim l0206
' Const mc020E = 4 ' &H4%
' Const mc0210 = 8 ' &H8%
  If gv06B2 <= 3 Then
    sub0214 gc0076
    gv7A30 = gv7A30 Or mc020E
    gv7A32 = mc0210
    Exit Sub
  End If
  sub048D
  gv0B6C = ""
  If gv0884 = 3 Then
    sub03FF &H8003
    If gv0D94 Then
      Get gv0882, , l0206
      gv0B6C = Space$(l0206)
      Get gv0882, , gv0B6C
    End If
  ElseIf gv0884 <> 2 Then
    sub0214 mc020E
    Exit Sub
  End If
  gv792A = True
  sub08F1
  sub017A
  gv792A = False
  gv79B6 = 0
End Sub

Sub sub0769(p021C() As Integer, p0222 As Integer)
Dim l0224 As Integer
Dim l0226 As String
' Const mc0228 = 2 ' &H2%
Static m022A As Integer
' Const mc022C = 1 ' &H1%
  Select Case p021C(p0222 - 1) And &HFFC0
  Case &HC000, &HC500, &HC600
    l0224 = 0
  Case &HC100, &HC200
    l0224 = p021C(p0222 - 2)
    If l0224 And &H4000 Then
      l0224 = l0224 And &HFF
    Else
      l0224 = 0
    End If
  Case &H4600
    l0224 = 0
  Case Else
    l0224 = p021C(p0222 + mc0228)
    If (l0224 And &H7FC0) = &H5000 Then
      l0224 = l0224 And &HFF
    Else
      sub0214 mc00F0
      Exit Sub
    End If
  End Select
  l0226 = ")"
  p0222 = p0222 + 6
  While l0224 > 0
    l0226 = Format$(p021C(p0222 + m022A) + p021C(p0222 + mc022C) - 1) & l0226
    If p021C(p0222 + mc022C) Then
      l0226 = Format$(p021C(p0222 + mc022C)) & " To " & l0226
    End If
    l0224 = l0224 - 1
    If l0224 Then l0226 = ", " & l0226
    p0222 = p0222 + 2
  Wend
  sub0739 "(" & l0226
End Sub

Sub sub0C65(p022E As Long)
Dim l0230 As Integer
' Const mc0232 = 1 ' &H1%
' Const mc0234 = 2 ' &H2%
  l0230 = p022E \ 2
  sub0739 "  " & fn0C02(l0230)
  If gv0980(l0230 + mc0232) And 1 Then
    sub0769 gv0980(), l0230 + 4
  End If
  If gv0980(l0230 + mc0234) >= gc78C8 Or gv0980(l0230 + mc0234) < 0 Then
    sub0776 gc78CA, gv0980(l0230 + mc0234), False
  Else
    sub0776 gv0980(l0230 + mc0234), gv0980(l0230 - 1), False
  End If
  sub05F9
  p022E = gv0980(l0230 + mc0232) And &HFFFE&
End Sub

Sub sub0776(p0236 As Integer, p0238 As Integer, p023A As Integer)
  Select Case p0236 And &HF
  Case 0
  Case gc78CA
    sub0739 " As " & fn0C02(fn0326(p0238, 2))
  Case gc78C6
    sub0739 " As String *" & Str$(fn032E(p0238))
  Case Is < 8
    sub0739 " As " & gv1538(p0236 And &HF)
  Case gc78C8:
    If gv0F7C(p0238 And &HFF).M2427 < 0 Then sub0739 "()"
    sub0739 " As "
    If p023A And p0238 And &H8000 Then sub0739 "New "
    sub0739 gv0F7C(p0238 And &HFF).M1BD4
  Case Else
    If fn0C1A(p0236) Then
      sub0739 " As " & fn0C02(fn0326(p0236, 2))
    Else
      sub01CC
      sub0739 " As " & Hex$(p0236)
    End If
  End Select
End Sub

Sub sub0C74(p0242 As Integer)
Dim l0244 As Long
' Const mc0246 = 3 ' &H3%
' Const mc0248 = 1 ' &H1%
  sub0739 "Type " & fn0C02(p0242): sub05F9
  l0244 = fn032E(gv0980(p0242 + mc0246))
  If gv0980(p0242 + mc0248) And 1 Then
    l0244 = fn032E(gv0980((l0244& \ 2) - 1))
  End If
  While l0244
    sub0C65 l0244
  Wend
  sub0739 "End Type": sub05F9: sub05F9
End Sub

Function fn0C84(p024C() As Integer, p0252 As Integer, p0254 As Integer, p0256 As Integer) As Integer
Dim l0258 As Integer
' Const mc025A = 2 ' &H2%
  Select Case p0256
  Case gc78D2
    fn0C84 = fn0B94(p024C(), p0252)
    Exit Function
  End Select
  Select Case p0254
  Case 0
    fn0C84 = 0
  Case 1
    fn0C84 = 1
  Case 2, 3
    fn0C84 = 2
  Case 4
    fn0C84 = 4
  Case 5
    fn0C84 = 4
  Case 6
    fn0C84 = 8
  Case 7
    fn0C84 = 2
  Case gc78C6
    fn0C84 = fn0326(p024C(p0252 - 1) + 1, 2)
  Case gc78CA
    l0258 = p024C(p0252 - 1)
    If Not fn0C1A(l0258) Then
      sub0214 mc00A4
      Exit Function
    End If
    l0258 = fn0326(l0258, 2)
    fn0C84 = fn0326(gv0980(l0258 + mc025A) + 1, 2)
  Case gc78C8
    fn0C84 = 2
  Case Else
    sub01CC
  End Select
End Function

Function fn0C8E(p025E As Integer) As String
  fn0C8E = fn0C49(gv79BC(p025E).M158E)
End Function

Function fn0B94(p0262() As Integer, p0268 As Integer) As Integer
Dim l026A As Integer
' Const mc026C = 1 ' &H1%
  l026A = p0262(p0268 + 2)
  Select Case l026A And &HFFF0
  Case &HC000, &HC500, &HC600
    fn0B94 = 3
  Case &HC100, &HC200
    l026A = p0262(p0268 + 1)
    If l026A And &H4000 Then
      l026A = l026A And &HF
    Else
      l026A = 8
    End If
    fn0B94 = l026A * 2 + 9
  Case &H4600
    fn0B94 = 3
  Case Else
    sub0214 mc026C
    Exit Function
  End Select
End Function
