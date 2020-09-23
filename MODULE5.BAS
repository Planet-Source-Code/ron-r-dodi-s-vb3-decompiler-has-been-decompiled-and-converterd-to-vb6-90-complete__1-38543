Attribute VB_Name = "MODULE5"
' Module5
' Option Explicit
Dim m000E As T16A8
Dim m0018 As String
Dim m001E As T16D3
Dim m0026 As Integer
Const mc005A = 4 ' &H4%
Const mc006E = 70 ' &H46%
Const mc007A = 128 ' &H80%
Const mc00F2 = 67 ' &H43%
Const mc00F8 = 88 ' &H58%
Const mc0104 = 64 ' &H40%
Const mc0106 = 8 ' &H8%
Const mc0122 = 1 ' &H1%
Const mc0144 = 36 ' &H24%
Const mc015C = 229 ' &HE5%
Const mc0178 = 1 ' &H1%
Const mc0180 = -2 ' &HFFFE%
Const mc0184 = 224 ' &HE0%
Const mc0186 = 9 ' &H9%

Function fn03C5(p00A2 As Integer) As String
Dim l00A4 As String
Dim l00A8 As T207C
  Get gv0882, gv0B60 + fn032E(p00A2), l00A8
  l00A4 = Space$(Asc(l00A8.M208E))
  Get gv0882, , l00A4
  fn03C5 = l00A4
End Function

Function fn03D1(p00AE As Integer) As String
Dim l00B0 As String
  l00B0 = fn0348(gv0B6C, p00AE - 1)
  If InStr(l00B0, " ") Or InStr(gc151A, Right$(l00B0, 1)) Then
    l00B0 = "[" & l00B0 & "]"
  End If
  fn03D1 = l00B0
End Function

Sub sub0377(p0188 As Integer)
Dim l018A As String
Dim l018C As String
Dim l018E As Integer
Dim l0190 As Integer
Dim l0194 As String * 2
Dim l0196
Dim l0198
Dim l019A As Integer
Dim l019C As Integer
Dim l019E As String
Dim l01A0 As Integer
Dim l01A6 As Long
Dim l01A8 As Long
Dim l01AA As Integer
Dim l01AC As String
Dim l01AE As Integer
Dim l01B0 As String
Dim l01B2 As Long
Dim l01B4 As Long
Const mc01C6 = 1 ' &H1%
  Get gv0882, , m001E
  l01A6 = m001E.M16E2 And &H80000000
  l01A8 = m001E.M16E2 Xor l01A6
  l01A8 = l01A8 - Len(m001E) - 1
  l01B4 = Seek(gv0882) + l01A8 + 1
  If l01A6 Then
    Get gv0882, , l0194
    l01A8 = l01A8 - 2
  End If
  Get gv0882, , gv09F6: l01AA = Asc(gv09F6)
  If l01AA Then
    sub01CC
    l01B0 = Space$(l01AA)
    Get gv0882, , l01B0
    l01A8 = l01A8 - l01AA
  Else
    l0190 = Asc(m001E.M16EB)
    l01B0 = gv0FB6(gv0B38 + l0190).M1BD4
    m001E.M16E2 = m001E.M16E2 + Len(l01B0)
  End If
  l01AC = Chr$(Len(l01B0)) & l01B0
  l01B2 = Seek(gv0882)
  Get gv0882, , gv09F6
  l01AE = Asc(gv09F6)
  If l01AE = &HFF Then
    Get gv0882, , gv09F6: l018E = Asc(gv09F6)
    l018C = Space$(l018E)
    Get gv0882, , l018C
    For l01AE = 1 To gv13C2
      If l018C = gv1390(l01AE).M280D Then Exit For
    Next l01AE
    If l01AE > gv13C2 Then
      gv7A4A = l018C
      sub0214 mc0104
      l01AE = 0
    End If
  Else
    l01AE = l01AE + 1
    l018C = gv1390(l01AE).M280D
  End If
  If l01A6 Then l01AC = l0194 & l01AC
  If p0188 = 0 Then sub0386 gv0B38 + l0190, l01AE, l01A6 <> 0
  Get gv0882, l01B2 + l01A8 - 2, l0198
  If l0198 <> &HFF Then
    l019A = gv1390(l01AE).M2A1E + 1
    Get gv0882, l01B2 + l01A8 - 2 * l019A - 2, l0198
    If l0198 = (l019A * &H100 Or &HFF) Then
      l01A8 = l01A8 - 2 * l019A
    Else
      l019A = &HFF
      Do
        l01A8 = l01A8 - 2
        l019A = l019A + &H100
        Get gv0882, l01B2 + l01A8 - 2, l0198
      Loop Until l0198 = l019A
      l019A = l0198 \ &H100
    End If
  End If
  m001E.M16E2 = m001E.M16E2 - 2 * l019A - 1
  If p0188 Then
    Put m0026, , m001E
    Put m0026, , l01AC
    Seek gv0882, l01B2
    l018A = Space$(&H4000)
    While l01A8 > Len(l018A)
      Get gv0882, , l018A: Put m0026, , l018A
      l01A8 = l01A8 - Len(l018A)
    Wend
    l018A = Space$(l01A8 - 1)
    Get gv0882, , l018A: Put m0026, , l018A
    If Asc(Right$(l018A, 1)) <> &HFF Then sub01CC
    Get gv0882, , gv09F6
    If Asc(gv09F6) <> l019A Then sub01CC
    If l019A Then Seek gv0882, Seek(gv0882) + 2 * l019A
    If Seek(gv0882) <> l01B4 Then sub01CC
  ElseIf l019A Then
    For l0196 = 0 To l019A - 1
      Get gv0882, , l0198
      If l0198 Then
        If l01AE Then
          l019C = gv1390(l01AE).M2A14 + l0196
          l019C = gv14B2(l019C)
          l019E = gv147E(l019C).M1BD4
        Else
          l019E = "event" & fn02BE(l0196)
        End If
        If l0190 Then
          l019E = l01B0 & "_" & l019E
        ElseIf l01AE Then
          l019E = gv1390(l01AE).M280D & "_" & l019E
        Else
          l019E = "ctrl?_" & l019E
        End If
        If l0198 And 1 Then
          l01A0 = gv0DA6(fn0D6C(l0198) + mc01C6)
          sub0397 l01A0, l019E
          If l01AE Then sub03A6 l01A0, l019C, l01A6 <> 0
        Else
          sub01CC
        End If
      End If
    Next l0196
  End If
  Seek gv0882, l01B4
End Sub

Sub sub03B3()
End Sub

Sub sub03DE()
Dim l00B8
Dim l00BA As Integer
Dim l00BC
Dim l00BE
Dim l00C0 As String
Dim l00C2 As Integer
Dim l00C4 As Integer
Dim l00C6 As Long
Dim l00CA As T1471
Dim l00CE As T14B8
' Const mc00F2 = 67 ' &H43%
Dim l00F4 As Long
' Const mc00F8 = 88 ' &H58%
' Const mc0104 = 64 ' &H40%
' Const mc0106 = 8 ' &H8%
Dim l0108() As Integer
  Get gv0882, gv0D9A, l00CA
  sub03EE
  gv0F32 = 0: gv0F30 = 0: gv0F34 = 0
  gv0F42 = 0
  If l00CA.M1491 = -1 Then
    gv0F48 = 0
  Else
    gv0F48 = l00CA.M1491 And -2
  End If
  gv0F92 = 0
  gv0F36 = fn0350(l00CA.M1498)
  gv0F3A = fn088D(l00CA.M14A3)
  gv0F3E = fn088D(l00CA.M14AE)
  ReDim gv1002(0), gv1036(0)
  Do
    Get gv0882, , l00CE
    If Asc(l00CE.M14C8) = 0 Then Exit Sub
    gv0F34 = gv0F34 + 1
    LSet gv0F1A(gv0F34) = l00CE
    l00BA = gv0F1A(gv0F34).M15F7 And &HFF
    gv0F1A(gv0F34).M15F7 = l00BA
    Select Case l00BA
    Case mc00F2, mc006E
      gv0F32 = gv0F32 + 1
      l00C0 = fn088D(Asc(l00CE.M14D2))
      If l00BA = mc006E Then
        l00C6 = Seek(gv0882)
        sub03FF l00CE.M14D7: l00F4 = gv0D9A
        sub03FF l00CE.M14D7 + 1
        Seek gv0882, l00C6
        If gv0D94 = 0 Or l00F4 >= gv0D9A Then
          gv79A2 = 0
        End If
        If Right$(l00C0, 4) <> ".FRM" Then
          gv7A4A = l00C0
          sub0214 mc007A
        End If
        If gv0F42 = 0 Then
          gv0F42 = gv0F34
          gv0F46 = l00CE.M14FC
        End If
        If l00CE.M14DF > gv0F92 Then gv0F92 = l00CE.M14DF
        If Asc(l00CE.M14F3) And 1 Then
          gv0F7C(l00CE.M14DF).M2427 = &H14 + 1
        Else
          gv0F7C(l00CE.M14DF).M2427 = &HD + 1
        End If
        gv0F7C(l00CE.M14DF).M2431 = l00CE.M14D7
      Else
        gv0F30 = gv0F30 + 1
        sub040A
        sub0419 0, l00C0
      End If
      gv0F02(gv0F34) = l00C0
    Case mc00F8
      l00C2 = Asc(l00CE.M14D2)
      If l00C2 = &HFF Then
        Get gv0882, , gv09F6
        gv0F02(gv0F34) = fn088D(Asc(gv09F6))
        For l00C2 = gv13C2 To 1 Step -1
          If gv1390(l00C2).M280D = gv0F02(gv0F34) Then Exit For
        Next
        If l00C2 = 0 Then
          gv7A4A = gv0F02(gv0F34)
          sub0214 mc0104
          gv1390(0).M280D = "?"
        End If
      Else
        l00C2 = l00C2 + 1
        gv0F02(gv0F34) = gv1390(l00C2).M280D
      End If
      gv0F1A(gv0F34).M2396 = l00C2
      If gv0F7C(l00CE.M14DF).M2427 Then
        If gv0F7C(l00CE.M14DF).M2427 <> l00C2 Then
          gv7A4A = gv0F02(gv0F34)
          sub0214 mc0104
        End If
      Else
        gv0F7C(l00CE.M14DF).M2427 = l00C2
        gv0F7C(l00CE.M14DF).M1BD4 = gv1390(l00C2).M280D
      End If
      If l00CE.M14E9 <> &H1FF Then
        gv0F7C(l00CE.M14E9).M2427 = -l00C2
        gv0F7C(l00CE.M14E9).M1BD4 = gv1390(l00C2).M280D
      End If
    Case Else
      sub0214 mc0106
      Exit Do
    End Select
    gv0F1A(gv0F34).M15F7 = l00BA
    Select Case l00BA
    Case mc006E, mc00F8
      Get gv0882, , l00BE
      If l00BE > 0 Then
        ReDim l0108%(l00BE)
        For l00BC = 1 To l00BE
          Get gv0882, , l0108(l00BC)
        Next
        For l00BC = 1 To l00BE
          l00C4 = l0108(l00BC)
          Get gv0882, , l00B8
          If l00BA = mc006E Then
            If l00C4 > UBound(gv1036) Then ReDim Preserve gv1036(l00C4)
            If gv1036(l00C4).M2496 = 0 Then
              gv1036(l00C4).M249D = l00B8
              gv1036(l00C4).M2496 = l00CE.M14DF
            End If
          Else
            If l00C4 > UBound(gv1002) Then ReDim Preserve gv1002(l00C4)
            If gv1002(l00C4) = 0 Then
              gv1002(l00C4) = gv141A(gv1390(l00C2).M29F5 + l00B8)
            End If
          End If
        Next
      End If
    End Select
  Loop
End Sub

Sub sub0427()
Dim l0114
Dim l0116 As Integer
Dim l0118 As Integer
Dim l011A As Integer
Dim l011C As Integer
' Const mc0122 = 1 ' &H1%
  sub01E6 l011C, gc0096
  For l0114 = 1 To gv0F32
    If gv0F1A(l0114).M15F7 <> mc00F2 Then
      l0116 = gv0F1A(l0114).M14FC
      l011A = fn0D6C(l0116)
      l0118 = gv0DA6(l011A + mc0122)
      sub0433 l0118, (gv0F02(l0114))
      gv7970(l0118).M2315 = gv0F1A(l0114).M15F7
      gv7970(l0118).M231F = Asc(gv0F1A(l0114).M14F3)
      gv7970(l0118).M14D7 = gv0F1A(l0114).M14D7
      gv7970(l0118).M14DF = gv0F1A(l0114).M14DF
      gv0F1A(l0114).M238D = l0118
    Else
      
    End If
  Next l0114
  sub0205 l011C
End Sub

Sub sub0442()
Dim l0136 As Long
Dim l0138
Dim l013A As Integer
Dim l013C As Integer
' Const mc0144 = 36 ' &H24%
' Const mc015C = 229 ' &HE5%
  sub01E6 l013C, gc00BE & gc00EA & gc00E2
  gv0D94 = 0
  sub0452 gv0D80 + 1
  gv0B54 = gv0D9A
  Get gv0882, gv0B54 + mc0144, gv0B36
  l0136 = &H60
  Get gv0882, gv0B54 + l0136, l013A: l0136 = l0136 + 2
  gv0B58 = gv0B54 + l0136
  l0136 = l0136 + fn032E(l013A)
  Get gv0882, gv0B54 + l0136, l0138: l0136 = l0136 + 2
  Get gv0882, gv0B54 + l0136, l013A: l0136 = l0136 + 2
  gv0B5C = gv0B54 + l0136
  gv09B2 = fn032E(l013A)
  gv09B6 = (gv09B2 \ 2) - 1
  ReDim gv0980(0 To gv09B6), gv784E(0 To gv09B6)
  ReDim gv781C(0)
  gv7880 = String$(gv09B6, 0) & Chr$(mc015C)
  For l0138 = 0 To gv09B6
    Get gv0882, , gv0980(l0138)
  Next
  l0136 = l0136 + gv09B2
  Get gv0882, gv0B54 + l0136, l013A: l0136 = l0136 + 2
  l0136 = l0136 + fn032E(l013A)
  Get gv0882, gv0B54 + l0136, l013A: l0136 = l0136 + 2
  gv0B60 = gv0B54 + l0136
  l0136 = l0136 + fn032E(l013A)
  gv0B64 = l0136
  sub0205 l013C
End Sub

Sub sub045E()
Dim l0160 As Long
Dim l0162 As Integer
Dim l0166 As Integer
Dim l0168 As Integer
Dim l016C As Integer
' Const mc0178 = 1 ' &H1%
  sub01E6 l016C, gc00BE & gc00E6 & gc00DE
  If gv0B64 = 0 Then sub01CC
  sub0452 gv0D80 + 1
  If gv090E Then
    sub046E l016C, gc00BE & gc00D6
    sub047F
    sub046E l016C, gc00BE & gc00CA
  End If
  l0160 = gv0B64
  Do
    Get gv0882, gv0B54 + l0160, l0168: l0160 = l0160 + 2
    If l0168 = 0 Or l0168 = -1 Then Exit Do
    l0166 = fn0D6C(l0168)
    If gv0DA6(l0166) <> mc0178 Then sub01CC
    l0168 = gv0DA6(l0166 + mc0122)
    gv7970(l0168).M2329 = l0160
    If gv090E Then sub016B l0168
    Get gv0882, gv0B54 + l0160, l0162: l0160 = l0160 + 2
    l0160 = l0160 + fn032E(l0162)
    If gv7970(l0168).M2315 = mc006E Then
      Get gv0882, gv0B54 + l0160, l0162: l0160 = l0160 + 2
      l0160 = l0160 + l0162
      Get gv0882, gv0B54 + l0160, l0162: l0160 = l0160 + 2
      l0160 = l0160 + l0162
    Else
      Get gv0882, gv0B54 + l0160, l0162: l0160 = l0160 + 2
      l0160 = l0160 + l0162
    End If
  Loop
  sub0205 l016C
End Sub

Sub sub047F()
Dim l017A As Integer
Dim l017C
' Const mc0180 =-2 ' &HFFFE%
' Const mc0184 = 224 ' &HE0%
' Const mc0186 = 9 ' &H9%
  gv2326 = mc0180
  If gv09B2 < 6 Then sub01CC: Exit Sub
  l017A = gv0980(2)
  Do While l017A
    sub049C l017A
  Loop
  For l017C = gv09B6 - 2 To 0 Step -3
    If gv0980(l017C + 1) Or gv0980(l017C + 2) Then Exit For
    l017A = gv0980(l017C) Xor &H8000
    If l017A <= 0 Or l017A > gv0F92 Then Exit For
    sub04A7 l017C + 1, 2, mc0184, mc0186, 0
    sub04B5 gv0F7C(l017A).M1BD4, l017C + 1
  Next l017C
  sub04C4
End Sub

Sub sub048D()
  gv0D94 = 0
  sub0452 gv0D80
  Get gv0882, gv0D9A, gv09F6
  Select Case gv0884
  Case 1 To 3
    sub03DE
  Case Else
    sub0214 mc0106
  End Select
End Sub

Sub sub04D4(p004A As Integer)
Dim l004C As String
Dim l004E
Dim l0050
Dim l0052 As Integer
Dim l0054 As Integer
Dim l0056 As Integer
' Const mc005A = 4 ' &H4%
' Const mc006E = 70 ' &H46%
' Const mc007A = 128 ' &H80%
  If gv0884 <> 2 And gv0884 <> 3 Then
    sub0214 mc005A
    Exit Sub
  End If
  If p004A Then
    sub01E6 l0054, gc00C2 & "*.FRM"
    sub01E6 l0056, gc00BA & "empty.frm"
    m0026 = FreeFile
    Open gv0B80 & "empty.frm" For Binary Access Read As m0026
    Get m0026, 1, m000E
    m0018 = Space$(LOF(m0026) - m000E.M16CB - 5)
    Get m0026, m000E.M16CB + 6, m0018
    Close m0026
    If gv0884 = 2 Then
      Mid$(m000E.M16B8, 3, 1) = Chr$(&H2B)
    End If
    sub0205 l0056
  Else
    sub01E6 l0054, gc00BE & "*.FRM"
  End If
  For l004E = 1 To gv0F32
    If gv0F1A(l004E).M15F7 = mc006E Then
      sub01E6 l0056, gc00BA & "RC" & Hex$(gv0F1A(l004E).M14D7)
      sub03FF gv0F1A(l004E).M14D7
      If gv0D94 = 0 Then
        gv7A4A = gv0F02(l004E)
        sub0214 mc007A
      Else
        Get gv0882, gv0D9A, m000E
        l0052 = Asc(m000E.M16C3)
        If p004A Then
          gv0B38 = gv0F7C(gv0F1A(l004E).M14DF).M2431
          l004C = gv7A8C & gv0F02(l004E)
          sub046E l0056, gc00C2 & gv0F02(l004E)
          Open l004C For Binary As m0026
          Put m0026, , m000E
        Else
          sub046E l0056, gc00BE & gv0F02(l004E)
          gv0FE8 = gv0FE8 + 1
          gv0B38 = gv0FE8
          ReDim Preserve gv0FB6(LBound(gv0FB6) To gv0FE8 + l0052)
          If gv0F7C(gv0F1A(l004E).M14DF).M2431 <> gv0F1A(l004E).M14D7 Then
            sub01CC
          End If
          gv0F7C(gv0F1A(l004E).M14DF).M2431 = gv0FE8
          sub03FF gv0F1A(l004E).M14D7 + 1
          If gv0D94 = 0 Then
            sub0214 gc0082
            End
          End If
          For l0050 = 0 To l0052
            Get gv0882, , gv09F6
            If Asc(gv09F6) = 0 Then
              gv0FB6(gv0FE8).M1BD4 = "control" & Hex$(l0050)
            Else
              gv0FB6(gv0FE8).M1BD4 = Space$(Asc(gv09F6))
              Get gv0882, , gv0FB6(gv0FE8).M1BD4
            End If
            gv0FE8 = gv0FE8 + 1
          Next l0050
          gv0F7C(gv0F1A(l004E).M14DF).M1BD4 = gv0FB6(gv0B38).M1BD4
          sub03FF gv0F1A(l004E).M14D7
        End If
        Get gv0882, gv0D9A, m000E
        sub0377 p004A
        Get gv0882, , gv09F6
        If p004A Then Put m0026, , gv09F6
        If Asc(gv09F6) = 1 Then
          sub0377 p004A
          Do
            Get gv0882, , gv09F6
            If p004A Then Put m0026, , gv09F6
            Select Case Asc(gv09F6)
            Case 1, 3
              sub0377 p004A
            Case 2
            Case 4, 5
              Exit Do
            Case Else
              gv7A4A = gv0F02(l004E)
              sub0214 mc007A
              Exit Do
            End Select
          Loop
        End If
        If Asc(gv09F6) = 5 Then
          sub0377 p004A
          Do
            Get gv0882, , gv09F6
            If p004A Then Put m0026, , gv09F6
            Select Case Asc(gv09F6)
            Case 2
              sub0377 p004A
            Case 3
            Case 4
              Exit Do
            Case Else
              gv7A4A = gv0F02(l004E)
              sub0214 mc007A
              Exit Do
            End Select
          Loop
        End If
        If Asc(gv09F6) <> 4 Then
          sub0214 mc007A
        End If
        If p004A Then
          m000E.M16CB = Seek(m0026) - 6
          Put m0026, , m0018
          Put m0026, 1, m000E
          Close m0026
        End If
      End If
      sub0205 l0056
    End If
  Next l004E
  sub0205 l0054
End Sub

Sub sub04E4()
Dim l0036 As Integer
Dim l0038 As String
Dim l003A As Integer
Dim l003C As Integer
Dim l0040 As Integer
Dim l0042 As Integer
  For l0036 = 1 To gv79A2
    l0038 = fn0C3E(l0036)
    If Right$(l0038, 4) = ".FRM" Then
      l0038 = fn0E8D(l0038, "FRM")
      l003A = FreeFile
      sub01E6 l0040, l0038
      Open l0038 For Append As l003A
      l003C = FreeFile
      Mid$(l0038, Len(l0038) - 2) = "BAS"
      sub01E6 l0042, " + " & l0038
      Open l0038 For Input As l003C
      While Not EOF(l003C)
        Line Input #l003C, l0038
        Print #l003A, l0038
      Wend
      Close l003A, l003C
      sub0205 l0042
      sub0205 l0040
    End If
  Next
End Sub

