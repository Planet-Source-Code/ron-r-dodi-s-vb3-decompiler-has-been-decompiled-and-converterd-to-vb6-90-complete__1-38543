Attribute VB_Name = "MODULE171"
' Module17
' Option Explicit
Const mc000A = -1 ' &HFFFF%
Dim m000C() As Integer
Dim m003E As Integer
Dim m0040 As Integer
Dim m0042 As Integer
Const mc0074 = 8 ' &H8%
Const mc0076 = 9 ' &H9%
Const mc0078 = 64 ' &H40%
Const mc0084 = -2 ' &HFFFE%
Const mc008A = 5 ' &H5%
Const mc009C = 227 ' &HE3%
Const mc00D2 = 2048 ' &H800%
Const mc00D6 = 2 ' &H2%
Const mc00E0 = 15 ' &HF%
Const mc00E2 = 13 ' &HD%
Const mc00E4 = 48 ' &H30%
Const mc00F4 = 8 ' &H8%
Const mc00F8 = 32 ' &H20%
Const mc00FA = 8192 ' &H2000%
Const mc00FC = 128 ' &H80%
Const mc0100 = 224 ' &HE0%
Const mc0102 = 96 ' &H60%
Const mc0104 = 224 ' &HE0%
Const mc0106 = 192 ' &HC0%
Const mc0108 = 160 ' &HA0%
Const mc010A = 32 ' &H20%
Const mc0114 = 2 ' &H2%
Const mc0116 = 3 ' &H3%
Const mc0118 = 1 ' &H1%
Const mc0124 = 229 ' &HE5%
Const m0128 = 0 ' &H0%
Const mc012A = 225 ' &HE1%
Const mc012C = 226 ' &HE2%
Const mc012E = 233 ' &HE9%
Const mc0136 = 231 ' &HE7%
Const mc0150 = 160 ' &HA0%
Const mc0180 = 12 ' &HC%
Const mc0182 = 32 ' &H20%
Const mc0184 = 2 ' &H2%
Const mc0186 = 6 ' &H6%
Const mc018A = 7 ' &H7%
Const mc01B8 = 16 ' &H10%

Sub sub03A6(p0044 As Integer, p0046 As Integer, p0048 As Integer)
  gv79BC(p0044).M2348 = p0046
  gv79BC(p0044).M2351 = p0048
End Sub

Sub sub0CDA(p0052 As Integer, p0054 As Integer)
Dim l0056 As Integer
Dim l0058 As String
Dim l005A As String
Dim l005C As Integer
Dim l005E As Integer
Dim l0060 As Integer
Dim l0062 As Integer
' Const mc0074 = 8 ' &H8%
' Const mc0076 = 9 ' &H9%
' Const mc0078 = 64 ' &H40%
  l0056 = gv79BC(p0054).M2348
  l0058 = gv147E(l0056).M2A6D
  l005A = gv147E(l0056).M1B23
  If gv79BC(p0054).M2351 Then
    If Len(l0058) Then
      l0058 = "Index%, " & l0058
    Else
      l0058 = "Index%"
    End If
    l005A = Chr$(1) & l005A
  End If
  l005E = gv09B8(p0052) - 6
  If l005E Mod 4 Then sub01CC
  l005E = l005E \ 4
  If l005E >= Len(l005A) Then
    sub01CC
    Exit Sub
  End If
  l005E = Len(l005A) - 1 - l005E
  l0060 = Asc(Mid$(l005A, l005E + 1, 1))
  l005C = 0
  While l005E > 0
    l005C = InStr(l005C + 1, l0058, ",")
    If l005C = 0 Then sub01CC
    l005E = l005E - 1
  Wend
  l0058 = Trim$(Mid$(l0058, l005C + 1))
  l005C = InStr(l0058, ",")
  If l005C Then l0058 = Left$(l0058, l005C - 1)
  l005C = InStr(l0058, " ")
  If l005C Then
    l0058 = RTrim$(Left$(l0058, l005C))
  Else
    l005C = InStr(gc151A, Right$(l0058, 1))
    If l005C Then l0058 = Left$(l0058, Len(l0058) - 1)
  End If
  Select Case l0060
  Case 1 To 5
  Case 6: l0060 = 7
  Case 7: l0060 = mc0074
  Case 8: l0060 = mc0076
  Case Else: sub01CC
  End Select
  sub098D p0052, mc0078, l0060, l0062
  sub097E l0058, p0052
End Sub

Sub sub09DE()
End Sub

Private Sub sub0CFF(p007A As Integer)
Dim l007C
Dim l007E As Integer
Dim l0080 As Integer
Dim l0082 As Integer
' Const mc0084 =-2 ' &HFFFE%
' Const mc008A = 5 ' &H5%
' Const mc009C = 227 ' &HE3%
  gv2326 = mc0084
  ReDim m000C%(gv79EE)
  m003E = 0
  For l007C = 1 To gv79EE
    If gv79BC(l007C).M230D = p007A Then
      l007E = gv79BC(l007C).M2364
      l0080 = gv79BC(l007C).M2341
      If l007E Then
        m000C(l007E) = l007C
        m003E = m003E + 1
      End If
      If gv0DA6(l0080 + mc008A) Then l0082 = l0082 + 1
    End If
  Next
  If l0082 Then
    If gv7970(gv79B6).M14DF Then l0080 = &H1A \ 2 Else l0080 = 6 \ 2
    Mid$(gv7884, l0080, l0082) = String$(l0082, mc009C)
  End If
End Sub

Function fn0D0C(p00A2 As Integer) As Integer
On Error Resume Next
  fn0D0C = Asc(Mid$(gv09F0, p00A2, 1))
End Function

Private Sub sub0D1B(ByVal pv00A6 As Integer)
Dim l00A8 As Integer
Dim l00AA
Dim l00B2 As T16F5
Dim l00B4 As Integer
  If pv00A6 Then
    gv09F0 = String$(gv09EE, 0)
    Exit Sub
  End If
  If gv79EE = 0 Then sub01CC: Exit Sub
  For pv00A6 = 1 To gv79A2
    l00A8 = 0
    l00B4 = gv7970(pv00A6).M230D
    For l00AA = 1 To gv79EE
      If gv79BC(l00AA).M230D = l00B4 Then
        Get gv0882, fn0D74(gv79BC(l00AA).M2341), l00B2
        If l00B2.M17C8 And &H1000000 Then
          l00A8 = l00A8 + 1
          If l00A8 > 255 Then sub01CC
          gv79BC(l00AA).M2364 = l00A8
        ElseIf gv79BC(l00AA).M1A97 Then
          l00A8 = l00A8 + 1
          gv79BC(l00AA).M2364 = l00A8
        End If
      End If
    Next
  Next
End Sub

Sub sub0A00(p00C0 As T24BE, p00C2 As Integer, p00C4 As String)
Dim l00C6 As String
Dim l00CA As Integer
Dim l00CC As Integer
Dim l00CE As Integer
Dim l00D0 As Integer
' Const mc00D2 = 2048 ' &H800%
' Const mc00D6 = 2 ' &H2%
' Const mc00E0 = 15 ' &HF%
' Const mc00E2 = 13 ' &HD%
' Const mc00E4 = 48 ' &H30%
  l00CE = p00C0.M24FD
  l00D0 = p00C0.M24E5
  l00C6 = gv79BC(l00D0).M1B23
  p00C2 = p00C2 + 1
  If Len(l00C6) <> p00C2 Then
    If Len(l00C6) Then
      sub0214 mc00D2
    Else
      l00C6 = String$(p00C2, 0)
    End If
  End If
  While p00C2 > 1
    If l00CE < 0 Then
      If gv792A And mc00D6 Then
        p00C4 = ",?" + p00C4
      End If
      p00C2 = p00C2 - 1
    Else
      l00CA = gv10CE(l00CE).M24F1
      If l00CA Then
        l00CC = Asc(Mid$(l00C6, p00C2, 1))
        If l00CC = 0 Then
          Mid$(l00C6, p00C2, 1) = Chr$(l00CA)
        ElseIf (l00CA Xor l00CC) And &HF Then
          If gv79BC(l00D0).M15FE = -1 Then
            If (l00CC And mc00E0) <> mc00E2 Then
              l00CC = mc00E2 Or (l00CC And mc00E4)
              Mid$(l00C6, p00C2, 1) = Chr$(l00CC)
            End If
          ElseIf (l00CC And &HF) > (l00CA And &HF) Then
            Mid$(l00C6, p00C2, 1) = Chr$(l00CA)
          End If
        ElseIf (l00CA Or l00CC) <> l00CC Then
          Mid$(l00C6, p00C2, 1) = Chr$(l00CA Or l00CC)
        End If
        If gv792A And mc00D6 Then
          p00C4 = "," + gv10CE(l00CE).M24F7 + p00C4
        End If
        p00C2 = p00C2 - 1
      End If
      l00CE = gv10CE(l00CE).M24FD
    End If
  Wend
  p00C0.M24FD = l00CE
  If l00C6 <> gv79BC(l00D0).M1B23 Then
    gv79BC(l00D0).M1B23 = l00C6
    gv090E = True
  End If
End Sub

Sub sub09BB()
Dim l00E8 As Integer
Dim l00EC
Dim l00EE As Integer
Dim l00F0 As Integer
Dim l00F2 As Integer
' Const mc00F4 = 8 ' &H8%
' Const mc00F8 = 32 ' &H20%
' Const mc00FA = 8192 ' &H2000%
' Const mc00FC = 128 ' &H80%
' Const mc0100 = 224 ' &HE0%
' Const mc0102 = 96 ' &H60%
' Const mc0104 = 224 ' &HE0%
' Const mc0106 = 192 ' &HC0%
' Const mc0108 = 160 ' &HA0%
' Const mc010A = 32 ' &H20%
  gv2326 = mc0084
  l00E8 = m0042 - 1
  If Asc(Mid$(gv7884, m0042, 1)) And mc00F4 Then l00E8 = l00E8 - 1
  Mid$(gv09F0, 1, l00E8) = String$(l00E8, 0)
  For l00EC = 1 To m003E
    l00E8 = fn0308(gv09F0, Chr$(l00EC))
    If l00E8 Then
      Do While l00E8 > 3
        l00E8 = l00E8 - 1
        l00EE = Asc(Mid$(gv09F0, l00E8))
        If l00EE And l00EE <> l00EC Then
          Mid$(gv09F0, 1, l00E8) = String$(l00E8, 0)
          If l00E8 >= m0042 Then
            m0042 = l00E8 + 1
          End If
          Exit For
        End If
      Loop
      Exit For
    End If
  Next
  m0040 = gv09EE
  For l00EC = m003E To 1 Step -1
    l00F0 = m000C(l00EC)
    If gv79BC(l00F0).M15FE <> -1 Then
      gv79BC(l00F0).M236D = InStr(gv09F0, Chr$(l00EC))
      If gv79BC(l00F0).M236D = 0 Then
        gv79BC(l00F0).M236D = m0040
      Else
        m0040 = gv79BC(l00F0).M236D
      End If
      l00F2 = gv79BC(l00F0).M235A
      If l00F2 Then
        If Asc(Mid$(gv09F0, m0040, 1)) > l00EC Then
          m0040 = m0040 - 1
        End If
        l00EE = gv09B8(m0040)
        Do While l00EE <> l00F2
          GoSub L1A894
          If l00EE = l00EC Then
            If gv0DA6(gv79BC(l00F0).M2341 + mc008A) = 6 Then
              m0040 = m0040 - 1
              GoSub L1A894
            End If
          End If
          If l00EE = l00EC Then
            gv79BC(l00F0).M236D = m0040
            Mid$(gv09F0, m0040, 1) = Chr$(l00EC)
            l00EE = l00F2
          ElseIf l00EE Then
            sub0214 mc00F8
            l00EE = 0
            Exit Do
          Else
            m0040 = m0040 - 1
            l00EE = gv09B8(m0040)
            If m0040 < m0042 Then
              sub0214 mc00FA
              l00EE = 0
              Exit Do
            End If
          End If
        Loop
        If l00EE = l00F2 Then
          l00EE = gv79BC(l00F0).M2341
          sub0970 m0040, 0, mc00FC, gv0DA6(l00EE + mc008A), 0
          sub097E fn0C8E(l00F0), m0040
        Else
          sub0214 mc00FA
        End If
      End If
    End If
  Next
  If m0040 <> m0042 Then
    m0042 = m0040
  End If
  l00F2 = 0
  For l00E8 = m0042 To gv09EE
    l00EE = Asc(Mid$(gv09F0, l00E8, 1))
    If l00EE > m003E Then
      l00EE = 0
      Mid$(gv09F0, l00E8, 1) = Chr$(l00EE)
    End If
    l00EC = Asc(Mid$(gv7884, l00E8, 1))
    If l00EE Then
      l00F0 = m000C(l00EE)
      If l00EE < l00F2 Then
        sub01CC
        Mid$(gv09F0, 1, l00E8 - 1) = String$(l00E8 - 1, 0)
        l00F2 = l00EE
      ElseIf l00EE > l00F2 Then
        m0040 = l00E8
        Do
          l00F2 = l00F2 + 1
          gv79BC(m000C(l00F2)).M236D = m0040
        Loop While l00F2 < l00EE
      End If
      Select Case l00EC And mc0100
      Case mc0078
        If gv79BC(l00F0).M2348 Then
          sub0CDA l00E8, l00F0
        End If
      Case mc00FC
      Case mc0102
      Case mc0104
      Case mc0106
      Case mc0108
      Case Else
        sub0214 mc00FA
      End Select
    Else
      If l00EC >= mc010A Then Mid$(gv09F0, l00E8, 1) = Chr$(l00F2)
    End If
  Next
  For l00EC = 1 To m003E
    l00F0 = m000C(l00EC)
    If gv79BC(l00F0).M15FE >= 0 Then
      If gv79BC(l00F0).M236D < m0042 Then
        sub0214 mc00F8
      End If
    End If
  Next
Exit Sub

L1A894:
  l00EE = gv09B8(m0040)
  If l00EE And l00EE <> l00F2 Then
    l00EE = 0
  Else
    l00EE = Asc(Mid$(gv09F0, m0040, 1))
    If l00EE = 0 Then
    ElseIf l00EE <> l00EC Then
      sub0214 mc00F8
      Return
    End If
    l00E8 = Asc(Mid$(gv7884, m0040, 1))
    If l00E8 And (l00E8 And mc0100) <> mc00FC Then
      If l00EE = l00EC Then l00EE = 0
    Else
      If l00EE = 0 Then l00EE = l00EC
    End If
  End If
Return
End Sub

Sub sub0A0E(ByVal pv010C As Integer, p010E As Integer)
Dim l0110 As Integer
' Const mc0114 = 2 ' &H2%
' Const mc0116 = 3 ' &H3%
' Const mc0118 = 1 ' &H1%
  pv010C = fn0D6C(pv010C)
  Select Case gv0DA6(pv010C)
  Case mc0114, mc0116
    l0110 = gv0DA6(pv010C + mc0118)
    If l0110 Then
    If Len(gv79BC(l0110).M1B23) <> p010E + 1 Then
      If Len(gv79BC(l0110).M1B23) = 0 Then
        gv79BC(l0110).M1B23 = Chr$(gv0DA6(pv010C + mc008A)) & String$(p010E, 0)
      Else
        sub01CC
      End If
    End If
    Else
      sub01CC
    End If
  Case Else
    sub01CC
  End Select
End Sub

Sub sub0CED()
Dim l011A As Integer
Dim l011E As Integer
' Const mc0124 = 229 ' &HE5%
' Const m0128 = 0 ' &H0%
' Const mc012A = 225 ' &HE1%
' Const mc012C = 226 ' &HE2%
' Const mc012E = 233 ' &HE9%
' Const mc0136 = 231 ' &HE7%
  gv781A = True
  sub0739 "' main.txt - global definitions": sub05F9
  l011A = 3
  If Asc(Right$(gv7880, 1)) <> mc0124 Then
    sub01CC
    Mid$(gv7880, gv09B6 + 1, 1) = Chr$(mc0124)
  End If
  Do
    l011E = Asc(Mid$(gv7880, l011A, 1))
    Select Case l011E And mc0100
    Case m0128, mc0104
      Select Case l011E
      Case 0
        While Asc(Mid$(gv7880, l011A, 1)) = 0
          l011A = l011A + 1
        Wend
      Case mc012A
        sub0C74 l011A
        l011A = InStr(l011A, gv7880, Chr$(mc012C))
        If l011A = 0 Then
          sub0214 mc00F8
          Exit Do
        Else
          l011A = l011A + 1
        End If
      Case mc0124: Exit Do
      Case 8 To &H1F
        l011A = l011A + 1
      Case mc009C: sub01CC: l011A = l011A + 1
      Case mc012E: l011A = l011A + 1
      Case mc0104
        If gv784E(l011A) Then sub01CC
        l011A = l011A + 1
      Case mc0136
        sub072B l011A, gv7880, True
        sub05F9
        l011A = l011A + 1
      Case Else
        l011A = l011A + 1
        sub0214 mc00F8
      End Select
    Case mc010A
      sub0214 mc00F8
      sub0739 "Global " & fn0C02(l011A) & " ' As ?"
      sub05F9
      l011A = l011A + 1
    Case mc0106
      sub072B l011A, gv7880, True
      sub05F9
      l011A = l011A + 1
    Case mc0108
      sub075D l011A, gv0980(), gv7880, True
      sub05F9
      l011A = l011A + 1
    Case Else
      sub01CC
      l011A = l011A + 1
    End Select
  Loop
End Sub

Sub sub09A9(p0140 As Integer)
Dim l0142 As Integer
Dim l0144 As Integer
Dim l0146 As Integer
' Const mc0150 = 160 ' &HA0%
  If p0140 <> gv79B6 Then sub01CC: sub016B p0140
  sub0CFF gv79B8
  If gv792A Then
    m0042 = gv09EE
    m0040 = m0042
    Exit Sub
  End If
  If m003E Then
    m0042 = gv79BC(m000C(1)).M236D
  Else
    m0042 = gv09EE
  End If
  gv781A = True
  If mc000A Then sub0739 "' Option Explicit": sub05F9
  For l0142 = 1 To gv79EE
    If gv79BC(l0142).M230D = gv79B8 Then
      If gv79BC(l0142).M1A97 = 0 Then
        sub0741 l0142
      End If
    End If
  Next
  If gv79A6.M14DF Then
    l0142 = &H1A \ 2
  Else
    l0142 = 3
  End If
  Do While l0142 < gv09EE
    l0144 = Asc(Mid$(gv09F0, l0142, 1))
    l0146 = Asc(Mid$(gv7884, l0142, 1))
    Select Case l0146 And mc0100
    Case m0128, mc0104
      Select Case l0146
      Case 0
        While Asc(Mid$(gv7884, l0142, 1)) = 0
          l0142 = l0142 + 1
        Wend
      Case mc0124
        Exit Do
      Case mc009C
        l0142 = l0142 + 1
      Case 8 To &H1F
        l0142 = l0142 + 1
      Case mc012E
        l0142 = l0142 + 2
      Case Else
        sub01CC
        l0142 = l0142 + 1
      End Select
    Case mc010A
      sub01CC
      If l0144 = 0 Then sub0739 "Dim " & fn0C27(l0142) & " ' As ?": sub05F9
      l0142 = l0142 + 1
    Case mc0106
      sub072B l0142, gv7884, False: sub05F9
      l0142 = l0142 + 1
    Case mc0150
      l0142 = l0142 + 1
    Case mc0102
    If l0144 = 0 Then
      sub0739 "Dim "
      sub075D l0142, gv09B8(), gv7884, False
      sub05F9
    End If
      l0142 = l0142 + 1
    Case mc00FC, mc0078
      l0142 = l0142 + 1
    Case Else
      sub01CC
      l0142 = l0142 + 1
    End Select
  Loop
End Sub

Sub sub07B4()
Dim l0152 As Integer
Dim l0154 As Integer
Dim l0156 As Integer
Dim l015A As String * 1
Dim l015C As Integer
  l015A = Chr$(gv79BC(gv7A2A).M2364)
  m0040 = gv79BC(gv7A2A).M236D
  If gv792A Then
    If m0042 = 0 Then
    End If
    m0040 = l0152
    If m0040 = 0 Then
      m0040 = m0042
    End If
    Exit Sub
  End If
  If gv79F2.M15F7 <= &H16 Then
    If gv79F2.M15F7 <> &H16 Then sub01CC: Exit Sub
  End If
  If m0040 = 0 Then
    m0040 = InStr(gv09F0, l015A)
    If m0040 = 0 Then Exit Sub
    gv79BC(gv7A2A).M236D = m0040
  End If
  l0152 = m0040
  If Mid$(gv09F0, l0152, 1) > l015A Then Exit Sub
  l015C = gv79BC(gv7A2A).M235A
  If l015C Then
    If gv09B8(l0152) = l015C Or gv09B8(l0152) = 0 Then
      sub097E fn0C8E(gv7A2A), l0152
      l0152 = InStr(l0152 + 1, gv09F0, l015A)
    End If
  End If
  Do While l0152
    If Mid$(gv09F0, l0152, 1) > l015A Then Exit Do
    l0154 = Asc(Mid$(gv7884, l0152, 1))
    Select Case l0154 And mc0100
    Case mc0102
      sub0739 "Static "
      sub075D l0152, gv09B8(), gv7884, False
      sub05F9
    Case mc00FC
      If gv09B8(l0152) <> l015C Then
        If gv09B8(l0152) And &H8001 Then
            sub0739 "Dim "
            sub075D l0152, gv09B8(), gv7884, False
            sub05F9
        End If
      End If
    Case mc0108
      l0156 = Asc(Mid$(gv7880, fn0326(gv09B8(l0152), 2), 1))
      If (l0156 And mc0100) = mc0106 Then
        sub0739 "' "
        sub072B fn0326(gv09B8(l0152), 2), gv7880, True
        sub05F9
      End If
    Case mc0106
      sub0739 "' "
      sub072B l0152, gv7884, False: sub05F9
    End Select
    If l0152 >= Len(gv09F0) Then
      l0152 = 0
    Else
      l0152 = InStr(l0152 + 1, gv09F0, l015A)
      If l0152 > Len(gv7884) Then
        sub0214 mc00F8
        Mid$(gv09F0, l0152, 1) = Chr$(0)
        l0152 = 0
      End If
    End If
  Loop
End Sub

Sub sub074E(p0166 As Integer)
Dim l0168 As Integer
Dim l016A As Integer
Dim l016C As Integer
Dim l016E As Integer
Dim l0170 As Integer
Dim l0172 As String
Dim l0174 As String
Dim l0176 As Integer
Dim l0178 As String
Dim l017A As Integer
Dim l017E As Integer
' Const mc0180 = 12 ' &HC%
' Const mc0182 = 32 ' &H20%
' Const mc0184 = 2 ' &H2%
' Const mc0186 = 6 ' &H6%
' Const mc018A = 7 ' &H7%
  If gv792A Then Exit Sub
  sub0739 " ("
  l0168 = gv79BC(p0166).M2348
  If l0168 Then
    If gv79BC(p0166).M2351 Then
      sub0739 ("Index%")
      If Len(gv147E(l0168).M2A6D) Then sub0739 ", "
    End If
    sub0739 gv147E(l0168).M2A6D
  ElseIf gv79BC(p0166).M15FE = -1 Then
    l0174 = gv79BC(p0166).M1B23
    For l0168 = 2 To Len(l0174)
      If l016A Then sub0739 ", "
      l016A = l016A + 1
      l017E = Asc(Mid$(l0174, l016A + 1, 1))
      l017A = l017E And mc00E0
      If l017E Then
        If l017A = mc0180 Then
          l017E = l017E And mc0182
        ElseIf l017E = mc00E2 Then
          l017E = l017E And mc00E4
        Else
          l017E = l017E And &H38
        End If
        If l017E = 0 Then sub0739 "ByVal "
      End If
      sub0739 "p" & Format$(l016A)
      Select Case l017A
      Case 0
      Case mc0184 To mc0186, mc0180
        sub0739 Mid$(gc1120, l017A, 1)
      Case mc018A: sub0739 " As Variant"
      Case Else
        sub0739 " As Any"
      End Select
    Next
  Else
    l0172 = Chr$(gv79BC(p0166).M2364)
    l0168 = InStr(gv09F0, l0172)
    l0174 = gv79BC(p0166).M1B23
    l0176 = gv79BC(p0166).M236D
    l0178 = gv79BC(p0166).M2378
    If l0168 <> l0176 Then
      If l0176 = 0 Or l0176 < l0168 Then
        l0176 = l0168
        gv79BC(p0166).M236D = l0168
        gv090E = True
      ElseIf l0168 = 0 Then
        l0168 = l0176
      Else
        l0176 = l0168
      End If
    End If
    l0170 = gv79BC(p0166).M235A
    If l0170 Then
      l0168 = InStr(l0168 + 1, gv09F0, l0172)
      l0176 = l0168
    End If
    If Len(l0174) And Len(l0178) = 0 Then
      If l0176 Then
        l0178 = Chr$(0)
        Do While l0168
          l016C = Asc(Mid$(gv7884, l0168, 1))
          Select Case l016C And mc0100
          Case mc00FC
            If (gv09B8(l0168) And &H8001) = 0 Then
              l0178 = l0178 + Chr$(l0168 - l0176)
            Else
              Exit Do
            End If
          Case mc0078
            l0178 = l0178 + Chr$(l0168 - l0176)
          End Select
          l0168 = InStr(l0168 + 1, gv09F0, l0172)
        Loop
      Else
      End If
    End If
    If Len(l0178) <> Len(l0174) Then
      If Len(l0174) Then
        l0174 = ""
      End If
    End If
    l0168 = l0176
    Do While l0168
      l016E = Asc(Mid$(gv09F0, l0168, 1))
      If l016E <> 0 And l016E <> Asc(l0172) Then Exit Do
      l016C = Asc(Mid$(gv7884, l0168, 1))
      Select Case l016C And mc0100
      Case mc00FC
        If (gv09B8(l0168) And &H8001) = 0 Then
          If l016A Then sub0739 ", "
          l016A = l016A + 1
          sub0739 "ByVal "
          sub075D l0168, gv09B8(), gv7884, False
        Else
          Exit Do
        End If
      Case mc0078
        If l016A Then sub0739 ", "
        l016A = l016A + 1
        If Len(l0174) > l016A Then
          l017E = Asc(Mid$(l0174, l016A + 1, 1))
          If l017E Then
            If (l017E And mc00E0) = mc0180 Then
              If l017E And mc0182 Then
              Else
                sub0739 "ByVal "
              End If
            ElseIf l017E And &H38 Then
            Else
              sub0739 "ByVal "
            End If
          End If
        End If
        sub075D l0168, gv09B8(), gv7884, False
      End Select
      l0168 = InStr(l0168 + 1, gv09F0, l0172)
    Loop
    l0174 = gv79BC(p0166).M1B23
    l0168 = Len(l0174) - 1
    While l016A < l0168
      If l016A Then sub0739 ", "
      sub0739 "p"
      l016A = l016A + 1
    Wend
  End If
  sub0739 ")"
  l0168 = gv79BC(p0166).M2341
  l0168 = gv0DA6(l0168 + mc008A)
  If l0168 Then sub0776 l0168, 0, False
  sub05F9
End Sub

Sub sub09CC()
  m0040 = gv79BC(gv7A2A).M236D
  If m0040 = 0 Then
    m0040 = gv09EE
  End If
  gv10E4 = 0
End Sub

Function fn0D2A(p0190 As Integer) As String
Dim l0192 As String
Dim l0194 As Integer
Dim l0198 As Integer
Dim l019A As Integer
  l0192 = fn0BE7(p0190)
  If Len(l0192) Then
    fn0D2A = l0192
    Exit Function
  End If
  l019A = Asc(Mid$(gv7884, p0190, 1))
  Select Case l019A And mc0100
  Case mc0108
    fn0D2A = fn0C02(fn0326(gv09B8(p0190), 2))
    Exit Function
  Case m0128, mc0104
    Select Case l019A
    Case mc009C
      fn0D2A = fn0C8E(gv0DA6(fn0D6C(gv09B8(p0190)) + mc0118))
      Exit Function
    Case 0
      sub01CC
    Case Else
      sub01CC
    End Select
    fn0D2A = "v" & fn02BE(p0190 * 2)
    Exit Function
  Case mc010A
    l0198 = gv09B8(p0190)
    sub01CC
    If p0190 < m0042 Then
      If l0198 And &H8001 Then
        l0194 = mc0106
      ElseIf l0198 < gv09B6 Then
        l0194 = mc0108
      Else
        l0194 = mc0106
      End If
    Else
      If l0198 = 0 Then
        l0194 = mc0102
      ElseIf l0198 And &H8001 Then
        l0194 = mc00FC
      Else
        sub01CC
      End If
    End If
  Case Else
    sub01CC
  End Select
  fn0D2A = fn0C27(p0190)
End Function

Sub sub0A1E(p01A2 As Integer)
Dim l01A4 As Integer
Dim l01A6 As Integer
Dim l01AA As String * 1
  l01AA = Chr$(gv79BC(gv7A2A).M2364)
  If m0040 = gv09EE Then
    Exit Sub
  End If
    l01A6 = m0040
    Do
      l01A4 = gv09B8(l01A6)
      If l01A4 <= p01A2 Then
        If l01A4 < p01A2 Then Exit Do
        If Asc(Mid$(gv7884, l01A6 - 1, 1)) = mc0076 Then Exit Sub
        sub0970 l01A6, 1, mc00FC, mc0076, 0
        Mid$(gv09F0, l01A6, 1) = l01AA
        Exit Sub
      End If
      l01A6 = InStr(l01A6 + 1, gv09F0, l01AA)
    Loop While l01A6
End Sub

Sub sub0BBA(p01AC As Integer, p01AE As Integer)
Dim l01B0 As Integer
Dim l01B2 As Integer
Dim l01B4 As Integer
Dim l01B6 As Integer
' Const mc01B8 = 16 ' &H10%
  l01B2 = Asc(Mid$(gv7884, p01AC, 1)) And mc0100
  If l01B2 > p01AE Then Exit Sub
  l01B6 = Asc(Mid$(gv09F0, p01AC, 1))
  If gv7A2A Then
    l01B4 = gv79BC(gv7A2A).M2364
    If l01B6 Then
      If l01B6 <> l01B4 Then
        If p01AE = mc0102 Then
          l01B2 = Asc(Mid$(gv7884, p01AC, 1)) And Not mc0100
          If l01B2 And mc01B8 Then
            Exit Sub
          End If
          p01AE = mc0106
          Mid$(gv7884, p01AC, 1) = Chr$(l01B2 Or p01AE)
          gv0910 = True
          Exit Sub
        ElseIf p01AE <> mc0106 Then
          If gv0B86 Then sub01CC
        End If
      End If
    End If
  End If
  l01B0 = gv09B8(p01AC)
  If p01AE <= mc010A Then
    If l01B0 = 0 Then
      p01AE = mc0102
    ElseIf l01B0 < 0 Then
    ElseIf l01B0 And 1 Then
      sub01CC
    ElseIf l01B0 And &H8001 Then
      sub01CC
      If -l01B0 > gv79F2.M15F7 Then p01AE = mc00FC
    ElseIf l01B0 >= 6 And l01B0 < gv79F2.M1733 \ &H200 Then
      p01AE = mc0078
    Else
      p01AE = mc010A
    End If
  End If
  If l01B6 Or gv7A2A = 0 Then
    Exit Sub
  End If
  Select Case p01AE
  Case mc0078, mc00FC
    If p01AC < m0042 Then m0042 = p01AC
    GoSub L1C80E
  Case mc0108
  Case mc0102
    If l01B0 Then
      p01AE = mc0106
    Else
      GoSub L1C80E
    End If
  Case mc0106
  Case mc010A
    GoSub L1C80E
  Case mc0104
  Case Else
    sub01CC
  End Select
Exit Sub

L1C80E:
  If p01AC < m0042 Then Return
  If l01B4 > m003E Then
  Else
    Mid$(gv09F0, p01AC, 1) = Chr$(l01B4)
    If p01AC < m0040 Then
      gv79BC(gv7A2A).M236D = p01AC
      m0040 = p01AC
    End If
    gv0910 = True
  End If
Return
End Sub
