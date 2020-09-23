Attribute VB_Name = "MODULE121"
' Module12
' Option Explicit
Const mc00E2 = 64 ' &H40%
Const mc010E = 4130815  ' &H3F07FF&
Const mc0112 = 4130752  ' &H3F07C0&
Const mc0116 = 4130754  ' &H3F07C2&
Const mc011A = 1026 ' &H402%
Const mc0194 = 48 ' &H30%
Const mc01CE = 256 ' &H100%
Const mc01D0 = 5 ' &H5%
Const mc01D2 = 32 ' &H20%
Const mc01D4 = 4 ' &H4%
Const mc01D6 = 8 ' &H8%
Const mc01DA = 64 ' &H40%
Const mc01EC = 2 ' &H2%
Const mc01EE = 3 ' &H3%
Const mc01F0 = 32 ' &H20%
Const mc01F2 = 5 ' &H5%
Const mc01F4 = 1 ' &H1%
Const mc01FA = 2048 ' &H800%
Const mc01FC = 2 ' &H2%
Const mc01FE = 7 ' &H7%
Const mc0200 = 9 ' &H9%
Const mc0202 = 10 ' &HA%
Const mc0204 = 224 ' &HE0%
Const mc0206 = 32 ' &H20%
Const mc0208 = 64 ' &H40%
Const mc020A = 128 ' &H80%
Const mc020C = 192 ' &HC0%
Const mc020E = 16 ' &H10%
Const mc0210 = 2 ' &H2%
Const mc0218 = 9 ' &H9%
Const mc021A = 1 ' &H1%
Const mc021E = 512 ' &H200%
Const mc0220 = 12 ' &HC%
Const mc0222 = 9 ' &H9%
Const mc0224 = 10 ' &HA%
Const mc0226 = 8192 ' &H2000%
Const mc023E = -2 ' &HFFFE%
Const mc0240 = 96 ' &H60%
Const mc0242 = 224 ' &HE0%
Const mc024E = 10 ' &HA%
Const mc0250 = 16 ' &H10%
Const mc0258 = 4 ' &H4%
Const mc0284 = 224 ' &HE0%

Function fn09EE() As Integer
Dim l01A6 As Integer
Dim l01A8 As Integer
Dim l01AC As Integer
Dim l01AE As Integer
Dim l01B0 As Integer
Dim l01B2 As Integer
Dim l01B4 As Integer
Dim l01B6 As Integer
Dim l01B8 As String
Dim l01BA As String
Dim l01BC As Integer
Dim l01C0 As T24BE
Dim l01C4 As Integer
Dim l01C6 As Long
Dim l01C8 As Long
Dim l01CA As Long
' Const mc01CE = 256 ' &H100%
' Const mc01D0 = 5 ' &H5%
' Const mc01D2 = 32 ' &H20%
' Const mc01D4 = 4 ' &H4%
' Const mc01D6 = 8 ' &H8%
' Const mc01DA = 64 ' &H40%
' Const mc01EC = 2 ' &H2%
' Const mc01EE = 3 ' &H3%
' Const mc01F0 = 32 ' &H20%
' Const mc01F2 = 5 ' &H5%
' Const mc01F4 = 1 ' &H1%
' Const mc01FA = 2048 ' &H800%
' Const mc01FC = 2 ' &H2%
' Const mc01FE = 7 ' &H7%
' Const mc0200 = 9 ' &H9%
' Const mc0202 = 10 ' &HA%
' Const mc0204 = 224 ' &HE0%
' Const mc0206 = 32 ' &H20%
' Const mc0208 = 64 ' &H40%
' Const mc020A = 128 ' &H80%
' Const mc020C = 192 ' &HC0%
' Const mc020E = 16 ' &H10%
' Const mc0210 = 2 ' &H2%
' Const mc0218 = 9 ' &H9%
' Const mc021A = 1 ' &H1%
' Const mc021E = 512 ' &H200%
' Const mc0220 = 12 ' &HC%
' Const mc0222 = 9 ' &H9%
' Const mc0224 = 10 ' &HA%
' Const mc0226 = 8192 ' &H2000%
  sub0952
  l01C0.M1CAA = gv0918
  l01C0.M24CD = gv2326
  l01C0.M24FD = gv10E4
  l01A6 = gv231E.M2D5A
  gv0912 = gv1584.M2B24 * 2
  If gv0912 < 0 Then
    Get gv0882, , gv0912: gv0914 = gv0918 + 4 + gv0912
  Else
    gv0914 = gv0918 + 2 + gv0912
  End If
  If l01A6 = 0 Then
    sub0214 mc01CE
    fn09EE = mc0194
    gv10E4 = gv10E4 + 1
    gv10CE(gv10E4) = l01C0
    Exit Function
  End If
  Select Case gv1584.M2B0C And &HF
  Case mc01D0
    fn09EE = mc01D2
    gv10E4 = -1
    l01C0.M24FD = -1
  Case mc01D4
    fn09EE = mc0194
  Case mc01D6
    l01C0.M24F1 = gc1146
    If gv792A > 0 Then
      If gv792A And gc10EA Then
        l01C0.M24F7 = "$"
        Get gv0882, gv0918 + 2, l01A8
        Get gv0882, , l01A6
        If l01A8 > 2 Then
          Get gv0882, , l01A6
          If l01A6 Then
            l01C0.M24EB = Space$(l01A6): Get gv0882, , l01C0.M24EB
          End If
        End If
        l01C0.M24EB = """" + l01C0.M24EB + """"
      End If
    End If
  Case Else
    If gv792A > 0 Then
      l01AC = Asc(Mid$(gv10E6, gv2326 + 1, 1))
      l01B6 = Asc(Mid$(gv10E6, gv2326 + 2, 1))
      If l01B6 And gc117A Then
        l01B6 = l01B6 Xor gc117A
      End If
      If l01AC Then
        l01AC = gv2326 - l01AC
        l01AE = Asc(Mid$(gv10E6, l01AC, 1))
        l01B4 = Asc(Mid$(gv10E6, l01AC + 1, 1))
        If l01AE > gc10F2 Then
          Get gv0882, gv0914 - 2, l01A8: l01A8 = fn0326(l01A8, 2)
          If gv1584.M2B14 And mc01DA Then
            l01C0.M24D3 = l01A8
          End If
          Select Case l01B6
          Case 0
          Case gc1116
            Select Case l01AE
            Case gc10FA
              l01A8 = fn0336(l01A8, 2)
              l01A8 = fn0D6C(l01A8)
            Case gc10F6
              l01A8 = fn0D6C(gv09B8(l01A8))
            Case Else
              sub0214 mc01CE
            End Select
            l01B4 = gv0DA6(l01A8)
            If l01B4 <> mc01EC And l01B4 <> mc01EE Then
              sub0214 mc01F0
            End If
            l01B4 = gv0DA6(l01A8 + mc01F2)
            l01B4 = gv1180(l01B4)
            l01A8 = gv0DA6(l01A8 + mc01F4)
            l01AE = gc10FE
            l01C0.M24E5 = l01A8
            If l01B4 = gc1136 Then
              l01B2 = gc1138
            End If
          Case gc110C
            l01B4 = Asc(Mid$(gv7884, l01A8, 1))
            Select Case l01AE
            Case gc10F8, gc10F6, gc10F4
            Case gc10FA
              l01C4 = Asc(Mid$(gv7880, fn0326(gv09B8(l01A8), 2), 1))
              If l01C4 < l01B4 Then
                Mid$(gv7880, fn0326(gv09B8(l01A8), 2), 1) = Chr$(l01B4)
              ElseIf l01C4 > l01B4 Then
                l01B4 = l01C4
                Mid$(gv7884, l01A8, 1) = Chr$(l01B4)
              End If
            Case Else
              sub0214 mc01F0
              l01B4 = 0
            End Select
            If (l01B4 And &HF) Then
              l01B4 = gv1180(l01B4 And &HF)
            Else
              l01B4 = gc1140
            End If
            If l01B4 = gc1146 Then l01B0 = gc116C
          Case gc1114
            If l01B4 <> gc113E Then sub0214 mc01FA
            If gv09B8(l01A8) < 0 Then
              l01AE = gc10F8
            Else
              l01AE = gc10F4
            End If
          Case gc111C
            Get gv0882, gv0914, l01A6
            If gv0884 = 3 And l01A6 = &H6A63 Then
              l01B4 = gc1146
            ElseIf gv0884 = 2 And l01A6 = &H5D63 Then
              l01B4 = gc1146
            Else
              l01A6 = Asc(Mid$(gv7884, l01A8, 1))
              Select Case l01A6 And &H1F
              Case 0
                  l01B4 = gc114A
              Case mc01FC
                l01B4 = gc1134
              Case mc01FE
                l01B4 = gc1146
              Case mc0200
                l01B4 = gv1180(l01A6 And &H1F)
              Case mc0202
                sub01CC
                l01B4 = gv1180(l01A6 And &H1F)
              Case Else
                l01B2 = gc114A
              End Select
            End If
          Case gc111A
            l01A6 = Asc(Mid$(gv7884, l01A8, 1))
            Select Case l01A6 And mc0204
            Case Is <= mc0206
              If gv09B8(l01A8) < 0 Then
                l01AE = gc10F8
              End If
            Case mc0208
              l01AE = gc10F4
            Case mc020A
              l01AE = gc10F8
            Case mc020C
              l01AE = gc10FC
            Case Else
              sub0214 mc01F0
              l01A6 = 0
            End Select
            If l01A6 And &HF Then
              l01B4 = gv1180(l01A6 And &H1F)
              If l01B4 = gc1130 Then
                l01B4 = gc1148
              ElseIf l01B4 And 8 Then
                l01B0 = gc116C
              ElseIf l01AE = gc10F4 Then
                l01B0 = gc116E
              End If
            Else
              l01B4 = gc1148
            End If
          Case gc1112
            l01AE = gv09B8(l01A8 + 2)
            l01B4 = l01AE And &HFF
            Select Case l01AE And &HF00
            Case 0
              l01AE = gc10FA
            Case &H100, &H200
              l01AE = gc10F6
            Case &H500
              l01AE = gc10F4
            Case &H600
              l01AE = gc10F8
            Case Else
              sub0214 mc01FA
            End Select
            Get gv0882, gv0918 + 2, l01A6
            If l01A6 < 0 Then
              l01B4 = gc1144
            Else
              If l01B4 = 0 Then
                l01B4 = gc1140
              Else
                l01B4 = gv1180(l01B4)
                If l01B4 And 8 Then
                  l01B0 = gc116C
                Else
                  l01B0 = gc116E
                End If
              End If
              l01B4 = l01B4 Or mc020E
            End If
          Case gc110E, gc1110
            If l01AE <> gc1102 Then sub0214 mc01FA
            l01A6 = gv0980(l01A8 + mc0210)
            Select Case l01A6
            Case Is < 0, Is >= mc0218
              l01A6 = mc0202
            Case mc01FE
              l01B0 = gc116C
            Case Is < mc01FE
            End Select
            l01B4 = gv1180(l01A6)
            If gv0980(l01A8 + mc021A) And 1 Then
              l01B4 = l01B4 Or mc020E
              If l01B6 <> gc1110 Then sub0214 mc01FA
            ElseIf l01B6 = gc1110 Then
              sub0214 mc01FA
            End If
          End Select
          l01C0.M24D9 = l01AE
          l01C0.M24DF = l01B4
          If gv792A And gc10EA Then
            l01C0.M24EB = Mid$(gc10EE, l01AE, 1)
            If l01B4 = gc1144 Then
              l01C0.M24EB = l01C0.M24EB + "a" + fn02BE(l01C0.M24D3 * 2)
            Else
              If l01B4 And mc020E Then
                l01C0.M24EB = l01C0.M24EB + "a"
                l01B4 = l01B4 And &HF
              ElseIf l01AE <> gc10FE Then
                l01C0.M24EB = l01C0.M24EB + "v"
              End If
              l01C0.M24EB = l01C0.M24EB + fn02BE(fn032E(l01C0.M24D3) * 2)
              If l01B4 > 1 Then
                l01C0.M24EB = l01C0.M24EB + Mid$(gc1120, l01B4, 1)
              End If
            End If
          Else
            l01B4 = l01B4 And &HF
          End If
          If l01B2 = 0 Then l01B2 = l01B4 And &H3F
          If l01B2 >= gc114C Then sub0214 mc01FA
          l01B2 = l01B2 Or l01B0
          l01A6 = Asc(Mid$(gv10E6, l01AC + 2, 1))
          l01B8 = Mid$(gv10E6, l01AC + 3, l01A6)
        Else
          l01B2 = gc1130
          l01A6 = 0
          Select Case l01B6
          Case 0
            l01A6 = Asc(Mid$(gv10E6, l01AC + 2, 1))
            l01B8 = Mid$(gv10E6, l01AC + 3, l01A6)
          Case gc1118
            Get gv0882, gv0914 - 4, l01AC
            l01AC = gv10EC + (l01AC And &HFF) * &H18
            l01AC = l01AC + Asc(Mid$(gv10E6, l01AC, 1)) + 1
            Get gv0882, , l01A6
            l01A6 = l01A6 + 1
            l01B8 = Mid$(gv10E6, l01AC, l01A6)
          Case gc111E
            l01A6 = Asc(Mid$(gv10E6, l01AC + 2, 1))
            l01B8 = Mid$(gv10E6, l01AC + 3, l01A6)
          Case Else
            sub0214 mc01CE
          End Select
        End If
        If l01A6 Then
          l01C0.M24F1 = l01B2 Or l01B0
          l01B2 = l01B2 And gc1176
          For l01AC = Len(l01B8) To 2 Step -1
            l01A6 = Asc(Mid$(l01B8, l01AC, 1))
            If l01A6 And gc1168 Then
              Select Case l01A6 And &H1F
              Case gc114C
                Get gv0882, gv0918 + 2, l01A6
                If l01C0.M24E5 Then
                  sub0A00 l01C0, l01A6, l01BA
                Else
                  If l01A6 > 0 Then GoSub L12ADE
                End If
              Case gc114E
                l01B0 = 0
                l01A6 = gc1134: l01B4 = l01A6
                Get gv0882, gv0918 + 2, l01A8
                If l01A8 >= 0 Then GoSub L12C4C
                GoSub L12C4C
              Case Else
                sub0214 mc021E
              End Select
            ElseIf l01A6 Then
              If (l01A6 And &HF) = gc1130 Then
                l01B0 = l01A6 And gc116E
                l01B4 = l01B2 Or l01B0
                If l01B4 > &H17 Then
                  sub0214 mc01FA
                  l01B4 = l01B4 Xor gc116E
                End If
              Else
                l01B4 = l01A6 And gc1176
              End If
              GoSub L12C4C
            End If
          Next
          l01A6 = Asc(Left$(l01B8, 1))
          If l01A6 And gc1168 Then
            sub0214 mc01FA
          ElseIf l01A6 Then
            l01B4 = l01A6 And gc1174
            Select Case l01B4
            Case gc1148, gc114A
              If l01B2 > gc1130 Then
                l01A6 = (l01A6 Xor l01B4) Or l01B2
              End If
            Case Is >= gc114C
              sub0214 mc01FA
            End Select
          End If
          GoSub L12FF2
          If gv792A And gc10EA Then
            l01C0.M24EB = l01C0.M24EB + "(" + Mid$(l01BA, 2) + ")"
          End If
        End If
      ElseIf l01B6 Then
        sub0214 mc01F0
      End If
    ElseIf gv792A = True Then
      If gv1584.M2B14 And mc01DA Then
        Get gv0882, gv0914 - 2, l01A8
        If gv231E.M2D6A = mc0220 Then
          Get gv0882, gv0918 + 2, l01A6
          If gv231E.M2D61 = mc0222 Then
            sub0798 l01A8, gv231E.M2D61, gv231E.M2D6A
            l01A8 = gv09B8(fn0326(l01A8, 2))
          ElseIf gv231E.M2D61 <> mc0224 Then
          End If
          sub0A0E l01A8, l01A6
        Else
          sub0798 l01A8, gv231E.M2D61, gv231E.M2D6A
        End If
      ElseIf gv0884 = 3 Then
        Select Case gv2326
        Case &H4FA6
          Get gv0882, gv0914 - 2, l01A6
          sub0A1E l01A6
        Case &H6F79
        End Select
      ElseIf gv0884 = 2 Then
        Select Case gv2326
        Case &H4D89
          Get gv0882, gv0914 - 2, l01A6
          sub0A1E l01A6
        Case &H6231
        End Select
      End If
    End If
  End Select
  If gv792A > 0 Then
    gv10E4 = gv10E4 + 1
    gv10CE(gv10E4) = l01C0
  End If
Exit Function

L12ADE:
  If l01A6 <= 0 Then
    If l01A6 = &H8000 Then
      If l01C0.M24F1 <> gc1144 Then
        l01C0.M24F1 = gc1144
      End If
    End If
    Return
  End If
  l01AE = l01C0.M24FD
  While l01A6 > 0
    If l01AE < 0 Then
      If gv792A And gc10EA Then
        l01BA = ",?" + l01BA
      End If
      l01A6 = l01A6 - 1
    Else
      l01BC = gv10CE(l01AE).M24F1
      If l01BC Then
        l01B8 = l01B8 + Chr$(l01BC)
        If gv792A And gc10EA Then
          l01BA = "," + gv10CE(l01AE).M24F7 + l01BA
        End If
        l01A6 = l01A6 - 1
      End If
      l01AE = gv10CE(l01AE).M24FD
    End If
  Wend
  l01C0.M24FD = l01AE
Return

L12C4C:
  If l01B4 And gc116A Then sub0214 mc01FA: l01B4 = l01B4 Xor gc116A
  l01AE = l01C0.M24FD
  Do While l01AE > 0
    l01C4 = gv10CE(l01AE).M24F1 And gc1176
    If l01C4 Then
      If l01C4 <> l01B4 Then
        l01C6 = gv1150(l01B4)
        l01C8 = gv1150(l01C4)
        l01CA = l01C6 And l01C8
        If l01C6 = 0 Or l01C8 = 0 Then
          sub0214 mc01FA
        ElseIf l01CA = 0 Then
        ElseIf l01CA = l01C6 Then
          If gv10CE(l01AE).M24E5 Then
            sub0214 mc01FA
          ElseIf gv10CE(l01AE).M24D3 > 0 Then
            sub098D gv10CE(l01AE).M24D3, (gv10CE(l01AE).M24D9 * &H20) And &HFF, (gv1196(l01B4 And gc1174)), 0
          Else
            sub0214 mc01FA
          End If
          l01C4 = gv10CE(l01AE).M24F1 Xor l01C4
          gv10CE(l01AE).M24F1 = l01B4 Or l01C4
        ElseIf l01CA = l01C8 Then
          l01B4 = l01C4
          If (l01A6 And &HF) = gc1130 Then
            l01B2 = l01B4
            If l01C0.M24D3 Then
              If l01C0.M24E5 Then
                sub0214 mc0226
              Else
                sub0214 mc0226
              End If
            End If
          End If
        Else
          sub0214 mc01FA
        End If
      End If
      If gv792A And gc10EA Then
        l01BA = "," + gv10CE(l01AE).M24F7 + l01BA
      End If
      l01C0.M24FD = gv10CE(l01AE).M24FD
      Return
    End If
    l01AE = gv10CE(l01AE).M24FD
  Loop
  sub0214 mc01FA
  If gv792A And gc10EA Then
    l01BA = l01BA + ",?"
  End If
  l01C0.M24FD = 0
Return

L12FF2:
  If (l01A6 And gc1174) = gc1130 Then
    l01B0 = l01A6 And Not gc1174
    l01A6 = l01B2 Or l01B0
  End If
  If l01A6 And 8 Then l01A6 = l01A6 And Not gc116E
  If l01A6 = gc1130 Then sub0214 mc01FA
  l01C0.M24F1 = l01A6
  If l01A6 = 0 Then Return
  If l01C0.M24D3 And l01C0.M24E5 = 0 Then
    If (l01C0.M24DF And gc1174) > gc1146 Then
      If l01C0.M24DF <> (l01A6 And gc1174) Then
        sub098D l01C0.M24D3, l01C0.M24D9 * &H20, (gv1196(l01A6 And &HF)), 0
      End If
    End If
  End If
  If gv792A And gc10EA Then
    If l01A6 And &HF Then
      l01C0.M24F7 = Mid$(gc1128, l01A6 And &HF, 1)
    Else
      l01C0.M24F7 = "?"
    End If
    If l01A6 And gc116C Then
      l01C0.M24F7 = "R" + l01C0.M24F7
    ElseIf l01A6 And gc116E Then
      l01C0.M24F7 = "^" + l01C0.M24F7
    End If
    If Len(l01C0.M24EB) = 0 Then l01C0.M24EB = l01C0.M24F7
  End If
Return
End Function

Function fn0A2D(p0262 As T24BE) As String
Dim l0264 As String
Dim l0266 As Integer
Dim l0268 As Integer
  If p0262.M24D9 <= 1 Then Exit Function
  l0266 = p0262.M24D9
  l0264 = Mid$(gc10EE, l0266, 1)
  l0268 = p0262.M24DF
  If l0268 = gc1144 Then
    l0264 = l0264 + "a" + fn02BE(p0262.M24D3 * 2)
  Else
    If l0268 And mc020E Then
      l0264 = l0264 + "a"
      l0268 = l0268 And &HF
    ElseIf l0266 <> gc10FE Then
      l0264 = l0264 + "v"
    End If
    l0264 = l0264 + fn02BE(p0262.M24D3 * 2)
    If l0268 > 1 Then
      l0264 = l0264 + Mid$(gc1120, l0268, 1)
    End If
  End If
  fn0A2D = l0264
End Function

Function fn0A38() As String
Dim l0156 As String
Dim l0158 As Integer
  l0158 = fn09EE()
  If gv231E.M2D5A Then l0156 = fn0B49(gv158E)
  If Len(l0156) = 0 Then l0156 = "****"
  l0156 = Chr$(9) & l0156
  If gv10E4 >= 0 Then l0156 = " " & gv10CE(gv10E4).M24EB & l0156
  If gv1584.M2B24 < 0 Then
    Seek gv0882, gv0914
  Else
    gv0918 = gv0918 + 2
    Seek gv0882, gv0918
    While gv0918 < gv0914
      Get gv0882, , gv0912
      l0156 = l0156 & " +" & fn02BE(gv0912)
      gv0918 = gv0918 + 2
    Wend
  End If
  fn0A38 = l0156
End Function

Sub sub01AB()
  sub023E gv0914, gv091C
  If gv0914 <= 1 Then
    sub01CC
    Exit Sub
  End If
  Seek gv0882, gv0914
If gv792A Then
    sub09CC
    Do While gv0914 < gv091C
      If fn09EE() = mc0194 Then
        Exit Do
      End If
    Loop
    sub09DE
Else
    gv097C = 0
    ReDim gv094A(gv79F2.M17BE + 20)
    sub07A6
    gv094A(gv097C + 1).M1CAA = gv0914
    sub05D8 gv091C - gv0914, 1
End If
End Sub

Sub sub01F5()
Dim l0228 As Integer
Dim l022A As Integer
Dim l022C
Dim l022E
Dim l0230 As Integer
Dim l0234 As T1F4C
Dim l0236 As Integer
' Const mc023E =-2 ' &HFFFE%
' Const mc0240 = 96 ' &H60%
' Const mc0242 = 224 ' &HE0%
' Const mc024E = 10 ' &HA%
' Const mc0250 = 16 ' &H10%
' Const mc0258 = 4 ' &H4%
  sub01E6 l0236, gc00BE & gc00E6 & gc00DE
  gv2326 = mc023E
  gv7A2A = 0
  Do
    Get gv0882, , l0228
    Get gv0882, , l022C
    Get gv0882, , l022E
    If l022E <> &H1E Then Exit Do
    If l022C = 0 Then
      l022C = (l0228 - 4) \ 3
      While l022C > 0
        Get gv0882, , l0234
        l0228 = fn0326(l0234.M1D63, 2)
        Select Case Asc(l0234.M14D2)
        Case 7
          sub0970 l0228, 2, mc0240, mc01FE, 0
        Case 9
          Select Case gv09B8(l0228) And &HF000
          Case &H8000
            If (gv09B8(l0228 - 1) And &HFF00) <> &H4000 Then sub01CC
            sub0970 l0228, 2, mc0242, mc0218, 0
            sub097E fn0BAE(gv79B6, gv09B8(l0228) And &HFF), l0228
          Case &HC000
            sub0970 l0228, 2, mc0242, mc0218, 0
            sub0671 gv79A6.M14DF
            sub097E fn0AF0(gv09B8(l0228)), l0228
          Case 0
            If gv09B8(l0228 - 1) > gv0F92 Then
              sub01CC
            Else
              sub098D l0228, mc0240, mc0218, 0
            End If
          Case Else
            sub01CC
          End Select
        Case &H20
          If Not fn0C1A(gv09B8(l0228 - 1)) Then sub01CC
          sub098D l0228, mc0240, mc024E, 0
        Case &H80
          sub0998 l0228
        Case &HC0
          If gv09B8(l0228) Then sub01CC
          sub0998 l0228
        Case Else
          sub0214 mc0250
        End Select
        l022C = l022C - 1
      Wend
    Else
      If l0228 \ 2 <> l022C + 2 Then sub0214 mc01F0
      While l022C
        Get gv0882, , l022A
        l0228 = fn0326(l022A, 2)
        l0230 = Asc(Mid$(gv7884, l0228, 1))
        If l0230 = 0 Then
          If l022A And 1 Then
            If gv09B8(l0228 + 1) <> gv0B36 Then sub01CC
            sub0970 l0228, 2, mc020C Or 7, 0, 0
          Else
            sub0998 l0228
          End If
        End If
        l022C = l022C - 1
      Wend
    End If
  Loop
  sub046E l0236, gc00BE & gc00DA
  sub09A9 gv79B6
  For l0228 = 1 To gv79EE
    If gv79BC(l0228).M230D = gv79B8 Then
      If (gv791C And mc0258) = 0 Then
        
      End If
      DoEvents
      If gv79BC(l0228).M15FE <> -1 Then
        sub019C l0228
      End If
    End If
  Next l0228
  sub046E l0236, gc00C2 & gc00E6 & gc00D2
  sub09BB
  sub0205 l0236
End Sub

Sub sub04C4()
Dim l0276 As Integer
Dim l0278 As Integer
Dim l027A
Dim l027C
Dim l027E As Integer
' Const mc0284 = 224 ' &HE0%
' Const mc0286 = 231 ' &HE7%
  Get gv0882, gv0B5C + gv09B2, l0276
  Get gv0882, , l027A
  Get gv0882, , l027C
  If l027C <> &H1E Then sub01CC
  If l0276 \ 2 <> l027A + 2 Then sub01CC
  While l027A > 0
    Get gv0882, , l0278
    l0276 = fn0326(l0278, 2)
    l027E = Asc(Mid$(gv7880, l0276, 1))
    If l027E = 0 Then
      If l0278 And 1 Then
        If gv0980(l0276 + 1) <> gv0B36 Then sub01CC
        sub04A7 l0276, 2, mc0284, mc01FE, 0
        If Asc(Mid$(gv7880, l0276, 1)) <> mc0286 Then sub01CC
      Else
        sub095F l0276
      End If
    End If
    l027A = l027A - 1
  Wend
End Sub

Sub sub05E8(p026E As Integer)
  gv2326 = p026E
  If gv2326 <= 0 Then
    gv231E.M1B05 = 0
    gv231E.M2D5A = 0
  Else
    gv231E.M1B05 = Asc(Mid$(gv77FA, gv2326, 1))
    gv231E.M2D61 = Asc(Mid$(gv77FA, gv2326 + 1, 1))
    gv231E.M2D6A = Asc(Mid$(gv77FA, gv2326 + 2, 1))
    gv231E.M2D5A = gv234E.M24DF(gv2326 \ 3)
  End If
  gv158E = gv231E.M2D5A And &H1FF
  LSet gv1584 = gv15EA.M2B71(gv158E)
  gv1584.M2B24 = (gv1584.M2B0C And &HF000) \ &H1000
  gv1584.M2B2C = (gv1584.M2B14 And &HF000) \ &H1000
End Sub

Sub sub03EE()
  Erase gv0F7C
  ReDim gv0FB6(0)
  gv0FE8 = 0
  gv0F7C(1).M1BD4 = "Form"
    gv0F7C(1).M2427 = &HD + 1
    gv0F7C(1).M243B = 5
  gv0F7C(2).M1BD4 = "MDIForm"
    gv0F7C(2).M2427 = &H14 + 1
  gv0F7C(4).M1BD4 = "Control"
    gv0F7C(4).M2427 = 0
  gv0F7C(5).M1BD4 = "Controls"
    gv0F7C(5).M243B = -4
  gv0F7C(8).M1BD4 = "Forms"
    gv0F7C(8).M243B = -1
    gv0F7C(&HB).M1BD4 = "Database"
      gv0F7C(&HB).M2427 = &H20 + 1
      gv0F7C(&HB).M243B = &HC
    gv0F7C(&HC).M1BD4 = "TableDefs"
      gv0F7C(&HC).M2427 = &H1D + 1
      gv0F7C(&HC).M243B = -&HD
    gv0F7C(&HD).M1BD4 = "TableDef"
      gv0F7C(&HD).M2427 = &H1A + 1
      gv0F7C(&HD).M243B = &HF
    gv0F7C(&HE).M1BD4 = "Dynaset"
      gv0F7C(&HE).M2427 = &H1F + 1
      gv0F7C(&HE).M243B = &HF
    gv0F7C(&HF).M1BD4 = "Fields"
      gv0F7C(&HF).M2427 = &H1C + 1
      gv0F7C(&HF).M243B = -&H11
    gv0F7C(&H10).M1BD4 = "Indexes"
      gv0F7C(&H10).M2427 = &H1E + 1
      gv0F7C(&H10).M243B = -&H12
    gv0F7C(&H11).M1BD4 = "Field"
      gv0F7C(&H11).M2427 = &H19 + 1
    gv0F7C(&H12).M1BD4 = "Index"
      gv0F7C(&H12).M2427 = &H1B + 1
  If gv0884 = 3 Then
    gv0F7C(&H14).M1BD4 = "Object"
      gv0F7C(&H14).M2427 = &H24 + 1
    gv0F7C(&H15).M1BD4 = "QueryDef"
      gv0F7C(&H15).M2427 = &H21 + 1
    gv0F7C(&H16).M1BD4 = "Table"
      gv0F7C(&H16).M2427 = &H22 + 1
      gv0F7C(&H16).M243B = &HF
    gv0F7C(&H17).M1BD4 = "Snapshot"
      gv0F7C(&H17).M2427 = &H23 + 1
      gv0F7C(&H17).M243B = &HF
    gv0F7C(&H32).M1BD4 = "Printer"
      gv0F7C(&H32).M2427 = &HC + 1
    gv0F7C(&H33).M1BD4 = "Screen"
      gv0F7C(&H33).M2427 = &HE + 1
    gv0F7C(&H34).M1BD4 = "Clipboard"
      gv0F7C(&H34).M2427 = &HF + 1
    gv0F7C(&H3D).M1BD4 = "App"
      gv0F7C(&H3D).M2427 = &H15 + 1
  ElseIf gv0884 = 2 Then
    gv0F7C(&H2C).M1BD4 = "Printer"
      gv0F7C(&H2C).M2427 = &HC + 1
    gv0F7C(&H2D).M1BD4 = "Screen"
      gv0F7C(&H2D).M2427 = &HE + 1
    gv0F7C(&H2E).M1BD4 = "Clipboard"
      gv0F7C(&H2E).M2427 = &HF + 1
    gv0F7C(&H37).M1BD4 = "App"
      gv0F7C(&H37).M2427 = &H15 + 1
  Else
    sub01CC
  End If
End Sub

Sub sub0927()
  sub04D4 False
End Sub

Sub sub0952()
  gv0918 = gv0914
  Get gv0882, gv0914, gv2326
  sub05E8 gv2326
End Sub

Sub sub0A4B()
Dim l016A As Integer
Dim l016C As Integer
Dim l016E As Integer
Dim l0170 As Integer
' Const mc0194 = 48 ' &H30%
  gv792A = 1
  If gv0B84 Then gv792A = gv792A Or gc10EA
  
  sub01E6 l016E, gc00EE
  For l016C = 1 To gv79A2
    sub046E l016E, gc00EE & " " & fn0C3E(l016C)
    gv7A2A = 0
    sub016B l016C
    For l016A = 1 To gv79EE
      If gv79BC(l016A).M230D = gv79B8 Then
        If gv79BC(l016A).M15FE <> -1 Then
          sub01E6 l0170, fn0C8E(l016A)
          sub019C l016A
          sub0205 l0170
        End If
      End If
    Next
  Next
  gv792A = 0
  
  sub0205 l016E
  gv79B6 = 0
End Sub

Sub sub0A59()
Dim l026A
Dim l026C As Long
  l026C = 1
  For l026A = gc1132 To gc1146
    gv1150(l026A) = l026C
    l026C = l026C * 2
  Next
  l026C = &H10000
  For l026A = gc1132 To gc113C
    gv1150(l026A + 16) = l026C
    l026C = l026C * 2
  Next
  l026C = mc00E2
  For l026A = gc113E To gc1146
    gv1150(l026A + 16) = l026C
    l026C = l026C * 2
  Next
  gv1150(gc1130) = mc010E
  gv1150(gc1148) = mc0116
  gv1150(gc114A) = mc011A
  gv1150(gc117E) = mc0112
End Sub

Sub sub0A68()
  LSet gv1584 = gv15EA.M2B71(gv158E)
  gv1584.M2B24 = (gv1584.M2B0C And &HF000) \ &H1000
  gv1584.M2B2C = (gv1584.M2B14 And &HF000) \ &H1000
End Sub

