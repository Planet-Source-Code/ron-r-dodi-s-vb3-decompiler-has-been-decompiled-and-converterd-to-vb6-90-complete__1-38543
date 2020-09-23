Attribute VB_Name = "MODULE71"
' Module7
' Option Explicit
Dim m001C As Integer
Dim m001E As Integer
Dim m0026 As Integer
Dim m0032 As Long
Dim m0036 As Single
Dim m003A As Double
Dim m0042 As Currency
Dim m004A As Integer
Dim m004C As Integer
Dim m004E As Integer
Dim m0050 As Integer
Dim m0052 As Integer
Dim m0054 As Integer
Dim m0056 As Integer
Dim m0058 As String
Dim m005C As String
Dim m0060 As Integer
Dim m0062 As Integer
Dim m0064(255) As Integer
Dim m007A() As String
Dim m00AC As Integer
Const mc00B6 = 1000 ' &H3E8%
Const mc00DC = 9 ' &H9%
Const mc00E6 = 12 ' &HC%
Const mc00E8 = 10 ' &HA%
Const mc00F4 = 1 ' &H1%
Const mc0118 = 10 ' &HA%
Const mc011C = 8 ' &H8%
Const mc0126 = 9 ' &H9%
Const mc0128 = 8192 ' &H2000%
Const mc0146 = 1 ' &H1%
Const mc0148 = 2 ' &H2%
Const mc014A = 4 ' &H4%
Const mc014C = 5 ' &H5%
Const mc014E = 8 ' &H8%
Const mc0150 = 10 ' &HA%
Const mc0152 = 11 ' &HB%
Const mc0154 = 12 ' &HC%
Const mc0156 = 14 ' &HE%
Const mc0158 = 13 ' &HD%
Const mc015E = 256 ' &H100%
Const mc0160 = 1 ' &H1%
Const mc0162 = 3 ' &H3%
Const mc0164 = 1024 ' &H400%
Const mc0166 = 9 ' &H9%
Const mc0168 = 4 ' &H4%
Const mc016A = 5 ' &H5%
Const mc016C = 6 ' &H6%
Const mc016E = 7 ' &H7%
Const mc0170 = 8 ' &H8%
Const mc0172 = 10 ' &HA%
Const mc0174 = 2 ' &H2%
Const mc0176 = 512 ' &H200%
Const mc017A = 256 ' &H100%
Const mc0180 = 32 ' &H20%
Const mc0206 = 32 ' &H20%
Const mc022E = 224 ' &HE0%
Const mc0230 = 192 ' &HC0%
Const mc0232 = 16 ' &H10%
Const mc0234 = 17 ' &H11%
Const mc0248 = 32 ' &H20%

Private Function fn0691(p01AC As Integer) As String
    Select Case p01AC
    Case -6 To -1
        fn0691 = "L" & Hex$(gv0B9A) & ":"
    Case Else
      fn0691 = Format$(fn032E(p01AC))
    End Select
End Function

Private Function fn069B(p01B0 As Integer) As String
Dim l01B2 As Long
Dim l01B4 As Integer
    l01B2 = Seek(m0026)
    Get m0026, gv06A8 + fn032E(p01B0) + 4, l01B4
    Select Case l01B4
    Case -1
        fn069B = "L" & Hex$(fn032E(p01B0) + gv06A8 - 1)
    Case Else
      fn069B = Format$(fn032E(l01B4))
    End Select
    Seek m0026, l01B2
End Function

Function fn06FC(p01BA As String) As String
  If m005C = "" Then
    fn06FC = m0058 & p01BA
  Else
    fn06FC = m0058 & m005C
    m005C = ""
  End If
  m0058 = ""
End Function

Function fn0708() As String
  If m00AC <= 0 Then
    fn0708 = "<missed arg>"
  Else
    fn0708 = m007A(m00AC)
    m007A(m00AC) = ""
    m00AC = m00AC - 1
  End If
End Function

Private Function fn070F(p01C4 As Integer, p01C6 As Integer, p01C8 As Integer) As String
Dim l01CA As String
  If gv0B88 Then
    If gv231E.M2D6A = mc00DC Then
      l01CA = fn0782(p01C4)
      If p01C6 Then sub06CC
    Else
      l01CA = fn0C02(fn0326(p01C4, 2))
    End If
  Else
    If p01C4 < 0 Then
      If p01C4 <> -2 Then sub01CC
      Exit Function
    End If
    l01CA = fn078E(p01C4)
  End If
  If Len(l01CA) Then
    If p01C8 Then
      fn070F = "!" & l01CA
    Else
      fn070F = "." & l01CA
    End If
  End If
End Function

Function fn071C(p01CE As Integer) As String
  If p01CE > 0 And Len(gv0B6C) Then
    fn071C = "!" & fn03D1(p01CE)
    sub06CC
  Else
    fn071C = "!?" & fn02BE(p01CE)
  End If
End Function

Function fn0782(p0238 As Integer) As String
  fn0782 = fn0AF0(p0238)
End Function

Function fn078E(p023E As Integer) As String
Dim l0240 As Long
Dim l0242 As Integer
Dim l0244 As Integer
' Const mc0248 = 32 ' &H20%
  l0240 = fn032E(p023E)
    If p023E = 0 Then Beep: Exit Function
    If l0240 > gv09EA Then Beep: Exit Function
      l0244 = l0240 \ 2
      l0242 = Asc(Mid$(gv7884, l0244, 1))
      If (l0242 And &H1F) = 0 Or (l0242 And mc022E) = mc0248 Then
        sub0798 p023E, gv231E.M2D61, gv231E.M2D6A
      End If
      fn078E = fn0C27(l0244)
End Function

Sub sub05A9(p024A As Integer)
Dim l024C As String
Dim l024E As String
  If (p024A And &H7FFF) = 0 Then Exit Sub
  If Abs(p024A) > m00AC Then
    sub05B3 "<concat?>"
    Exit Sub
  End If
  If p024A < 0 Then
    p024A = -p024A
    Do While p024A
      l024C = fn0708(): p024A = p024A - 1
      If Left$(l024C, 8) <> "<default" Then Exit Do
    Loop
    While p024A
      l024E = fn0708(): p024A = p024A - 1
      If Left$(l024E, 8) = "<default" Then
        l024C = ", " & l024C
      Else
        l024C = l024E & ", " & l024C
      End If
    Wend
  Else
    l024C = fn0708(): p024A = p024A - 1
    While p024A > 0
      l024C = fn0708() & ", " & l024C
      p024A = p024A - 1
    Wend
  End If
  sub05B3 l024C
  p024A = 1
End Sub

Sub sub05B3(p01D6 As String)
On Error GoTo 501
  m00AC = m00AC + 1
  m007A(m00AC) = p01D6
Exit Sub

501 If m00AC < 0 Then
      m00AC = 1
    Else
      ReDim Preserve m007A(1 To UBound(m007A) + 10)
    End If
    Resume
End Sub

Sub sub05BB(p0252 As Integer)
Dim l0254 As String
  If m001C Or (p0252 And 1) Then sub05A9 p0252: Exit Sub
  While p0252 > 0
    l0254 = fn0708() & l0254
    If m007A(m00AC) = gc151E Then
      m00AC = m00AC - 1
    Else
      l0254 = fn0708() & " To " & l0254
    End If
    p0252 = p0252 - 2
    If p0252 Then l0254 = ", " & l0254
  Wend
  sub05B3 l0254
  p0252 = 1
End Sub

Sub sub05CA()
  sub0214 mc0206
End Sub

Sub sub05D8(p012A As Long, p012C As Integer)
Dim l012E As String
Dim l0130 As Long
Dim l0132
Dim l0134
Dim l0136 As Integer
Dim l0138
Dim l013C As Integer
' Const mc0146 = 1 ' &H1%
' Const mc0148 = 2 ' &H2%
' Const mc014A = 4 ' &H4%
' Const mc014C = 5 ' &H5%
' Const mc014E = 8 ' &H8%
' Const mc0150 = 10 ' &HA%
' Const mc0152 = 11 ' &HB%
' Const mc0154 = 12 ' &HC%
' Const mc0156 = 14 ' &HE%
' Const mc0158 = 13 ' &HD%
' Const mc015E = 256 ' &H100%
' Const mc0160 = 1 ' &H1%
' Const mc0162 = 3 ' &H3%
' Const mc0164 = 1024 ' &H400%
' Const mc0166 = 9 ' &H9%
' Const mc0168 = 4 ' &H4%
' Const mc016A = 5 ' &H5%
' Const mc016C = 6 ' &H6%
' Const mc016E = 7 ' &H7%
' Const mc0170 = 8 ' &H8%
' Const mc0172 = 10 ' &HA%
' Const mc0174 = 2 ' &H2%
' Const mc0176 = 512 ' &H200%
Static m0178 As Integer
' Const mc017A = 256 ' &H100%
' Const mc0180 = 32 ' &H20%
  gv0B88 = p012C And gc0B8C
  m001C = gv0B8E And gc0B92
  m001E = False
  m00AC = 0
  ReDim m007A(1 To 50)
On Error GoTo LD2A6
    m0026 = gv0882
  gv0C1E = -1
  l0130 = Seek(m0026) + p012A
  Get m0026, , m0056
  While Seek(m0026) < l0130

LCB5A:
    l0136 = m0056
      gv0918 = Seek(m0026) - 2
      sub05E8 l0136
        l0136 = gv231E.M2D5A
        If gv231E.M1B05 = 0 Then
        End If
    l013C = (l0136 And &HFC00) \ &H400
    Select Case gv1584.M2B0C And &HF
    Case mc0146, mc0148

LCBF8:
      Select Case gv1584.M2B24
      Case 0
      Case 1
        Get m0026, , m004E
      Case 2
        Get m0026, , m004E
        Get m0026, , m0050
      Case 3
        Get m0026, , m004E
        Get m0026, , m0050
        Get m0026, , m0052
      Case Else
        sub01CC
      End Select
    Case mc014A
      sub05F9
      m0026 = 0
      Exit Sub
    Case mc014C
      m001E = False
      If m00AC And gv0B98 = 0 Then
        sub0604 " ?"
        gv0B98 = -2
      End If
      sub05F9
      If m00AC Then sub060D
      m004A = 0
      gv0B9A = Loc(m0026) - 2
      m0060 = 0: m0062 = False
      If gv158E Then GoTo LCBF8
    Case mc014E
      Get m0026, , l0138
      Get m0026, , l0134
      If l0138 > 2 Then
        Get m0026, , l0134
      Else
        l0134 = 0
      End If
      l012E = Space$(l0134)
      If l0134 Then Get m0026, , l012E
      sub061C l012E
      sub05B3 l012E
      l0134 = l0138 - l0134 - 4
      If l0134 > 0 Then
        Seek m0026, Seek(m0026) + l0134
      End If
    Case mc0150: Get m0026, , m0032
    Case mc0152: Get m0026, , m0036
    Case mc0154:    Get m0026, , m003A
    Case mc0156: Get m0026, , m0042
    Case mc0158
      Get m0026, , l0134: l0134 = l0134 \ 2
      m0064(0) = l0134
      For l0132 = 1 To l0134
        Get m0026, , m0064(l0132)
      Next l0132
    Case Else
      gv7A34 = gv097C
      sub0214 mc015E
      gv0B98 = gv158E
      If gv1584.M2B24 Then
        Seek m0026, Seek(m0026) + gv1584.M2B24 * 2
      End If
    End Select
    Get m0026, , m0056
    Select Case gv1584.M2B14 And &HF
    Case mc0160: sub062D gv158E, l013C
    Case mc0162
      If gv1584.M2B14 And mc0164 Then m004C = 0
      sub063A fn0B59(), gv1584.M2B2C
    Case mc0166
      sub063A fn06FC(fn0B59()), m00AC
    Case mc0168: sub05B3 fn0B59()
    Case mc016A:  sub0646 fn0B59()
    Case mc016C: sub0650 fn0B59()
    Case mc016E:  sub065B fn0B59()
    Case mc0170: sub0664 fn0B59(), l013C, gv1584.M2B2C
    Case mc0172
      sub0664 fn06FC(fn0B59()), l013C, gv1584.M2B2C
    Case mc0174
      If gv1584.M2B14 And mc0176 Then l013C = 0
    Case m0178:
      gv0B98 = gv158E
    Case Else
      sub01CC
    End Select
    If gv1584.M2B14 And mc017A Then
      If gv0B98 = 0 Then gv0B98 = gv158E
    End If
    If gv1584.M2B14 And mc0164 Then
      m004C = 0: m004A = 0: m001E = False
      sub0671 0
    End If
  Wend
  m0026 = 0
Exit Sub

LD2A6:
  sub01CC
  sub0604 " ?"
  sub05F9
  If gv0C12 Then Print #gv0C12, "' --- unknown "; Hex$(gv158E); " @ "; Hex$(gv0B9A); ":"; Hex$(Loc(m0026) - 2); " ---"
    While (fn09EE() And mc0180) = 0: Wend
    m0056 = gv2326
    gv10E4 = 0
  On Error GoTo LD2A6
  If Seek(m0026) < l0130 Then Resume LCB5A
  m0026 = 0
  Exit Sub
End Sub

Sub sub05F9()
  If gv0C1E < 0 Then gv0C1E = 0: gv0C1A = "": Exit Sub
  gv0C1A = RTrim$(gv0C1A)
  While m00AC > 0
    gv0C1A = gv0C1A & " ?+" & fn0708() & "?"
  Wend
  If m001C And Len(gv0C1A) = 0 Then Exit Sub
  If gv0C12 Then
    Print #gv0C12, gv0C1A
  Else
    On Error Resume Next
    gv0C16.AddItem gv0C1A
    If Err Then
      If Len(gv0C1A) = 0 Then Exit Sub
      gv097C = gv097C + 1
      If gv097C >= UBound(gv094A) Then ReDim Preserve gv094A(gv097C + 1)
      gv094A(gv097C).M1CD8 = gv0C1A
      gv094A(gv097C).M1CAA = gv0B9A
      If m0026 Then gv094A(gv097C + 1).M1CAA = Seek(m0026)
    End If
  End If
  gv0C1A = ""
End Sub

Sub sub0604(p01EA As String)
  If m001E Then Exit Sub
  gv0C1A = gv0C1A & p01EA
End Sub

Sub sub060D()
  If m00AC = 0 Then Exit Sub
  If gv0C12 Then
    Print #gv0C12, "--- unused expressions (top down) ---"
    While m00AC > 0
      Print #gv0C12, , fn0708()
    Wend
  End If
End Sub

Sub sub061C(p0198 As String)
Dim l019A As Integer
  If Len(p0198) Then
    If InStr(p0198, Chr$(&HD)) Then sub01CC
    l019A = -1
    Do
      l019A = InStr(l019A + 2, p0198, """")
      If l019A = 0 Then Exit Do
      p0198 = Left$(p0198, l019A) & Mid$(p0198, l019A)
    Loop
  End If
  p0198 = """" & p0198 & """"
End Sub

Sub sub062D(p00CC As Integer, p00CE As Integer)
Dim l00D0 As Integer
Dim l00D2 As String
Dim l00D4 As Integer
Dim l00D6 As Integer
' Const mc00DC = 9 ' &H9%
' Const mc00E6 = 12 ' &HC%
' Const mc00E8 = 10 ' &HA%
' Const mc00F4 = 1 ' &H1%
  On (p00CC \ &H10) + 1 GoTo L99EC, L9F9A, LA694, LA86A, LA9E6, LACDE, LAEF4, LB066, LB1D8, LB9E6, LBA9E, LBD38, LBDCC, LBDD8, LBDE4, LC076, LC1F6, LC216, LC374, LC380, LC4D0, LC5E6

L99D6:
  Error mc00B6
Exit Sub

L99EC: On p00CC And &HF GoTo L9A38, L9A50, L9A56, L9A62, L9A9E, L9ADA, L9B04, L9B3A, L9B40, LB126, L9B4E, L9C64, L9CBC, L9D28, L9F1E
    sub06A5 p00CE: p00CE = 0
    Exit Sub
L9A38:
    sub06A5 m004E
    Exit Sub
L9A50:
L9A56:
    GoTo L99D6
L9A62:
    sub0604 fn0691(m0050) & " "
    Exit Sub
L9A9E:
    sub0604 fn0691(m0050) & Space$(m0052)
    Exit Sub
L9ADA:
    sub0604 ": "
    Exit Sub
L9B04:
    sub0604 ": "
    sub06AF m004E
    Exit Sub
L9B3A:
L9B40:
    GoTo L99D6

L9B4E:
    sub06B5 m004E, p00CE: p00CE = 0
      If gv231E.M2D6A = mc00DC Then
        l00D4 = fn0326(m004E, 2)
        l00D6 = gv09B8(l00D4 - 1)
        Select Case l00D6 And &HC000
        Case 0, &H8000
          sub0671 l00D6
        Case &H4000
          If l00D6 = &H4006 Then
            sub0671 l00D6
            l00D2 = fn0782(gv09B8(l00D4))
          Else
            sub0671 l00D6
          End If
        Case Else
          sub01CC
        End Select
      End If
    Exit Sub

L9C64:
    sub06C1 m004E, p00CE, 0
    p00CE = 0
    sub0604 " = " & fn0708()
  Exit Sub
L9CBC:
    sub06C1 m004E, p00CE, -1
    p00CE = 0
      If gv231E.M2D6A = mc00DC Then
        sub0671 gv09B8(fn0326(m004E, 2) - 1)
      End If
  Exit Sub

L9D28:
    l00D0 = m004E
    If l00D0 >= 0 Then
        l00D6 = fn0326(m0050, 2)
        gv0BBC.M15F7 = l00D6
        Select Case gv231E.M2D6A
        Case mc00E6
          sub0664 fn078E(l00D6 * 2), p00CE, l00D0
          Exit Sub
        Case mc00DC
          sub0671 gv09B8(l00D6 - 1)
          sub06CC
        Case mc00E8
          Select Case m0056
          Case &H7E0 To &H816
            If l00D0 Then sub05BB l00D0
          Case Else
  If l00D6 <= Len(gv7884) Then
            If (Asc(Mid$(gv7884, l00D6, 1)) And &HF) = 9 Then
             sub0671 gv09B8(l00D6 - 1)
            End If
  End If
          End Select
        Case Else
          sub01CC
        End Select
        If l00D0 > 1 Then sub05A9 l00D0
      sub0664 fn078E(m0050), p00CE, l00D0
    Else
      sub06B5 m0050, p00CE
      p00CE = 0
    End If
    Exit Sub
L9F1E:
    sub0664 fn078E(m0050), p00CE, m004E
    sub06DA fn0708()
    sub0604 " = " & fn0708()
    Exit Sub
L9F9A: On p00CC And &HF GoTo LA090, LA27C, LA382, LA434, LA4BA, LA526, LA5B4, LA5C0, LA5CE, LA5DC, LA630, LA63C, LA67C, LA682, LA688
    sub06C1 m0050, p00CE, 0
    sub06E4 m004E
    If p00CE = 0 Then
      If m0062 Then
        sub06C1 0, 0, -1
      Else
        sub06C1 0, 0, 0
      End If
    Else
      p00CE = 0
    End If
    Exit Sub
LA056:
    sub06C1 m0050, p00CE, 0
    sub06E4 m004E
    m0060 = -1
    Exit Sub
LA090:
    Select Case p00CE
    Case 0 To 7
      If gv231E.M2D6A = mc00DC Then
        sub0650 "." & fn0782(m004E)
      Else
        sub0650 fn070F(m004E, False, 0)
      End If
      If p00CE Then
        sub0650 Mid$(gc151A, p00CE, 1)
        p00CE = 0
      End If
    Case 9
      If m004E < 0 Then
        sub0650 "!" & fn0782(m004E)
      ElseIf m004E And 1 Then
        sub0650 fn070F(m004E, False, p00CE)
      ElseIf gv0884 < 3 Then
        sub0650 "!" & fn0782(m004E)
      Else
        sub0650 fn071C(m004E)
      End If
      p00CE = 0
    Case Else
      sub01CC
    End Select
    Exit Sub
LA27C:
    If p00CE = 9 Then
      sub06DA fn0708() & fn070F(m004E, False, p00CE)
      p00CE = 0
    Else
      sub06DA fn0708() & fn070F(m004E, False, 0)
      If p00CE Then
        sub0604 Mid$(gc151A, p00CE, 1)
        p00CE = 0
      End If
    End If
    sub0604 " = " & fn0708()
    Exit Sub
LA382:
    l00D2 = fn0708() & fn070F(m0050, True, p00CE = 9)
    If m004E >= 0 Then
      sub0664 l00D2, p00CE, m004E
    Else
      If p00CE Then
        l00D2 = l00D2 & Mid$(gc151A, p00CE, 1)
        p00CE = 0
      End If
      sub05B3 l00D2
    End If
    Exit Sub
LA434:
    l00D2 = fn0708() & fn070F(m0050, True, p00CE = 9)
    sub0664 l00D2, p00CE, m004E
    sub063A fn0708() & " = ", 1
    Exit Sub
LA4BA:
    gv0BBC.M15F7 = m004E
    gv0BBC.M206B = m0050
    If m0062 Then sub01CC
    m0062 = p00CC
    If m0060 Then
      sub06C1 m0060, 0, -1
    End If
    Exit Sub
LA526:
    If m0062 Then sub01CC
    gv0BBC.M15F7 = m004E
    If gv0B88 And gv0BBC.M15F7 > 7 Then gv0BBC.M15F7 = 0
    gv0BBC.M206B = m0050
    m0062 = p00CC
    If m0060 Then
      sub06C1 m0060, 0, -1
    End If
    Exit Sub
LA5B4:
  GoTo L99D6
LA5C0:
  GoTo L99D6
LA5CE:
  GoTo L99D6
LA5DC:
LA5E2:
    sub063A fn0B59(), 1
    If m0062 Then
      sub06C1 0, 0, -1
    End If
    Exit Sub
LA630:
    GoTo L99D6
LA63C:
    sub0604 "Type " & fn078E(m0050)
    Exit Sub
LA67C:
LA682:
LA688:
    GoTo L99D6
LA694:
  On p00CC And &HF GoTo LA6C8, LA6CE, LA6DA, LA700, LA794, LA79A, LA7A6, LA7F4, LA7FA, LA800, LA806, LA80C, LA812, LAA1A, LA85E
LA6C8:
LA6CE:
  GoTo L99D6
LA6DA:  sub0604 "Call "
LA700:
      l00D2 = fn0C8E(gv0DA6((fn0D6C(m0050)) + mc00F4))
    If m004E And p00CC = &H23 Then
      sub0664 l00D2, 0, m004E
      sub0604 fn0708()
    Else
      sub063A l00D2, m004E
    End If
    Exit Sub
LA794:
LA79A:
    GoTo L99D6
LA7A6:
    sub065B "To"
    sub063A "Case", 1
    Exit Sub
LA7F4:
LA7FA:
LA800:
LA806:
LA80C:
LA812:
    sub0646 fn0B59()
    sub063A "Case", 1
    Exit Sub

LA85E:
    GoTo L99D6
LA86A: On p00CC And &HF GoTo LA89E, LA8A4, LA8AA, LA8B0, LA8B6, LAAC6, LA8BE, LA8C4, LA8D0, LA908, LA90E, LA914, LA920, LA97E, LA958

LA89E:
LA8A4:
LA8AA:
LA8B0:
LA8B6:

LA8BE:
LA8C4:
  GoTo L99D6
LA8D0:
  sub0604 "End " & gv0B9E
  Exit Sub
LA908:
LA90E:
LA914:
    GoTo L99D6
LA920:
  sub06DA "Exit " & gv0B9E
  Exit Sub
LA958:
    sub065B "Step"
LA97E:
    sub065B "To"
    sub065B "="
    sub063A "For ", 1
    Exit Sub
LA9E6: On p00CC And &HF GoTo LAA32, LAA74, LAA86, LAA7A, LAACC, LAB2A, LAB30, LAB3C, LAAD2, LAB9C, LABA2, LABA8, LABB4, LABF2, LABFE

LAA1A:
LAA22:
    Exit Sub

LAA32:
  sub06DA "GoSub " & fn069B(m004E)
  Exit Sub
LAA74:
LAA7A:
  GoTo L99D6
LAA86:
    sub06DA "GoTo " & fn069B(m004E)
    Exit Sub
LAAC6:
LAACC:
LAAD2:
    sub06DA fn0B59() & " " & fn0708() & " Then "
    Exit Sub
LAB2A:
LAB30:
  GoTo L99D6
LAB3C:
    sub06DA "If " & fn0708() & " GoTo " & fn069B(m004E)
    Exit Sub

LAB9C:
LABA2:
LABA8:
  GoTo L99D6
LABB4:
    sub063A fn0B59(), 0
    If gv0B88 Then m00AC = m00AC - 1
    Exit Sub
LABF2:
  GoTo L99D6
LABFE:
    sub06DA fn0B59()
    Select Case m004E
    Case -1
      sub0604 "GoTo 0"
    Case -2
      sub0604 "Resume Next"
    Case Else
      sub0604 "GoTo " & fn069B(m004E)
    End Select
    Exit Sub
LACDE: On p00CC And &HF GoTo LAD10, LADD4, LADE0, LAE22, LAE28, LAE2E, LAE34, LAA22, LAE3C, LAE42, LAE4E, LAE7A, LAE80, LAE86, LAE8C
LAD10:
    sub06DA "On " & fn0708() & fn0B59()
    sub0604 fn069B(m0064(1))
    For l00D4 = 2 To m0064(0)
      sub0604 ", " & fn069B(m0064(l00D4))
    Next l00D4
    Exit Sub
LADD4:
    GoTo L99D6
LADE0:
    sub06DA "Resume " & fn069B(m004E)
    Exit Sub
LAE22:
LAE28:
LAE2E:
LAE34:

LAE3C:
LAE42:
  GoTo L99D6
LAE4E:
    sub0650 ", "
    Exit Sub
LAE7A:
LAE80:
LAE86:
LAE8C:
    l00D2 = fn0708()
    While m0054 > gv158E
      l00D2 = ", " & l00D2
      m0054 = m0054 - 1
    Wend
    sub0650 l00D2
    Exit Sub
LAEF4: On p00CC And &HF GoTo LAF26, LAF2C, LAF32, LAF9C, LAFC0, LAFEE, LB006, LB00C, LB036, LB03C, LB042, LB048, LB04E, LB054, LB05A
LAF26:
LAF2C:
LAF32:
    If p00CC And 1 Then l00D2 = "Step"
    If p00CC And 2 Then l00D2 = "-" & l00D2
    sub0664 l00D2, 0, 2
    Exit Sub
LAF9C:
    m0058 = "Debug."
    Exit Sub
LAFC0:
    m0058 = fn0708() & "."
    Exit Sub
LAFEE:
    m0054 = &H60
    Exit Sub
LB006:
LB00C:
    sub063A fn0B59(), m00AC
    Exit Sub
LB036:
LB03C:
LB042:
LB048:
LB04E:
LB054:
LB05A:
  GoTo L99D6
LB066: On p00CC And &HF GoTo LB098, LB0B0, LB09E, LB0A4, LB0B6, LB12C, LB162, LB170, LB17E, LB184, LB190, LB1BA, LB1C6, LB1CC

LB098:
LB09E:
LB0A4:
  GoTo L99D6
LB0B0:
LB0B6:
    While m00AC > 1
      sub0650 fn0708()
    Wend
    sub063A fn06FC("Print"), m00AC
    Exit Sub
LB126:
LB12C:
    m001E = m001C
    sub06F0 m004E, fn0708()
    Exit Sub
LB162:
    Exit Sub
LB170:
    Exit Sub
LB17E:
LB184:
    GoTo L99D6
LB190:
    sub063A fn0B59(), m004E
    Exit Sub
LB1BA:
LB1C0:
LB1C6:
LB1CC:
    GoTo L99D6
LB1D8: On p00CC And &HF GoTo LB252, LB258, LB25E, LB27E, LB26A, LB388, LB394, LB3CE, LB4AC, LB516, LB51C, LB624, LB630, LB6B2, LB678
LB20A:
    sub0646 ", "
    sub063A fn0B59(), 2
    Exit Sub
LB252:
LB258:
LB25E:
    GoTo L99D6
LB26A:
    l00D2 = fn0708()
LB27E:
    If m00AC > 1 Then sub0650 fn0708()
    Select Case m004E
    Case 0
    Case 1: l00D2 = l00D2 & ", B"
    Case 2: l00D2 = l00D2 & ", BF"
    Case Else: sub01CC
    End Select
    If Len(l00D2) Then sub05B3 l00D2
    sub063A fn06FC(fn0B59()), m00AC
    Exit Sub
LB388:
    GoTo L99D6
LB394:
    If m004E Then
    End If
    sub063A fn0B59(), m00AC
    Exit Sub
LB3CE:
LB3D4:
    Select Case m004E And &HFFFE
    Case &H8002
    Case &H4002
      l00D2 = "To " & fn0708()
      m007A(m00AC) = l00D2
    Case &H2
      sub065B "To"
    Case Else: sub01CC
    End Select
    sub063A fn0B59(), 2
    Exit Sub
LB4AC:
LB4B2:
    sub06DA fn0B59() & fn0708() & " = "
    sub0604 fn0708()
    Exit Sub

LB516:
LB51C:
    If p00CE Then
      sub06DA "Mid(" & fn0708() & ", "
      p00CE = 0
    Else
      sub06DA "Mid$(" & fn0708() & ", "
    End If
    l00D2 = fn0708()
    If p00CC And 1 Then sub05A9 2
    sub0604 fn0708() & ") = " & l00D2
    Exit Sub
LB624:
    GoTo L99D6
LB630:
    sub065B "As"
    sub063A fn0B59(), 1
    Exit Sub
LB678:
    sub0650 " Len = " & fn0708()
LB6B2:
    l00D2 = "As " & fn0708()
    l00D6 = m004E
    If l00D6 And &H8CD0 Then sub01CC
    Select Case l00D6 And &H7000
    Case &H1000: l00D2 = "Lock Read Write " & l00D2
    Case &H2000: l00D2 = "Lock Write " & l00D2
    Case &H3000: l00D2 = "Lock Read " & l00D2
    Case &H4000: l00D2 = "Shared " & l00D2
    End Select
    If l00D6 And &H300 Then
      If l00D6 And &H200 Then l00D2 = "Write " & l00D2
      If l00D6 And &H100 Then l00D2 = "Read " & l00D2
      l00D2 = "Access " & l00D2
    End If
    Select Case l00D6 And &H3F
    Case 1: l00D2 = "Input " & l00D2
    Case 2: l00D2 = "Output " & l00D2
    Case 4: l00D2 = "Random " & l00D2
    Case 8: l00D2 = "Append " & l00D2
    Case &H20: l00D2 = "Binary " & l00D2
    End Select
    sub06DA fn0B59() & fn0708() & " For " & l00D2
    Exit Sub
LB9E6: On p00CC And &HF GoTo LBA18, LBA1E, LBA24, LB20A, LBA2C, LBA32, LBA38, LBA44, LBA76, LBA7C, LB4B2, LBA84, LBA8A, LB3D4, LBA92
LBA18:
LBA1E:
LBA24:

LBA2C:
LBA32:
LBA38:
    GoTo L99D6
LBA44:
    m001E = m001C
    sub06DA fn0708()
    Exit Sub
LBA76:
LBA7C:

LBA84:
LBA8A:

LBA92:
    GoTo L99D6
LBA9E: On p00CC And &HF GoTo LBBBE, LBBCA, LBBE6, LBBEC, LBBF2, LBBF8, LBBFE, LBC04, LBC0A, LBC10, LBC1C, LBD1A, LBD20, LBD26, LBD2C
    l00D2 = fn0708() & ")"
    l00D2 = fn0708() & ", " & l00D2
    l00D2 = fn0708() & ")-(" & l00D2
    l00D2 = " (" & fn0708() & ", " & l00D2
    sub063A fn06FC(fn0B59()) & l00D2, 0
    Exit Sub
LBBBE:
    GoTo L99D6
LBBCA:
    m005C = fn0B59()
    Exit Sub
LBBE6:
LBBEC:
LBBF2:
LBBF8:
LBBFE:
LBC04:
LBC0A:
LBC10:
  GoTo L99D6
LBC1C:
      If m00AC > 0 Then
      If p00CE And &H20 Then
        p00CE = InStr(m007A(m00AC), "$")
        m007A(m00AC) = Left$(m007A(m00AC), p00CE - 1) & Mid$(m007A(m00AC), p00CE + 1)
      ElseIf p00CE Then
        m007A(m00AC) = "C" & gv1522(p00CE) & "(" & m007A(m00AC) & ")"
      End If
      End If
      p00CE = 0
      Exit Sub
LBD1A:
LBD20:
LBD26:
LBD2C:
  GoTo L99D6
LBD38: On p00CC And &HF GoTo LBD6C, LBD72, LBD78, LBD7E, LBD84, LBD8A, LBD90, LBD96, LBD9C, LBDA2, LBDA8, LBDAE, LBDB4, LBDBA, LBDC0

LBD6C:
LBD72:
LBD78:
LBD7E:
LBD84:
LBD8A:
LBD90:
LBD96:
LBD9C:
LBDA2:
LBDA8:
LBDAE:
LBDB4:
LBDBA:
LBDC0:
    GoTo L99D6
LBDCC: GoTo L99D6

LBDD8: GoTo L99D6

LBDE4: On p00CC And &HF GoTo LBE16, LBE1C, LBE22, LBE2E, LBE5C, LBE82, LBEBA, LBEF2, LBF38, LBF70, LBFB6, LBFEE, LC05E, LC06A

LBE16:
LBE1C:
LBE22:
    GoTo L99D6
LBE2E:
    sub05B3 Format$(p00CE): p00CE = 0
    Exit Sub
LBE5C:
    sub05B3 Format$(m004E)
    Exit Sub
LBE82:
    sub05B3 Format$(m0032) & "&"
    Exit Sub
LBEBA:
    sub05B3 "&H" & Hex$(m004E)
    Exit Sub
LBEF2:
    sub05B3 "&H" & Hex$(m0032) & "&"
    Exit Sub
LBF38:
    sub05B3 "&O" & Oct$(m004E)
    Exit Sub
LBF70:
    sub05B3 "&O" & Oct$(m0032) & "&"
    Exit Sub
LBFB6:
    sub05B3 Trim$(Str$(m0036)) & "!"
    Exit Sub
LBFEE:
    sub05B3 Trim$(Str$(m003A)) & "#"
    Exit Sub
LC026:
    sub05B3 Trim$(Str$(m0042)) & "@"
    Exit Sub
LC05E:
LC064:
LC06A:
  GoTo L99D6
LC076: On p00CC And &HF GoTo LC0AA, LC0B0, LC0B6, LC0BC, LC0C2, LC0C8, LC0CE, LC0DA, LC170, LC176, LC182, LC1D8, LC1DE, LC1E4, LC1EA

LC0AA:
LC0B0:
LC0B6:
LC0BC:
LC0C2:
LC0C8:
LC0CE:
    GoTo L99D6
LC0DA:
    sub0646 "TypeOf "
    sub0650 " Is "
      l00D2 = gv0F7C(m004E).M1BD4
      If Len(l00D2) = 0 Then l00D2 = Hex$(m004E): sub01CC
    sub0650 l00D2
    Exit Sub
LC170:
LC176:
    GoTo L99D6
LC182:
    gv0BBC.M15F7 = -m0050
    gv0BBC.M206B = m0052
    m0062 = True
    If m0060 Then sub06C1 m0060, 0, -1
    Exit Sub
LC1D8:
LC1DE:
LC1E4:
LC1EA:
    GoTo L99D6
LC1F6: If p00CC = &H107 Then GoTo LC026
  GoTo L99D6

LC216: On p00CC And &HF GoTo LC24A, LC250, LC256, LC25C, LC262, LC268, LC26E, LC274, LC27A, LC286, LC28C, LC312, LC31E, LC362, LC368

LC24A:
LC250:
LC256:
LC25C:
LC262:
LC268:
LC26E:
LC274:
LC27A:
    GoTo L99D6
LC286:
LC28C:
    l00D4 = m004E
    l00D2 = fn0B2C(l00D4)
    If p00CC And 1 Then l00D2 = fn06FC(l00D2)
    If l00D4 >= 0 Then
      sub063A l00D2, m0050
    Else
      sub0664 l00D2, 0, m0050
    End If
    Exit Sub
LC312:
    GoTo L99D6
LC31E:
    If m004E <> &HFC Then GoTo L99D6
    sub063A "Dim Shared ", 0
    Exit Sub
LC362:
LC368:
  GoTo L99D6
LC374:
  GoTo L99D6
LC380: On p00CC And &HF GoTo LC3B2, LC3B8, LC3C4, LC41E, LA056, LC42C, LC494, LC49A, LC4A0, LC4A6, LC4AC, LC4B2, LC4BE, LC4C4

LC3B2:
LC3B8:
    GoTo L99D6
LC3C4:
    sub063A "Set", 1
    sub0604 " = " & fn0708()
    Exit Sub
LC41E:
    GoTo L99D6

LC42C:
    If m004E Then l00D2 = "Text" Else l00D2 = "Binary"
    sub063A fn0B59() & l00D2, 0
    Exit Sub
LC494:
LC49A:
LC4A0:
LC4A6:
LC4AC:
LC4B2:
LC4B8:
LC4BE:
LC4C4:
  GoTo L99D6
LC4D0: On p00CC And &HF GoTo LC504, LC50A, LC510, LA5E2, LC518, LC524, LC57C, LC5AE, LC5B4, LC5BA, LC5C0, LC5C6, LC5CC, LC5D2, LC5D8

LC504:
LC50A:
LC510:

LC518:
    GoTo L99D6
LC524:
  sub0671 m004E
  l00D2 = gv0F7C(m004E).M1BD4
  sub05B3 "New " & l00D2
  Exit Sub
LC57C:
    sub05B3 fn0B59()
    sub0671 &H4006
    Exit Sub
LC5AE:
LC5B4:
LC5BA:
LC5C0:
LC5C6:
LC5CC:
LC5D2:
LC5D8:
  GoTo L99D6

LC5E6: On p00CC And &HF GoTo LC618, LC61E, LC624, LC62A, LC630, LC636, LC63C, LC642, LC648, LC64E, LC654, LC65A, LC660, LC66C, LC6B8
LC618:
LC61E:
LC624:
LC62A:
LC630:
LC636:
LC63C:
LC642:
LC648:
LC64E:
LC654:
LC65A:
LC660:
  GoTo L99D6
LC66C:
  l00D2 = fn0708() & "." & fn03D1(m0050)
  sub063A l00D2, m004E
Exit Sub
LC6B8:
  GoTo L99D6
End Sub

Sub sub063A(p01E4, ByVal pv01E6 As Integer)
  If m004C Then
    sub0604 ", "
  Else
    m004C = Len(gv0C1A) + 1
    sub06DA p01E4
  End If
  If pv01E6 Then
    sub05A9 pv01E6
    sub06DA fn0708()
  End If
End Sub

Sub sub0646(ByVal p01C0 As String)
  If m00AC > 0 Then m007A(m00AC) = p01C0 & m007A(m00AC)
End Sub

Sub sub0650(p01BE As String)
  If m00AC <= 0 Then sub05B3 "<missed arg>"
  m007A(m00AC) = m007A(m00AC) & p01BE
End Sub

Sub sub065B(ByVal p01A6 As String)
Dim l01A8 As String
  l01A8 = fn0708()
  If m00AC < 1 Then
    m00AC = 1
    m007A(m00AC) = "<missed arg>"
  End If
  m007A(m00AC) = m007A(m00AC) & " " & p01A6 & " " & l01A8
End Sub

Sub sub0664(p01D8 As String, p01DA As Integer, ByVal pv01DC As Integer)
Dim l01DE As String
  If p01DA Then
    p01D8 = p01D8 & Mid$(gc151A, p01DA, 1)
    p01DA = 0
  End If
  If pv01DC Then
    sub05A9 pv01DC
    l01DE = fn0708()
  End If
  sub05B3 p01D8 & "(" & l01DE & ")"
End Sub

Sub sub067C(p019C As Integer)
Dim l019E As Integer
Dim l01A0 As String
  l01A0 = Space$(p019C)
  If p019C > 0 Then
    Get m0026, , l01A0
    If p019C And 1 Then Get m0026, , gv09F6
    l019E = InStr(l01A0, Chr$(&HD))
    While l019E
      l01A0 = Left$(l01A0, l019E - 1) & String$(Asc(Mid$(l01A0, l019E + 1, 1)), Mid$(l01A0, l019E + 2, 1)) & Mid$(l01A0, l019E + 3)
      l019E = InStr(l01A0, Chr$(&HD))
    Wend
  End If
  sub05B3 fn0B59() & l01A0
End Sub

Sub sub0684()
  ReDim gv094A(10), m007A(50)
  gv0B88 = True
  gv097C = 0
End Sub

Private Sub sub06B5(p01E0 As Integer, p01E2 As Integer)
  sub05B3 fn078E(p01E0)
  If p01E2 Then m007A(m00AC) = m007A(m00AC) & Mid$(gc151A, p01E2, 1)
End Sub

Private Sub sub06C1(ByVal pv0110 As Integer, p0112 As Integer, p0114 As Integer)
Dim l0116 As Integer
' Const mc0118 = 10 ' &HA%
' Const mc011C = 8 ' &H8%
' Const mc0126 = 9 ' &H9%
' Const mc0128 = 8192 ' &H2000%
  If p0112 = 0 And p0114 = -1 And m0062 Then
    p0112 = gv0BBC.M15F7
    p0114 = gv0BBC.M206B
    l0116 = True
    m0062 = False
  End If
  If p0114 = -1 Then
    p0114 = 0
  ElseIf p0114 < -1 Then
    l0116 = True
  End If
  If p0112 Then m0060 = 0
  If pv0110 Then
    If m004A Then sub0604 ", " Else m004A = True
    sub0604 fn078E(pv0110)
    If Not l0116 And p0112 > 0 And p0112 < 8 And p0112 <> 6 Then
      sub0604 Mid$(gc151A, p0112, 1)
      Exit Sub
    End If
  End If
  If p0112 = 0 Then Exit Sub
  If p0114 < 0 Then
    sub06F0 (p0114 And &H7F) - 1, " As New "
  Else
    sub06F0 p0114 - 1, " As "
  End If
  If p0112 < 0 Then
    sub0604 "String *" & Str$(-p0112)
    Exit Sub
  End If
    pv0110 = gv09B8(p0112 - 1)
    p0112 = Asc(Mid$(gv7884, p0112, 1))
    Select Case p0112 And &HF
    Case 0
      sub01CC
    Case mc0118
        sub0604 fn0C02(fn0326(pv0110, 2))
    Case mc011C
      sub0604 "String *" & Str$(fn032E(pv0110))
    Case Is < 8
      sub0604 gv1538(p0112 And &HF)
    Case mc0126
      If gv0F7C(pv0110 And &HFF).M2427 < 0 Then
        sub0214 mc0128
        sub0604 "()"
      End If
      sub0604 gv0F7C(pv0110 And &HFF).M1BD4
    Case Else
      sub01CC
      sub0604 "?" & Hex$(p0112)
    End Select
End Sub

Sub sub06DA(ByVal p01E8 As String)
  If m001E Then Exit Sub
  If Len(gv0C1A) Then
    If Right$(gv0C1A, 1) <> " " Then
      gv0C1A = gv0C1A & " " & p01E8
      Exit Sub
    End If
  End If
  gv0C1A = gv0C1A & p01E8
End Sub

Sub sub06E4(p00BA As Integer)
  If p00BA Then sub05BB p00BA Else sub05B3 ""
  sub0604 "(" & fn0708() & ")"
End Sub

Sub sub06F0(p01EC As Integer, ByVal p01EE As String)
  If m001E Then Exit Sub
  sub06AF p01EC
  sub0604 p01EE
End Sub

Sub sub072B(ByVal pv01F0 As Integer, p01F2 As String, p01F4 As Integer)
Dim l01F6 As Long
Dim l01F8 As Long
Dim l01FA As Integer
Dim l01FC As String
' Const mc0206 = 32 ' &H20%
  l01FA = Asc(Mid$(p01F2, pv01F0, 1)) And &HF
  If l01FA >= mc011C Then Exit Sub
  If p01F4 Then
    l01F6 = gv0B5C + 2 * CLng(pv01F0)
    If p01F4 = True Then
      sub0739 "Global "
    End If
    sub0739 "Const " & fn0C02(pv01F0) & " ="
  Else
    l01F6 = gv0B68 + 2 * CLng(pv01F0)
    sub0739 "Const " & fn0C27(pv01F0) & " ="
  End If
  l01F8 = Seek(gv0882)
  Seek gv0882, l01F6
  Select Case l01FA
  Case 0:
    Get gv0882, , l01FA
    sub0739 Str$(l01FA) & " ' ? &H" & fn02B5(l01FA)
    Do
      pv01F0 = pv01F0 + 1
      If Asc(Mid$(p01F2, pv01F0, 1)) Then Exit Do
      Get gv0882, , l01FA
      sub0739 fn02B5(l01FA)
    Loop
  Case 1: Get gv0882, , l01FA: sub0739 Str$(l01FA) & " ' &H" & Hex$(l01FA) & "%"
  Case 2: Get gv0882, , m0032: sub0739 Str$(m0032) & "& ' &H" & Hex$(m0032) & "&"
  Case 3: Get gv0882, , m0036: sub0739 Str$(m0036) & "!"
  Case 4:
    Get gv0882, , m003A: l01FC = Str$(m003A)
    If InStr(l01FC, ".") = 0 Then l01FC = l01FC & "#"
    sub0739 l01FC
  Case 5: Get gv0882, , m0042: sub0739 Str$(m0042) & "@"
  Case 6: sub0214 mc0206
  Case 7
    Get gv0882, , l01FA
    Get gv0882, gv0B58 + l01FA, l01FA
    Get gv0882, gv0B58 + l01FA, l01FA
    l01FC = Space$(l01FA)
    If l01FA > 0 Then Get gv0882, , l01FC
    sub061C l01FC
    sub0739 " " & l01FC
  Case mc011C: sub01CC
  Case mc0126
    Get gv0882, l01F6 - 2, l01FA
    sub0739 " As " & gv0F7C(l01FA And &HFF).M1BD4
  Case mc0118: sub01CC
  Case Else
    sub01CC
    Do
      Get gv0882, , l01FA: sub0739 fn02B5(l01FA)
      pv01F0 = pv01F0 + 1
    Loop Until Asc(Mid$(p01F2, pv01F0, 1))
  End Select
  Seek gv0882, l01F8
End Sub

Sub sub0741(p020A As Integer)
Dim l020E As T16F5
Dim l0210 As String
  Get gv0882, fn0D74(gv79BC(p020A).M2341), l020E
  If l020E.M172B And 1 Then
    gv0B9E = "Sub"
  Else
    gv0B9E = "Function"
  End If
  If l020E.M1733 <> &HC Then sub01CC
  l0210 = fn0C8E(p020A)
  gv0C0A.M20A0 = fn03C5(l020E.M1791)
  gv0C0A.M20A7 = fn03C5(l020E.M17A7)
  sub0739 "Declare " & gv0B9E & " " & l0210
  If Right$(gv0C0A.M20A0, 1) = "." Then
    gv0C0A.M20A0 = Left$(gv0C0A.M20A0, Len(gv0C0A.M20A0) - 1)
  End If
  sub0739 " Lib """ & gv0C0A.M20A0 & """"
  If gv0C0A.M20A7 <> l0210 Then
    sub0739 " Alias """ & gv0C0A.M20A7 & """"
  End If
  sub074E p020A
End Sub

Sub sub075D(ByVal pv021E As Integer, p0220() As Integer, p0226 As String, p0228 As Integer)
Dim l022A As Integer
Dim l022C As Integer
' Const mc022E = 224 ' &HE0%
' Const mc0230 = 192 ' &HC0%
' Const mc0232 = 16 ' &H10%
' Const mc0234 = 17 ' &H11%
  l022C = Asc(Mid$(p0226, pv021E, 1))
  If (l022C And mc022E) = mc0230 Then sub072B pv021E, p0226, p0228: Exit Sub
  If p0228 Then
    sub0739 "Global " & fn0C02(pv021E)
  Else
    sub0739 fn0C27(pv021E)
  End If
  If l022C And mc0232 Then
    l022A = Asc(Mid$(p0226, pv021E + 1, 1))
    Select Case l022A
    Case mc0234
      sub0769 p0220(), pv021E + 3
    Case Else
      sub01CC
    End Select
  End If
  sub0776 l022C, p0220(pv021E - 1), True
End Sub

Sub sub07A6()
Dim l00BE As Integer
  l00BE = gv0B88
  gv0C1E = 0
  gv0B9A = 0
  gv0B88 = True
  If gv79F2.M172B And 1 Then
    gv0B9E = "Sub"
  Else
    gv0B9E = "Function"
  End If
  If gv792A Then
  Else
    If gv79F2.M1733 And &H40 Then sub0739 "Private "
    sub0739 gv0B9E & " " & fn0C8E(gv7A2A)
  End If
  sub074E gv7A2A
  If gv0B8E = 0 Then sub07B4
  gv0B88 = l00BE
End Sub

