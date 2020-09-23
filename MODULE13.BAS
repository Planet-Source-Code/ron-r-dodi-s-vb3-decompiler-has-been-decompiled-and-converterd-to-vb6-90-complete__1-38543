Attribute VB_Name = "MODULE131"
' Module13
' Option Explicit
Const mc000E = 3 ' &H3%
Const mc0010 = 4 ' &H4%
Const mc003C = 17 ' &H11%
Const mc003E = 18 ' &H12%
Const mc0040 = 19 ' &H13%
Const mc0042 = 20 ' &H14%
Const mc0044 = 21 ' &H15%
Const mc004A = 31 ' &H1F%
Dim m004C As Integer
Dim m008C As Long
Dim m0090 As Long
Const mc0096 = 1 ' &H1%
Const mc0098 = 2 ' &H2%
Const mc009A = 3 ' &H3%
Dim m009E As T2A90
Dim m00A6(3) As String
Const mc00F2 = 1 ' &H1%
Const mc011A = 16384 ' &H4000%
Const mc0158 = 4096 ' &H1000%
Const mc015C = 64 ' &H40%

Sub sub0ABF(p00BC As Integer)
  m00A6(0) = "<no/std obj>"
  m00A6(mc009A) = "<ctrl>"
  m00A6(mc0098) = "<class>"
  m00A6(mc0096) = "<prop>"
  sub0AB2 p00BC
End Sub

Sub sub0ACC()
End Sub

Function fn0AD8(p00C6 As Integer, p00C8 As Long, p00CA As Integer) As String
Dim l00CE As String * 256
Dim l00D0 As Integer
  Get p00C6, p00C8 + fn032E(p00CA), l00CE
  l00D0 = InStr(l00CE, Chr$(0))
  If l00D0 Then
    fn0AD8 = Left$(l00CE, l00D0 - 1)
  Else
    MsgBox gc0072
    fn0AD8 = l00CE
  End If
End Function

Function fn0AE4(p00D8 As Integer) As String
  fn0AE4 = gv1260(gv13E6(p00D8).M15F7 And gc1240)
End Function

Sub sub0671(ByVal pv00DA As Integer)
  Select Case pv00DA And &HC000
  Case 0, &H8000
    pv00DA = pv00DA And &HFF
    m009E.M15F7 = mc0098
    m009E.M1BDC = pv00DA
    m009E.M1BD4 = gv0F7C(pv00DA).M1BD4
  Case &H4000
    If pv00DA = &H4006 Then
      m009E.M15F7 = mc0098
      m009E.M1BDC = gv79A6.M14DF
      m009E.M1BD4 = "me"
    Else
      m009E.M15F7 = mc0098
      pv00DA = pv00DA And &HFF
      m009E.M1BDC = pv00DA
      m009E.M1BD4 = gv0F7C(pv00DA).M1BD4
    End If
  Case Else
    Beep
    m009E.M15F7 = 0
  End Select
End Sub

Function fn0AF0(p00EA As Integer) As String
Dim l00EC As Integer
Dim l00EE As Integer
Dim l00F0 As String
' Const mc00F2 = 1 ' &H1%
  l00EC = m009E.M1BDC
  l00EE = p00EA And &HFF
  Select Case p00EA
  Case -2
    If l00EC Then
      If m009E.M15F7 = mc0098 Then
        l00EC = gv0F7C(l00EC).M243B
      Else
        l00EC = 0
      End If
    End If
    If l00EC Then
      m009E.M1BD4 = m009E.M1BD4 & "<." & gv0F7C(l00EC).M1BD4 & ">"
      m009E.M1BDC = l00EC
    Else
      m009E.M1BD4 = m009E.M1BD4 & "<.hide2>"
      m009E.M15F7 = 0
      sub01CC
    End If
    Exit Function
  Case &HC0FE
    l00F0 = "Controls"
    m009E.M15F7 = mc0098
    l00EC = 5
  Case &HC0FD
    l00F0 = "Count"
    m009E.M15F7 = 0
    l00EC = 0
  Case Else
    If l00EC = 0 Then
      sub0214 mc00F2
    End If
    Select Case m009E.M15F7
    Case mc0098
      Select Case p00EA And &HFF00
      Case &HC000
        l00EC = gv0F7C(l00EC).M2427
L142A8:
        If l00EC > 0 Then
          l00EC = gv1390(l00EC).M29F5
          l00EC = gv141A(l00EC + l00EE)
          l00F0 = gv13E6(l00EC).M1BD4
          m009E.M15F7 = mc0096
        End If
      Case &H8000
L14320:
        l00EC = gv0F7C(l00EC).M2431
        If l00EC = 0 Then
          sub01CC
        Else
          l00EC = l00EC + l00EE
          m009E.M15F7 = mc009A
          l00F0 = gv0FB6(l00EC).M1BD4
        End If
      Case 0
        If l00EC = 4 Then
          If l00EE <= UBound(gv1002) Then
            l00EC = gv1002(l00EE)
          Else
            sub01CC
          End If
        End If
        If l00EC = 20 Then
          l00F0 = fn03D1(l00EE)
        ElseIf l00EC = 1 Then
          If l00EE <= UBound(gv1036) Then
            l00EC = gv1036(l00EE).M2496
            l00EE = gv1036(l00EE).M249D
            l00EC = gv0F7C(l00EC).M2431
            l00F0 = gv0FB6(l00EC + l00EE).M1BD4
            l00EC = 4
            m009E.M15F7 = mc0098
          Else
            sub01CC
          End If
        ElseIf l00EC Then
          m009E.M15F7 = mc0096
          l00F0 = gv13E6(l00EC).M1BD4
        Else
          sub01CC
        End If
      Case Else
        sub01CC
      End Select
    Case mc009A
      Select Case p00EA And &HF000
      Case &HC000
      Case Else
      End Select
      l00EC = Abs(gv0FB6(l00EC).M2427)
      GoTo L142A8
    Case Else
      If l00EE <= UBound(gv1002) Then
        l00EC = gv1002(l00EE)
        m009E.M15F7 = mc0096
        l00F0 = gv13E6(l00EC).M1BD4
      Else
        sub01CC
        l00F0 = fn03D1(p00EA)
        m009E.M15F7 = 0
      End If
    End Select
  End Select
  m009E.M1BDC = l00EC
  If m009E.M15F7 = mc0096 Then
    l00EC = gv13E6(l00EC).M15F7
    Select Case l00EC And gc1240
    Case mc003C: l00EC = &HB
    Case mc003E: l00EC = &HE
    Case mc0040: l00EC = &HF
    Case mc0042: l00EC = &HC
    Case mc004A: l00EC = 1
    Case m004C: l00EC = 4
    Case gc125E: l00EC = &H14
    Case mc0044: l00EC = &H10
    Case Else: l00EC = 0
    End Select
    If l00EC Then
      m009E.M1BDC = l00EC
      m009E.M15F7 = mc0098
    End If
  End If
  If Len(l00F0) = 0 Then
    l00F0 = "P?" & Hex$(p00EA)
    m009E.M15F7 = 0
  End If
  m009E.M1BD4 = m009E.M1BD4 & "." & l00F0
  fn0AF0 = l00F0
End Function

Sub sub0AB2(p010C As Integer)
Dim l010E As Integer
Dim l0110
Dim l0112
Const mc011A = 16384 ' &H4000%
On Error Resume Next
  gv1418 = 0: gv144C = 1
  ReDim gv13E6(0), gv141A(0)
  gv14B0 = 0: gv14E4 = 1
  ReDim gv147E(0), gv14B2(0)
  gv13C2 = 0
  ReDim gv1390(0)
  l010E = FreeFile
  gv7A4A = gc11AC
  If p010C Then sub046E p010C, gc00BA & gv7A4A
  Open gv0B80 & gv7A4A For Binary Access Read As l010E
  Get l010E, , l0110
  If l0110 <> (mc000E Or &H8000) Then
    Close l010E
    sub0214 mc011A
   ' End
  End If
  Get l010E, , l0112
  For l0110 = 0 To l0112
    Get l010E, , l0112
    gv1276(l0110) = Space$(l0112)
    Get l010E, , gv1276(l0110)
  Next l0110
  Get l010E, , l0112
  For l0110 = 0 To l0112
    Get l010E, , l0112
    gv1260(l0110) = Space$(l0112)
    Get l010E, , gv1260(l0110)
  Next l0110
  Get l010E, , l0112: ReDim gv13E6(-l0112 To 0)
  m008C = fn032E(-l0112) - 1
  For l0110 = 1 To l0112 + 1
    Get l010E, , gv13E6(1 - l0110)
  Next l0110
  Get l010E, , l0112: ReDim gv147E(-l0112 To 0)
  m0090 = fn032E(-l0112) - 1
  For l0110 = 1 To l0112 + 1
    Get l010E, , gv147E(1 - l0110)
  Next
  Close l010E
  If gv1260(mc0040) = "" Then
    gv1260(mc0040) = "Field"
    gv1260(mc0042) = "TableDef"
    gv1260(mc0044) = "Index"
  End If
End Sub

Sub sub0A96()
Resume Next
'called (p1, p2, p3, p4 ) = As type
End Sub

Sub sub0AA5()

End Sub

Sub sub06CC()
Dim l012C As Integer
  Select Case m009E.M15F7
  Case mc0096
    l012C = gv13E6(m009E.M1BDC).M15F7
    If l012C And gc123E Then
      l012C = l012C Xor gc123E
    Else
    End If
  Case mc0098
    l012C = m009E.M1BDC
    If l012C < 0 Then
      l012C = -l012C
    ElseIf l012C = 20 Then
      l012C = 0
    Else
      If gv0F7C(l012C).M2427 < 0 Then
        If -gv0F7C(l012C).M2427 = gv0F7C(l012C - 1).M2427 Then
          l012C = l012C - 1
        Else
          l012C = -gv0F7C(l012C).M2427
        End If
      Else
        l012C = gv0F7C(l012C).M243B
        If l012C < 0 Then
          l012C = -l012C
        Else
          l012C = Abs(gv0F7C(l012C).M243B)
        End If
      End If
      If l012C Then
        m009E.M1BD4 = m009E.M1BD4 & "<." & gv0F7C(l012C).M1BD4 & ">"
      Else
        m009E.M1BD4 = m009E.M1BD4 & "<.defcoll>"
      End If
    End If
  Case mc009A
    l012C = m009E.M1BDC
  End Select
  m009E.M1BD4 = m009E.M1BD4 & "()"
  If l012C Then
    m009E.M1BDC = l012C
  Else
    m009E.M15F7 = 0
  End If
End Sub

Sub sub0419(p012E As Integer, ByVal p0130 As String)
Dim l0132 As Integer
Dim l0134
Dim l0136
Dim l0138
Dim l013A As Integer
Dim l013C
Dim l013E As Long
Dim l0142 As T29DB
Dim l0144
Dim l0148
Dim l014A
Dim l014C As Integer
' Const mc0158 = 4096 ' &H1000%
' Const mc015C = 64 ' &H40%
' Const mc0160 = 4 ' &H4%
  l013A = fn0308(p0130, ".")
  If l013A = 0 Then
    p0130 = p0130 & ".300"
  Else
    p0130 = Left$(p0130, l013A) & "300"
  End If
  If p012E Then sub046E p012E, gc00BA & p0130
On Error GoTo L1545C

L14E96:
  gv7A4A = gv0B80 & p0130
  l013A = FileLen(gv7A4A)
  If l013A = 0 Then Error 1000
  l0132 = FreeFile
  Open gv7A4A For Binary Access Read As l0132
  Get l0132, , l0144
  Get l0132, , l013C
  If l0144 = mc0010 Then
    l013E = 0
    If l013E = 0 Then
      sub0214 gc006E
      Error 1002
    End If
  ElseIf l0144 <> mc000E Then
    Error 1001
  End If
  ReDim Preserve gv1390(gv13C2 + l013C)
  For l0134 = 1 To l013C
    If l0144 = mc000E Then
      Get l0132, , l0142
    Else
      Get l0132, , l014A
      Get l0132, , l014C
'      sub0A96 l0148, l014A, l014C, l0142
    End If
    If gv792A Then
      If p012E Then sub046E p012E, gc00BA & l0142.M280D
      
    End If
    If l0142.M29FE >= 0 Then
      If l0144 = mc000E Then
        l0142.M29F5 = gv144C
        ReDim Preserve gv141A(gv144C + l0142.M29FE + 1)
        ReDim Preserve gv13E6(LBound(gv13E6) To gv1418 + l0142.M29FE + 1)
      End If
      For l0138 = 0 To l0142.M29FE
        If l0144 = mc000E Then
          Get l0132, , l013A
          If l013A > 0 Then
            gv1418 = gv1418 + 1
            Get l0132, , gv13E6(gv1418)
            l013A = gv1418
          End If
          gv141A(gv144C) = l013A
          gv144C = gv144C + 1
        Else
          l013A = gv141A(l0142.M29F5 + l0138)
        End If
        If l013A > 0 Then
          If (gv13E6(l013A).M15F7 And &HFF) = &H70 Then
            Select Case gv13E6(l013A).M1BD4
            Case "Fields": gv13E6(l013A).M15F7 = mc0040 Or gc123E
            Case "TableDefs": gv13E6(l013A).M15F7 = mc0042 Or gc123E
            Case "Indexes": gv13E6(l013A).M15F7 = mc0044 Or gc123E
            Case Else: sub0214 mc0158
            End Select
          End If
        End If
      Next l0138
    Else
      l0142.M29F5 = 0
    End If
    If l0142.M2A1E >= 0 Then
      If l0144 = mc000E Then
        l0142.M2A14 = gv14E4
        ReDim Preserve gv14B2(gv14E4 + l0142.M2A1E + 1)
        ReDim Preserve gv147E(LBound(gv147E) To gv14B0 + l0142.M2A1E + 1)
        For l0136 = 0 To l0142.M2A1E
          Get l0132, , l013A
          If l013A > 0 Then
            gv14B0 = gv14B0 + 1
            l013A = gv14B0
            Get l0132, , gv147E(gv14B0)
          End If
          gv14B2(gv14E4) = l013A
          gv14E4 = gv14E4 + 1
        Next l0136
      End If
    Else
      l0142.M2A14 = 0
    End If
    gv13C2 = gv13C2 + 1
    gv1390(gv13C2) = l0142
  Next l0134

L1542A:
  If l0132 Then Close l0132
  If l0148 Then Close l0148
Exit Sub

L1545C:
  If l0132 Then Close l0132: l0132 = 0
  If l0148 Then Close l0148: l0148 = 0
  gv7A30 = gv7A30 And Not mc015C
  sub0214 mc015C
  If gv7A48 = mc0160 Then Resume L14E96
  Resume L1542A
End Sub
