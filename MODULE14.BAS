Attribute VB_Name = "MODULE141"
' Module14
' Option Explicit
Const mc00C2 = 4 ' &H4%
Const mc00CA = 16384 ' &H4000%
Const mc00DC = 1 ' &H1%
Const mc00E4 = 1 ' &H1%
Const mc00E6 = 2 ' &H2%
Const mc00E8 = 2 ' &H2%
Const mc00EA = 3 ' &H3%
Const mc00EC = 3 ' &H3%
Const mc00EE = 4 ' &H4%
Const mc00F0 = 4 ' &H4%
Const mc00F2 = 5 ' &H5%
Const mc00F4 = 5 ' &H5%
Const mc00F6 = 6 ' &H6%
Const mc00F8 = 6 ' &H6%
Const mc00FA = 7 ' &H7%
Const mc00FC = 7 ' &H7%
Const mc00FE = 12 ' &HC%
Const mc0100 = 8 ' &H8%
Const mc0102 = 10 ' &HA%
Const mc0104 = 9 ' &H9%
Const mc0106 = 8 ' &H8%
Const mc0108 = 10 ' &HA%
Const mc010A = 9 ' &H9%

Function fn0B59() As String
Dim l00A4 As Integer
Dim l00A6 As Integer
  l00A4 = gv1584.M2B1C
  If l00A4 <= 0 Then Exit Function
  l00A6 = InStr(l00A4, gv157E, "§")
  fn0B59 = Mid$(gv157E, l00A4, l00A6 - l00A4)
End Function

Sub sub0B65()
End Sub

Function fn0B2C(p00B0 As Integer) As String
Dim l00B2 As Integer
Dim l00B4 As Integer
  l00B2 = gv10EC + (p00B0 And &HFF) * &H18
  l00B4 = Asc(Mid$(gv10E6, l00B2, 1))
  fn0B2C = Mid$(gv10E6, l00B2 + 1, l00B4)
End Function

Sub sub0B36(p00BA As Integer)
Dim l00BC As Integer
Dim l00BE
 Const mc00C2 = 4 ' &H4%
 Const mc00CA = 16384 ' &H4000%
  Select Case gv0884
  Case 2: gv10EC = &H7500
  Case 3: gv10EC = &H5600
  Case Else
    sub0214 mc00C2
    Exit Sub
  End Select
  l00BC = FreeFile
  gv7A4A = "vbdis" & Format$(gv0884) & gc1512
  If p00BA Then sub046E p00BA, gc00BA & gv7A4A
  Open gv0B80 & gv7A4A For Binary As l00BC
  Get l00BC, , l00BE
  l00BE = l00BE Xor (gv0884 * &H100)
  If l00BE = 2 Then
    Get l00BC, , gv234E
    gv77FA = String$(gc1518 * 3, 0): Get l00BC, , gv77FA
    Get l00BC, , l00BE
    gv10E6 = String$(l00BE, 0): Get l00BC, , gv10E6
  Else
    sub0214 mc00CA
    gv097E = True
  End If
  Close l00BC
End Sub

Function fn0B49(p00D0 As Integer) As String
Dim l00D2 As Integer
Dim l00D4 As Integer
  l00D2 = Abs(gv15EA.M2B71(p00D0).M2B1C)
  If l00D2 = 0 Then Exit Function
  l00D4 = InStr(l00D2, gv157E, "§")
  fn0B49 = Mid$(gv157E, l00D2, l00D4 - l00D2)
End Function

Sub sub0B1A(p00D6 As Integer)
Dim l00D8 As Integer
Dim l00DA
 Const mc00DC = 1 ' &H1%
 Const mc00E4 = 1 ' &H1%
 Const mc00E6 = 2 ' &H2%
 Const mc00E8 = 2 ' &H2%
 Const mc00EA = 3 ' &H3%
 Const mc00EC = 3 ' &H3%
 Const mc00EE = 4 ' &H4%
 Const mc00F0 = 4 ' &H4%
 Const mc00F2 = 5 ' &H5%
 Const mc00F4 = 5 ' &H5%
 Const mc00F6 = 6 ' &H6%
 Const mc00F8 = 6 ' &H6%
 Const mc00FA = 7 ' &H7%
 Const mc00FC = 7 ' &H7%
 Const mc00FE = 12 ' &HC%
 Const mc0100 = 8 ' &H8%
 Const mc0102 = 10 ' &HA%
 Const mc0104 = 9 ' &H9%
 Const mc0106 = 8 ' &H8%
 Const mc0108 = 10 ' &HA%
 Const mc010A = 9 ' &H9%
  l00D8 = FreeFile
  gv7A4A = gc150E
  If p00D6 Then sub046E p00D6, gc00BA & gv7A4A
  Open gv0B80 & gv7A4A For Binary As l00D8
  'Get l00D8, , l00DA
  If l00DA = 0 Then sub0214 mc00CA
  gv157E = String$(l00DA, 0)
  Get l00D8, , gv15EA
 ' Get l00D8, , gv157E
  Close l00D8
  gv1196(mc00DC) = 0
  gv1196(mc00E6) = mc00E4
  gv1196(mc00EA) = mc00E8
  gv1196(mc00EE) = mc00EC
  gv1196(mc00F2) = mc00F0
  gv1196(mc00F6) = mc00F4
  gv1196(mc00FA) = mc00F8
  gv1196(mc00FE) = mc00FC
  gv1196(mc0102) = mc0100
  gv1196(mc0106) = mc0104
  gv1196(mc010A) = mc0108
  gv1180(0) = mc00DC
  gv1180(mc00E4) = mc00E6
  gv1180(mc00E8) = mc00EA
  gv1180(mc00EC) = mc00EE
  gv1180(mc00F0) = mc00F2
  gv1180(mc00F4) = mc00F6
  gv1180(mc00F8) = mc00FA
  gv1180(mc00FC) = mc00FE
  gv1180(mc0100) = mc0102
  gv1180(mc0104) = mc0106
  gv1180(mc0108) = mc010A
  gv1522(1) = "Int"
  gv1522(2) = "Lng"
  gv1522(3) = "Sng"
  gv1522(4) = "Dbl"
  gv1522(5) = "Cur"
  gv1522(6) = "Var"
  gv1522(7) = "Str"
  gv1538(1) = "Integer"
  gv1538(2) = "Long"
  gv1538(3) = "Single"
  gv1538(4) = "Double"
  gv1538(5) = "Currency"
  gv1538(6) = "Variant"
  gv1538(7) = "String"
  gv1538(8) = "String *"
  sub0A59
End Sub
