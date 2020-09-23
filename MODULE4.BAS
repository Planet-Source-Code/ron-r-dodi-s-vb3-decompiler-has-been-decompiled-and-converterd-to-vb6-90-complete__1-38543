Attribute VB_Name = "MODULE4"
' Module4
' Option Explicit

Function fn0288(p0080 As String) As Long
  gv0A5E.M1D4F = p0080
  LSet gv0A64 = gv0A5E
  fn0288 = gv0A64.M1D80
End Function

Function fn029B(p008E As Integer) As String
  fn029B = Right$(Hex$(p008E Or &H100), 2)
 End Function

Function fn02A3(p0092 As Integer) As String
  fn02A3 = " " & Right$(Hex$(p0092 Or &H100), 2)
End Function

Function fn02AC(p0096 As Long) As String
Dim l009A As String * 9
  RSet l009A = Hex$(p0096)
  fn02AC = l009A
End Function

Function fn02B5(p009E As Integer) As String
  fn02B5 = " " & Right$(Hex$(p009E Or &H10000), 4)
End Function

Function fn02BE(ByVal p00A2 As Long) As String
  fn02BE = Right$(Hex$(p00A2 Or &H10000), 4)
End Function

Function fn02C6(p00A6 As Long) As Integer
  fn02C6 = (p00A6 And &HFFFF0000) \ &H10000
End Function

Function fn02D0(p00AA As Integer, p00AC As Integer) As Long
  fn02D0 = (CLng(p00AA) And &HFFFF&) * p00AC
End Function

Function fn02D8(ByVal p00B0 As Long) As Integer
  If p00B0 And &H8000& Then
    fn02D8 = CInt(p00B0 Or &HFFFF0000)
  Else
    fn02D8 = p00B0 And &HFFFF&
  End If
End Function

Function fn02E2(p00B4 As Integer) As String
  gv0A20.M1D63 = p00B4
  LSet gv0A0C = gv0A20
  fn02E2 = gv0A0C.M1D4F
End Function

Function fn02E9(p00B8 As Long) As String
  gv0A64.M1D80 = p00B8
  LSet gv0A5E = gv0A64
  fn02E9 = gv0A5E.M1D4F
End Function

Function fn02F0(p00BC As String, p00BE As Integer) As Integer
  gv0A0C.M1D4F = Mid$(p00BC, p00BE, 2)
  LSet gv0A20 = gv0A0C
  fn02F0 = gv0A20.M1D63
End Function

Function fn02F9(ByVal p00C2 As String) As String
  If Right$(p00C2, 1) = "\" Then fn02F9 = p00C2 Else fn02F9 = p00C2 & "\"
End Function

Function fn0308(p00C6 As String, ByVal p00C8 As String) As Integer
Dim l00CA As Integer
  Do
    fn0308 = l00CA
    l00CA = InStr(l00CA + 1, p00C6, p00C8)
  Loop While l00CA
End Function

Function fn031E(p00D2 As Integer, p00D4 As Integer) As Integer
Dim l00D6 As Long
  l00D6 = fn032E(p00D2) + p00D4
  If l00D6 >= &H8000& Then l00D6 = l00D6 Or &HFFFF0000
  fn031E = l00D6
End Function

Function fn0326(p00DA As Integer, p00DC As Integer) As Integer
  fn0326 = fn032E(p00DA) \ p00DC
End Function

Function fn032E(p00E0 As Integer) As Long
  fn032E = CLng(p00E0) And &HFFFF&
End Function

Function fn0336(p00E4 As Integer, p00E6 As Integer) As Integer
Dim l00E8 As Long
  l00E8 = fn032E(p00E4) * p00E6
  If l00E8 >= &H8000& Then l00E8 = l00E8 Or &HFFFF0000
  fn0336 = l00E8
End Function

Function fn0348(p00F0 As String, p00F2 As Integer) As String
Dim l00F4 As Integer
  If p00F2 < 0 Then
    fn0348 = "$" & fn02BE(p00F2) & "?"
  Else
    l00F4 = InStr(p00F2, p00F0, Chr$(0)) - p00F2
    fn0348 = Mid$(p00F0, p00F2, l00F4)
  End If
End Function

Function fn0350(p00F8 As String) As String
Dim l00FA As Integer
  l00FA = InStr(p00F8, Chr$(0))
  If l00FA Then
    fn0350 = Left$(p00F8, l00FA - 1)
  Else
    fn0350 = p00F8
  End If
End Function

Sub sub027B(p0074 As ComboBox, p0076 As Variant)
Dim l007A
Dim l007C As Long
  If VarType(p0076) = 8 Then
    For l007A = p0074.ListCount - 1 To 0 Step -1
      If p0074.List(l007A) = p0076 Then Exit For
    Next
  Else
    l007C = p0076
    For l007A = p0074.ListCount - 1 To 0 Step -1
      If p0074.ItemData(l007A) = l007C Then Exit For
    Next
  End If
  p0074.ListIndex = l007A
End Sub

Sub sub028F(p0082 As Integer, p0084 As Integer)
Dim l008A As Long
  gv0ADC.M1DBD = p0084
  gv0ADC.M1DC5 = p0082
  l008A = p0084 And &HFFFF&
  gv0ADC.M1DFA = (l008A And &H1F) * 2
  gv0ADC.M1DF3 = (l008A \ &H20) And &H3F
  gv0ADC.M1DEC = (l008A \ &H800) And &H1F
  l008A = p0082 And &HFFFF&
  gv0ADC.M1DE5 = l008A And &H1F
  gv0ADC.M1DDE = (l008A \ &H20) And &HF
  gv0ADC.M1DD6 = ((l008A \ &H200) And &H7F) + 1980
  gv0ADC.M1DB8 = DateSerial(gv0ADC.M1DD6, gv0ADC.M1DDE, gv0ADC.M1DE5) + TimeSerial(gv0ADC.M1DEC, gv0ADC.M1DF3, gv0ADC.M1DFA)
  gv0ADC.M0348 = Format$(gv0ADC.M1DB8, "hh:mm:ss")
  gv0ADC.M1DCE = Format$(gv0ADC.M1DB8, "dd.mm.yyyy")
End Sub

Sub sub0312(p00CC As Variant)
  gv0ADC.M1DB8 = p00CC
  gv0ADC.M1DD6 = Year(p00CC)
  gv0ADC.M1DDE = Month(p00CC)
  gv0ADC.M1DE5 = Day(p00CC)
  gv0ADC.M1DEC = Hour(p00CC)
  gv0ADC.M1DF3 = Minute(p00CC)
  gv0ADC.M1DFA = Second(p00CC)
  gv0ADC.M1DBD = (fn0336(gv0ADC.M1DEC, &H800) Or gv0ADC.M1DF3 * &H20 Or gv0ADC.M1DFA \ 2)
  gv0ADC.M1DC5 = (gv0ADC.M1DD6 - 1980) * &H200 + gv0ADC.M1DDE * &H20 + gv0ADC.M1DE5
  gv0ADC.M0348 = Format$(gv0ADC.M1DB8, "hh:mm:ss")
  gv0ADC.M1DCE = Format$(gv0ADC.M1DB8, "dd.mm.yyyy")
End Sub

Sub sub033E(p00EA As Variant)
  gv0ADC.M1DB8 = p00EA
End Sub

