Attribute VB_Name = "MODULE81"
' Module8
' Option Explicit

Sub sub0739(p0078 As String)
  gv0C1A = gv0C1A & p0078
End Sub

Sub sub06A5(p001E As Integer)
  gv0C1A = Space$(p001E)
  gv0C1E = p001E
End Sub

Sub sub06AF(p0010 As Integer)
Dim l0012 As Integer
  l0012 = p0010 - Len(gv0C1A)
  If l0012 > 0 Then gv0C1A = gv0C1A & Space$(l0012)
 End Sub

Sub sub07E0(p008A As Integer, p008C As Long, p008E As Long, p0090 As Long)
Dim l0094
Dim l0096 As Integer
Dim l0098 As Integer
Dim l009A As Long
Dim l009E As String
Dim l00A0 As String
  l00A0 = Hex$(p0090): If Len(l00A0) < 4 Then l00A0 = Space$(4)
  Seek p008A, p008C + p008E
  l009A = p0090 - p008E
  If gv0C12 = 0 And l009A > 512 Then l009A = 512
  l0096 = 16
  l009E = Space$(l0096)
  While l009A > 0
    If l009A < l0096 Then l0096 = l009A: l009E = Space$(l0096)
    Get p008A, , l009E
    For l0094 = 1 To l0096
      l0098 = Asc(Mid$(l009E, l0094, 1))
      sub0739 fn02A3(l0098)
      If l0098 < 32 Or l0098 > &H7E Then Mid$(l009E, l0094, 1) = " "
    Next l0094
    If 49 > Len(gv0C1A) Then sub0739 Space$(49 - Len(gv0C1A))
    RSet l00A0 = Hex$(p008E)
    gv0C1A = l00A0 & gv0C1A & l009E: sub07F0
    p008E = p008E + l0096
    l009A = l009A - l0096
  Wend
  p008E = p0090
  Seek p008A, p008C + p008E
End Sub

Sub sub07F0()
On Error Resume Next
  If gv0C12 Then
    Print #gv0C12, gv0C1A
  Else
    If Len(gv0C1A) Then gv0C16.AddItem gv0C1A
  End If
  gv0C1A = ""
End Sub

Sub sub07FA(p002E As Integer, p0030 As Long, p0032 As Long, p0034 As Long)
Dim l0036
Dim l0038 As Integer
Dim l003A As Long
Dim l003C As Integer
Dim l003E As String
  l003E = Hex$(p0034): If Len(l003E) < 4 Then l003E = Space$(4)
  l003A = p0034 - p0032
  If gv0C12 = 0 And l003A > 512 Then l003A = 512
  Seek p002E, p0030 + p0032
  l0038 = 16
  While l003A > 0
    RSet l003E = Hex$(p0032): gv0C1A = l003E
    If l003A < l0038 Then l0038 = l003A
    For l0036 = 1 To l0038 Step 2
      Get p002E, , l003C
      sub0739 fn02B5(l003C)
    Next l0036
    p0032 = p0032 + l0038
    l003A = l003A - l0038
    sub07F0
  Wend
  p0032 = p0034
  Seek p002E, p0030 + p0032
End Sub

Sub sub080A(p0042 As String, p0044 As Integer)
Dim l0046 As Integer
  l0046 = InStr(p0044, p0042, Chr$(0)) - p0044
  sub0739 " '" & Mid$(p0042, p0044, l0046) & "'"
  sub07F0
End Sub

Sub sub0816()
  If gv0C12 Then
    Close gv0C12
    gv0C12 = 0
  End If
  Set gv0C16 = Nothing
  Screen.MousePointer = 0
End Sub

Sub sub0820(p004C As Integer, p004E As Long, p0050 As Long)
Dim l0052
Dim l0054 As Integer
Dim l0056 As String
Dim l0058 As Integer
Dim l005A As Long
Dim l005C As Long
  If p004E = 0 Then Exit Sub
  Seek p004C, p004E
  l005A = p0050 - p004E
  If gv0C12 = 0 And l005A > 512 Then l005A = 512
  l0054 = 16
  l0056 = Space$(l0054)
  l005C = 0
  While l005A > 0
    If l005C < &H1000 Then
      gv0C1A = fn02BE(Int(l005C))
    Else
      gv0C1A = Hex$(l005C)
    End If
    If l005A < 16 Then l0054 = l005A: l0056 = Space$(l005A)
    Get p004C, , l0056
    For l0052 = 1 To l0054
      l0058 = Asc(Mid$(l0056, l0052, 1))
      sub0739 fn02A3(l0058)
      If l0058 < 32 Then Mid$(l0056, l0052, 1) = " "
    Next l0052
    If 54 > Len(gv0C1A) Then sub0739 Space$(54 - Len(gv0C1A))
    sub0739 l0056
    sub07F0
    l005C = l005C + l0054
    l005A = l005A - l0054
  Wend
End Sub

Sub sub082A(p0062 As Integer, p0064 As Long, p0066 As Long)
Dim l006A
Dim l006C As Integer
Dim l0070 As Long
Dim l0072 As Long
Dim l0074 As Integer
  l0072 = 0
  Seek p0062, p0064
  l0070 = p0066 - p0064
  If gv0C12 = 0 And l0070 > 512 Then l0070 = 512
  l006C = 16
  While l0070 > 0
    If l0072 < &H1000 Then
      gv0C1A = fn02BE(Int(l0072))
    Else
      gv0C1A = Hex$(l0072)
    End If
    If l0070 < l006C Then l006C = l0070
    For l006A = 1 To l006C Step 2
      Get p0062, , l0074
      sub0739 fn02B5(l0074)
    Next l006A
    l0072 = l0072 + l006C
    l0070 = l0070 - l006C
    sub07F0
  Wend
End Sub

Sub sub0834(p0076 As String)
  If gv0C12 Then
    sub01CC
    Close gv0C12
  End If
  gv0C12 = FreeFile
  Screen.MousePointer = 11
  Open p0076 For Output As gv0C12
End Sub

Sub sub083D(p0022 As Control)
  Set gv0C16 = p0022
  p0022.Clear
End Sub

Sub sub0847(p0024 As String, p0026 As Integer, p0028 As String)
Dim l002A
Dim l002C As Integer
  For l002A = 1 To Len(p0028)
    Select Case Mid$(p0028, l002A, 1)
    Case "b"
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026, 1)) + &H100), 2)
      p0026 = p0026 + 1
    Case "w"
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026 + 1, 1)) + &H100), 2)
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026, 1)) + &H100), 2)
      p0026 = p0026 + 2
    Case "l"
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026 + 3, 1)) + &H100), 2)
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026 + 2, 1)) + &H100), 2)
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026 + 1, 1)) + &H100), 2)
      sub0739 Right$(Hex$(Asc(Mid$(p0024, p0026, 1)) + &H100), 2)
      p0026 = p0026 + 4
    Case "n"
      sub07F0
    Case "s"
      l002C = Asc(Mid$(p0024, p0026, 1))
      sub0739 "'" & Mid$(p0024, p0026 + 1, l002C - 1) & "'"
      p0026 = p0026 + l002C + 1
    Case "t"
      l002C = Len(gv0C1A) Mod 14
      If l002C Then sub0739 Space$(14 - l002C)
    Case "z"
      l002C = InStr(p0026, p0024, Chr$(0)) - p0026
      sub0739 "'" & Mid$(p0024, p0026, l002C) & "'"
    Case "Z"
      l002C = InStr(p0026, p0024, Chr$(0)) - p0026
      sub0739 "'" & Mid$(p0024, p0026, l002C) & "'"
      p0026 = p0026 + l002C + 1
    Case "\"
      p0026 = p0026 + 1
    Case Else
      sub0739 Mid$(p0028, l002A, 1)
    End Select
  Next l002A
End Sub

Sub sub0851(p007A As Long, p007C() As String)
Dim l0082 As Long
Dim l0084
Dim l0088 As Integer
  l0082 = 1
  For l0084 = 0 To UBound(p007C)
    If p007A And l0082 Then
      GoSub LFA6C
      sub0739 p007C(l0084)
      p007A = p007A Xor l0082
    End If
    l0082 = l0082 * 2
  Next
  If p007A Then
    GoSub LFA6C
    sub0739 "0x" & Hex$(p007A)
  End If
Exit Sub

LFA6C:
  If l0088 Then
    sub0739 " | "
  Else
    sub0739 " ": l0088 = True
  End If
Return
End Sub

Sub sub0860(p0014 As Integer, p0016 As Long, p0018 As Integer, p001A As Integer)
  sub07E0 p0014, p0016, fn032E(p0018), fn032E(p001A)
  p0018 = p001A
End Sub

