Attribute VB_Name = "Module6"
' Module18
'' Option Explicit
Const mc0094 = 4 ' &H4%
Const mc00A0 = 16 ' &H10%

Function fn0D6C(ByVal pv0046 As Integer) As Integer
  fn0D6C = (fn0326(pv0046, 8) And &H1FFF&) + ((pv0046 And 6) * &H1000)
End Function

Function fn0D74(ByVal pv004C As Integer) As Long
Dim l004E As Integer
  l004E = (pv004C \ &H2000) And 3
  fn0D74 = 1 + fn0142(l004E + 3) + CLng(pv004C And &H1FFF) * 8
End Function

Function fn0D7F(ByVal pv0054 As Integer) As Integer
  fn0D7F = fn02D8(fn032E(pv0054 And &H1FFF) * 8 + ((pv0054 \ &H1000) And 6))
End Function

Sub sub0D89(p005A As Integer, p005E As Form)
Dim l0060 As Integer
Dim l0062 As String
Dim l0064 As Integer
Dim l0066 As Integer
Const mc0094 = 4 ' &H4%
Const mc00A0 = 16 ' &H10%
  Select Case p005A
  Case gc791E
    gv791C = 0
  Case gc7922
    gv791C = gv791C And Not (gc7922 Or gc7926 Or gc7928)
  Case gc7926
    gv791C = gv791C And Not (gc7928 Or gc7926)
  Case gc7928
    If gv791C And p005A Then
      gv791C = gv791C And Not gc7926
    End If
  Case Else
    If p005A And gv791C Then
      MsgBox gc0086
      Exit Sub
    End If
  End Select
  l0064 = p005A Or (p005A - 1)
  l0064 = l0064 And Not gv791C
  If l0064 And gc791E Then
    sub0684
    gv7A8C = ""
    p005E.Caption = gc0092
    Screen.MousePointer = 0
    
    
   ' l0062 =
    If Len(l0062) = 0 Then Exit Sub
    gv0876 = l0062
    l0060 = fn0308(l0062, "\")
    If l0060 Then
      gv087A = Left$(l0062, l0060)
      l0062 = Mid$(l0062, l0060 + 1)
    Else
      sub01CC
    End If
    gv087E = l0062
    l0060 = InStr(l0062, ".")
    If l0060 Then l0062 = Left$(l0062, l0060 - 1)
    gv7A90 = l0062
    gv791C = gv791C Or gc791E
    p005E.Caption = "VBDis " & gv7A90
    
  End If
  If l0064 And gc7920 Then
    
    
    sub01E6 l0066, gc00BA & gv087E
    sub00AE
    If gv0884 Then
      sub0B36 l0066
      If gv097E Then
        gv0884 = 0
        gv791C = 0
      End If
      sub0419 l0066, "VBRUN" & Format(gv0884) & "00.VBX"
    Else
      MsgBox gc0036
      gv791C = 0
    End If
    Screen.MousePointer = 0
    sub0205 l0066
    If gv791C Then
      gv791C = gv791C Or gc7920
      p005E.Caption = "VBDis " & gv087E

    Else
      p005E.Caption = gc0092
      Exit Sub
    End If
    Screen.MousePointer = 11
  End If
  If l0064 And gc7922 Then
    sub01E6 l0066, gc00BE & gv087E
    sub0C58
    
    sub0205 l0066
    If gv7A30 And mc0094 Then
      gv791C = 0
      Screen.MousePointer = 0
      Exit Sub
    Else
      gv791C = gv791C Or gc7922
    End If
  End If
  If l0064 And gc7924 Then
    If (gv791C And gc7924) = 0 Then
      
      sub01E6 l0066, gc00BE & gc00EE
      sub0A4B
      gv791C = gv791C Or gc7924
      sub0205 l0066
    End If
  End If
  If l0064 And gc7926 Then
    sub01E6 l0066, gc009E
    sub0D96
    sub046E l0066, gc00A6
    gv791C = gv791C Or gc7926
    gv090E = True
    l0064 = 0
    l0062 = fn0E8D("", "mak")
    p005E.Caption = l0062
    MsgBox l0062 & Chr$(10) & gc00A6, mc00A0, gc00AA
  End If
  If l0064 And gc7928 Then
    
    sub01E6 l0066, gc00AE
    sub04E4
    gv791C = gv791C Or gc7928
    gv090E = True
    sub046E l0066, gc00B2
  End If
  Screen.MousePointer = 0
End Sub

Private Sub sub0D54()
  Screen.MousePointer = 11
  DoEvents
End Sub

Sub sub046E(p00AA As Integer, ByVal p00AC As String)
Dim l00AE As Integer
 ' l00AE =
  If p00AA > l00AE Then
    p00AA = l00AE
  Else
   While p00AA < l00AE
      
    Wend
  End If
  
 ' sub0D54 p00AC
End Sub

Sub sub0205(p00B0 As Integer)
  'While
    
 ' Wend
  Screen.MousePointer = 0
End Sub

Sub sub01E6(p00B2 As Integer, p00B4 As String)
  
'p00B2 =
  'sub0D54 p00B4
End Sub

Sub sub0D5E(p00B6 As Integer)
Const mc00C0 = 32 ' &H20%
  If Len(gv0B80) Then Exit Sub
 ' sub0578 p00B6
  sub0B1A p00B6
  sub0ABF p00B6
  gv0EA2(1) = -22 - 2
  gv0EA2(2) = -22 - 4
  gv0EA2(3) = gv0EA2(2)
  gv0EA2(4) = -22 - 8
  gv0EA2(5) = gv0EA2(4)
  gv0EA2(6) = -22 - &H10
  gv0EA2(7) = 1
End Sub

Sub sub01CC()
Const mc00C0 = 32 ' &H20%
    sub0214 mc00C0
End Sub
