Attribute VB_Name = "MODULE191"
' Module19
' Option Explicit
Const mc0060 = -32760 ' &H8008%
Const mc0062 = 16768 ' &H4180%
Const mc0064 = 8016 ' &H1F50%
Const mc0070 = 16 ' &H10%
Const mc0074 = 48 ' &H30%
Const mc0076 = 1 ' &H1%
Const mc0094 = 2 ' &H2%
Const mc00AC = 4 ' &H4%
Const mc00AE = 2 ' &H2%
Const mc00B0 = 3 ' &H3%
Const mc00B2 = 7 ' &H7%

Function fn0DC5() As Integer
  fn0DC5 = gv7A30
End Function

Sub sub0214(p0068 As Variant)
' Const mc0070 = 16 ' &H10%
' Const mc0074 = 48 ' &H30%
' Const mc0076 = 1 ' &H1%
  If gv7A34 = 0 Then
    gv7A34 = gv097C: gv7A36 = gv0918
  End If
  If VarType(p0068) = 8 Then
    MsgBox p0068, mc0070, gc0022
    gv7A2E = -1
  Else
    If gv7A30 And p0068 Then Exit Sub
    sub0DD4 Val(p0068), mc0074, mc0076
  End If
End Sub

Function fn0DDF() As Integer
  fn0DDF = gv7A2E
End Function

Sub sub040A()
  gv7A2E = 0: gv7A34 = 0
End Sub

Sub sub0DD4(p007A As Integer, p007C As Integer, p007E As Integer)
Dim l0080 As Integer
Dim l0082 As String
' Const mc0094 = 2 ' &H2%
' Const mc00AC = 4 ' &H4%
' Const mc00AE = 2 ' &H2%
' Const mc00B0 = 3 ' &H3%
' Const mc00B2 = 7 ' &H7%
  If gv7A2E = 0 Then gv7A2E = p007A
  If p007A And mc0060 Then l0080 = l0080 Or gc7A80
  If p007A And mc0062 Then l0080 = l0080 Or gc7A7E
  If p007A And mc0064 Then l0080 = l0080 Or gc7A7C
  If l0080 = 0 Then l0080 = gc7A7A
  gv7A30 = gv7A30 Or p007A
  Select Case p007A
  Case 0
  Case gc7A4E: l0082 = gc0032
  Case gc7A50: l0082 = gc0036
  Case gc7A52: l0082 = "VB Version" & Str$(gv0884) & gc003A
  Case gc7A54: l0082 = gc003E
  Case gc7A56: l0082 = gc0042
  Case gc7A58: l0082 = gc0046
  Case gc7A5A
    If Right$(gv7A4A, 4) = ".300" Then
      l0082 = Left$(gv7A4A, Len(gv7A4A) - 3) & "VBX"
    Else
      l0082 = gv7A4A
    End If
    l0082 = gc004E & l0082
    p007C = mc0070
    p007E = mc0094
  Case gc7A5C: l0082 = gv7A4A & gc0052
  Case gc7A5E: l0082 = gc0056
  Case gc7A60: l0082 = gc005A
  Case gc7A62: l0082 = gc005E
  Case gc7A64: l0082 = gc0062
  Case gc7A66: l0082 = gc0066
  Case gc7A68: l0082 = gc006A
  Case gc7A6A: l0082 = gc006E & Chr$(10) & gv7A4A
  Case gc7A6C: l0082 = gc004A
  Case Else: l0082 = gc002A & Format$(p007A)
  End Select
  If Len(l0082) Then
    If Right$(l0082, 1) = "!" Then l0082 = l0082 & Chr$(10) & gc0026
    gv7A48 = MsgBox(l0082, p007C Or p007E, gc0022)
    Select Case gv7A48
    Case mc00AC
      Exit Sub
    Case mc00AE, mc00B0, mc00B2
      GoTo L1DA36
    End Select
  End If
  l0080 = l0080 And Not gv7A32
  gv7A32 = gv7A32 Or l0080
  Select Case l0080
  Case 0
  Case gc7A80
    MsgBox gc000E, mc0070, gc000A
    gv7A48 = mc00B0
  Case gc7A7E
    gv7A48 = MsgBox(gc0012, 48 + 1, gc000A)
  Case gc7A7C
    gv7A48 = MsgBox(gc0016, 32 + 1, gc000A)
  Case Else
    gv7A48 = MsgBox(gc001A, 64 + 1, gc000A)
  End Select
  Select Case gv7A48
  Case mc00AE, mc00B0, mc00B2
  Case Else
    Exit Sub
  End Select

L1DA36:
  gv7A32 = gv7A32 Or gc7A82
  
End Sub
