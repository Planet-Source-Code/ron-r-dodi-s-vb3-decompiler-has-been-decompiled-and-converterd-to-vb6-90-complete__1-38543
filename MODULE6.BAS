' Module6
Option Explicit
Declare Function extfn0514 Lib "Kernel" Alias "GetPrivateProfileString" (ByVal p1$, ByVal p2$, ByVal p3$, ByVal p4$, ByVal p5%, ByVal p6$) As Integer
Declare Function extfn052F Lib "Kernel" Alias "GetSystemDirectory" (ByVal p1$, ByVal p2%) As Integer
Declare Function extfn0545 Lib "Kernel" Alias "GetWindowsDirectory" (ByVal p1$, ByVal p2%) As Integer
Declare Sub extsub055C Lib "User" Alias "WinHelp" ()
Const mc0040 = 48 ' &H30%

Function fn0567 (ByVal p0020 As String) As String
Dim l0022 As Integer
Dim l0024 As String
  l0024 = Space$(128)
  l0022 = extfn0514("Visual Basic" & Chr$(0), p0020 & Chr$(0), "", l0024, 128, "vb.ini")
  fn0567 = Left$(l0024, l0022)
End Function

Sub sub0578 (p0026 As Integer)
Dim l0028 As Integer
Dim l002C As String
Dim l002E As Integer
' Const mc0040 = 48 ' &H30%
' Const mc004C = 64 ' &H40%
On Error Resume Next
  gv0B80 = fn02F9((App.Path))
  gv7A4A = gc11AC
  If  p0026 Then sub046E p0026, gc00BA & gv7A4A
  While Len(Dir$(gv0B80 & gv7A4A, 0)) = 0
    l002E = Screen.MousePointer
    Screen.MousePointer = 0
      MsgBox gv0B80 & gv7A4A, mc0040, gc007E
        ÿ™<.Tag = gv7A4A
        ÿ™<.Show 1
    Screen.MousePointer = l002E
    l002C =   ÿ™<.Tag
    If  Len(l002C) = 0 Then End
    If  Right$(l002C, 3) = "300" Then
      l0028 = fn0308(l002C, "\")
      If  l0028 Then gv0B80 = Left$(l002C, l0028)
    End If
  Wend
  l002C = Space$(256)
  l0028 = extfn0545(l002C, Len(l002C))
  gv0B74 = fn02F9(Left$(l002C, l0028))
  l0028 = extfn052F(l002C, Len(l002C))
  gv0B78 = fn02F9(Left$(l002C, l0028))
  gv0B7C = fn0567("vbpath")
  If  Len(gv0B7C) = 0 Then
    gv0B7C = gv0B80
    If  Len(Dir$(gv0B7C & "vb.exe")) = 0 Then
      gv0B7C = ""
    End If
  End If
  If  Len(gv0B7C) Then
    If  Right$(gv0B7C, 1) <> "\" Then gv0B7C = gv0B7C & "\"
  Else
    MsgBox gc007A, mc004C
  End If
  Err = 0
End Sub
