Attribute VB_Name = "MODULE21"
' Module21
' Option Explicit
Const mc004A = 2 ' &H2%
Const mc006A = 8192 ' &H2000%

Sub sub0D96()
  If gv79A2 = 0 Then
    MsgBox gc0082
    Exit Sub
  End If
  sub0E26
  sub0E52
  sub04D4 True
End Sub

Sub sub0E26()
Dim l0006 As String
Dim l0008 As Integer
  l0006 = fn0E8D(gv0F36, "MAK")
  sub01E6 l0008, gc00C2 & l0006
  sub0834 l0006
  sub0E38
  sub0816
  sub0205 l0008
End Sub

Sub sub0E38()
Dim l0040 As Integer
Dim l0042 As Integer
Dim l0044 As String
' Const mc004A = 2 ' &H2%
' Const mc006A = 8192 ' &H2000%
  If gv0F48 Then
    l0040 = fn0D6C(gv0F48)
    l0040 = gv0DA6(l0040 + mc004A)
    If l0040 = 0 Then sub01CC
    For l0042 = 1 To gv79A2
      If gv7970(l0042).M230D = l0040 Then
        l0044 = fn0C3E(l0042)
        If InStr(l0044, ".") = 0 Then l0044 = l0044 & ".BAS"
        sub0739 l0044
        sub07F0
        Exit For
      End If
    Next
  ElseIf gv0F42 Then
    l0040 = fn0D6C(gv0F46)
    sub0739 gv0F02(gv0F42): sub07F0
  Else
  End If
  For l0042 = 1 To gv0F30
    sub0739 gv0F02(l0042)
    sub07F0
  Next
  sub0739 "main.txt": sub07F0
  For l0042 = 1 To gv79A2
    If gv7970(l0042).M230D <> l0040 Then
      l0044 = fn0C3E(l0042)
      If InStr(l0044, ".") = 0 Then l0044 = l0044 & ".BAS"
      sub0739 l0044
      sub07F0
    End If
  Next
  If Len(gv0B7C) Then
    l0040 = FreeFile
On Error Resume Next
    Open gv0B7C & "autoload.mak" For Input As l0040
    If Err = 0 Then
      While Not EOF(l0040)
        Line Input #l0040, l0044
        If UCase$(Left$(l0044, 7)) = "PROJWIN" Then
          sub0739 l0044: sub07F0
        End If
      Wend
      Close l0040
    End If
On Error GoTo 0
  End If
  If gv0F44 Then
    sub0214 mc006A
    sub0739 "IconForm="""
    sub0739 gv0F02(gv0F44)
    sub0739 """"
    sub07F0
  End If
  sub0739 "Title="""
  sub0739 gv0F3A
  sub0739 """": sub07F0
  l0044 = gv0876
  l0042 = fn0308(l0044, "\")
  If l0042 Then l0044 = Mid$(l0044, l0042 + 1)
  sub0739 "ExeName="""
  sub0739 l0044
  sub0739 """": sub07F0
  If Len(gv0F3E) Then
    sub0739 "HelpFile="""
    sub0739 gv0F3E
    sub0739 """": sub07F0
  End If
End Sub

Sub sub0E44(p0010 As Integer)
' called (p1 As type, p2%)
Dim l0014 As String
Dim l0016 As Integer
Dim l0018 As Integer
Dim l001A As Integer
  Screen.MousePointer = 11
  If p0010 <> gv79B6 Then sub016B p0010
  l0014 = fn0E8D(fn0C3E(p0010), "bas")
  sub0834 l0014
  l0018 = gv0C12
  Print #gv0C12, "' "; fn0C3E(p0010)
  sub01E6 l001A, gc00A2
  sub09A9 p0010
  For l0016 = 1 To gv79EE
    If gv79BC(l0016).M230D = gv79B8 Then
      If gv79BC(l0016).M15FE <> -1 Then
        sub046E l001A, gc00C2 & fn0C8E(l0016)
        Print #gv0C12,
        sub019C l0016
      End If
    End If
  Next
  sub0816
  sub0205 l001A
End Sub

Sub sub0E52()
Dim l0036 As Integer
Dim l0038 As Integer
  sub01E6 l0038, gc009E
    sub046E l0038, gc00C2 & "main.txt"
    sub0834 fn0E8D("main", "txt")
    sub0CED
    sub0816
  For l0036 = 1 To gv79A2
    sub046E l0038, gc00C2 & fn0C3E(l0036)
   ' sub0E44 l0036, 0
  Next l0036
  sub0205 l0038
End Sub

