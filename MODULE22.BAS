Attribute VB_Name = "Module7"
' Module22
'' Option Explicit

Sub sub0E80()
Dim l001C As Integer
Dim l001E As String
  l001E = gv7A84
  l001C = fn0308(l001E, "\")
  If l001C Then
    gv7A88 = Left(l001E, l001C)
    l001E = Mid$(l001E, l001C + 1)
  End If
  l001C = InStr(l001E, ".")
  If l001C Then l001E = Left$(l001E, l001C - 1)
  gv7A90 = l001E
End Sub

Function fn0E8D(p0024 As String, ByVal p0026 As String) As String
Dim l0028 As Integer
 If gv7A8C = "" Then
  If Len(p0024$) Then
    l0028 = InStr(p0024, ".")
    If l0028 Then
      fn0E8D = gv7A8C & Left$(p0024, l0028) & p0026
    Else
      fn0E8D = gv7A8C & p0024 & "." & p0026
    End If
  Else
    fn0E8D = gv7A8C & gv7A90 & "." & p0026
  End If
  End If
  
End Function

Sub sub0E99()
Dim l002E As Integer
Dim l0030 As Integer
Dim l0032 As String
Dim l0034 As String
  gv7AAA = 0
  l0030 = FreeFile
  gv087A = gv7A88
  Open gv7A84 For Input As #l0030
  While Not EOF(l0030)
    Line Input #l0030, l0032
    l002E = InStr(l0032, "=")
    If l002E Then
      If Mid$(l0032, l002E + 1, 1) = """" Then
        l0034 = Mid$(l0032, l002E + 2)
        l0034 = Left$(l0034, Len(l0034) - 1)
        Select Case Left$(l0032, l002E - 1)
        Case "ExeName"
          gv087E = l0034
        Case "Path"
          gv087A = l0034
        End Select
      End If
    Else
      If Right$(l0032, 1) <> "X" And Right$(l0032, 3) <> "DLL" Then
        gv7AAA = gv7AAA + 1
        gv7A94(gv7AAA) = l0032
      End If
    End If
  Wend
  Close l0030
  l002E = InStr(gv087E, ".")
  If l002E Then gv087E = Left$(gv087E, l002E - 1)
  If Right$(gv087A, 1) <> "\" Then gv087A = gv087A & "\"
  gv0876 = gv087A & gv087E & ".EXE"
End Sub
