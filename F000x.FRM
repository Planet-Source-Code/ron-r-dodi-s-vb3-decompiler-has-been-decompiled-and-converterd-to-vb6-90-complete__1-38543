VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.MDIForm F000x 
   Appearance      =   0  'Flat
   BackColor       =   &H8000000C&
   Caption         =   "Disassembler"
   ClientHeight    =   5640
   ClientLeft      =   1635
   ClientTop       =   3435
   ClientWidth     =   7665
   Icon            =   "F000x.frx":0000
   LinkTopic       =   "MDIForm1"
   Begin MSComDlg.CommonDialog Com 
      Left            =   1440
      Top             =   3000
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Menu control3 
      Caption         =   "&File"
      Begin VB.Menu control4 
         Caption         =   "Open *.MAK"
      End
      Begin VB.Menu control5 
         Caption         =   "Open *.&EXE"
      End
      Begin VB.Menu control6 
         Caption         =   "&Text..."
      End
      Begin VB.Menu control7 
         Caption         =   "-"
      End
      Begin VB.Menu control8 
         Caption         =   "&Prototype"
      End
      Begin VB.Menu control9 
         Caption         =   "&Make Project"
      End
      Begin VB.Menu control10 
         Caption         =   "&Info..."
      End
      Begin VB.Menu control11 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu control12 
      Caption         =   "&Edit"
      Begin VB.Menu control13 
         Caption         =   "&Classes"
      End
      Begin VB.Menu control14 
         Caption         =   "&Functions"
      End
      Begin VB.Menu control15 
         Caption         =   "&Statements"
      End
      Begin VB.Menu control16 
         Caption         =   "&Variables"
      End
   End
   Begin VB.Menu control17 
      Caption         =   "&Show"
      Begin VB.Menu control18 
         Caption         =   "&Classes"
      End
      Begin VB.Menu control19 
         Caption         =   "Contro&ls"
      End
      Begin VB.Menu control20 
         Caption         =   "&Labels"
      End
      Begin VB.Menu control21 
         Caption         =   "&Variables"
      End
      Begin VB.Menu control22 
         Caption         =   "&Resource"
      End
      Begin VB.Menu control23 
         Caption         =   "&User Types"
      End
      Begin VB.Menu control24 
         Caption         =   "Su&bs"
         Visible         =   0   'False
      End
      Begin VB.Menu control25 
         Caption         =   "-"
      End
      Begin VB.Menu control26 
         Caption         =   "Fixed Font"
      End
   End
   Begin VB.Menu control27 
      Caption         =   "&Debug"
      Begin VB.Menu control28 
         Caption         =   "&Segments"
      End
      Begin VB.Menu control29 
         Caption         =   "&Tables"
      End
      Begin VB.Menu control30 
         Caption         =   "&Module Tables"
      End
      Begin VB.Menu control31 
         Caption         =   "&Tokens"
      End
      Begin VB.Menu control32 
         Caption         =   "Re&ferences"
      End
      Begin VB.Menu control33 
         Caption         =   "match tokens"
      End
      Begin VB.Menu control34 
         Caption         =   "scan source"
      End
      Begin VB.Menu control35 
         Caption         =   "scan subs"
      End
      Begin VB.Menu control36 
         Caption         =   "scan tokens"
      End
      Begin VB.Menu control37 
         Caption         =   "ExtSubs"
      End
      Begin VB.Menu control38 
         Caption         =   "Statements"
      End
      Begin VB.Menu control39 
         Caption         =   "-"
      End
      Begin VB.Menu control40 
         Caption         =   "DebugMode"
      End
      Begin VB.Menu control41 
         Caption         =   "Demo"
      End
      Begin VB.Menu control42 
         Caption         =   "Lite"
      End
      Begin VB.Menu control43 
         Caption         =   "Pro"
      End
      Begin VB.Menu control44 
         Caption         =   "Voll"
      End
   End
   Begin VB.Menu control45 
      Caption         =   "&Windows"
      WindowList      =   -1  'True
      Begin VB.Menu control46 
         Caption         =   "&Wide"
      End
   End
   Begin VB.Menu control47 
      Caption         =   "popup"
      Visible         =   0   'False
      Begin VB.Menu control48 
         Caption         =   "To &Clipboard"
      End
      Begin VB.Menu control49 
         Caption         =   "To Co&llection"
      End
      Begin VB.Menu control50 
         Caption         =   "To &Win"
      End
      Begin VB.Menu control51 
         Caption         =   "To &File"
      End
      Begin VB.Menu control52 
         Caption         =   "To &Printer"
      End
      Begin VB.Menu control53 
         Caption         =   "&Font"
      End
   End
End
Attribute VB_Name = "F000x"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' F0000.FRM
 ' Option Explicit
Dim m001A As Integer
Const mc00D4 = 8 ' &H8%

Sub control10_Click()
 ' Form2.Show 1
End Sub

Sub control46_Click()
Dim l0020 As Integer
  l0020 = Not control46.Checked
  control46.Checked = l0020
  m001A = 2 + l0020
  Arrange m001A
End Sub

Sub control19_Click()
  '  sub0142 gc0112
End Sub

Sub control13_Click()
 ' If gv02C8 Then
   ' sub0142 gc0116
 ' Else
  '  sub0142 gc010E
 ' End If
End Sub

Sub control15_Click()
 ' If gv02C8 Then
   ' sub0142 gc0116
 ' Else
  '  sub0142 gc010E
 ' End If
End Sub

Sub control14_Click()
 ' If gv02C8 Then
  '  sub0142 gc0116
 ' Else
  '  sub0142 gc010E
 ' End If
End Sub

Sub control16_Click()
'If gv02C8 Then
  ' sub0142 gc0116
'Else
   ' sub0142 gc010E
'End If

End Sub

Sub control5_Click()
 '  sub0496
  Com.Tag = "*.EXE"
  Com.ShowOpen
 ' gv0E28 = Com.Tag
  '  If Len(gv0E28) Then
   '   gv02F4 = ""
   '   gv0E2C = ""
   '   Caption = App.Title
   
 ' F0007.Show
  
 ' F0007.Show
  '    Set gv02DC = F0010
   '  control0. �x �� .Value = 1
  '  F0010.Show
  '  F0007.Show
'MS
   '   Arrange m001A
   ' End If
End Sub

Sub control11_Click()
Dim l0050
  Unload Me
  For l0050 = Forms.Count - 1 To 0 Step -1
    Unload Forms(l0050)
  Next
  Close
  End
End Sub

Sub control26_Click()
 ' F0015.Show 1
End Sub

Sub control53_Click()
  '  sub0142 gc010E
End Sub

Sub control21_Click()
control5 = Format$(gv3064 + 1)
End Sub

Sub control4_Click()
  sub24DB
End Sub

Sub control33_Click()
  MsgBox gc0112
End Sub

Sub control9_Click()
'If gv02C8 Then
 '  sub0142 gc0116
'Else
  ' Call sub0231
'End If
End Sub

Sub control22_Click()
'If gv02C8 Then
 '  sub0142 gc0116
'Else
  '  Form2.Show
  '  Form2.control4.Value = 1
'End If
End Sub

Sub control35_Click()
  MsgBox gc0112
End Sub

Sub control36_Click()
  MsgBox gc0112
End Sub

Sub control18_Click()
  '  sub0142 gc0112
End Sub

Sub control37_Click()
  '  MsgBox gc0112
End Sub

Sub control20_Click()
'If gv02C8 Then
 ' sub0142 gc0112
'Else
'   �, �   v
' Labels
 ' Form15�#  6�
 ' Courier New�.Show
'End If
End Sub

Sub control30_Click()
  MsgBox gc0112
End Sub

Sub control32_Click()
  '  sub0142 gc0112
End Sub

Sub control28_Click()
  '  sub0142 gc0112
End Sub

Sub control29_Click()
  '  sub0142 gc0112
End Sub

Sub control31_Click()
  '  sub0142 gc0112
End Sub

Sub control23_Click()
 '  p< .Show
End Sub

Sub control38_Click()
 '  Form2.Show
 '  Form2.control4.Value = True
End Sub

Sub control48_Click()
 ' Dim l0070
 ' Dim l0072 As String
  '  Clipboard.Clear
   ' For l0070 = 0 To gv02D6.ListCount - 1
   '   l0072 = l0072 & gv02D6.List(l0070) & Chr$(13) & Chr$(10)
  '  Next l0070
  '  Clipboard.SetText l0072
End Sub

Sub control49_Click()
Dim l007C
Dim l0080 As Form
Dim l0084 As ListBox
If gv02C8 Then
  '  sub0142 gc0116
Else
  If gv02D6 Is Nothing Then Exit Sub
  Set l0080 = gv02D6.Parent
 'Set l0084 = I�K �7�hD�4 J2 EI�K" nJ( rL�EI�K. nJ( rL�KI�e^[EI�+2
' H EI>. �7j9M
' � R 5InJ( m1< iO5I�gJ�J@ cJ/��Oh��aK�  XK*  �O j7EI>� �
' KI�e^[
  If l0084 Is gv02D6 Then Beep: Exit Sub
  If l0084.ListCount Then l0084.AddItem ""
  For l007C = 0 To gv02D6.ListCount - 1
    l0084.AddItem gv02D6.List(l007C)
    l0084.ItemData(l0084.NewIndex) = gv02D6.ItemData(l007C)
  Next
  ' I�K �7�hD�4 J2 EI�K" nJ( rL�EI�K. nJ( rL�KI�e^[EI�+2
' H EI>.Show
End If
End Sub

Sub control51_Click()
 ' Call sub00D7
End Sub

Sub control52_Click()
Dim l008A
  For l008A = 0 To gv02D6.ListCount - 1
    Printer.Print gv02D6.List(l008A)
  Next l008A
  Printer.EndDoc
End Sub

Sub control50_Click()
Dim l0090
Dim l0094 As Form
Dim l0098 As ListBox
' Dim l009C As New Form
' � Copy Form15.
'   �          ��          �
  If gv02D6 Is Nothing Then Exit Sub
  ' Set l0098 = l009C
' �
' Collection    0  p  �   B #����$Form15    60  7p  8�  �              i�                              .    �    P
'   �                      ��
  Set l0094 = gv02D6.Parent
  l009C.Move l0094.Left + gv02D6.Left, l0094.Top + gv02D6.Top, gv02D6.Width, gv02D6.Height
  l009C.Caption = gv02D6.List(0)
  l0098.FontName = gv02D6.FontName
  l0098.FontSize = gv02D6.FontSize
  l0098.FontBold = gv02D6.FontBold
  For l0090 = 0 To gv02D6.ListCount - 1
    l0098.AddItem gv02D6.List(l0090)
    l0098.ItemData(l0098.NewIndex) = gv02D6.ItemData(l0090)
  Next
  l009C.Show
End Sub

Sub control6_Click()
Dim l009E As Integer
 Const mc00D4 = 8 ' &H8%
On Error Resume Next
    Com.Tag = "*.*"
    Com.ShowOpen
   If Len(Com.Tag) Then
 '    Unload �,��   w
' *.TXT.Form15.Text1
'   Load �,��   w
' *.TXTp  �  �  �       B #����$Form15p  6�  7�  8�      �              ��                              C    ��    pText1  �          �                      
  '   �,��   w
' *.TXTp  �  �  �       B #����$Form15p  6�  7�  8�      �              ��                              C    ��    pText1  �          �                                                                                .Caption =   �Ɏ.Tag
    l009E = FreeFile
   Open Com.Tag For Input As l009E
  
' *.TXT.Form15.Text1
' *.MAK.Form25
' P    �                            9�    2    � sync�@ Z ���                    0    �  @�
 ' = Input$(LOF(l009E), l009E)
    Close l009E
  
' *.TXT.Form15.Text1.Show
    Arrange m001A
  End If
End Sub

Sub MDIForm_Load()


  control27.Visible = False
  '  sub26F1
 '  Set gv02D0 = Me
  m001A = 2
 '  sub0588
End Sub

Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  '  sub0496
End Sub

Sub MDIForm_Resize()
  If WindowState = 1 Then Exit Sub
  If gv097C Then
   '  If  �
' To &Clipboard��.ActiveForm.WindowState <> 0 Then Exit Sub
    Arrange m001A
  Else
    Arrange m001A
  End If
End Sub

Sub MDIForm_Unload(Cancel As Integer)
  End
End Sub

Sub sub24DB()
MousePointer = 11
  '  sub0496
 Com.Tag = "*.MAK;*.VBP"
  Com.ShowOpen
 If Len(Com.Tag) Then
    gv287C = 0
    gv02F4 = ""
    gv0E2C = ""
    gv02F0 = ""
    Caption = App.Title
  '   �
 ' MS .Show
  '   �
 ' MS .SetFocus
  gv02EC = Com.Tag
  '   
'  ��                    �4    � inity �   ��       �Y�                    /    �@ ��P �  �                  /    �@ @ P  �  Ɉ                           .control4.Value = 1
    '  sub00DF Len(gv0E28), "exefile exists"
    Set gv02DC = Form2
    gv02DC.control4.Value = 1
    If gv045E < mc00D4 Then
      gv02DC.Show
    End If
  '   
'  ��                    �4    � inity �   ��       �Y�                    /    �@ ��P �  �                  /    �@ @ P  �  Ɉ                           .Show
    ' Unload MS
    Arrange m001A
    DoEvents
  End If
MousePointer = 0
End Sub

' Sub    1 �To Co&llection�Ʉ   2 �To &Win�q�   3 �To &File��   4 �To &Printer�9�   5 �&Font��         _Resize ()
 '  If WindowState = 1 Then Exit Sub
 '  control2.Width =    1 �To Co&llection�Ʉ   2 �To &Win�q�   3 �To &File��   4 �To &Printer�9�   5 �&Font��         .ScaleWidth
' End Sub

Sub sub26F1()
  control8.Visible = False
  control23.Enabled = True
  control21.Enabled = True
  control18.Enabled = False
  control19.Enabled = False
  control20.Enabled = False
' If gv02C8 Then
' End If
' If gv02CA Then
  control22.Enabled = True
' Else
  ' control22.Enabled = False
' End If
End Sub

