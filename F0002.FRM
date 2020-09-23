VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form f0002 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "Open File"
   ClientHeight    =   3165
   ClientLeft      =   570
   ClientTop       =   2415
   ClientWidth     =   4425
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   LinkTopic       =   "Form4"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3165
   ScaleWidth      =   4425
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   3240
      Top             =   2400
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton control1 
      Appearance      =   0  'Flat
      Caption         =   "Ok"
      Default         =   -1  'True
      Height          =   372
      Left            =   3480
      TabIndex        =   5
      Top             =   2700
      Width           =   852
   End
   Begin VB.CommandButton control2 
      Appearance      =   0  'Flat
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   372
      Left            =   2520
      TabIndex        =   4
      Top             =   2700
      Width           =   852
   End
   Begin VB.ComboBox control3 
      Appearance      =   0  'Flat
      Height          =   288
      ItemData        =   "F0002.frx":0000
      Left            =   2520
      List            =   "F0002.frx":0007
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   120
      Visible         =   0   'False
      Width           =   1812
   End
   Begin VB.FileListBox control4 
      Appearance      =   0  'Flat
      Height          =   2370
      Left            =   2520
      TabIndex        =   2
      Top             =   120
      Width           =   1815
   End
   Begin VB.DirListBox control5 
      Appearance      =   0  'Flat
      Height          =   2616
      Left            =   120
      TabIndex        =   1
      Top             =   480
      Width           =   2292
   End
   Begin VB.DriveListBox control6 
      Appearance      =   0  'Flat
      Height          =   288
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   2292
   End
End
Attribute VB_Name = "f0002"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Sub control2_Click()
 '   ÿ™<.Tag = ""
  Hide
End Sub

Sub control1_Click()
Dim l001E As String
Dim l0020 As String
  l001E = control4.FileName
  If l001E = "" Then sub01CC: Exit Sub
  l0020 = control4.Path
  If Right$(l0020, 1) <> "\" Then l0020 = l0020 & "\"
  f0001.control3.Tag = l0020 & l001E
  f0001.control3.Text = control4.FileName
  Hide
  f0001.Show
End Sub

Sub control3_Change()
Dim l002A As Integer
  l002A = control3.ListIndex
  If l002A < 0 Then Exit Sub
  control4.Pattern = control3.List(l002A)
End Sub

Sub control5_Change()
  control4.Path = control5.List(control5.ListIndex)
End Sub

Sub control5_Click()
  control4.Path = control5.List(control5.ListIndex)
End Sub

Sub control6_Change()
  control5.Path = control6.Drive
End Sub

Sub control4_DblClick()
  control1.Value = 1
End Sub

Sub Form_Activate()
  Caption = gc00B6 & Tag
  control4.Path = control5.List(control5.ListIndex)
  control4.Pattern = Tag
End Sub

