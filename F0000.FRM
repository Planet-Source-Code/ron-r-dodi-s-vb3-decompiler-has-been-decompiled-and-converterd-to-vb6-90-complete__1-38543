VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form f0000 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "DoDi's VB Discompiler"
   ClientHeight    =   3630
   ClientLeft      =   1035
   ClientTop       =   1890
   ClientWidth     =   9285
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
   Icon            =   "F0000.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3630
   ScaleWidth      =   9285
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   2280
      Top             =   3240
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton control7 
      Appearance      =   0  'Flat
      Caption         =   "scan"
      Height          =   255
      Left            =   3240
      TabIndex        =   11
      Top             =   0
      Width           =   615
   End
   Begin VB.ComboBox control0 
      Appearance      =   0  'Flat
      Height          =   3105
      Left            =   3240
      Style           =   1  'Simple Combo
      TabIndex        =   3
      Top             =   540
      Width           =   1812
   End
   Begin VB.CommandButton control2 
      Appearance      =   0  'Flat
      Caption         =   "+"
      Height          =   252
      Left            =   8640
      TabIndex        =   7
      Top             =   240
      Visible         =   0   'False
      Width           =   252
   End
   Begin VB.CommandButton control3 
      Appearance      =   0  'Flat
      Caption         =   "+"
      Height          =   252
      Left            =   6720
      TabIndex        =   6
      Top             =   300
      Visible         =   0   'False
      Width           =   252
   End
   Begin VB.CommandButton control4 
      Appearance      =   0  'Flat
      Caption         =   "+"
      Height          =   252
      Left            =   4800
      TabIndex        =   10
      Top             =   300
      Visible         =   0   'False
      Width           =   252
   End
   Begin VB.ListBox control5 
      Appearance      =   0  'Flat
      Height          =   3345
      Left            =   120
      TabIndex        =   9
      Top             =   60
      Width           =   3015
   End
   Begin VB.CommandButton control6 
      Appearance      =   0  'Flat
      Caption         =   "done"
      Height          =   252
      Left            =   4080
      TabIndex        =   8
      Top             =   0
      Visible         =   0   'False
      Width           =   735
   End
   Begin VB.ComboBox control8 
      Appearance      =   0  'Flat
      Height          =   3105
      Left            =   7080
      Style           =   1  'Simple Combo
      TabIndex        =   5
      Top             =   540
      Width           =   2052
   End
   Begin VB.ComboBox control9 
      Appearance      =   0  'Flat
      Height          =   3105
      Left            =   5160
      Style           =   1  'Simple Combo
      TabIndex        =   4
      Top             =   540
      Width           =   1812
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000009&
      Caption         =   "How about a vote?"
      Height          =   255
      Left            =   5160
      TabIndex        =   12
      Top             =   0
      Width           =   3855
   End
   Begin VB.Label control10 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Controls"
      ForeColor       =   &H80000008&
      Height          =   192
      Left            =   7080
      TabIndex        =   2
      Top             =   300
      Width           =   1812
   End
   Begin VB.Label control11 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Subroutines"
      ForeColor       =   &H80000008&
      Height          =   192
      Left            =   5160
      TabIndex        =   1
      Top             =   300
      Width           =   1524
   End
   Begin VB.Label control12 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Modules"
      ForeColor       =   &H80000008&
      Height          =   192
      Left            =   3240
      TabIndex        =   0
      Top             =   300
      Width           =   1344
   End
   Begin VB.Menu control13 
      Caption         =   "&File"
      Begin VB.Menu control14 
         Caption         =   "&Open..."
      End
      Begin VB.Menu control15 
         Caption         =   "Im&prove"
      End
      Begin VB.Menu control16 
         Caption         =   "&Combine Forms"
      End
      Begin VB.Menu control17 
         Caption         =   "Start &VB"
      End
      Begin VB.Menu control18 
         Caption         =   "-"
      End
      Begin VB.Menu control19 
         Caption         =   "&Info..."
      End
      Begin VB.Menu control20 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuWindow 
      Caption         =   "&Window"
      WindowList      =   -1  'True
      Begin VB.Menu mnuWindowNewWindow 
         Caption         =   "&New Window"
      End
   End
End
Attribute VB_Name = "f0000"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Dim m001A As Integer
Const mc0052 = 32 ' &H20%
Const mc005C = 16 ' &H10%

Private Sub mnuWindowNewWindow_Click()
 F000x.Show
End Sub



Sub control2_Click()


 control9.AddItem fn0C8E(gv7A2A)
 DoEvents
End Sub

'Sub ÿ&Info...ÿé9_Change ()
 ' control9.Clear
 ' sub0F15 ÿ&Info...ÿé9
'End Sub

Private Sub control14_Click()
Const mc0064 = 8 ' &H8%
  control16.Enabled = False
  gv791C = 0
  
  sub0D89 mc005C, Me
  sub0D89 mc0052, Me
  f0002.Show
  If gv791C And mc005C Then
    control15.Enabled = True
    control16.Enabled = True
  End If
End Sub

Private Sub control15_Click()
Const mc0064 = 8 ' &H8%
  gv791C = gv791C And Not (mc005C Or mc0064)
  sub0D89 mc005C, Me
End Sub

Private Sub control16_Click()
Const mc0064 = 8 ' &H8%
  sub0D89 mc0052, Me
End Sub

Private Sub control19_Click()
Const mc0064 = 8 ' &H8%
 f0003.Show
End Sub

Private Sub control20_Click()
Const mc0064 = 8 ' &H8%
  End
End Sub

Private Sub control4_Click()
control9.Text = fn0C3E(gv79B6)
  DoEvents
End Sub

Private Sub control6_Click()
  m001A = False
End Sub

Private Sub control7_Click()
  m001A = 1
  Caption = gv0876
 control0.Clear
  control9.Clear
  control8.Clear
  DoEvents
End Sub

Private Sub control8_Change()
  sub0F15 control8
End Sub

Private Sub control9_Change()
  sub0F15 control9
End Sub

Private Sub Form_Load()
Dim l0048 As Integer
Const mc0052 = 32 ' &H20%
Const mc005C = 16 ' &H10%
Const mc0064 = 8 ' &H8%
  Show
  sub01E6 l0048, gc008E
  m001A = 1
  sub0D5E l0048
  sub0205 l0048
End Sub

Private Sub Form_Unload(Cancel As Integer)
Const mc0064 = 8 ' &H8%
  End
End Sub

Private Sub sub0F15(p0044 As ComboBox)
Static m0046 As Integer
  If m0046 Then Exit Sub
  m0046 = True
  If m001A Then
    p0044.AddItem p0044.Text
    p0044.ListIndex = p0044.NewIndex
    DoEvents
  Else
    Beep
  End If
  m0046 = False
End Sub

