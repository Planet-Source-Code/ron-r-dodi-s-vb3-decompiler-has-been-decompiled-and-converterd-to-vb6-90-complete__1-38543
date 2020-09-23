VERSION 5.00
Begin VB.Form f0003 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "DoDi's VB Tools"
   ClientHeight    =   1335
   ClientLeft      =   1125
   ClientTop       =   2280
   ClientWidth     =   4965
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
   Icon            =   "F0003.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1335
   ScaleWidth      =   4965
   Begin VB.CommandButton control2 
      Appearance      =   0  'Flat
      Caption         =   "wow !"
      Default         =   -1  'True
      Height          =   552
      Left            =   3960
      TabIndex        =   4
      Top             =   420
      Width           =   852
   End
   Begin VB.Label control3 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "D-70180 Stuttgart"
      ForeColor       =   &H80000008&
      Height          =   252
      Left            =   1920
      TabIndex        =   5
      Top             =   960
      Width           =   1932
   End
   Begin VB.Label control4 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Weissenburgstr. 2 c"
      ForeColor       =   &H80000008&
      Height          =   252
      Left            =   1920
      TabIndex        =   6
      Top             =   720
      Width           =   1932
   End
   Begin VB.Label control5 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "2.0"
      ForeColor       =   &H80000008&
      Height          =   192
      Left            =   1080
      TabIndex        =   3
      Top             =   180
      Width           =   2292
   End
   Begin VB.Label control6 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Dr. H.-P. Diettrich"
      ForeColor       =   &H80000008&
      Height          =   252
      Left            =   1920
      TabIndex        =   2
      Top             =   480
      Width           =   1932
   End
   Begin VB.Label control7 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Copyright 1995"
      ForeColor       =   &H80000008&
      Height          =   192
      Left            =   240
      TabIndex        =   1
      Top             =   480
      Width           =   1572
   End
   Begin VB.Label control8 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      Caption         =   "Version"
      ForeColor       =   &H80000008&
      Height          =   192
      Left            =   240
      TabIndex        =   0
      Top             =   180
      Width           =   852
   End
End
Attribute VB_Name = "f0003"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' F0003.FRM
Sub Form_Load()
Dim l001A As String
 ' I£*0 Ã£*2 Ã¸w72 Ã¸wL6 KIÙe^[                                  .Picture = Screen.ActiveForm.Icon
  Caption = "DoDi's " & App.Title
  control5 = gc0432 & gc0006 & l001A
End Sub
Sub control2_Click()
  Unload Me
End Sub
