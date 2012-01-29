#tag Class
Protected Class App
Inherits ConsoleApplication
	#tag Event
		Function Run(args() as String) As Integer
		  Call Console.SetTitle("BSBot " + Str(Version))
		  dataflow = New Mutex("dataflow")
		  Dim old As UInt16 = SetConsoleTextColor(Console.TEXT_GREEN)
		  Stdout.Write("BS Bot ")
		  stdout.Write(Str(Version) + EndOfLine)
		  Call SetConsoleTextColor(old)
		  Print("Copyright (c)2011 Boredom Software")
		  Print("http://www.boredomsoft.org")
		  Print("------------------------------------")
		  If args.Ubound > 0 Then
		    If args(1) = "--debug" Then
		      DebugMode = True
		      OutPutInfo("Debug mode selected.")
		    End If
		  End If
		  loadConfFiles()
		  roIRC = New roIRCSocket
		  connect(gServer, gPort, gNick, gPassword)
		  While True  //Sleep, Poll, Repeat
		    If dataflow.TryEnter Then
		      #If TargetLinux Then
		        Soft Declare Function usleep Lib "libc" (seconds As UInt32) as UInt32
		        Call uSleep(1000)
		      #elseIf TargetWin32 Then
		        Soft Declare Sub Sleep Lib "Kernel32" (millisecs As Integer)
		        Sleep(1000)
		      #endif
		      If ping = 60 Then
		        'App.roirc.SendPing
		        ping = ping + 1
		      ElseIf ping >= 120 Then
		        'reconnect
		      Else
		        ping = ping + 1
		      End If
		      dataflow.Leave
		      roIRC.Poll
		    End If
		  Wend
		End Function
	#tag EndEvent

	#tag Event
		Function UnhandledException(error As RuntimeException) As Boolean
		  OutPutFatal("An unhandled exception has occurred!")
		  Dim err() As String = Error.CleanStack
		  For i As Integer = 0 To err.Ubound
		    Print("   " + err(i))
		  Next
		  
		  Print("Quitting")
		  Quit(1)
		End Function
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub connect(server As String, port As Integer, user As String, password As String = "")
		  if LoadWarningLevel = 2 Then Return
		  If isConnected Then
		    roIRC.Close
		    isConnected = False
		  End If
		  OutPutInfo("Connecting to server '" + gServer + "' on port " + Str(gPort))
		  roirc = New roIRCSocket
		  roirc.Address = server
		  roirc.Port = port
		  roirc.cNick = user
		  OutPutInfo("   Using nickname: " + user)
		  roirc.cUserName = roirc.cNick
		  If password <> "" Then
		    roIRC.sPassword = password
		    OutPutInfo("   Using supplied password")
		  Else
		    roIRC.sPassword = ""
		    OutPutInfo("   Using no password")
		  End If
		  roirc.Connect
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub disconnect()
		  If roIRC <> Nil And isConnected Then
		    roirc.Disconnect
		    roIRC.Close
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub loadConfFiles()
		  OutPutInfo("Loading bot.conf...")
		  Dim f As FolderItem = App.ExecutableFile.Parent.Child("bot.conf")
		  Dim old As UInt16
		  If f.Exists Then
		    Dim tis As TextInputStream
		    tis = tis.Open(f)
		    Dim lines() As String = Split(tis.ReadAll, EndOfLine.Windows)
		    For Each line As String In lines
		      Select Case NthField(line, "=", 1)
		      Case "server"
		        gServer = NthField(line, "=", 2)
		        OutPutInfo("   Server: " + gServer)
		      Case "port"
		        gPort = Val(NthField(line, "=", 2))
		        OutPutInfo("   Port: " + Str(gPort))
		      Case "nick"
		        gNick = NthField(line, "=", 2)
		        OutPutInfo("   Nickname: " + gNick)
		      Case "password"
		        gPassword = NthField(line, "=", 2)
		        OutPutInfo("   Password: " + gPassword)
		      Case "channel"
		        gChannel = NthField(line, "=", 2)
		        OutPutInfo("   Channel: " + gChannel)
		      Case "owner"
		        gOwner = NthField(line, "=", 2)
		        OutPutInfo("   Owner: " + gOwner)
		      Case "scripts"
		        Dim d As FolderItem = GetFolderItem(NthField(line, "=", 2))
		        If d <> Nil Then
		          If d.AbsolutePath <> App.ExecutableFile.Parent.AbsolutePath Then
		            gPlugDirectory = d
		            OutPutInfo("   Script Directory: " + gPlugDirectory.AbsolutePath)
		          Else
		            OutPutWarning("Invalid Script Directory")
		            Print(gPlugDirectory.AbsolutePath)
		            LoadWarningLevel = 1
		          End If
		        Else
		          OutPutWarning("Invalid Script Directory")
		          Print(gPlugDirectory.AbsolutePath)
		          LoadWarningLevel = 1
		        End If
		      Case "autorejoin"
		        If Val(NthField(line, "=", 2)) = 1 Then
		          gAutorejoin = True
		          OutPutInfo("   Autorejoin: On")
		        Else
		          gAutorejoin = False
		          OutPutInfo("   Autorejoin: Off")
		        End If
		      Case "logfile"
		        gLogfile = GetFolderItem(NthField(line, "=", 2))
		        If gLogfile.AbsolutePath = App.ExecutableFile.AbsolutePath Then
		          gLogfile = Nil
		          OutPutWarning("   Invalid Log File Path")
		          LoadWarningLevel = 1
		        Else
		          OutPutInfo("   Logfile: " + gLogfile.AbsolutePath)
		        End If
		      Else
		        If Left(line, 2) <> "//" And line.Trim <> "" Then
		          OutPutWarning("Invalid configuration directive: " + line)
		          If LoadWarningLevel <= 1 Then LoadWarningLevel = 1
		        End If
		      End Select
		    Next
		  Else
		    OutPutFatal("Configuration file is missing!")
		    Print("       Configuration file expected to be at:")
		    Print("       " + App.ExecutableFile.Parent.Child("bot.conf").AbsolutePath)
		    LoadWarningLevel = 2
		  End If
		  If LoadWarningLevel < 2 Then
		    Validate()
		    OutPutInfo(Str(loadScripts) + " scripts loaded")
		    Print(" ")
		  End If
		  Select Case LoadWarningLevel
		  Case 0 //No error
		    OutPutInfo("Bootstrap completed successfully")
		    Print(" ")
		  Case 1 //Recoverable errors
		    OutPutWarning("Bootstrap completed with errors. Unexpected behavior may result. Refer to the above warnings.")
		    Print(" ")
		  Case 2 //Fatal error
		    old = SetConsoleTextColor(Console.TEXT_RED Or Console.TEXT_BOLD)
		    Stdout.Write("Bootstrap failed!")
		    Call SetConsoleTextColor(Old)
		    Call SetConsoleTextColor(Console.TEXT_RED)
		    stdout.Write(" Unable to continue. Refer to the above warnings")
		    Call SetConsoleTextColor(Old)
		    Quit(1)
		  End Select
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub reconnect()
		  disconnect()
		  connect(Globals.gServer, Globals.gPort, Globals.gNick, Globals.gPassword)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Validate()
		  Dim f As FolderItem = App.ExecutableFile.Parent.Child("authUsers.conf")
		  If Not f.Exists Then
		    OutPutWarning("authUsers.conf file is missing!")
		    Print("       authUsers.conf file expected to be at:")
		    Print("       " + f.AbsolutePath)
		    If LoadWarningLevel <= 1 Then LoadWarningLevel = 1
		  End If
		  
		  If gServer = "" Then
		    OutPutFatal("Server not defined!")
		    LoadWarningLevel = 2
		  End If
		  
		  If gPort = -1 Then
		    OutPutFatal("Port not defined!")
		    LoadWarningLevel = 2
		  End If
		  If gChannel = "" Then
		    OutPutFatal("Channel not defined!")
		    LoadWarningLevel = 2
		  End If
		  If gNick = "" Then
		    OutPutFatal("Nickname not defined!")
		    LoadWarningLevel = 2
		  End If
		  
		  If gOwner = ""  Then
		    OutPutFatal("Owner not defined!")
		    LoadWarningLevel = 2
		  End If
		  
		  
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		isConnected As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		logFile As TextOutputStream
	#tag EndProperty

	#tag Property, Flags = &h0
		roirc As roIRCSocket
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="isConnected"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
