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
		    ParseArgs(args)
		  End If
		  If Bootstrap() Then
		    roIRC = New roIRCSocket
		    connect(gServer, gPort, gNick, gPassword)
		    While True  //Sleep, Poll, Repeat
		      Static timot As Integer
		      If dataflow.TryEnter Then
		        #If TargetLinux Then
		          Soft Declare Function usleep Lib "libc" (seconds As UInt32) as UInt32
		          Call uSleep(1000)
		        #elseIf TargetWin32 Then
		          Soft Declare Sub Sleep Lib "Kernel32" (millisecs As Integer)
		          Sleep(1000)
		          //FIXME: This doesn't work in Linux? o.O
		          If Not roirc.IsConnected Then
		            If timot > 10 Then
		              OutPutFatal("The server did not respond in a timely manner.")
		              OutPutFatal("Quitting...")
		              Quit(6)
		            End If
		            timot = timot + 1
		          End If
		        #endif
		        dataflow.Leave
		        roIRC.Poll
		      End If
		    Wend
		  Else
		    Quit(1)
		  End If
		End Function
	#tag EndEvent

	#tag Event
		Function UnhandledException(error As RuntimeException) As Boolean
		  Select Case error.ErrorNumber
		  Case -7
		    OutPutFatal("A script has failed the sanity check but we are not in Check Mode.")
		    OutPutFatal("Memory has likely been corrupted!")
		    OutPutFatal("Quitting...")
		    Quit(7)
		  Else
		    Dim err() As String = Error.CleanStack
		    OutPutFatal("An unhandled exception has occurred!")
		    For i As Integer = 0 To err.Ubound
		      Print("   " + err(i))
		    Next
		    OutPutFatal("Quitting...")
		    Quit(2)
		  End Select
		End Function
	#tag EndEvent


	#tag Method, Flags = &h21
		Private Function Bootstrap() As Boolean
		  loadConfFiles()
		  loadScriptFiles()
		  Select Case LoadWarningLevel
		  Case 0 //No error
		    OutPutInfo("Bootstrap completed successfully")
		    Print(" ")
		    Return True
		  Case 1 //Recoverable errors
		    OutPutWarning("Bootstrap completed with errors. Refer to the above warnings.")
		    Print(" ")
		    Return True
		  Case 2 //Fatal error
		    Console.OldSetting = SetConsoleTextColor(Console.TEXT_RED Or Console.TEXT_BOLD)
		    Stdout.Write("Bootstrap failed!")
		    Call SetConsoleTextColor(Console.OldSetting)
		    Call SetConsoleTextColor(Console.TEXT_RED)
		    stdout.Write(" Unable to continue. Refer to the above warnings")
		    Call SetConsoleTextColor(Console.OldSetting)
		    Return False
		  End Select
		End Function
	#tag EndMethod

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
		  manualDisconnect = False
		  roirc.Connect
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub disconnect()
		  If roIRC <> Nil And isConnected Then
		    roirc.Disconnect
		    roIRC.Close
		    manualDisconnect = True
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub loadConfFiles()
		  OutPutInfo("Loading bot.conf...")
		  If gConfFile = Nil Then
		    gConfFile = App.ExecutableFile.Parent.Child("bot.conf")
		  End If
		  If gConfFile.Exists Then
		    Dim tis As TextInputStream
		    tis = tis.Open(gConfFile)
		    Dim lines() As String = Split(tis.ReadAll, EndOfLine.Windows)
		    For Each line As String In lines
		      Select Case NthField(line, "=", 1)
		      Case "server"
		        gServer = NthField(line, "=", 2)
		        OutPutInfo("   Server: " + gServer)
		      Case "port"
		        gPort = Val(NthField(line, "=", 2))
		        OutPutInfo("   Port: " + Str(gPort))
		      Case "reassign"
		        Dim old, news As String
		        old = NthField(NthField(line, ">", 1), "=", 2)
		        news = NthField(line, ">", 2)
		        old = old.ConvertEncoding(Encodings.UTF16)
		        news = news.ConvertEncoding(Encodings.UTF16)
		        If Reassignments = Nil Then Reassignments = New Dictionary
		        Reassignments.Value(old) = news
		        OutPutInfo("Trigger " + old + " reassigned to " + news)
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
		      Case "MaxCallDepth"
		        gMaxScriptDepth = Val(NthField(line, "=", 2).Trim)
		        OutPutInfo("   MaxCallDepth: " + Str(gMaxScriptDepth))
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
		      Case "Nickserv"
		        gNickServ = NthField(line, "=", 2)
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
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub loadScriptFiles()
		  If LoadWarningLevel < 2 Then
		    Validate()
		    OutPutInfo(Str(loadScripts + BuiltIncount) + " scripts loaded (" + Str(BuiltIncount) + " BuiltIns, " _
		    + Str(Scripts.Count - BuiltIncount) + " external; " + Str(failedCount) + " not loaded)")
		    Print(" ")
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ParseArgs(args() As String)
		  For i As Integer = 1 To UBound(args)
		    Select Case args(i)
		    Case "--debug"
		      DebugMode = True
		      OutPutInfo("Debug mode selected.")
		    Case "--conf"
		      If UBound(args) > i Then
		        gConfFile = GetFolderItem(args(i + 1))
		        If gConfFile.AbsolutePath = App.ExecutableFile.AbsolutePath Or Not gConfFile.Exists Or gConfFile.Directory Then
		          OutPutFatal("The configuration file specified does not exist!")
		          Print("       Command line argument specified an invalid or non-existent file:")
		          Print("       " + args(i) + " " + args(i + 1))
		          LoadWarningLevel = 2
		          Return
		        Else
		          OutPutInfo("Using " + gConfFile.AbsolutePath + " as configuration file.")
		          i = i + 1
		        End If
		      Else
		        OutPutFatal("The --config argument was passed but no configuration file was specified.")
		        LoadWarningLevel = 2
		        Return
		      End If
		    Case "--motd"
		      Globals.gMOTD = True
		      OutPutInfo("MOTD Supression Off")
		    Else
		      OutPutWarning("Invalid argument: " + args(i))
		      If LoadWarningLevel < 2 Then LoadWarningLevel = 1
		    End Select
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub reconnect()
		  disconnect()
		  manualDisconnect = False
		  connect(Globals.gServer, Globals.gPort, Globals.gNick, Globals.gPassword)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Validate()
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
