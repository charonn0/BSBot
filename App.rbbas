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
		  If Bootstrap() And ghalt < 2 Then
		    bsIrc = New bsIrcSocket
		    connect(gServer, gPort, gNick, gPassword)
		    'InitInteractive() nope
		    While True
		      DoEvents
		    Wend
		  Else
		    Halt(1)
		  End If
		  
		  
		Exception err As EndException
		  If bsIrc <> Nil Then
		    bsIrc.Write("/quit") // BSBot " + Str(Version))
		    bsIrc.Disconnect
		    bsIrc.Close
		  End If
		  Raise err
		End Function
	#tag EndEvent

	#tag Event
		Function UnhandledException(error As RuntimeException) As Boolean
		  Select Case error.ErrorNumber
		  Case -7
		    OutPutFatal("A script has failed the sanity check but we are not in Check Mode.")
		    OutPutFatal("Memory has likely been corrupted!")
		    OutPutFatal("Quitting...")
		    Halt(7)
		  Else
		    Dim err() As String = Error.CleanStack
		    OutPutFatal("An unhandled exception has occurred!")
		    For i As Integer = 0 To err.Ubound
		      Print("   " + err(i))
		    Next
		    OutPutFatal("Quitting...")
		    Halt(2)
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
		    stdout.Write(" Unable to continue. Refer to the above warnings" + EndOfLine)
		    Call SetConsoleTextColor(Console.OldSetting)
		    ErrorLog("Bootstrap failed!")
		    ErrorLog("Refer to above warnings.")
		    Return False
		  End Select
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub connect(server As String, port As Integer, user As String, password As String)
		  if LoadWarningLevel = 2 Then Return
		  If isConnected Then
		    bsIrc.Close
		    isConnected = False
		  End If
		  gServer = server
		  gPort = port
		  gNick = user
		  gPassword = password
		  OutPutInfo("Connecting to server '" + gServer + "' on port " + Str(gPort))
		  bsIrc = New bsIrcSocket
		  bsIrc.Address = gserver
		  bsIrc.Port = gport
		  bsIrc.cNick = gNick
		  Settings.gPassword = gPassword
		  OutPutInfo("   Using nickname: " + gNick)
		  bsIrc.cUserName = bsIrc.cNick
		  
		  manualDisconnect = False
		  bsIrc.Secure = SSL
		  bsIrc.Connect
		  
		  If gpassword <> "" Then
		    OutPutInfo("   Using supplied password")
		  Else
		    OutPutInfo("   Using no password")
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub disconnect()
		  If bsIrc <> Nil And isConnected Then
		    bsIrc.Disconnect
		    bsIrc.Close
		    manualDisconnect = True
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub loadConfFiles()
		  If gConfFile = Nil Then
		    #If Not DebugBuild Then
		      gConfFile = App.ExecutableFile.Parent.Child("bot.conf")
		    #else
		      gConfFile = App.ExecutableFile.Parent.Parent.Child("bot.conf")
		    #endif
		  End If
		  If gConfFile.Exists Then
		    OutPutInfo("Loading bot.conf...")
		    Dim tis As TextInputStream
		    tis = tis.Open(gConfFile)
		    Dim lines() As String = Split(tis.ReadAll, EndOfLine.Windows)
		    For Each line As String In lines
		      Select Case NthField(line, "=", 1)
		      Case "server"
		        If gServer <> "" Then Continue
		        gServer = NthField(line, "=", 2)
		        OutPutInfo("   Server: " + gServer)
		      Case "port"
		        If gPort <> 0 Then Continue
		        gPort = Val(NthField(line, "=", 2))
		        OutPutInfo("   Port: " + Str(gPort))
		      Case "SSL"
		        SSL = True
		      Case "reassign"
		        Dim old, news As String
		        old = NthField(NthField(line, ">", 1), "=", 2)
		        news = NthField(line, ">", 2)
		        old = old.ConvertEncoding(Encodings.UTF16)
		        news = news.ConvertEncoding(Encodings.UTF16)
		        If Reassignments = Nil Then Reassignments = New Dictionary
		        Reassignments.Value(old) = news
		        OutPutInfo("   Trigger " + old + " reassigned to " + news)
		      Case "Blacklist"
		        Dim trigger As String = NthField(line, "=", 2)
		        trigger = trigger.ConvertEncoding(Encodings.UTF16)
		        If BlackList = Nil Then BlackList = New Dictionary
		        BlackList.Value(trigger) = 1
		        OutPutInfo("Trigger " + trigger + " blacklisted!")
		      Case "nick"
		        If gNick <> "" Then Continue
		        gNick = NthField(line, "=", 2)
		        OutPutInfo("   Nickname: " + gNick)
		      Case "password"
		        If gPassword <> "" Then Continue
		        gPassword = NthField(line, "=", 2)
		        OutPutInfo("   Password: " + gPassword)
		      Case "channel"
		        If gChannel <> "" Then Continue
		        gChannel = NthField(line, "=", 2)
		        OutPutInfo("   Channel: " + gChannel)
		      Case "SyncUsers"
		        If SyncUsers Then Continue
		        If NthField(line, "=", 2).Trim = "True" Then SyncUsers = True
		      Case "owner"
		        If gOwner <> "" Then Continue
		        gOwner = NthField(line, "=", 2)
		        OutPutInfo("   Owner: " + gOwner)
		      Case "MaxCallDepth"
		        gMaxScriptDepth = Val(NthField(line, "=", 2).Trim)
		        OutPutInfo("   MaxCallDepth: " + Str(gMaxScriptDepth))
		      Case "ThrottleGS"
		        If NthField(line, "=", 2).Trim = "false" Then ThrottleGS = False
		      Case "scripts"
		        If gPlugDirectory <> Nil Then Continue
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
		        If gAutorejoin Then Continue
		        If Val(NthField(line, "=", 2)) = 1 Then
		          gAutorejoin = True
		          OutPutInfo("   Autorejoin: On")
		        Else
		          gAutorejoin = False
		          OutPutInfo("   Autorejoin: Off")
		        End If
		      Case "logfile"
		        If gLogfile <> Nil Then Continue
		        Dim tmp As String = NthField(line, "=", 2)
		        gLogfile = GetFolderItem(tmp)
		        'If gLogfile = Nil Then
		        
		        If gLogfile.AbsolutePath = App.ExecutableFile.AbsolutePath Then
		          gLogfile = Nil
		          OutPutWarning("   Invalid Log File Path")
		          LoadWarningLevel = 1
		        Else
		          OutPutInfo("   Logfile: " + gLogfile.AbsolutePath)
		        End If
		      Case "Nickserv"
		        If gNickServ <> "" Then Continue
		        gNickServ = NthField(line, "=", 2)
		      Else
		        If Left(line, 2) <> "//" And line.Trim <> "" Then
		          OutPutWarning("Invalid configuration directive: " + line)
		          If LoadWarningLevel <= 1 Then LoadWarningLevel = 1
		        End If
		      End Select
		    Next
		  Else
		    If ghalt < 2 Then
		      OutPutFatal("Configuration file is missing!")
		      Print("       Configuration file expected to be at:")
		      Print("       " + App.ExecutableFile.Parent.Child("bot.conf").AbsolutePath)
		      LoadWarningLevel = 2
		    Else
		      OutPutInfo("No valid bot.conf file was found.")
		    End If
		  End If
		  
		  If OwnOverride = Nil Then
		    Dim f As FolderItem = App.ExecutableFile.Parent.Child("owners.conf")
		    If f <> Nil Then
		      If f.Exists Then
		        OwnOverride = f
		        OutPutInfo("Using " + OwnOverride.AbsolutePath + " as owners file.")
		      End If
		    End If
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
		    Select Case args(i).Trim
		    Case "--debug"
		      DebugMode = True
		      OutPutInfo("Debug mode selected.")
		    Case "--server"
		      If UBound(args) > i Then
		        gServer = args(i + 1)
		        OutPutInfo("Using " + gServer + " as IRCServer")
		        i = i + 1
		      Else
		        OutPutWarning("--server was passed but no server was specified.")
		        LoadWarningLevel = 1
		      End If
		    Case "--port"
		      If UBound(args) > i Then
		        Dim s As String = args(i + 1)
		        If IsNumeric(s) Then
		          gPort = Val(s)
		          OutPutInfo("Using " + s + " as port number")
		        Else
		          OutPutWarning("--port expects a number between 1 and 65534")
		          LoadWarningLevel = 1
		        End If
		        i = i + 1
		      Else
		        OutPutWarning("--port was passed but no port number was specified.")
		        LoadWarningLevel = 1
		      End If
		    Case "--ssl"
		      SSL = True
		    Case "--nick"
		      If UBound(args) > i Then
		        gNick = args(i + 1)
		        OutPutInfo("Using " + gNick + " as bot nick")
		        i = i + 1
		      Else
		        OutPutWarning("--nick was passed but no nickname was specified.")
		        LoadWarningLevel = 1
		      End If
		    Case "--password"
		      If UBound(args) > i Then
		        gPassword = args(i + 1)
		        Dim s As String = "*****"
		        s = Left(gPassword, 1) + s
		        OutPutInfo("Using " + s + " as password")
		        i = i + 1
		      Else
		        OutPutWarning("--password was passed but no password was specified.")
		        LoadWarningLevel = 1
		      End If
		    Case "--log"
		      If UBound(args) > i Then
		        Dim d As FolderItem = GetFolderItem(args(i + 1))
		        If d.Directory Then
		          OutPutWarning("--log was passed but pointed to a directory.")
		          LoadWarningLevel = 1
		        ElseIf d.AbsolutePath = App.ExecutableFile.AbsolutePath Then
		          OutPutWarning("--log was passed but pointed to nothing")
		          LoadWarningLevel = 1
		        Else
		          OutPutInfo("Using " + d.AbsolutePath + " as log file.")
		          gLogfile = d
		          i = i + 1
		        End If
		      Else
		        OutPutFatal("--auth was passed but no authUsers file was specified.")
		        LoadWarningLevel = 2
		        Return
		      End If
		    Case "--auth"
		      If UBound(args) > i Then
		        AuthOverride = GetFolderItem(args(i + 1))
		        If AuthOverride.Directory Then
		          OutPutWarning("--auth was passed but pointed to a directory.")
		          LoadWarningLevel = 1
		        ElseIf Not AuthOverride.Exists Then
		          OutPutWarning("--auth was passed but pointed to a nonexistent file.")
		          LoadWarningLevel = 1
		        ElseIf AuthOverride.AbsolutePath = App.ExecutableFile.AbsolutePath Then
		          OutPutWarning("--auth was passed but pointed to nothing")
		          LoadWarningLevel = 1
		        Else
		          OutPutInfo("Using " + AuthOverride.AbsolutePath + " as authUsers file.")
		          i = i + 1
		        End If
		      Else
		        OutPutFatal("--auth was passed but no authUsers file was specified.")
		        LoadWarningLevel = 2
		        Return
		      End If
		    Case "--channel"
		      If UBound(args) > i Then
		        gChannel = args(i + 1)
		        OutPutInfo("Using " + gChannel + " as Channel")
		        i = i + 1
		      Else
		        OutPutWarning("--channel was passed but no channel was specified.")
		        LoadWarningLevel = 1
		      End If
		    Case "--own"
		      If UBound(args) > i Then
		        gOwner = args(i + 1)
		        OutPutInfo("Using " + gOwner + " as owner")
		        i = i + 1
		      Else
		        OutPutWarning("--own was passed but no owner was specified.")
		        LoadWarningLevel = 1
		      End If
		    Case "--owners"
		      If UBound(args) > i Then
		        Dim d As FolderItem = GetFolderItem(args(i + 1))
		        If d.Directory Then
		          OutPutWarning("--owners was passed but pointed to a directory.")
		          LoadWarningLevel = 1
		        ElseIf Not d.Exists Then
		          OutPutWarning("--owners was passed but pointed to a nonexistent file.")
		          LoadWarningLevel = 1
		        ElseIf d.AbsolutePath = App.ExecutableFile.AbsolutePath Then
		          OutPutWarning("--owners was passed but pointed to nothing")
		          LoadWarningLevel = 1
		        Else
		          OwnOverride = d
		          OutPutInfo("Using " + OwnOverride.AbsolutePath + " as owners file.")
		          i = i + 1
		        End If
		      Else
		        OutPutFatal("--owners was passed but no owners file was specified.")
		        LoadWarningLevel = 2
		        Return
		      End If
		    Case "--config"
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
		        OutPutFatal("--config was passed but no configuration file was specified.")
		        LoadWarningLevel = 2
		        Return
		      End If
		    Case "--motd"
		      Globals.gMOTD = True
		      OutPutInfo("MOTD Supression Off")
		    Case "--halt"
		      OutPutInfo("Will Halt on fatal error")
		      ghalt = 1
		    Case "--check"
		      OutPutInfo("Script Check Only Mode")
		      ghalt = 2
		    Case "--scripts"
		      If UBound(args) > i Then
		        Dim d As FolderItem = GetFolderItem(args(i + 1))
		        If d <> Nil Then
		          If d.Directory Then
		            gPlugDirectory = d
		            OutPutInfo("Using " + gPlugDirectory.AbsolutePath + " as scripts directory.")
		            i = i + 1
		          Else
		            OutPutWarning("--scripts was passed but pointed to a file.")
		            LoadWarningLevel = 1
		          End If
		        Else
		          OutPutWarning("--scripts was passed but no scripts directory was specified.")
		          LoadWarningLevel = 1
		        End If
		      Else
		        OutPutWarning("--scripts was passed but no scripts directory was specified.")
		      End If
		    Else
		      If args(i).Trim = "" Then Continue
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
		  If ghalt = 2 Then Return
		  
		  If AuthOverride = Nil Then
		    Dim f As FolderItem 
		    #If Not DebugBuild Then
		      f = App.ExecutableFile.Parent.Child("authUsers.conf")
		    #else
		      f = App.ExecutableFile.Parent.Parent.Child("authUsers.conf")
		    #endif
		    If f = Nil Then
		      OutPutInfo("No authUsers.conf file. Some features disabled.")
		    Else
		      If f.Exists Then
		        AuthOverride = f
		      Else
		        OutPutInfo("No authUsers.conf file. Some features disabled.")
		      End If
		    End If
		  End If
		  
		  If OwnOverride = Nil Then
		    Dim f As FolderItem
		    #If Not DebugBuild Then
		      f = App.ExecutableFile.Parent.Child("owners.conf")
		    #else
		      f = App.ExecutableFile.Parent.Parent.Child("owners.conf")
		    #endif
		    If f = Nil Then
		      OutPutInfo("No owners.conf file. Some features disabled.")
		    Else
		      If f.Exists Then
		        OwnOverride = f
		      Else
		        OutPutInfo("No owners.conf file. Some features disabled.")
		      End If
		    End If
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
		bsIrc As bsIrcSocket
	#tag EndProperty

	#tag Property, Flags = &h0
		isConnected As Boolean
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
