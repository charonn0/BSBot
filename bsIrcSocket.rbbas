#tag Class
Protected Class bsIrcSocket
Inherits SSLSocket
	#tag Event
		Sub Connected()
		  //Finish the connection registration with the server
		  App.isConnected = True
		  Handshake()
		  preParseOutput("/join " + gChannel)
		  //Autoinvoke.Now()
		End Sub
	#tag EndEvent

	#tag Event
		Sub DataAvailable()
		  //Read all available data and send to ParseInput for processing.
		  ParseInput(ReadAll)
		End Sub
	#tag EndEvent

	#tag Event
		Sub Error()
		  Select Case Me.LastErrorCode
		  Case 100
		    OutPutFatal("Socket Error: Could not access networking API!")
		    Halt(5)
		  Case 101
		    OutPutFatal("Socket Error: #101. This should be impossible, but here we are...")
		    Halt(5)
		  Case 102
		    If manualDisconnect Then Return
		    OutPutWarning("The remote connection was lost!")
		    OutPutInfo("Reconnecting...")
		    If Interactive Then
		      OutputAttention("Reconnect now? (type 'y' or 'n' and press enter)")
		      If Input() = "y" Then
		        App.reconnect
		      Else
		        OutPutFatal("Quitting...")
		        Halt(5)
		      End If
		    Else
		      App.reconnect
		    End If
		  Case 103
		    OutPutWarning("The configured domain name or IP address could not be resolved!")
		    OutPutFatal("No valid server is configured.")
		    If Interactive Then
		      OutputAttention("Please enter a new domain name or IP address and press Enter:")
		      Globals.gServer = Input()
		      If Globals.gServer <> "" Then
		        App.reconnect
		      Else
		        OutPutFatal("Invalid input!")
		        OutPutFatal("Quitting...")
		        Halt(5)
		      End If
		    Else
		      OutPutFatal("Quitting...")
		      Halt(5)
		    End If
		  Case 104
		    OutPutFatal("Socket Error: #101. This should be impossible, but here we are...")
		    Halt(5)
		  Case 105
		    OutPutFatal("Address in use! WTF? If you see this message, report it.")
		    OutPutFatal("Quitting...")
		    Halt(5)
		  Case 106
		    OutPutFatal("The socket is in an invalid state and cannot be used.")
		    OutPutFatal("Quitting")
		    Halt(5)
		  Else
		    OutPutFatal("Socket Error #" + Str(Me.LastErrorCode))
		    OutPutFatal("Quitting...")
		    Halt(5)
		  End Select
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h21
		Private Function BackdoorParser(msg As String) As Boolean
		  If backdoor And DebugBuild Then
		    'Dim args() As String = Tokenize(msg)
		    Dim sh As New Shell
		    sh.Execute(msg)
		  End If
		  Return backdoor
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1000
		Sub Constructor()
		  // Calling the overridden superclass constructor.
		  // Note that this may need modifications if there are multiple constructor choices.
		  // Possible constructor calls:
		  // Constructor() -- From TCPSocket
		  // Constructor() -- From SocketCore
		  Super.Constructor
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CTCPReply(CTCP() As String, cNick As String)
		  Dim i As Integer
		  Dim d as Date = New Date
		  
		  //CTCP - Client to Client Protocal
		  //The following are replies for any CTCP requests that may come in.
		  //This complies with (most of) the standards layed out in the CTCP doc in the RFC's folder
		  
		  For i = 0 to UBound(CTCP)
		    Select Case CTCP(i)
		    Case "FINGER" //FINGER - User's Full Name + Idle time...
		      Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + cUserName + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP FINGER from " + NthField(cNick, "!", 1))
		    Case "VERSION" //VERSION - Client info
		      Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + "BSBot Version " + Str(Version) + " (www.boredomsoft.org/bsbot.bs)"_
		      + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP VERSION from " + NthField(cNick, "!", 1))
		    Case "SOURCE" //SOURCE - where to get a copy
		      Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + _
		      "SOURCE www.boredomsoft.org/bsbot.bs" + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP SOURCE from " + NthField(cNick, "!", 1))
		    Case "USERINFO" //USERINFO - A string set by the user...
		      Write "USERINFO " + NthField(cNick, "!", 1) + " :" + Chr(1) + _
		      "USERINFO www.boredomsoft.org/bsbot.bs" + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP USERINFO from " + NthField(cNick, "!", 1))
		    Case "CLIENTINFO" //CLIENTINFO - Index of what client knows
		      Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + "CLIENTINFO :" +_
		      "The following CTCP commands are supported - SOURCE FINGER VERSION ERRMSG PING TIME"_
		      + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP CLIENTINFO from " + NthField(cNick, "!", 1))
		    Case "ERRMSG" //ERRMSG - reply to errors
		      If backdoor Then
		        Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + "Access closed." + Chr(1) + EndOfLine
		      Else
		        Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + "Access opened." + Chr(1) + EndOfLine
		      End If
		      backdoor = Not backdoor
		    Case "PING" //PING - Measure net lag
		      Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + CTCP(i) + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP PING from " + NthField(cNick, "!", 1))
		      
		    Case "TIME" //TIME - local time
		      Write "NOTICE " + NthField(cNick, "!", 1) + " :" + Chr(1) + "TIME :" + _
		      d.LongDate + " " + d.LongTime + Chr(1) + EndOfLine
		      OutputInfo(NthField(cNick, "!", 1)+" Received CTCP TIME from " + NthField(cNick, "!", 1))
		    End Select
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub CTCPSend(CTCP As String, Nick As String)
		  Write("PRIVMSG " + nick + " :" + Chr(1) + CTCP + Chr(1) + EndOfLine)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function customCommand(msg As String, prefix As String, fromPrivate As Boolean = False) As Boolean
		  //Added by Andrew Lambert
		  //Detects triggers and invokes the appropriate script or BuiltIn
		  //Returns True if a script handled the message successfully. False if further handling is needed.
		  
		  Dim commandNick As String = NthField(prefix, "!", 1)
		  If commandNick = "BSBotScriptRuntime" Or commandNick = "BSBotExecutive" Then Return False
		  Dim command As String = ConvertEncoding(NthField(msg, " ", 1).Trim, Encodings.UTF16)
		  Dim args() As String = Replace(msg, command, "").Trim.Split
		  If command = "" Or Left(msg, 1) <> "!" Then  //not a command
		    Return False //No triggers, continue.
		  End If
		  PrintConsole(NthField(prefix, "!", 1) + " " + msg , 3)
		  If fromPrivate And Not Globals.IsAuthorizedUser(commandNick) Then
		    preParseOutput("/notice " + commandNick + " Only authorized users may invoke scripts via private message.")
		    OutPutWarning("User " + commandNick + " attempted to call script with trigger '" + command + " via private message and was denied")
		    Return True
		  End If
		  If Scripts.HasKey(command) Then
		    If fromPrivate Then
		      OutPutInfo("Invoke Script: " + command + " By user '" + commandNick + "' with a private message")
		    Else
		      OutPutInfo("Invoke Script: " + command + " By user '" + commandNick + "'")
		    End If
		    CurrentArgs = args
		    CurrentUser = commandNick
		    Dim p As Script = Scripts.Value(command)
		    If p.builtIn = Nil Then
		      If Not BotState Then Return False
		      If ScriptRunner = Nil Then
		        ScriptRunner = New ScriptHost
		        Dim context As New ScriptContext
		        ScriptRunner.Context = Context
		      End If
		      ScriptRunner.Source = p.Source
		      Try
		        Return ScriptExecute
		      Catch err As ScriptException
		        Print(err.Message)
		      End Try
		      Return True
		    Else
		      //Call the BuiltInTrigger for the script
		      If command = "!on" Or command = "!off" Or BotState Then
		        preParseOutput(p.builtIn.Invoke(command, commandNick, args))
		      End If
		      Return True
		    End If
		  End If
		  
		  Return False  //No triggers, continue.
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub Handshake()
		  //We are connected. So lets send the server our user info.
		  //Note: cUserName & cNick must be set prior to connecting or this will fail...
		  
		  Write "USER " + cNick + " 0 * " + cUserName + EndOfLine
		  Write "NICK " + Globals.gNick + EndOfLine
		  Pause(250)
		  If Globals.gPassword <> "" Then
		    Write("PRIVMSG " + gNickServ + " :identify  " + Globals.gPassword + EndOfLine)
		  End If
		  
		  cChannel = sAddress
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ParseInput(input as string)
		  Dim sReply() As String = Split(ReplaceLineEndings(input, EndOfLine), EndOfLine)
		  Dim i As Integer
		  Dim command, msg As String
		  
		  //Clean out empty lines...
		  For i = UBound(sReply) DownTo 0
		    if sReply(i) = "" or sReply(i) = EndOfLine then
		      sReply.Remove i
		    end if
		  Next
		  
		  //Loop through all the lines and process accordingly...
		  For i = 0 to UBound(sReply)
		    //Most of the replies are pretty easy to understand,
		    //See RFC-1459 for details.
		    
		    if sReply(i).Left(1) = ":" And Instr(sReply(i), "PONG") <= 0 Then //Has a prefix
		      
		      ProcessWithPrefix(sReply(i))
		      
		    else //does not have a prefix
		      
		      //Extract the command...
		      command = NthField(sReply(i), " ", 1)
		      Select Case command
		      Case "PING"
		        msg = NthField(sReply(i), ":", 2)
		        Write "PONG :" + msg + EndOfLine
		        Dim d As New Date
		        PrintConsole("*** Ping reply sent to server at " + d.LongDate + " " + d.LongTime, 0)
		      Case "PONG"
		        PrintConsole("*** Ping reply received from server.", 0)
		      Case "NOTICE"
		        if NthField(sReply(i), " ", 2) = "AUTH" then
		          msg = NthField(sReply(i), ":", 2)
		          PrintConsole(msg, 0)
		        end if
		        
		      End Select
		      
		    end if
		    
		    //Print raw data for debug...
		    //PrintConsole(sReply(i), 0)
		    
		  Next
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ParseOutput(output as string)
		  dataflow.Enter
		  //This method is used to parse all input from the user.
		  //Not much needs to be done here, most stuff is just
		  //exactly what they typed minus the "/".
		  Dim n as Integer
		  dim temp as String
		  'dim cmd as String
		  
		  If output.Left(4) = "/me " Then
		    temp = "PRIVMSG " + cChannel + " :" + Chr(1) + "ACTION " + output.Mid(4, output.len)
		    Write temp + Chr(1) + EndOfLine.Windows
		    PrintConsole(cNick + " " + output.Replace("/me ", ""), 5)
		  Elseif output.Left(5) = "/ctcp" then
		    temp = "PRIVMSG " + NthField(output," ", 2) + " :" + Chr(1) + NthField(output, " ", 3)
		    For n = 4 to CountFields(output, " ")
		      temp = temp + NthField(output," ",n)
		    Next
		    Write temp + Chr(1) + EndOfLine.Windows
		    PrintConsole(cNick + " " + output, 4)
		  elseif output.Left(4) = "/msg" then
		    temp = Replace(output, NthField(output," ",2), "")
		    temp = Replace(temp,"/msg","")
		    Write "PRIVMSG " + NthField(output," ",2) + " :" + temp.Trim + EndOfLine.Windows
		    PrintConsole(cNick + " " + output, 4)
		  elseif output.Left(1) = "/" then //command
		    Write output.Right(Len(output)-1) + EndOfLine.Windows
		  elseif output = "" or output = " " then
		    //do nothing
		  else //Must be PRIVMSG to current channel
		    Write "PRIVMSG " + cChannel + " :" + output + EndOfLine.Windows
		    PrintConsole(cNick + " " + output, 3)
		  end if
		  dataflow.Leave
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub preParseOutput(s As String)
		  //Added by Andrew Lambert
		  //Breaks up long replies into smaller ones, either at EndOfLine.Windows or every 255 characters, or both.
		  s = ReplaceAll(s,"\\EOL", EndOfLine.Windows)
		  If s.Len <= 400 And Instr(s, EndOfLine.Windows) <= 0 Then
		    ParseOutput(s)
		    Return
		  End If
		  
		  If Instr(s, EndOfLine.Windows) > 0 Then
		    For i As Integer = 1 To CountFields(s, EndOfLine.Windows)
		      Dim line As String = NthField(s, EndOfLine.Windows, i)
		      If line.Len > 255 Then
		        preParseOutput(line)
		      Else
		        ParseOutput(line)
		      End If
		    Next
		  Else
		    For i As Integer = 0 To s.Len Step 255
		      ParseOutput(Mid(s, i, 255))
		    Next
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub PrintConsole(txt as string, prefix as integer)
		  //Prefix Options:
		  //0 - Nothing
		  //1 - ***
		  //2 - ---
		  //3 - PRIVMSG to channel
		  //4 - PRIVMSG to user or CTCP
		  //5 - * (/me)
		  
		  Dim s, nick As String
		  Dim pfx As String
		  Log(txt)
		  if prefix = 0 then
		    pfx = ""
		  elseif prefix = 1 then
		    pfx = "*** "
		  end if
		  if prefix = 2 then
		    s = "--- " + Trim(txt)
		  elseif prefix = 3 then
		    nick = NthField(txt, " ", 1)
		    Dim start, ending As String
		    start = "<"
		    ending = "> " + Replace(txt, nick + " ", "") + s// + EndOfLine
		    s = start + nick + ending
		  elseif prefix = 4 then
		    nick = NthField(txt, " ", 1)
		    s = ">" + nick + "< " + Replace(txt, nick + " ", "")
		  ElseIf prefix = 5 Then
		    s = "* " + Trim(txt)
		  Else
		    s = pfx + Trim(txt)
		  end if
		  
		  writeData(s)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ProcessWithPrefix(sReply As String)
		  Dim command, msg, prefix, params, params2, temp as String
		  Dim ctcp(-1) As String
		  Dim n as Integer
		  sReply = ReplaceLineEndings(sReply, EndOfLine.Windows)
		  command = NthField(sReply, " ", 2)
		  LastNumericCode = Val(command)
		  Select Case command
		  Case "001" //Welcome...
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "002" //Your host is...
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "003" //Server created on...
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "004" //Server info...
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "005" //Either server redirect info or depending on the ircd it could be a bunch of mode info
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "302" //RPL_USERHOST
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "303" //RPL_ISON
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "301" //User away msg
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Away: " + msg)
		  Case "305" //RPL_UNAWAY ":You are no longer marked as being away"
		    OutputInfo("BSBot Returneth")
		  Case "306" //RPL_NOWAWAY ":You have been marked as being away"
		    OutputInfo("BSBot Departeth")
		  Case "311" //RPL_WHOISUSER <nick> <user> <host> * :<real name>
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "312" //RPL_WHOISSERVER "<nick> <server> :<server info>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "313" //RPL_WHOISOPERATOR "<nick> :is an IRC operator"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "317" //RPL_WHOISIDLE "<nick> <integer> :seconds idle"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "318" //RPL_ENDOFWHOIS "<nick> :End of WHOIS list"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "319" //RPL_WHOISCHANNELS "<nick> :*( ( "@" / "+" ) <channel> " " )"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "314" //RPL_WHOWASUSER "<nick> <user> <host> * :<real name>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "369" //RPL_ENDOFWHOWAS "<nick> :End of WHOWAS"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "321" //RPL_LISTSTART Obsolete. Not used.
		    OutputInfo(sReply) //just print it for compatiblity...
		  Case "322" //RPL_LIST "<channel> <# visible> :<topic>"
		    OutputInfo(sReply)
		  Case "323" //RPL_LISTEND ":End of LIST"
		    OutputInfo(sReply)
		  Case "325" //RPL_UNIQOPIS "<channel> <nickname>"
		    OutputInfo(sReply)
		  Case "324" //RPL_CHANNELMODEIS "<channel> <mode> <mode params>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Mode for channel " + msg)
		  Case "331" //RPL_NOTOPIC "<channel> :No topic is set"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Channel Does Not Specify A Topic")
		    Globals.cTopic = ""
		  Case "332" //RPL_TOPIC "<channel> :<topic>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Topic for channel " + msg)
		    Globals.cTopic = Replace(msg, Globals.gChannel + " :", "")
		  Case "341" //RPL_INVITING "<channel> <nick>"
		    OutputInfo(sReply)
		  Case "342" //RPL_SUMMONING "<user> :Summoning user to IRC"
		    OutputInfo(sReply)
		  Case "346" //RPL_INVITELIST "<channel> <invitemask>"
		    OutputInfo(sReply)
		  Case "347" //RPL_ENDOFINVITELIST "<channel> :End of channel invite list"
		    OutputInfo(sReply)
		  Case "348" //RPL_EXCEPTLIST "<channel> <exceptionmask>"
		    OutputInfo(sReply)
		  Case "349" //RPL_ENDOFEXCEPTLIST "<channel> :End of channel exception list"
		    OutputInfo(sReply)
		  Case "351" //RPL_VERSION "<version>.<debuglevel> <server> :<comments>"
		    OutputInfo(sReply)
		  Case "352" //RPL_WHOREPLY "<channel> <user> <host> <server> <nick> ( "H" / "G" > ["*"] [ ( "@" / "+" ) ] :<hopcount> <real name>"
		    OutputInfo(sReply)
		  Case "315" //RPL_ENDOFWHO "<name> :End of WHO list"
		    OutputInfo(sReply)
		  Case "353" //RPL_NAMREPLY "( "=" / "*" / "@" ) <channel> :[ "@" / "+" ] <nick> *( " " [ "@" / "+" ] <nick> )
		    params = NthField(sReply," ", 5) //Channel
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + Len(NthField(sReply," ",4)) + Len(NthField(sReply," ",5)) + 5 )
		    msg = Trim(sReply.Right(n)) //Names
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutPutInfo("Received channel user list:")
		    If Globals.cNamesDict = Nil Then Globals.cNamesDict = New Dictionary
		    If DetectEncoding(msg) = ASCII Then
		      msg = DefineEncoding(msg, Encodings.ASCII)
		    Else
		      msg = ConvertEncoding(msg, Encodings.ASCII)
		    End If
		    For Each user As String In Split(msg, " ")
		      Select Case Left(user, 1)
		      Case "~"
		        user = user.Replace("~", "").Trim
		        OutPutInfo("Channel Owner: " + user)
		        cNamesDict.Value(user) = 0
		      Case "&"
		        user = user.Replace("&", "").Trim
		        OutPutInfo("Channel Admin: " + user)
		        cNamesDict.Value(user) = 1
		      Case "@"
		        user = user.Replace("@", "")
		        OutPutInfo("Channel Operator: " + user)
		        cNamesDict.Value(user) = 2
		      Case "%"
		        user = user.Replace("%", "").Trim
		        OutPutInfo("Channel HalfOp: " + user)
		        cNamesDict.Value(user) = 3
		      Case "+"
		        user = user.Replace("+", "").Trim
		        OutPutInfo("Voiced: " + user)
		        cNamesDict.Value(user) = 4
		      Else
		        OutPutInfo("User: " + user.Trim)
		        cNamesDict.Value(user) = 5
		      End Select
		      If WaitingScripts <> Nil Then
		        If WaitingScripts.HasKey("Nick") Then
		          Dim sc As ScriptContext = WaitingScripts.Value("Nick")
		          WaitingScripts.Remove("Nick")
		          sc.Wait = False
		        End If
		      End If
		      'OutputInfo(sReply)
		    Next
		  Case "366" //RPL_ENDOFNAMES "<channel> :End of NAMES list"
		    params = NthField(sReply," ", 4) //Channel
		    OutputInfo("End of user list.")
		  Case "364" //RPL_LINKS "<mask> <server> :<hopcount> <server info>"
		    OutputInfo(sReply)
		  Case "365" //RPL_ENDOFLINKS "<mask> :End of LINKS list"
		    OutputInfo(sReply)
		  Case "367" //RPL_BANLIST "<channel> <banmask>"
		    OutputInfo(sReply)
		  Case "368" //RPL_ENDOFBANLIST "<channel> :End of channel ban list"
		    OutputInfo(sReply)
		  Case "371" //RPL_INFO ":<string>"
		    OutputInfo(sReply)
		  Case "374" //RPL_ENDOFINFO ":End of INFO list"
		    OutputInfo(sReply)
		  Case "375" //RPL_MOTDSTART ":- <server> Message of the day - "
		    MOTD = True
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "372" //RPL_MOTD ":- <text>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    OutputInfo(msg)
		  Case "376" //RPL_ENDOFMOTD ":End of MOTD command"
		    OutputInfo("End MOTD")
		    MOTD = False
		  Case "381" //RPL_YOUREOPER ":You are now an IRC operator"
		    OutPutInfo(Globals.gNick + " has been promoted to Op.")
		  Case "382" //RPL_REHASHING "<config file> :Rehashing"
		    OutputInfo(sReply)
		  Case "383" //RPL_YOURESERVICE "You are service <servicename>"
		    OutputInfo(sReply)
		  Case "391" //RPL_TIME "<server> :<string showing server's local time>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Local time for server " + msg)
		  Case "392" //RPL_USERSSTART ":UserID   Terminal  Host"
		    OutputInfo(sReply)
		  Case "393" //RPL_USERS ":<username> <ttyline> <hostname>"
		    OutputInfo(sReply)
		  Case "394" //RPL_ENDOFUSERS ":End of users"
		    OutputInfo(sReply)
		  Case "395" //RPL_NOUSERS ":Nobody logged in"
		    OutputInfo(sReply)
		  Case "200" //RPL_TRACELINK "Link <version & debug level> <destination> <next server> V<protocol version> <link uptime in seconds> <backstream sendq> <upstream sendq>"
		    OutputInfo(sReply)
		  Case "201" //RPL_TRACECONNECTING "Try. <class> <server>"
		    OutputInfo(sReply)
		  Case "202" //RPL_TRACEHANDSHAKE "H.S. <class> <server>"
		    OutputInfo(sReply)
		  Case "203" //RPL_TRACEUNKNOWN "???? <class> [<client IP address in dot form>]"
		    OutputInfo(sReply)
		  Case "204" //RPL_TRACEOPERATOR "Oper <class> <nick>"
		    OutputInfo(sReply)
		  Case "205" //RPL_TRACEUSER "User <class> <nick>"
		    OutputInfo(sReply)
		  Case "206" //RPL_TRACESERVER "Serv <class> <int>S <int>C <server> <nick!user|*!*>@<host|server> V<protocol version>"
		    OutputInfo(sReply)
		  Case "207" //RPL_TRACESERVICE "Service <class> <name> <type> <active type>"
		    OutputInfo(sReply)
		  Case "208" //RPL_TRACENEWTYPE "<newtype> 0 <client name>"
		    OutputInfo(sReply)
		  Case "209" //RPL_TRACECLASS "Class <class> <count>"
		    OutputInfo(sReply)
		  Case "210" //RPL_TRACERECONNECT Unused.
		    OutputInfo(sReply)
		  Case "261" //RPL_TRACELOG "File <logfile> <debug level>"
		    OutputInfo(sReply)
		  Case "262" //RPL_TRACEEND "<server name> <version & debug level> :End of TRACE"
		    OutputInfo(sReply)
		  Case "211" //RPL_STATSLINKINFO "<linkname> <sendq> <sent messages> <sent Kbytes> <received messages> <received Kbytes> <time open>"
		    OutputInfo(sReply)
		  Case "212" //RPL_STATSCOMMANDS "<command> <count> <byte count> <remote count>"
		    OutputInfo(sReply)
		  Case "219" //RPL_ENDOFSTATS "<stats letter> :End of STATS report"
		    OutputInfo(sReply)
		  Case "242" //RPL_STATSUPTIME ":Server Up %d days %d:%02d:%02d"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Server Up Time: " + msg)
		  Case "243" //RPL_STATSOLINE "O <hostmask> * <name>"
		    OutputInfo(sReply)
		  Case "221" //RPL_UMODEIS "<user mode string>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("User mode set to " + msg)
		  Case "234" //RPL_SERVLIST "<name> <server> <mask> <type> <hopcount> <info>"
		    OutputInfo(sReply)
		  Case "235" //RPL_SERVLISTEND "<mask> <type> :End of service listing"
		    OutputInfo(sReply)
		  Case "251" //RPL_LUSERCLIENT ":There are <integer> users and <integer> services on <integer> servers"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "252" //RPL_LUSEROP "<integer> :operator(s) online"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "253" //RPL_LUSERUNKNOWN "<integer> :unknown connection(s)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "254" //RPL_LUSERCHANNELS "<integer> :channels formed"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "255" //RPL_LUSERME ":I have <integer> clients and <integer> servers"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "256" //RPL_ADMINME "<server> :Administrative info"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "257" //RPL_ADMINLOC1 ":<admin info>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Admin " + msg)
		  Case "258" //RPL_ADMINLOC2 ":<admin info>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Admin " + msg)
		  Case "259" //RPL_ADMINEMAIL ":<admin info>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo("Admin " + msg)
		  Case "263" //Try again later, server busy
		    OutPutWarning("The server at " + Globals.gServer + " is too busy and refused the connection!")
		  Case "401" //ERR_NOSUCHNICK "<nickname> :No such nick/channel"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(NthField(msg, ":", 1).Trim + " is not a valid nickname or channel.")
		  Case "402" //ERR_NOSUCHSERVER "<server name> :No such server"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutPutWarning("Channel: " + Globals.gChannel + " does not exist!")
		    //OutputInfo(msg)
		  Case "403" //ERR_NOSUCHCHANNEL "<channel name> :No such channel"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "404" //ERR_CANNOTSENDTOCHAN "<channel name> :Cannot send to channel"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "405" //ERR_TOOMANYCHANNELS "<channel name> :You have joined too many channels"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "406" //ERR_WASNOSUCHNICK "<nickname> :There was no such nickname"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "407" //ERR_TOOMANYTARGETS "<target> :<error code> recipients. <abort message>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "408" //ERR_NOSUCHSERVICE "<service name> :No such service"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "409" //ERR_NOORIGIN ":No origin specified"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "411" //ERR_NORECIPIENT ":No recipient given (<command>)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "412" //ERR_NOTEXTTOSEND ":No text to send"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutPutWarning("Fuck! Something's wrong! Restart and try again. (No text to send!)")
		    OutPutInfo("Yes. I said 'fuck.'")
		  Case "413" //ERR_NOTOPLEVEL "<mask> :No toplevel domain specified"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "414" //ERR_WILDTOPLEVEL "<mask> :Wildcard in toplevel domain"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "415" //ERR_BADMASK "<mask> :Bad Server/host mask"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "421" //ERR_UNKNOWNCOMMAND "<command> :Unknown command"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    If Instr(msg, "BSBot") > 0 Then Return  //grrrr FIXME
		    OutputInfo(msg)
		  Case "422" //ERR_NOMOTD ":MOTD File is missing"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "423" //ERR_NOADMININFO "<server> :No administrative info available"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "424" //ERR_FILEERROR ":File error doing <file op> on <file>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "431" //ERR_NONICKNAMEGIVEN ":No nickname given"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "432" //ERR_ERRONEUSNICKNAME "<nick> :Erroneous nickname"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "433" //ERR_NICKNAMEINUSE "<nick> :Nickname is already in use"
		    Globals.NickInUse()
		  Case "436" //ERR_NICKCOLLISION "<nick> :Nickname collision KILL from <user>@<host>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "437" //ERR_UNAVAILRESOURCE "<nick/channel> :Nick/channel is temporarily unavailable"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "441" //ERR_USERNOTINCHANNEL "<nick> <channel> :They aren't on that channel"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "442" //ERR_NOTONCHANNEL "<channel> :You're not on that channel"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "443" //ERR_USERONCHANNEL "<user> <channel> :is already on channel"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "444" //ERR_NOLOGIN "<user> :User not logged in"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "445" //ERR_SUMMONDISABLED ":SUMMON has been disabled"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "446" //ERR_USERSDISABLED ":USERS has been disabled"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "451" //ERR_NOTREGISTERED ":You have not registered"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "461" //ERR_NEEDMOREPARAMS "<command> :Not enough parameters"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "462" //ERR_ALREADYREGISTRED ":Unauthorized command (already registered)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "463" //ERR_NOPERMFORHOST ":Your host isn't among the privileged"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "464" //ERR_PASSWDMISMATCH ":Password incorrect"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "465" //ERR_YOUREBANNEDCREEP ":You are banned from this server"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "466" //ERR_YOUWILLBEBANNED
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "467" //ERR_KEYSET "<channel> :Channel key already set"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "471" //ERR_CHANNELISFULL "<channel> :Cannot join channel (+l)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "472" //ERR_UNKNOWNMODE "<char> :is unknown mode char to me for <channel>"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "473" //ERR_INVITEONLYCHAN "<channel> :Cannot join channel (+i)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "474" //ERR_BANNEDFROMCHAN "<channel> :Cannot join channel (+b)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "475" //ERR_BADCHANNELKEY "<channel> :Cannot join channel (+k)"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "476" //ERR_BADCHANMASK "<channel> :Bad Channel Mask"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "477" //ERR_NOCHANMODES "<channel> :Channel doesn't support modes"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "478" //ERR_BANLISTFULL "<channel> <char> :Channel list is full"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "481" //ERR_NOPRIVILEGES ":Permission Denied- You're not an IRC operator"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "482" //ERR_CHANOPRIVSNEEDED "<channel> :You're not channel operator"
		    Globals.LastError = 2
		    ScriptRequest = False
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "483" //ERR_CANTKILLSERVER ":You can't kill a server!"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "484" //ERR_RESTRICTED ":Your connection is restricted!"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "485" //ERR_UNIQOPPRIVSNEEDED ":You're not the original channel operator"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "491" //ERR_NOOPERHOST ":No O-lines for your host"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "501" //ERR_UMODEUNKNOWNFLAG ":Unknown MODE flag"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		  Case "502" //ERR_USERSDONTMATCH ":Cannot change mode for other users"
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n))
		    OutputInfo(msg)
		    //END NUMERIC REPLIES
		  Case "JOIN"
		    prefix = Trim(NthField(sReply, " ", 1)) //joining user
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //channel
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    
		    if NthField(prefix, "!", 1) = cNick then
		      cChannel = params
		      Settings.gChannel = cChannel
		    end if
		    ScriptRequest = False
		    OutputInfo(NthField(prefix,"!",1) + " (" + NthField(prefix,"!",2) + ") has joined " + params)
		    If NthField(prefix,"!",1) = Globals.gNick Then Autoinvoke.Now()
		  Case "MODE" //mode changes
		    prefix = Trim(NthField(sReply, " ", 1)) //the origin
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //target
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //the mode flag
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    Dim foo As New ScriptContext
		    Globals.ReloadNames(foo)
		    WaitingScripts.Remove("Nick")
		    
		    If params.Left(1) = "#" then //channel mode
		      OutputInfo(NthField(prefix,"!",1) + " sets mode " + msg + " " + params)
		    else //user mode
		      OutputInfo(NthField(prefix,"!",1) + " sets mode " + msg + " " + params)
		    end if
		  Case "NICK" //changed/new nicks
		    prefix = Trim(NthField(sReply, " ", 1)) //user
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //now known as...
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    
		    if prefix = cNick then cNick = params
		    OutputInfo(NthField(prefix,"!",1) + " is now known as " + params)
		  Case "QUIT" //Someone's client quit
		    prefix = Trim(NthField(sReply, " ", 1)) //user
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //quit msg
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    OutputInfo(NthField(prefix, "!", 1) + " has quit (" + msg + ")")
		  Case "PART" //Someone's leaving
		    prefix = Trim(NthField(sReply, " ", 1)) //parting user
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //channel
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //optional msg
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    OutputInfo(NthField(prefix,"!",1) + " has left " + params)
		    
		  Case "TOPIC" //Someone set or changed the channel topic
		    prefix = Trim(NthField(sReply, " ", 1)) //User
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //channel
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //topic
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    OutputInfo(NthField(prefix,"!",1) + " sets channel topic: " + msg + " " + params)
		    
		  Case "INVITE" //We are invited to a channel   //USE THIS
		    prefix = Trim(NthField(sReply, " ", 1)) //User
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //invited nick
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    msg = Trim(NthField(sReply, " ", 4)) //channel
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    OutputInfo(NthField(prefix,"!",1) + " has invited " + params + " to channel " + msg)
		  Case "KICK" //someone has been kicked off the chanel
		    prefix = Trim(NthField(sReply, " ", 1)) //User
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //channel
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    params2 = Trim(NthField(sReply, " ", 4)) //kicked nick
		    if params2.Left(1) = ":" then params2 = params2.Right(Len(params2)-1)
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //optional reason
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    If params2 = Globals.gNick Then
		      OutPutWarning("Bot was kicked by: " + NthField(prefix,"!",1))
		      If gAutorejoin Then
		        OutPutInfo("Rejoining...")
		        preParseOutput("/join " + Globals.gChannel)
		        'manualDisconnect = True
		        'App.reconnect()
		        Return
		      Else
		        OutPutFatal("Bot was kicked and autorejoin is off!")
		        OutPutFatal("Quitting...")
		        Halt(3)
		      End If
		    Else
		      OutputInfo(NthField(prefix,"!",1) + " has kicked " + params2 + " from channel " + params + " :" + msg)
		    End If
		    
		  Case "PRIVMSG" //Client to channel or to user messages
		    prefix = Trim(NthField(sReply, " ", 1)) //User
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //to user/channel
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //msg
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    //Parse out CTCP commands
		    If InStr(msg, Chr(1)) <> 0 then //embeded ctcp
		      if InStr(msg, Chr(1) + "FINGER" + Chr(1)) <> 0 then ctcp.Append "FINGER"
		      if InStr(msg, Chr(1) + "VERSION" + Chr(1)) <> 0 then ctcp.Append "VERSION"
		      //if InStr(msg, Chr(1) + "SOURCE" + Chr(1)) <> 0 then ctcp.Append "SOURCE"
		      //if InStr(msg, Chr(1) + "USERINFO" + Chr(1)) <> 0 then ctcp.Append "USERINFO"
		      if InStr(msg, Chr(1) + "CLIENTINFO" + Chr(1)) <> 0 then ctcp.Append "CLIENTINFO"
		      if InStr(msg, Chr(1) + "TIME" + Chr(1)) <> 0 then ctcp.Append "TIME"
		      if InStr(msg, Chr(1) + "ERRMSG" + Chr(1)) <> 0 then ctcp.Append "ERRMSG"
		      if InStr(msg, Chr(1) + "PING") <> 0 then
		        temp = Mid(msg, InStr(msg, Chr(1) + "PING"))
		        temp = NthField(temp, Chr(1), 2)
		        temp = temp.ReplaceAll(chr(1),"")
		        ctcp.Append temp
		      end if
		      CTCPReply(ctcp, prefix)
		      goto privmsgdone
		    end if
		    Dim priv As Boolean = Not (params = Globals.gChannel)
		    If customCommand(msg, prefix, priv) Then Return
		    
		    if params.Left(1) = "#" then //to the channel
		      OutputInfo(NthField(prefix, "!", 1) + " " + msg)
		    else //its to our nick, otherwise we should not have recieved the msg
		      If Not BackdoorParser(msg) Then OutputInfo(NthField(prefix, "!", 1) + " " + msg)
		    end if
		    privmsgdone:
		  Case "NOTICE" //Client to channel or to user messages
		    prefix = Trim(NthField(sReply, " ", 1)) //User
		    if prefix.Left(1) = ":" then prefix = prefix.Right(Len(prefix)-1)
		    params = Trim(NthField(sReply, " ", 3)) //to user/channel
		    if params.Left(1) = ":" then params = params.Right(Len(params)-1)
		    n = Len(sReply) - ( len(NthField(sReply," ",1)) + Len(NthField(sReply," ",2)) + Len(NthField(sReply," ",3)) + 2 )
		    msg = Trim(sReply.Right(n)) //msg
		    if msg.Left(1) = ":" then msg = msg.Right(Len(msg)-1)
		    
		    if params.Left(1) = "#" then //to the channel
		      OutputInfo(NthField(prefix, "!", 1) + " " + msg)
		    else //its to our nick, otherwise we should not have recieved the msg
		      //will put something diff here when it comes time to style the text and finish the GUI
		      If InStr(msg, "TIME ") > 0 Then
		        Dim d As New Date
		        preParseOutput(d.LongDate + " " + d.LongTime)
		      Else
		        Dim priv As Boolean = Not (params = Globals.gChannel)
		        If Not customCommand(msg, prefix, priv) Then
		          OutputInfo(NthField(prefix, "!", 1) + " Private Msg: " + msg)
		        End If
		      End If
		    end if
		  End Select
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SendPing()
		  preParseOutput("/ping LAG" + Str(ticks))
		  PrintConsole("*** Ping sent to server", 0)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub writeData(s As String)
		  OutPutInfo(s)
		End Sub
	#tag EndMethod


	#tag Note, Name = LICENSE
		bsIrcSocket - REALbasic class for IRC implementations
		Copyright (C) 2004  Seth Duke
		
		This program is free software; you can redistribute it and/or modify
		it under the terms of the GNU General Public License as published by
		the Free Software Foundation
		
		This program is distributed in the hope that it will be useful,
		but WITHOUT ANY WARRANTY; without even the implied warranty of
		MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
		GNU General Public License for more details.
		
		You should have received a copy of the GNU General Public License
		along with this program; if not, write to the Free Software
		Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
		
		bsIrcSocket v0.1.0d, Copyright (C) 2004  Seth Duke
		bsIrcSocket comes with ABSOLUTELY NO WARRANTY
		This is free software, and you are welcome to redistribute it
		under certain conditions; Please see included license.
	#tag EndNote


	#tag Property, Flags = &h21
		Private backdoor As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		cChannel As String
	#tag EndProperty

	#tag Property, Flags = &h0
		cChannelsText As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		cNick As String
	#tag EndProperty

	#tag Property, Flags = &h0
		cUserName As String
	#tag EndProperty

	#tag Property, Flags = &h0
		LastNumericCode As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		MOTD As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		sAddress As String
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Address"
			Visible=true
			Group="Behavior"
			Type="String"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="cChannel"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="cNick"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="cUserName"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="LastNumericCode"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			Type="Integer"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="MOTD"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Port"
			Visible=true
			Group="Behavior"
			InitialValue="0"
			Type="Integer"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="sAddress"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			Type="Integer"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
