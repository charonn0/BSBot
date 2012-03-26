#tag Module
Protected Module Globals
	#tag Method, Flags = &h0
		Sub Debug(msg As String)
		  If Not DebugMode Then Return
		  Log("Debug: " + msg)
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		    Window1.output.RowPicture(Window1.output.LastIndex) = blue
		  #Else
		    OutPutDebug(msg)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DetectEncoding(s As String) As Integer
		  //Very simple and naive. Detects the encoding of the passed string.
		  If MidB(s, 2, 1) = Chr(0) And MidB(s, 4, 1) = Chr(0) Then
		    Return Unicode
		  Else
		    Return ASCII
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Halt(ExitCode As Integer)
		  If ghalt = 1 Then
		    If Interactive Then
		      OutputAttention("BSBot has halted execution due to an error." + EndOfLine + "Press Enter to quit.")
		      #If Not TargetHasGUI Then Call Input()
		    Else
		      OutPutFatal("BSBot has halted execution due to an error.")
		    End If
		  ElseIf ghalt = 2 Then
		    Select Case LoadWarningLevel
		    Case 0
		      OutPutInfo("Check run completed with no warnings.")
		    Case 1
		      OutPutInfo("Check run completed with warnings.")
		    Case 2
		      OutPutInfo("Check run did not complete due to Fatal Errors.")
		    End Select
		    OutPutInfo("Press Enter to quit...")
		    #If Not TargetHasGUI Then Call Input()
		  End If
		  Quit(ExitCode)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsAuthorizedUser(User As String) As Boolean
		  If TargetHasGUI And OverrideUser > -1 Then
		    Return (OverrideUser = 2) Or (OverrideUser = 3) Or (OverrideUser = 1)
		  End If
		  
		  
		  If User.Trim = "" Or User.Trim = "BSBotScriptRuntime" Then Return False  //The Script runtime is NEVER authorized.
		  If IsOwner(User) Then Return True
		  Dim f As FolderItem
		  If AuthOverride = Nil Then
		    Return False
		  Else
		    f = AuthOverride
		  End If
		  
		  If f.Exists Then
		    Dim tis As TextInputStream
		    tis = tis.Open(f)
		    Dim tmp() As String = Split(tis.ReadAll, ":")
		    tis.Close
		    For Each userName As String In tmp
		      If user.Trim = userName.Trim Then
		        Return True
		      End If
		    Next
		    Return False
		  Else
		    Return False
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsOwner(User As String) As Boolean
		  If TargetHasGUI And OverrideUser > -1 Then
		    Return (OverrideUser = 2) Or (OverrideUser = 3)
		  End If
		  
		  If User.Trim = "" Or User.Trim = "BSBotScriptRuntime" Then Return False  //The Script runtime is NEVER authorized.
		  If User.Trim = "BSBotExecutive" Then Return True  //BSBotExecutive is ALWAYS authorized
		  Dim f As FolderItem
		  If OwnOverride = Nil Then
		    Return False
		  Else
		    f = OwnOverride
		  End If
		  If f.Exists Then
		    Dim tis As TextInputStream
		    tis = tis.Open(f)
		    Dim tmp() As String = Split(tis.ReadAll, ":")
		    tis.Close
		    For Each userName As String In tmp
		      If user.Trim = userName.Trim Then
		        Return True
		      End If
		    Next
		    Return False
		  Else
		    Return False
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub NickInUse()
		  #If Not TargetHasGUI Then
		    OutPutWarning("The nickname " + Globals.gNick + " is already in use!")
		    If Not ScriptRequest Then
		      If Interactive Then
		        OutputAttention("Enter a new nickname and press enter, or press Control+C to quit:")
		        Globals.gNick = Input()
		        Log("Interactive user selected '" + Globals.gNick + "' as bot nick")
		        App.reconnect
		      Else
		        OutPutFatal("The bot nick name is invalid or in use.")
		        manualDisconnect = True
		        App.disconnect
		      End If
		    Else
		      LastError = 19
		    End If
		  #endif
		  
		  ScriptRequest = False
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Pause(millisecs As Integer)
		  //Bad! Bad!
		  #If TargetLinux Then
		    Soft Declare Function usleep Lib "libc" (seconds As UInt32) as UInt32
		    Call uSleep(millisecs)
		  #elseIf TargetWin32 Then
		    Soft Declare Sub Sleep Lib "Kernel32" (millisecs As Integer)
		    Sleep(millisecs)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ReloadNames(ByRef Context As ScriptContext)
		  #If Not TargetHasGUI Then
		    Globals.cNamesDict = New Dictionary
		    App.bsIrc.preParseOutput("/names " + Globals.gChannel)
		    If WaitingScripts = Nil Then WaitingScripts = New Dictionary
		    WaitingScripts.Value("Nick") = Context
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function StringToHex(src as string) As Integer()
		  Dim ret() As Integer
		  
		  For i As Integer = 1 To LenB(src)
		    ret.Append(AscB(MidB(src, i, 1)))
		  next
		  return ret
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		BotState As Boolean = True
	#tag EndProperty

	#tag Property, Flags = &h0
		cNamesDict As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		cTopic As String
	#tag EndProperty

	#tag Property, Flags = &h0
		dataflow As Mutex
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  return mghalt
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If mghalt < value Then mghalt = value
			End Set
		#tag EndSetter
		ghalt As Integer
	#tag EndComputedProperty

	#tag Property, Flags = &h1
		Protected LastError As Integer
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  return mLoadWarningLevel
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If value > mLoadWarningLevel Then
			    mLoadWarningLevel = value
			  End If
			End Set
		#tag EndSetter
		LoadWarningLevel As Integer
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		manualDisconnect As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mghalt As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mLoadWarningLevel As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mOverrideUser As Integer = -1
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  #If TargetHasGUI Then
			    Return mOverrideUser
			  #Else
			    Return -1
			  #endif
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  #If TargetHasGUI Then
			    mOverrideUser = value
			  #endif
			End Set
		#tag EndSetter
		OverrideUser As Integer
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		ScriptRequest As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Version As Double = 0.41
	#tag EndProperty

	#tag Property, Flags = &h0
		WaitingScripts As Dictionary
	#tag EndProperty


	#tag Constant, Name = ASCII, Type = Double, Dynamic = False, Default = \"0", Scope = Public
	#tag EndConstant

	#tag Constant, Name = Unicode, Type = Double, Dynamic = False, Default = \"1", Scope = Public
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="BotState"
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="cTopic"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="ghalt"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="LoadWarningLevel"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="manualDisconnect"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="ScriptRequest"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Version"
			Group="Behavior"
			InitialValue="0.2"
			Type="Double"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
