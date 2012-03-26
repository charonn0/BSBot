#tag Module
Protected Module Console
	#tag Method, Flags = &h21
		Private Function acquireStdOut() As Integer
		  Static stdOutHandle As Integer
		  If stdOutHandle <= 0 Then
		    Declare Function GetStdHandle Lib "Kernel32" (hIOStreamType As Integer) As Integer
		    Const STD_OUTPUT_HANDLE = -12
		    stdOutHandle = GetStdHandle(STD_OUTPUT_HANDLE)
		  End If
		  Return stdOutHandle
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetCurrentTitle() As String
		  //Returns the console window title.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    Declare Function GetConsoleTitleW Lib "Kernel32" (Contitle As Ptr, mbsize As Integer) As Integer
		    Dim mb As New MemoryBlock(256)
		    Call GetConsoleTitleW(mb, mb.Size)
		    Return mb.Wstring(0)
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetOriginalTitle() As String
		  //Windows Vista and later only, returns the console window's original title.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    If System.IsFunctionAvailable("GetConsoleOriginalTitleW", "Kernel32") Then
		      Soft Declare Function GetConsoleOriginalTitleW Lib "Kernel32" (Contitle As Ptr, mbsize As Integer) As Integer
		      
		      Dim mb As New MemoryBlock(0)
		      mb = New MemoryBlock(GetConsoleOriginalTitleW(mb, 0))
		      Call GetConsoleOriginalTitleW(mb, mb.Size)
		      Return mb.Wstring(0)
		    Else
		      Return ""
		    End If
		  #endif
		End Function
	#tag EndMethod

	#tag DelegateDeclaration, Flags = &h0
		Delegate Function HandlerRoutine(CTRLType As Integer) As Boolean
	#tag EndDelegateDeclaration

	#tag Method, Flags = &h0
		Function OverrideCTRL_C(NewFunction As HandlerRoutine) As Boolean
		  #pragma Unused NewFunction
		  //Call this function with a HandlerRoutine Delegate.
		  //On success, this function returns True and the specified function is invoked any time the user
		  //presses Control+c. Ordinarily Control+C immediately terminates a console application. This function overrides that behavior.
		  //To reverse this, call ResetCTRL_C. Subsequent calls to this function will override the previous HandlerRoutine.
		  
		  //Your custom HandlerRoutine must return True to prevent the application from exiting, or false to allow termination.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    Declare Function SetConsoleCtrlHandler Lib "Kernel32" (handlerRoutine As Ptr, add As Boolean) As Boolean
		    Return SetConsoleCtrlHandler(NewFunction, True)
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ResetCTRL_C() As Boolean
		  //Resets the Control+C behavior to default. See: OverrideCTRL_C.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    Declare Function SetConsoleCtrlHandler Lib "Kernel32" (handlerRoutine As Ptr, add As Boolean) As Boolean
		    Return SetConsoleCtrlHandler(Nil, False)
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SendCTRL_C()
		  //Sends a Control+C signal. By default this will immediately interrupt and terminate the application. You can override this
		  //behavior using the OverrideCTRL_C function.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    Declare Function GenerateConsoleCtrlEvent Lib "Kernel32" (ctrlEvent As Integer, processGroup As Integer) As Boolean
		    Const CTRL_C_EVENT = 0
		    Call GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SetConsoleTextColor(NewColor As UInt16) As UInt16
		  #pragma Unused NewColor //Just to shut up the warnings
		  
		  //Sets the current console text colors to the passed color constant. Multiple (non-conflicting) values can be 
		  //set by Or'ing the constants.
		  //Returns the previous color info on success, or zero on error.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    Declare Function SetConsoleTextAttribute Lib "Kernel32" (hConsole As Integer, attribs As UInt16) As Boolean
		    
		    Dim stdOutHandle As Integer = acquireStdOut()
		    If stdOutHandle <= 0 Then Return 0
		    Dim buffInfo As CONSOLE_SCREEN_BUFFER_INFO = Buffer
		    If SetConsoleTextAttribute(stdOutHandle, NewColor) Then
		      Return buffInfo.Attribute
		    Else
		      Return 0
		    End If
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SetTitle(cTitle As String) As String
		  #pragma Unused cTitle //Just to shut up the warnings
		  
		  //Sets the console window title.
		  
		  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
		    Declare Function SetConsoleTitleW Lib "Kernel32" (cTitle As WString) As Boolean
		    
		    Dim oldTitle As String = GetCurrentTitle()
		    Call SetConsoleTitleW(cTitle)
		    Return oldTitle
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ShowMessageBox(msg As String, title As String, options As Integer = 0) As Integer
		  //Shows a messagebox even if the application has no GUI (a console application)
		  //Buttons, icons, and other options are documented at http://msdn.microsoft.com/en-us/library/ms645505%28v=vs.85%29.aspx
		  
		  Declare Function MessageBoxW Lib "User32" (HWND As Integer, text As WString, caption As WString, type As Integer) As Integer
		  Return MessageBoxW(0, msg, title, options)
		End Function
	#tag EndMethod


	#tag ComputedProperty, Flags = &h1
		#tag Getter
			Get
			  //Returns a CONSOLE_SCREEN_BUFFER_INFO structure for the current process's screen buffer
			  
			  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
			    Declare Function GetConsoleScreenBufferInfo Lib "Kernel32" (hConsole As Integer, ByRef buffinfo As CONSOLE_SCREEN_BUFFER_INFO) As Boolean
			    Dim buffInfo As CONSOLE_SCREEN_BUFFER_INFO
			    Dim stdOutHandle As Integer = acquireStdOut()
			    If GetConsoleScreenBufferInfo(stdOutHandle, buffInfo) Then
			      Return buffInfo
			    End If
			  #endif
			End Get
		#tag EndGetter
		Protected Buffer As CONSOLE_SCREEN_BUFFER_INFO
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Declare Function GetConsoleCursorInfo Lib "Kernel32" (cHandle As Integer, ByRef CurseInfo As CONSOLE_CURSOR_INFO) As Boolean
			  Dim conInfo As CONSOLE_CURSOR_INFO
			  Call GetConsoleCursorInfo(acquireStdOut, conInfo)
			  
			  Return conInfo.Height
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  Declare Function GetConsoleCursorInfo Lib "Kernel32" (cHandle As Integer, ByRef CurseInfo As CONSOLE_CURSOR_INFO) As Boolean
			  Declare Function SetConsoleCursorInfo Lib "Kernel32" (cHandle As Integer, ByRef CurseInfo As CONSOLE_CURSOR_INFO) As Boolean
			  Dim conInfo As CONSOLE_CURSOR_INFO
			  Call GetConsoleCursorInfo(acquireStdOut, conInfo)
			  conInfo.Height = value
			  Call SetConsoleCursorInfo(acquireStdOut, conInfo)
			End Set
		#tag EndSetter
		CursorHeight As Integer
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Declare Function GetConsoleCursorInfo Lib "Kernel32" (cHandle As Integer, ByRef CurseInfo As CONSOLE_CURSOR_INFO) As Boolean
			  Dim conInfo As CONSOLE_CURSOR_INFO
			  Call GetConsoleCursorInfo(acquireStdOut, conInfo)
			  
			  Return conInfo.Visible
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  Declare Function GetConsoleCursorInfo Lib "Kernel32" (cHandle As Integer, ByRef CurseInfo As CONSOLE_CURSOR_INFO) As Boolean
			  Declare Function SetConsoleCursorInfo Lib "Kernel32" (cHandle As Integer, ByRef CurseInfo As CONSOLE_CURSOR_INFO) As Boolean
			  Dim conInfo As CONSOLE_CURSOR_INFO
			  Call GetConsoleCursorInfo(acquireStdOut, conInfo)
			  conInfo.Visible = value
			  Call SetConsoleCursorInfo(acquireStdOut, conInfo)
			End Set
		#tag EndSetter
		CursorVisible As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  //Returns True if the current console app is in fullscreen mode, false if not.
			  
			  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
			    If System.IsFunctionAvailable("GetConsoleDisplayMode", "Kernel32") Then
			      Soft Declare Function GetConsoleDisplayMode Lib "Kernel32" (ByRef flags As Integer) As Boolean
			      Dim flags As Integer
			      Call GetConsoleDisplayMode(flags)
			      If flags = 1 or flags = 2 Then
			        Return True
			      Else
			        Return False
			      End If
			    Else
			      Return False
			    End If
			  #endif
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  //Toggles the current console app between windowed and fullscreen mode.
			  #pragma Unused value
			  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
			    If System.IsFunctionAvailable("SetConsoleDisplayMode", "Kernel32") Then
			      Soft Declare Function SetConsoleDisplayMode Lib "Kernel32" (conHandle As Integer, flags As Integer, rect As Ptr) As Boolean
			      Declare Function GetStdHandle Lib "Kernel32" (hIOStreamType As Integer) As Integer
			      
			      Const STD_OUTPUT_HANDLE = -12
			      Dim stdOutHandle As Integer = acquireStdOut()
			      If stdOutHandle <= 0 Then Return
			      
			      If value Then
			        Call SetConsoleDisplayMode(stdOutHandle, 1, Nil)
			      Else
			        Call SetConsoleDisplayMode(stdOutHandle, 2, Nil)
			      End If
			      
			    End If
			  #endif
			End Set
		#tag EndSetter
		Fullscreen As Boolean
	#tag EndComputedProperty

	#tag ComputedProperty, Flags = &h0
		#tag Setter
			Set
			  #pragma Unused value
			  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
			    Declare Function SetConsoleCtrlHandler Lib "Kernel32" (handlerRoutine As Ptr, add As Boolean) As Boolean
			    Call SetConsoleCtrlHandler(Nil, value)
			  #endif
			End Set
		#tag EndSetter
		IgnoreCTRL_C As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h21
		Private mBlackList As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected OldSetting As UInt16
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Setter
			Set
			  #pragma Unused value
			  //Sets whether the console window is visible or not
			  #If Not TargetHasGUI And TargetWin32 Then  //Windows Console Applications only
			    Declare Function GetConsoleWindow Lib "Kernel32" () As Integer
			    Declare Function ShowWindow Lib "User32" (HWND As Integer, cmd As Integer) As Boolean
			    
			    Const SW_RESTORE = 9
			    Const SW_HIDE = 0
			    
			    Dim conHWND As Integer = GetConsoleWindow()
			    
			    If value Then
			      Call ShowWindow(conHWND, SW_RESTORE)
			    Else
			      Call ShowWindow(conHWND, SW_HIDE)
			    End If
			  #endif
			End Set
		#tag EndSetter
		Visible As Boolean
	#tag EndComputedProperty


	#tag Constant, Name = BACKGROUND_BLUE, Type = Double, Dynamic = False, Default = \"&h0010", Scope = Public
	#tag EndConstant

	#tag Constant, Name = BACKGROUND_BOLD, Type = Double, Dynamic = False, Default = \"&h0080", Scope = Public
	#tag EndConstant

	#tag Constant, Name = BACKGROUND_GREEN, Type = Double, Dynamic = False, Default = \"&h0020", Scope = Public
	#tag EndConstant

	#tag Constant, Name = BACKGROUND_RED, Type = Double, Dynamic = False, Default = \"&h0040", Scope = Public
	#tag EndConstant

	#tag Constant, Name = TEXT_BLUE, Type = Double, Dynamic = False, Default = \"&h0001", Scope = Public
	#tag EndConstant

	#tag Constant, Name = TEXT_BOLD, Type = Double, Dynamic = False, Default = \"&h0008", Scope = Public
	#tag EndConstant

	#tag Constant, Name = TEXT_GREEN, Type = Double, Dynamic = False, Default = \"&h0002", Scope = Public
	#tag EndConstant

	#tag Constant, Name = TEXT_RED, Type = Double, Dynamic = False, Default = \"&h0004", Scope = Public
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="CursorHeight"
			Group="Behavior"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="CursorVisible"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Fullscreen"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="IgnoreCTRL_C"
			Group="Behavior"
			Type="Boolean"
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
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
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
			Name="Visible"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
