#tag Class
Protected Class ScriptHost
Inherits RBScript
	#tag Event
		Sub CompilerError(line As Integer, errorNumber As Integer, errorMsg As String)
		  #pragma Unused errorMsg
		  #If TargetHasGUI Then Window1.TestField.AddBookmark(line)
		  If Not CheckMode Then 
		    OutputWarning("Script sanity check failure at runtime:")
		    OutPutWarning(Str(errorNumber) + ", " + ErrorCodeToString(errorNumber) + ", line: " + Str(line))
		    OutPutWarning("Script Aborted!")
		    Self.Reset()
		    Return
		  Else
		    OutputWarning("Script sanity check failure: " + Str(errorNumber) + ", " + ErrorCodeToString(errorNumber) + ", line: " + Str(line))
		    LoadWarningLevel = 1
		  End If
		End Sub
	#tag EndEvent

	#tag Event
		Function Input(prompt As String) As String
		  //Scripts really shouldn't be calling this since no user may be at the keyboard.
		  
		  If Interactive Then
		    OutputAttention("A Script Requires Manual Input: " + prompt)
		    Print(prompt)
		    Return Input()
		  Else
		    Globals.LastError = 21  //Input requested but not in Interactive mode
		    OutPutWarning("A script requested interactive user input, but the bot is not in interactive mode.")
		  End If
		End Function
	#tag EndEvent

	#tag Event
		Sub Print(msg As String)
		  OutPutInfo("Script: " + msg)
		End Sub
	#tag EndEvent

	#tag Event
		Sub RuntimeError(line As Integer, error As RuntimeException)
		  '#pragma BreakOnExceptions off
		  'Dim err As New ScriptException
		  'err.ErrorNumber = line
		  'err.Message = error.Message
		  'Raise err
		  If error.ErrorNumber = -7 Then Raise error
		  If error.ErrorNumber = 98 Then
		    OutPutWarning("Script has run out of time but is still running. Terminating!")
		  End If
		  #If TargetHasGUI Then Window1.TestField.AddBookmark(line)
		  Dim errName As String = error.Message
		  If errName = "" Then
		    OutPutWarning("Script raised an exception of type: " + Introspection.GetType(error).Name + " on line " + Str(line))
		  Else
		    OutputWarning("Script runtime error: " + error.Message + ", line: " + Str(line))
		  End If
		  Self.Reset
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h1000
		Sub Constructor()
		  TimeOutTimer = New Timer
		  TimeOutTimer.Period = 60000  //1 min
		  TimeOutTimer.Mode = Timer.ModeSingle
		  //AddHandler TimeOutTimer.Action, AddressOf Terminate
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Terminate(Sender As Timer)
		  #pragma Unused Sender
		  FakeError = True
		  Dim err As RuntimeException
		  err.ErrorNumber = 98
		  Raise err
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h0
		CheckMode As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private FakeError As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private TimeOutTimer As Timer
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="CheckMode"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="EncodingFont"
			Visible=true
			Group="Behavior"
			InheritedFrom="RBScript"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InheritedFrom="RBScript"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			Type="Integer"
			InheritedFrom="RBScript"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="RBScript"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Source"
			Visible=true
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
			InheritedFrom="RBScript"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="RBScript"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			Type="Integer"
			InheritedFrom="RBScript"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
