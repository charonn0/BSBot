#tag Module
Protected Module BuiltIns
	#tag Method, Flags = &h0
		Function About(trigger As String, caller As String, data() As String) As String
		  #pragma Unused data
		  #pragma Unused trigger
		  #pragma Unused caller
		  
		  Dim ret As String
		  For i As Integer = 0 To Scripts.Count - 1
		    Dim p As Script = scripts.Value(scripts.Key(i))
		    If Not p.Hidden Then ret = ret + scripts.Key(i) + " "
		  Next
		  
		  Return "BS Bot " + Str(Version) + EndOfLine.Windows + "Copyright (c)2011 Boredom Software" + _
		  EndOfLine.Windows + "http://www.boredomsoft.org" + EndOfLine.Windows + "-------------------------------------" + _
		  EndOfLine.Windows + "This bot is controlled by user " + Globals.gOwner + EndOfLine.Windows + "Available triggers:" + ret
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Authorize(trigger As String, caller As String, data() As String) As String
		  #pragma Unused trigger
		  If IsAuthorizedUser(caller) Then
		    If data.Ubound = 1 Then
		      If data(1) = "BSBotScriptRuntime" Or data(1) = "BSBotExecutive" Then Return "This user cannot be promoted."
		      If IsAuthorizedUser(data(1)) Then Return data(1) + " is already authorized."
		      Dim tos As TextOutputStream
		      tos = tos.Append(App.ExecutableFile.Parent.Child("authUsers.conf"))
		      tos.Write(":" + data(1))
		      tos.Close
		      If IsAuthorizedUser(data(1)) Then
		        Return data(1) + " added to authorized users list."
		      Else
		        OutPutWarning("Unable to update authorized users list!")
		        Return "Error updating authorized users list!"
		      End If
		    ElseIf data.Ubound > 1 Then
		      Return "Too many arguments"
		    Else
		      Return "No user specified."
		    End If
		  Else
		    Return "Access Denied."
		  End If
		End Function
	#tag EndMethod

	#tag DelegateDeclaration, Flags = &h0
		Delegate Function BuiltInTrigger(trigger As String, caller As String, data() As String) As String
	#tag EndDelegateDeclaration

	#tag Method, Flags = &h0
		Function Deauthorize(trigger As String, caller As String, data() As String) As String
		  #pragma Unused trigger
		  If IsAuthorizedUser(caller) Then
		    If data(1) = "BSBotScriptRuntime" Or data(1) = "BSBotExecutive" Then Return "This user cannot be demoted."
		    If data.Ubound = 1 Then
		      If IsOwner(caller) And IsOwner(data(1)) Then Return "You must have shell access to demote an owner."
		      If IsOwner(data(1)) Then Return data(1) + " outranks you."
		      Dim tos As TextOutputStream
		      Dim tis As TextInputStream
		      tis = tis.Open(App.ExecutableFile.Parent.Child("authUsers.conf"))
		      Dim s As String = tis.ReadAll
		      tis.Close
		      Dim users() As String = Split(s, ":")
		      If users.IndexOf(data(1)) > -1 Then
		        users.Remove(users.IndexOf(data(1)))
		        tos = tos.Create(App.ExecutableFile.Parent.Child("authUsers.conf"))
		        tos.Write(Join(users, ":"))
		        tos.Close
		        Return data(1) + " has been removed from the authorized users list."
		      Else
		        Return data(1) + " was not on the authorized users list."
		      End If
		    ElseIf data.Ubound > 1 Then
		      Return "Too many arguments"
		    Else
		      Return "No user specified."
		    End If
		  Else
		    Return "Access Denied."
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function explain(trigger As String, caller As String, data() As String) As String
		  #pragma Unused trigger
		  #pragma Unused caller
		  
		  If data.Ubound = 1 Then
		    Dim trigQuery As String = data(1)
		    trigQuery = ConvertEncoding(trigQuery, Encodings.UTF16)
		    If Scripts.HasKey(trigQuery) Then
		      Dim s As Script = Scripts.Value(trigQuery)
		      If s.Description <> "" Then
		        Return s.Description
		      Else
		        Return "No description"
		      End If
		    Else
		      Return "Unknown trigger"
		    End If
		  ElseIf data.Ubound > 1 Then
		    Return "Too many arguments"
		  Else
		    Return "No trigger specified."
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function OnOff(trigger As String, caller As String, data() As String) As String
		  #pragma Unused data
		  
		  If IsAuthorizedUser(caller) Then
		    If trigger = "!on" Then
		      If BotState Then
		        Return "Bot is already on."
		      Else
		        BotState = True
		        Return "Bot is now on."
		      End If
		    ElseIf trigger = "!off" Then
		      If Not BotState Then
		        Return "Bot is already off."
		      Else
		        BotState = False
		        Return "Bot is now off."
		      End If
		    End If
		  Else
		    Return "Access denied."
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Reload(trigger As String, Caller As String, data() As String) As String
		  #pragma Unused data
		  #pragma Unused trigger
		  If IsAuthorizedUser(caller) Then
		    ScriptRunner = Nil
		    Dim plcount As Integer = loadScripts
		    OutPutInfo(Str(plcount + BuiltIncount) + " scripts loaded (" + Str(BuiltIncount) + " BuiltIns, " _
		    + Str(Scripts.Count - BuiltIncount) + " external; " + Str(failedCount) + " not loaded)")
		    Return Str(plcount) + " scripts loaded"
		  Else
		    OutPutWarning("User '" + Caller + "' denied access to !reload command")
		    Return "Access Denied."
		  End If
		End Function
	#tag EndMethod


	#tag Note, Name = Adding Builtin Triggers
		Built-in triggers are script triggers which are hard-coded into the program (though they can be reassigned.)
		They are loaded before external scripts are and take precedence in trigger assignment (i.e. an external script can't
		declare a trigger which is used for a BuiltIn.)
		
		BuiltIn triggers, when triggered, will invoke the BuiltInTrigger delegate assigned to that trigger. This
		module is where BuiltInTrigger delegates should be written.
		
		BuiltInTrigger functions conform to the BuiltInTrigger delegate prototype: 
		     Function(String, String, String()) As String
		
		The first argument is the trigger itself. The second argument is the nickname of the user
		that used the trigger. The final argument is an array of the entire message. For example, if there were a trigger "!test"
		and it was called by the user "Bob" like this:
		
		<Bob>!test this is a test!
		
		The arguments would be "!test", "Bob", and an array like this:
		        data(0) = "!test"
		        data(1) = "this"
		        data(2) = "is"
		        data(3) = "a"
		        data(4) = "test!"
		
		
		After creating your BuiltInTrigger function, you must cause it to be loaded whenever scripts are loaded. See ScriptHelpers.loadBuiltinTriggers
	#tag EndNote


	#tag ViewBehavior
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
	#tag EndViewBehavior
End Module
#tag EndModule
