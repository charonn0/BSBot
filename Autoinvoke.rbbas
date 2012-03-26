#tag Module
Protected Module Autoinvoke
	#tag Method, Flags = &h1
		Protected Sub Now()
		  If Autoinvoke.Scripts = Nil Then Return
		  
		  For i As Integer = 0 To Autoinvoke.Scripts.Ubound
		    If ScriptRunner = Nil Then
		      ScriptRunner = New ScriptHost
		      Dim context As New ScriptContext
		      ScriptRunner.Context = Context
		    End If
		    CurrentUser = "BSBotExecutive"
		    ScriptRunner.Source = Autoinvoke.Scripts(i).Source
		    Try
		      Call ScriptExecute
		    Catch err As ScriptException
		      Print(err.Message)
		    End Try
		  Next
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected Scripts() As Script
	#tag EndProperty


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
