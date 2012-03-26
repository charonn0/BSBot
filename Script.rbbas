#tag Class
Protected Class Script
	#tag Method, Flags = &h0
		Sub Constructor(f As FolderItem)
		  //Constructor for external scripts
		  
		  Dim tis As TextInputStream
		  tis = tis.Open(f)
		  Dim tmp As String = Tis.ReadAll
		  tis.Close
		  tmp = ReplaceLineEndings(tmp, EndOfLine.Windows)
		  //Script source strings must be encoded as UTF-16
		  If DetectEncoding(tmp) = ASCII Then
		    tmp = ConvertEncoding(tmp, Encodings.UTF16)
		  Else
		    tmp = DefineEncoding(tmp, Encodings.UTF16)
		  End If
		  Dim s() As String = Split(tmp, EndOfLine.Windows)  //The EOL character for scripts is EndOfLine.Windows regardless of platform.
		  IsValid = True
		  For i As Integer = UBound(s) DownTo 0
		    Dim line As String = s(i)
		    If Left(line, 1) = "#" Then
		      Select Case NthField(line, "=", 1).Uppercase
		      Case "#TRIGGER"
		        Trigger = NthField(line, "=", 2).Trim
		        s.Remove(i)
		        If Reassignments.HasKey(Me.Trigger) Then Me.Trigger = Reassignments.Value(Me.Trigger)
		        If Trigger = "AutoInvoke" Then 
		          Autoinvoked = True
		        Else
		          If Left(Trigger, 1) <> "!" Then
		            OutPutInfo("Script trigger disabled by trigger reassignment.")
		          End If
		        End If
		      Case "#SCRIPT"
		        Name = NthField(line, "=", 2).Trim
		        s.Remove(i)
		      Case "#AUTHOR"
		        Author = NthField(line, "=", 2).Trim
		        s.Remove(i)
		      Case "#VERSION"
		        Version = CDbl(NthField(line, "=", 2))
		        s.Remove(i)
		      Case "#HIDDEN"
		        Hidden = True
		        s.Remove(i)
		      Case "#DESCRIPTION", "#DESCIPTION"
		        Description = NthField(line, "=", 2).Trim
		        s.Remove(i)
		      Else
		        If Instr(line, "Raise ") > 0 Then
		          IsValid = False
		          OutPutWarning("The 'Raise' keyword is illegal in scripts.")
		          Exit For
		        End If
		        If Left(line, 4) = "#If " Or Left(line, 4) = "#Els" Or Left(line, 4) = "#End" Then
		          Continue For i
		        Else
		          IsValid = False
		        End If
		      End Select
		    End If
		  Next
		  If Trigger = "" Or Name = "" Then
		    OutPutWarning("Script: " + f.AbsolutePath + " is missing required header fields!")
		    IsValid = False
		  End If
		  Source = Join(s, EndOfLine.Windows)
		  File = f
		  If IsValid Then IsValid = ScriptCheck
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(builtIn As String, named As String, built As BuiltInTrigger)
		  //Constructor for builtIn scripts
		  Author = "Boredom Software"
		  file = App.ExecutableFile
		  IsValid = True
		  Name = named
		  Source = ""
		  Trigger = ConvertEncoding(builtIn, Encodings.UTF16)
		  Version = 1.0
		  BuiltIncount = BuiltIncount + 1
		  Hidden = True
		  Me.builtIn = built
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function ScriptCheck() As Boolean
		  If ScriptRunner = Nil Then
		    ScriptRunner = New ScriptHost
		    Dim context As New ScriptContext
		    ScriptRunner.Context = Context
		  End If
		  ScriptRunner.CheckMode = True
		  ScriptRunner.Source = Source
		  Try
		    OutputInfo("   Loading " + Name)
		    ScriptRunner.Precompile
		    ScriptRunner.CheckMode = False
		    Return True
		  Catch err As ScriptException
		    OutputWarning("Script: '" + Name + "' Sanity Check Failed: " + err.Message)
		    LoadWarningLevel = 1
		    ScriptRunner.CheckMode = False
		    Return False
		  End Try
		End Function
	#tag EndMethod


	#tag Note, Name = About This Class
		The Script class is the container for all script types, builtIn and external.
		
		External Scripts are constructed with a FolderItem. builtIns are constructed differently.
		
		Both types share most of the same properties. The main difference is that External scripts 
		load and execute the RBScript in their Source property whereas builtIns invoke the function
		pointed to by their builtIn property.
		
		An external scripts builtIn property will always be Nil. A builtIn script's Source Property will
		always be "".
		
		See also, builtIns.Notes
		
	#tag EndNote


	#tag Property, Flags = &h0
		Author As String
	#tag EndProperty

	#tag Property, Flags = &h0
		AutoInvoked As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		builtIn As BuiltInTrigger
	#tag EndProperty

	#tag Property, Flags = &h0
		Description As String
	#tag EndProperty

	#tag Property, Flags = &h0
		File As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		Hidden As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		IsValid As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		Name As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Source As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Trigger As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Version As Double
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Author"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="AutoInvoked"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Description"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Hidden"
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
			Name="IsValid"
			Group="Behavior"
			Type="Boolean"
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
			Name="Source"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
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
			Name="Trigger"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Version"
			Group="Behavior"
			Type="Double"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
