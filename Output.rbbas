#tag Module
Protected Module Output
	#tag Method, Flags = &h0
		Sub ErrorLog(msg As String)
		  If ErrorStream = Nil Then ErrorStream = ErrorStream.Append(App.ExecutableFile.Parent.Child("error.log"))
		  Dim d As New Date
		  ErrorStream.WriteLine(d.SQLDateTime + ": " + msg)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Log(msg As String)
		  If gLogfile <> Nil Then
		    If logStream = Nil Then logStream = logStream.Append(gLogfile)
		    Dim d As New Date
		    logStream.WriteLine(d.SQLDateTime + ": " + msg)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OutputAttention(msg As String)
		  If Not Interactive Then Return
		  ErrorLog("ATTENTION! '" + msg + "'")
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		    Window1.output.RowPicture(Window1.output.LastIndex) = red
		  #Else
		    stdout.Write("   ")
		    Dim old As UInt16 = SetConsoleTextColor(Console.TEXT_RED Or Console.BACKGROUND_BLUE)
		    stdout.Write("ATTENTION!" + EndOfLine)
		    Call SetConsoleTextColor(old)
		    stdout.Write("   ")
		    Call SetConsoleTextColor(Console.TEXT_BLUE Or Console.BACKGROUND_RED Or TEXT_BOLD)
		    stdout.Write("ATTENTION!" + EndOfLine)
		    Call SetConsoleTextColor(old)
		    stdout.Write("   ")
		    Call SetConsoleTextColor(Console.TEXT_RED Or Console.BACKGROUND_BLUE)
		    stdout.Write("ATTENTION!" + EndOfLine)
		    Call SetConsoleTextColor(old)
		    stdout.Write("   ")
		    Stdout.Write(msg + EndOfLine)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OutPutConsole(msg As String)
		  #If Not TargetHasGUI Then
		    If App.bsIrc <> Nil Then
		      If App.bsIrc.MOTD And Not Globals.gMOTD Then
		        Return
		      End If
		    End If
		  #endif
		  Log("Console: " + msg)
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		    Window1.output.RowPicture(Window1.output.LastIndex) = green
		  #Else
		    Dim old As UInt16 = SetConsoleTextColor(Console.TEXT_GREEN Or Console.TEXT_BLUE)
		    Stdout.Write("   Console: ")
		    Call SetConsoleTextColor(old)
		    Stdout.Write(msg + EndOfLine)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OutPutDebug(msg As String)
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		  #Else
		    Stdout.Write("   Debug: ")
		    Stdout.Write(msg + EndOfLine)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OutPutFatal(msg As String)
		  ErrorLog("FATAL: " + msg)
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		    Window1.output.RowPicture(Window1.output.LastIndex) = red
		  #Else
		    Dim old As UInt16 = SetConsoleTextColor(Console.TEXT_RED Or Console.TEXT_BOLD)
		    Stdout.Write("   FATAL: ")
		    Call SetConsoleTextColor(old)
		    Stdout.Write(msg + EndOfLine)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OutPutInfo(msg As String)
		  #If Not TargetHasGUI Then
		    If App.bsIrc <> Nil Then
		      If App.bsIrc.MOTD And Not Globals.gMOTD Then
		        Return
		      End If
		    End If
		  #endif
		  ErrorLog("Info: " + msg)
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		    Window1.output.RowPicture(Window1.output.LastIndex) = green
		  #Else
		    Dim old As UInt16 = SetConsoleTextColor(Console.TEXT_GREEN)
		    Stdout.Write("   Info: ")
		    Call SetConsoleTextColor(old)
		    Stdout.Write(msg + EndOfLine)
		  #endif
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub OutPutWarning(msg As String)
		  ErrorLog("WARN: " + msg)
		  #If TargetHasGUI Then
		    Window1.output.AddRow(msg)
		    Window1.output.RowPicture(Window1.output.LastIndex) = red
		  #Else
		    Dim old As UInt16 = SetConsoleTextColor(Console.TEXT_RED)
		    Stdout.Write("   WARN: ")
		    Call SetConsoleTextColor(old)
		    Stdout.Write(msg + EndOfLine)
		  #endif
		End Sub
	#tag EndMethod


	#tag Property, Flags = &h21
		Private ErrorStream As TextOutputStream
	#tag EndProperty

	#tag Property, Flags = &h0
		gLogfile As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h21
		Private logStream As TextOutputStream
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
