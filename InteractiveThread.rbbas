#tag Class
Protected Class InteractiveThread
Inherits Thread
	#tag Event
		Sub Run()
		  While True
		    RaiseEvent KeyPress(Input)
		  Wend
		End Sub
	#tag EndEvent


	#tag Hook, Flags = &h0
		Event KeyPress(Key As String)
	#tag EndHook


End Class
#tag EndClass
