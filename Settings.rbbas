#tag Module
Protected Module Settings
	#tag Property, Flags = &h0
		AuthOverride As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		BlackList As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		DebugMode As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		gAutorejoin As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		gChannel As String
	#tag EndProperty

	#tag Property, Flags = &h0
		gConfFile As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		gMaxScriptDepth As Integer = 3
	#tag EndProperty

	#tag Property, Flags = &h0
		gMOTD As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		gNick As String
	#tag EndProperty

	#tag Property, Flags = &h0
		gNickServ As String = "nickserv"
	#tag EndProperty

	#tag Property, Flags = &h0
		gOwner As String
	#tag EndProperty

	#tag Property, Flags = &h0
		gPassword As String
	#tag EndProperty

	#tag Property, Flags = &h0
		gPlugDirectory As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		gPort As Integer = 0
	#tag EndProperty

	#tag Property, Flags = &h0
		gServer As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Interactive As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		OwnOverride As FolderItem
	#tag EndProperty

	#tag Property, Flags = &h0
		Reassignments As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h0
		Verbose As Boolean
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="DebugMode"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gAutorejoin"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gChannel"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gMaxScriptDepth"
			Group="Behavior"
			InitialValue="3"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gMOTD"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gNick"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gNickServ"
			Group="Behavior"
			InitialValue="""""nickserv"""""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gOwner"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gPassword"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gPort"
			Group="Behavior"
			InitialValue="6667"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="gServer"
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Interactive"
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
			Name="Verbose"
			Group="Behavior"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
