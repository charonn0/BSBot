#tag Module
Protected Module MersenneTwister
	#tag Method, Flags = &h1
		Protected Sub InitByArray(ParamArray init_key as UInt32)
		  dim i, j, k as Integer
		  
		  dim key_length as Integer = UBound( init_key ) + 1
		  
		  init_genrand( 19650218 )
		  
		  i = 1
		  j = 0
		  if N > key_length then k = N else k = key_length
		  
		  while k > 0
		    mt( i ) = Bitwise.BitXor( mt( i ), (Bitwise.BitXor( mt( i - 1 ), Bitwise.ShiftRight( mt( i - 1 ), 30 ) ) * 1664525) ) + init_key( j ) + j
		    
		    ' Again, I don't think this is needed for RB
		    'mt( i ) = Bitwise.BitAnd( mt( i ), &hffffffff )
		    
		    i = i + 1
		    j = j + 1
		    
		    if i >= N then
		      mt( 0 ) = mt( N - 1)
		      i = 1
		    end if
		    
		    if j >= key_length then j = 0
		    
		    k = k - 1
		  wend
		  
		  for k = N - 1 downto 0
		    mt( i ) = Bitwise.BitXor( mt( i ), (Bitwise.BitXor( mt( i - 1 ), Bitwise.ShiftRight( mt( i - 1 ), 30 ) ) * 1566083941) ) - i
		    
		    ' Again, I don't think this is needed for RB
		    'mt( i ) = Bitwise.BitAnd( mt( i ), &hffffffff )
		    
		    i = i + 1
		    
		    if i >= N then
		      mt( 0 ) = mt( N - 1 )
		      i = 1
		    end if
		  next k
		  
		  mt( 0 ) = &h80000000 // MSB is 1; assuring non-zero initial array
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub init_genrand(s as UInt32)
		  mt( 0 ) = Bitwise.BitAnd( s, &hffffffff )
		  
		  for mti = 1 to N - 1
		    // See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier
		    // In the previous versions, MSBs of the seed affect
		    // only MSBs of the array mt[].
		    mt( mti ) = (1812433253 * (Bitwise.BitXor( mt( mti - 1 ), (Bitwise.ShiftRight( mt( mti - 1 ), 30 )))) + mti)
		    
		    //  for >32 bit machines
		    ' I think this is not needed because REALbasic doesn't
		    ' have shifting data type sizes like C does.
		    'mt( mti ) = Bitwise.BitAnd( mt( mti ), &hffffffff )
		  next mti
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Rand53Bit() As Double
		  // generates a random number on [0,1) with 53-bit resolution
		  
		  Dim a, b as UInt32
		  a = Bitwise.ShiftRight( RandInt32, 5 )
		  b = Bitwise.ShiftRight( RandInt32, 6 )
		  
		  return (a * 67108864.0 + b) * (1.0 / 9007199254740992.0)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function RandInt31() As Int32
		  // generates a random number on [0,0x7fffffff]-interval
		  
		  return Bitwise.ShiftRight( RandInt32, 1 )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function RandInt32() As UInt32
		  // generates a random number on [0,0xffffffff]-interval
		  
		  Dim y as UInt32
		  static mag01( 2 ) as UInt32 = Array( UInt32( &h0 ), UInt32( MATRIX_A ) )
		  
		  if mti >= N then
		    dim kk as Integer
		    
		    // If init_genrand hasn't been called yet
		    if mti = N + 1 then init_genrand( 5489 )  // Use the default seed
		    
		    for kk = 0 to N - M - 1
		      y = Bitwise.BitOr( Bitwise.BitAnd( mt( kk ), UPPER_MASK ), Bitwise.BitAnd( mt( kk + 1 ), LOWER_MASK ) )
		      mt( kk ) = Bitwise.BitXor( Bitwise.BitXor( mt( kk + M ), Bitwise.ShiftRight( y, 1 ) ), mag01( Bitwise.BitAnd( y, &h1 ) ) )
		    next kk
		    
		    for kk = kk to N - 1 - 1
		      y = Bitwise.BitOr( Bitwise.BitAnd( mt( kk ), UPPER_MASK ), Bitwise.BitAnd( mt( kk + 1 ), LOWER_MASK ) )
		      mt( kk ) = Bitwise.BitXor( Bitwise.BitXor( mt( kk + (M - N) ), Bitwise.ShiftRight( y, 1 ) ), mag01( Bitwise.BitAnd( y, &h1 ) ) )
		    next kk
		    
		    y = Bitwise.BitOr( Bitwise.BitAnd( mt( N - 1 ), UPPER_MASK ), Bitwise.BitAnd( mt( 0 ), LOWER_MASK ) )
		    mt( N - 1 ) = Bitwise.BitXOr( Bitwise.BitXor( mt( M - 1 ), Bitwise.ShiftRight( y, 1 ) ), mag01( Bitwise.BitAnd( y, &h1 ) ) )
		    
		    mti = 0
		  end if
		  
		  y = mt( mti )
		  mti = mti + 1
		  
		  // Tempering
		  y = Bitwise.BitXor( y, Bitwise.ShiftRight( y, 11 ) )
		  y = Bitwise.BitXor( y, Bitwise.BitAnd( Bitwise.ShiftLeft( y, 7 ), &h9d2c5680 ) )
		  y = Bitwise.BitXor( y, Bitwise.BitAnd( Bitwise.ShiftLeft( y, 15 ), &hefc60000 ) )
		  y = Bitwise.BitXor( y, Bitwise.ShiftRight( y, 18 ) )
		  
		  return y
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function RandReal1() As Double
		  // generates a random number on [0,1]-real-interval
		  
		  // divided by 2^32-1
		  return RandInt32 * (1.0 / 4294967295.0 )
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function RandReal2() As Double
		  // generates a random number on [0,1)-real-interval
		  
		  // divided by 2^32
		  return RandInt32 * (1.0 / 4294967296.0)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function RandReal3() As Double
		  // generates a random number on (0,1)-real-interval
		  
		  // divided by 2^32
		  Dim d as Double = RandInt32
		  return ((d + 0.5) * (1.0 / 4294967296.0))
		End Function
	#tag EndMethod


	#tag Note, Name = About this Module
		Aaron Ballman
		aaron@aaronballman.com
		Dec 19, 2005
		
		Based off the code found at:
		http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
		
		The Wikipedia article on the Mersenne Twister algorithm can be found at:
		http://en.wikipedia.org/wiki/Mersenne_Twister
		
		This code is provided as-is and I make no guarantees as to its fitness.  It requires
		RB2006r1 due to the use of the new unsigned datatypes.
		
		Each Rand function produces results in a slightly different fashion.  Here 
		is a table for each of the methods and its output.
		
		Method             Range
		--------------         --------------------------------------
		RandInt32         [0, &hffffffff]
		RandInt31         [0, &h7fffffff]
		RandReal1         [0,1]
		RandReal2         [0,1)
		RandReal3         (0,1)
		Rand53Bit         [0,1) with 53-bit resolution
	#tag EndNote


	#tag Property, Flags = &h21
		#tag Note
			This is the array for the state vector
		#tag EndNote
		Private mt(N) As UInt32
	#tag EndProperty

	#tag Property, Flags = &h21
		#tag Note
			625 is N + 1, but because RB doesn't let you put an expression in there, I had to hard-code it
		#tag EndNote
		Private mti As Int32 = 625
	#tag EndProperty


	#tag Constant, Name = LOWER_MASK, Type = Double, Dynamic = False, Default = \"&h7fffffff", Scope = Private
	#tag EndConstant

	#tag Constant, Name = M, Type = Double, Dynamic = False, Default = \"397", Scope = Private
	#tag EndConstant

	#tag Constant, Name = MATRIX_A, Type = Double, Dynamic = False, Default = \"&h9908b0df", Scope = Private
	#tag EndConstant

	#tag Constant, Name = N, Type = Double, Dynamic = False, Default = \"624", Scope = Private
	#tag EndConstant

	#tag Constant, Name = UPPER_MASK, Type = Double, Dynamic = False, Default = \"&h80000000", Scope = Private
	#tag EndConstant


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
