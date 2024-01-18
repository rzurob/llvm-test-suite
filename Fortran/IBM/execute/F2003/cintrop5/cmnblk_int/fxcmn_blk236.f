!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk236 cxcmn_blk206
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk236 
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block wiht BIND(C)
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : February 13, 2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that variables of
!*                               integer data types inside of common blocks do
!*                               interoperate with C variables
!*
!*                               Scope:  module
!*
!*				 This testcase will
!*				 verify 3-dimensional array variables inside of 
!*				 BIND(C) common block.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module fmod1 
	use iso_c_binding
        implicit none

	integer i

! ----------------------------------------------------------------------------
! Integer Declaration
!	- use ISO_C_BINDING modules	
!       - use default/non-default variables
! ----------------------------------------------------------------------------

	integer (LEN('k'))				:: int_s1(2,2,2)

	integer (2 )					:: int_s2(2,2,2)

	integer  					:: int_s4(2,2,2)

	integer (kind=int((4.4e0_8,6.5e0_8))+4 ) 	:: int_s8(2,2,2)
	
	INTEGER (C_SIGNED_CHAR 		)		:: int_C_SIGNED_CHAR(2,2,2)
	INTEGER (C_SHORT 		)		:: int_C_SHORT(2,2,2)
	INTEGER (C_INT 			)		:: int_C_INT(2,2,2)
	INTEGER (C_LONG 		)		:: int_C_LONG(2,2,2)
	INTEGER (C_LONG_LONG		)		:: int_C_LONG_LONG(2,2,2)
	INTEGER (C_SIZE_T 		)		:: int_C_SIZE_T(2,2,2)
	INTEGER (C_INTPTR_T 		)		:: int_C_INTPTR_T(2,2,2)
	INTEGER (C_INTMAX_T 		)		:: int_C_INTMAX_T(2,2,2)
	INTEGER (C_INT8_T 		)		:: int_C_INT8_T(2,2,2)
	INTEGER (C_INT16_T 		)		:: int_C_INT16_T(2,2,2)
	INTEGER (C_INT32_T 		)		:: int_C_INT32_T(2,2,2)
	INTEGER (C_INT64_T 		)		:: int_C_INT64_T(2,2,2)
	INTEGER (C_INT_LEAST8_T 	)		:: int_C_INT_LEAST8_T(2,2,2)
	INTEGER (C_INT_LEAST16_T 	)		:: int_C_INT_LEAST16_T(2,2,2)
	INTEGER (C_INT_LEAST32_T 	)		:: int_C_INT_LEAST32_T(2,2,2)
	INTEGER (C_INT_LEAST64_T 	)		:: int_C_INT_LEAST64_T(2,2,2)
	INTEGER (C_INT_FAST8_T 		)		:: int_C_INT_FAST8_T(2,2,2)
!	INTEGER (C_INT_FAST16_T 	)		:: int_C_INT_FAST16_T(2,2,2)  !** not supported on AIX5.1 and below 
	INTEGER (C_INT_FAST32_T 	)		:: int_C_INT_FAST32_T(2,2,2)
	INTEGER (C_INT_FAST64_T 	)		:: int_C_INT_FAST64_T(2,2,2)


	!*** Comparision matrices
        integer(1)              :: cmp_s1(8)
        integer(2)              :: cmp_s2(8)
        integer                 :: cmp_s4(8)
        integer(8)              :: cmp_s8(8)
        INTEGER(1)               :: cmp_C_SIGNED_CHAR(8)
        INTEGER(2)               :: cmp_C_SHORT(8)
        INTEGER(4)               :: cmp_C_INT(8)
        INTEGER(4)               :: cmp_C_LONG(8)
        INTEGER(8)               :: cmp_C_LONG_LONG(8)
        INTEGER(4)               :: cmp_C_SIZE_T(8)
        INTEGER(4)               :: cmp_C_INTPTR_T(8)
        INTEGER(8)               :: cmp_C_INTMAX_T(8)
        INTEGER(1)               :: cmp_C_INT8_T(8)
        INTEGER(2)               :: cmp_C_INT16_T(8)
        INTEGER(4)               :: cmp_C_INT32_T(8)
        INTEGER(8)               :: cmp_C_INT64_T(8)
        INTEGER(1)               :: cmp_C_INT_LEAST8_T(8)
        INTEGER(2)               :: cmp_C_INT_LEAST16_T(8)
        INTEGER(4)               :: cmp_C_INT_LEAST32_T(8)
        INTEGER(8)               :: cmp_C_INT_LEAST64_T(8)
        INTEGER(1)               :: cmp_C_INT_FAST8_T(8)
        INTEGER(4)               :: cmp_C_INT_FAST32_T(8)
        INTEGER(8)               :: cmp_C_INT_FAST64_T(8)

        integer(1)              :: res_s1(8)
        integer(2)              :: res_s2(8)
        integer                 :: res_s4(8)
        integer(8)              :: res_s8(8)
        INTEGER(1)               :: res_C_SIGNED_CHAR(8)
        INTEGER(2)               :: res_C_SHORT(8)
        INTEGER(4)               :: res_C_INT(8)
        INTEGER(4)               :: res_C_LONG(8)
        INTEGER(8)               :: res_C_LONG_LONG(8)
        INTEGER(4)               :: res_C_SIZE_T(8)
        INTEGER(4)               :: res_C_INTPTR_T(8)
        INTEGER(8)               :: res_C_INTMAX_T(8)
        INTEGER(1)               :: res_C_INT8_T(8)
        INTEGER(2)               :: res_C_INT16_T(8)
        INTEGER(4)               :: res_C_INT32_T(8)
        INTEGER(8)               :: res_C_INT64_T(8)
        INTEGER(1)               :: res_C_INT_LEAST8_T(8)
        INTEGER(2)               :: res_C_INT_LEAST16_T(8)
        INTEGER(4)               :: res_C_INT_LEAST32_T(8)
        INTEGER(8)               :: res_C_INT_LEAST64_T(8)
        INTEGER(1)               :: res_C_INT_FAST8_T(8)
        INTEGER(4)               :: res_C_INT_FAST32_T(8)
        INTEGER(8)               :: res_C_INT_FAST64_T(8)


! ----------------------------------------------------------------------------
! COMMON and BIND(C) statements
! ----------------------------------------------------------------------------
	common /blk_all/ int_s1, int_s2, int_s4, int_s8, int_C_SIGNED_CHAR, int_C_SHORT, 	&
			 int_C_INT, int_C_LONG, int_C_LONG_LONG, int_C_SIZE_T, int_C_INTPTR_T, int_C_INTMAX_T, int_C_INT8_T, 		&
			 int_C_INT16_T, int_C_INT32_T, int_C_INT64_T, int_C_INT_LEAST8_T, int_C_INT_LEAST16_T, int_C_INT_LEAST32_T, 	&
			 int_C_INT_LEAST64_T, int_C_INT_FAST8_T, int_C_INT_FAST32_T, int_C_INT_FAST64_T

        bind(c) ::   /blk_all/

end module fmod1 


program fxcmn_blk236
	use fmod1
	implicit none


! ----------------------------------------------------------------------------
! Integer Initialization
!       - use decimal, binary, octal values to assign values
!       - for some variables, assign 1 value to whole array for others, assign
!         specific values for each element in the array
!       - use RESHAPE to shape the array into a 3-D array use padding for some
! ----------------------------------------------------------------------------
	

        int_s1 			=  RESHAPE( (/b'1111111',o'0',-128,o'177',127,0,-0,-111/), (/2,2,2/))		

        int_s2 			=  RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111',0,-1277/), (/2,2,2/), (/-1,-2,-3/))

        int_s4 			=  RESHAPE( (/2147483647,b'1111111',-2147483648, 0, o'3641100',-2147483648,2147483647,-0, 1/), (/2,2,2/), (/-1,-2,-3/))

        int_s8  		= -9223372036854775807_8

        int_C_SIGNED_CHAR	= RESHAPE( (/b'1111111',o'0',-128, o'177',127/), (/2,2,2/), (/-1,-2,-3/))
        int_C_SHORT		= RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111'/), (/2,2,2/), (/-1,-2,-3/))
        int_C_INT		= RESHAPE( (/2147483647,b'1111111',-2147483648, 0, o'3641100'/), (/2,2,2/), (/-1,-2,-3/))
        int_C_LONG		=  2147483647 
        int_C_LONG_LONG		= -9223372036854775807_8
        int_C_SIZE_T		= RESHAPE( (/2147483647,b'1111111',-2147483648, 0, o'3641100'/), (/2,2,2/), (/-1,-2,-3/))
        int_C_INTPTR_T		=  1000000000           
        int_C_INTMAX_T		= RESHAPE( (/9223372036854775807_8,b'000000000',-9223372036854775807_8,o'3641100',-2147483648_8/),(/2,2,2/),(/-1_8,-2_8,-3_8/))
        int_C_INT8_T		= -128
        int_C_INT16_T		= RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111'/), (/2,2,2/), (/-1,-2,-3/))
        int_C_INT32_T		=  2147483647
        int_C_INT64_T		=  9223372036850123456_8
        int_C_INT_LEAST8_T	= -128
        int_C_INT_LEAST16_T	= RESHAPE( (/o'77777',o'0',-32768, 32767,b'1111111'/), (/2,2,2/), (/-1,-2,-3/))
        int_C_INT_LEAST32_T	=  5
        int_C_INT_LEAST64_T	=  1111111111111111111_8
        int_C_INT_FAST8_T	=  b'001'		! d'1'
!        int_C_INT_FAST16_T	=  o'100'		! d'64'     ! Only for AIX52, Mac, and Linux
        int_C_INT_FAST32_T 	= RESHAPE( (/2147483647,b'1111111',-2147483648, 0, o'3641100'/), (/2,2,2/), (/-1,-2,-3/))
        int_C_INT_FAST64_T	= RESHAPE( (/9223372036854775807_8,b'000000000',-9223372036854775807_8, o'3641100', -2147483648_8/), (/2,2,2/), (/-1_8,-2_8,-3_8/))



	cmp_s1 =  (/127,0,-128,127,127,0,0,-111/)

  	cmp_s2 =  (/32767,0,-32768,32767,127,0,-1277,-1 /)

  	cmp_s4 =  (/2147483647,127,-2147483648,0,1000000,-2147483648,2147483647,0 /)

  	cmp_s8 =  (/-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8 /)

  	cmp_c_signed_char =  (/127,0,-128,127,127,-1,-2,-3 /)

  	cmp_c_short =  (/32767,0,-32768,32767,127,-1,-2,-3 /)

  	cmp_c_int =  (/2147483647,127,-2147483648,0,1000000,-1,-2,-3 /)

  	cmp_c_long =  (/2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647 /)

  	cmp_c_long_long =  (/-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8,-9223372036854775807_8/)

  	cmp_c_size_t =  (/2147483647,127,-2147483648,0,1000000,-1,-2,-3 /)

  	cmp_c_intptr_t =  (/1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000 /)

  	cmp_c_intmax_t =  (/9223372036854775807_8,0_8,-9223372036854775807_8,1000000_8,-2147483648_8,-1_8,-2_8,-3_8 /)

  	cmp_c_int8_t =  (/-128,-128,-128,-128,-128,-128,-128,-128 /)

  	cmp_c_int32_t =  (/2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647 /)

  	cmp_c_int64_t =  (/9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8,9223372036850123456_8/)

  	cmp_c_int_least8_t =  (/-128,-128,-128,-128,-128,-128,-128,-128 /)

 	cmp_c_int_least16_t=  (/32767,0,-32768,32767,127,-1,-2,-3 /)

 	cmp_c_int_least32_t =  (/5,5,5,5,5,5,5,5 /)

 	cmp_c_int_least64_t =  (/1111111111111111111_8,1111111111111111111_8,1111111111111111111_8,1111111111111111111_8,1111111111111111111_8,1111111111111111111_8,1111111111111111111_8,1111111111111111111_8/)

 	cmp_c_int_fast8_t =  (/1,1,1,1,1,1,1,1 /)

 	cmp_c_int_fast32_t =  (/2147483647,127,-2147483648,0,1000000,-1,-2,-3 /)

 	cmp_c_int_fast64_t =  (/9223372036854775807_8,0_8,-9223372036854775807_8,1000000_8,-2147483648_8,-1_8,-2_8,-3_8 /)


! ----------------------------------------------------------------------------
! Integer Verification
!       - verify values before passing to C
! ----------------------------------------------------------------------------
        res_s1  =               RESHAPE( int_s1, (/8/))
        res_s2  =               RESHAPE( int_s2, (/8/))
        res_s4  =               RESHAPE( int_s4, (/8/))
        res_s8  =               RESHAPE( int_s8, (/8/))
        res_C_SIGNED_CHAR  =    RESHAPE( int_C_SIGNED_CHAR, (/8/))
        res_C_SHORT  =          RESHAPE( int_C_SHORT, (/8/))
        res_C_INT  =            RESHAPE( int_C_INT, (/8/))
        res_C_LONG  =           RESHAPE( int_C_LONG, (/8/))
        res_C_LONG_LONG  =      RESHAPE( int_C_LONG_LONG, (/8/))
        res_C_SIZE_T  =         RESHAPE( int_C_SIZE_T, (/8/))
        res_C_INTPTR_T  =       RESHAPE( int_C_INTPTR_T, (/8/))
        res_C_INTMAX_T  =       RESHAPE( int_C_INTMAX_T, (/8/))
        res_C_INT8_T  =         RESHAPE( int_C_INT8_T, (/8/))
        res_C_INT16_T  =        RESHAPE( int_C_INT16_T, (/8/))
        res_C_INT32_T  =        RESHAPE( int_C_INT32_T, (/8/))
        res_C_INT64_T  =        RESHAPE( int_C_INT64_T, (/8/))
        res_C_INT_LEAST8_T  =   RESHAPE( int_C_INT_LEAST8_T, (/8/))
        res_C_INT_LEAST16_T  =  RESHAPE( int_C_INT_LEAST16_T, (/8/))
        res_C_INT_LEAST32_T  =  RESHAPE( int_C_INT_LEAST32_T, (/8/))
        res_C_INT_LEAST64_T  =  RESHAPE( int_C_INT_LEAST64_T, (/8/))
        res_C_INT_FAST8_T  =    RESHAPE( int_C_INT_FAST8_T, (/8/))
        res_C_INT_FAST32_T  =   RESHAPE( int_C_INT_FAST32_T, (/8/))
        res_C_INT_FAST64_T  =   RESHAPE( int_C_INT_FAST64_T, (/8/))


        do i = 1, 8
          if ( res_s1(i)                .ne.    cmp_s1(i)   )           STOP 5
          if ( res_s2(i)                .ne.    cmp_s2(i)    )          STOP 6
          if ( res_s4(i)                .ne.    cmp_s4(i)    )          STOP 7
          if ( res_s8(i)                .ne.    cmp_s8(i)    )          STOP 8
          if ( res_C_SIGNED_CHAR(i)     .ne.    cmp_C_SIGNED_CHAR(i) )  STOP 9
          if ( res_C_SHORT(i)           .ne.    cmp_C_SHORT(i)    )     STOP 10
          if ( res_C_INT(i)             .ne.    cmp_C_INT(i)    )       STOP 11
          if ( res_C_LONG(i)            .ne.    cmp_C_LONG(i)    )      STOP 12
          if ( res_C_LONG_LONG(i)       .ne.    cmp_C_LONG_LONG(i)    ) STOP 13
          if ( res_C_SIZE_T(i)          .ne.    cmp_C_SIZE_T(i)    )    STOP 14
          if ( res_C_INTPTR_T(i)        .ne.    cmp_C_INTPTR_T(i)    )  STOP 15
          if ( res_C_INTMAX_T(i)        .ne.    cmp_C_INTMAX_T(i)    )  STOP 16
          if ( res_C_INT8_T(i)          .ne.    cmp_C_INT8_T(i)    )    STOP 17
!         if ( res_C_INT16_T(i)         .ne.    cmp_C_INT16_T(i)    )   STOP 18
          if ( res_C_INT32_T(i)         .ne.    cmp_C_INT32_T(i)    )   STOP 19
          if ( res_C_INT64_T(i)         .ne.    cmp_C_INT64_T(i)    )   STOP 10
          if ( res_C_INT_LEAST8_T(i)    .ne.    cmp_C_INT_LEAST8_T(i) ) STOP 21
          if ( res_C_INT_LEAST16_T(i)   .ne.    cmp_C_INT_LEAST16_T(i)) STOP 22
          if ( res_C_INT_LEAST32_T(i)   .ne.    cmp_C_INT_LEAST32_T(i)) STOP 23
          if ( res_C_INT_LEAST64_T(i)   .ne.    cmp_C_INT_LEAST64_T(i)) STOP 24
          if ( res_C_INT_FAST8_T(i)     .ne.    cmp_C_INT_FAST8_T(i)  ) STOP 25
          if ( res_C_INT_FAST32_T(i)    .ne.    cmp_C_INT_FAST32_T(i) ) STOP 26
          if ( res_C_INT_FAST64_T(i)    .ne.    cmp_C_INT_FAST64_T(i) ) STOP 27

        end do

! ----------------------------------------------------------------------------
!  Call to C subprogram  
! ----------------------------------------------------------------------------
!	CALL CSUB_ALL()


! ----------------------------------------------------------------------------
! Integer Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------
        res_s1  =           	RESHAPE( int_s1, (/8/))
        res_s2  =           	RESHAPE( int_s2, (/8/))
        res_s4  =           	RESHAPE( int_s4, (/8/))
        res_s8  =           	RESHAPE( int_s8, (/8/))
        res_C_SIGNED_CHAR  =    RESHAPE( int_C_SIGNED_CHAR, (/8/))
        res_C_SHORT  =          RESHAPE( int_C_SHORT, (/8/))
        res_C_INT  =           	RESHAPE( int_C_INT, (/8/))
        res_C_LONG  =           RESHAPE( int_C_LONG, (/8/))
        res_C_LONG_LONG  =      RESHAPE( int_C_LONG_LONG, (/8/))
        res_C_SIZE_T  =         RESHAPE( int_C_SIZE_T, (/8/))
        res_C_INTPTR_T  =       RESHAPE( int_C_INTPTR_T, (/8/))
        res_C_INTMAX_T  =       RESHAPE( int_C_INTMAX_T, (/8/))
        res_C_INT8_T  =         RESHAPE( int_C_INT8_T, (/8/))
        res_C_INT16_T  =        RESHAPE( int_C_INT16_T, (/8/))
        res_C_INT32_T  =        RESHAPE( int_C_INT32_T, (/8/))
        res_C_INT64_T  =        RESHAPE( int_C_INT64_T, (/8/))
        res_C_INT_LEAST8_T  =   RESHAPE( int_C_INT_LEAST8_T, (/8/))
        res_C_INT_LEAST16_T  =  RESHAPE( int_C_INT_LEAST16_T, (/8/))
        res_C_INT_LEAST32_T  =  RESHAPE( int_C_INT_LEAST32_T, (/8/))
        res_C_INT_LEAST64_T  =  RESHAPE( int_C_INT_LEAST64_T, (/8/))
        res_C_INT_FAST8_T  =    RESHAPE( int_C_INT_FAST8_T, (/8/))
        res_C_INT_FAST32_T  =   RESHAPE( int_C_INT_FAST32_T, (/8/))
        res_C_INT_FAST64_T  =   RESHAPE( int_C_INT_FAST64_T, (/8/))


	do i = 1, 8
	  if ( res_s1(i)    		.ne.    cmp_s1(i)   ) 		STOP 35
	  if ( res_s2(i)  	  	.ne.    cmp_s2(i)    ) 		STOP 36
	  if ( res_s4(i)    		.ne.    cmp_s4(i)    ) 		STOP 37
	  if ( res_s8(i)    		.ne.    cmp_s8(i)    ) 		STOP 38
	  if ( res_C_SIGNED_CHAR(i)    	.ne.    cmp_C_SIGNED_CHAR(i) )  STOP 39
	  if ( res_C_SHORT(i)    	.ne.    cmp_C_SHORT(i)    ) 	STOP 40
	  if ( res_C_INT(i)    		.ne.    cmp_C_INT(i)    ) 	STOP 41
	  if ( res_C_LONG(i)    	.ne.    cmp_C_LONG(i)    ) 	STOP 42
	  if ( res_C_LONG_LONG(i)    	.ne.    cmp_C_LONG_LONG(i)    ) STOP 43
	  if ( res_C_SIZE_T(i)    	.ne.    cmp_C_SIZE_T(i)    ) 	STOP 44
	  if ( res_C_INTPTR_T(i)    	.ne.    cmp_C_INTPTR_T(i)    ) 	STOP 45
	  if ( res_C_INTMAX_T(i)    	.ne.    cmp_C_INTMAX_T(i)    ) 	STOP 46
	  if ( res_C_INT8_T(i)    	.ne.    cmp_C_INT8_T(i)    ) 	STOP 47
!	  if ( res_C_INT16_T(i)    	.ne.    cmp_C_INT16_T(i)    ) 	STOP 48
	  if ( res_C_INT32_T(i)    	.ne.    cmp_C_INT32_T(i)    ) 	STOP 49
	  if ( res_C_INT64_T(i)    	.ne.    cmp_C_INT64_T(i)    ) 	STOP 50
	  if ( res_C_INT_LEAST8_T(i)    .ne.    cmp_C_INT_LEAST8_T(i) ) STOP 51
	  if ( res_C_INT_LEAST16_T(i)   .ne.    cmp_C_INT_LEAST16_T(i)) STOP 52
	  if ( res_C_INT_LEAST32_T(i)   .ne.    cmp_C_INT_LEAST32_T(i)) STOP 53
	  if ( res_C_INT_LEAST64_T(i)   .ne.    cmp_C_INT_LEAST64_T(i)) STOP 54
	  if ( res_C_INT_FAST8_T(i)     .ne.    cmp_C_INT_FAST8_T(i)  ) STOP 55
	  if ( res_C_INT_FAST32_T(i)    .ne.    cmp_C_INT_FAST32_T(i) ) STOP 56
	  if ( res_C_INT_FAST64_T(i)    .ne.    cmp_C_INT_FAST64_T(i) ) STOP 57

	end do

end program
