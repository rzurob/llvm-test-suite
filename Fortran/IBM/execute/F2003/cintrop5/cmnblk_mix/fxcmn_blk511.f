!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk511 cxcmn_blk501
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f fxcmn_blk511.o cxcmn_blk501.o fxcmn_blk511
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : Common block with BIND(C)
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
!*  DRIVER STANZA              : xlf95, xlc, gcc
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array variables
!*				 of default data types inside of common blocks are
!*				 interoperable with C variables 
!*
!*				 Test:  BIND(C) statement in internal subroutine
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk511
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none
	logical precision_r4

        integer :: 	int_d(5) 
        real :: 	real_d(5) 
        character 	char_d(16)


! ----------------------------------------------------------------------------
!      COMMON statement with same COMMON block repeated numourous times;
!      BIND(C) statement with one bind entity
! ----------------------------------------------------------------------------
        common /blk_d/         int_d               , /blk_d/ real_d           , /blk_d/   char_d                                                                               
        bind(c) :: /blk_d/


! ----------------------------------------------------------------------------
! Dafault Array Variables: 
!      1) Pass values into C sub-program
!      2) Check values passed back from C sub-program after modifications 
! ----------------------------------------------------------------------------

	!*** Default integer array
	!--- check "large" and "small" positive values
	!--- check "large" and "small" hegative values

        int_d   =	(/2100000009,-50,-0,99,-2100000001/)


        !*** Default real array
        !--- check "large" and "small" positive values
        !--- check "large" and "small" hegative values

        real_d  =	(/3.402823E+37,-0.0000009,-3.404443E+37,-0.115494E-37,1.175494E-39/)


        !*** Default char array
	!-- check compatibility with C for some escape sequences (such as \f, \t, \s, \b)
	!-- check single quotation and double quotation
	!-- check black spaces
	!-- check UPPER and lower case letters as well as some other characters (such as ! and .)
	!-- The following will output:  	I'm a "TrEe"!

        char_d 	= 	(/'\f','I','''','m',' ','a','\t','\"',"T",'r','E','e',"""","\.",'\b',"!"/)


	!*** Verify values before passing to C
       	if ( int_d(1) .ne. 2100000009 )          		 error stop 5
       	if ( int_d(2) .ne. -50 )            			 error stop 6
       	if ( int_d(3) .ne. -0 )            			 error stop 7
       	if ( int_d(4) .ne. 99 )            			 error stop 8
       	if ( int_d(5) .ne. -2100000001 )        		 error stop 9

       	if ( .not. precision_r4( real_d(1), 	3.402823E+37  )) error stop 10
       	if ( .not. precision_r4( real_d(2), 	-0.0000009    )) error stop 11
       	if ( .not. precision_r4( real_d(3), 	-3.404443E+37 )) error stop 12
       	if ( .not. precision_r4( real_d(4), 	-0.115494E-37 )) error stop 13
       	if ( .not. precision_r4( real_d(5), 	1.175494E-39  )) error stop 14

       	if ( char_d(1) 	.ne. 	'\f' 	)     			 error stop 15
       	if ( char_d(2) 	.ne. 	'I' 	)          		 error stop 16
       	if ( char_d(3) 	.ne. 	'''' 	)         		 error stop 17
       	if ( char_d(4) 	.ne. 	'm' 	)          		 error stop 18
       	if ( char_d(5) 	.ne. 	' ' 	)          		 error stop 19
       	if ( char_d(6) 	.ne. 	'a' 	)          		 error stop 20
       	if ( char_d(7) 	.ne. 	'\t' 	)         		 error stop 21
       	if ( char_d(8) 	.ne. 	"""" 	)         		 error stop 22
       	if ( char_d(9) 	.ne. 	'T' 	)          		 error stop 23
       	if ( char_d(10)	.ne. 	'r' 	)          		 error stop 24
       	if ( char_d(11)	.ne. 	'E' 	)          		 error stop 25
       	if ( char_d(12)	.ne. 	'e' 	)          		 error stop 26
       	if ( char_d(13)	.ne. 	'\"' 	)         		 error stop 27
       	if ( char_d(14)	.ne. 	'\.' 	)         		 error stop 28
       	if ( char_d(15)	.ne. 	'\b' 	)         		 error stop 29
       	if ( char_d(16)	.ne. 	'!' 	)          		 error stop 30


	!*** Call C subroutine
	call csub_d()


	!*** Verify values modified and passed back by C subroutine 
       	if ( int_d(5) .ne. 2100000009 )                          error stop 35
       	if ( int_d(4) .ne. -50 )                                 error stop 36
       	if ( int_d(3) .ne. -0 )                                  error stop 37
       	if ( int_d(2) .ne. 99 )                                  error stop 38
       	if ( int_d(1) .ne. -2100000001 )                         error stop 39

       	if ( .not. precision_r4( real_d(5),     3.402823E+37  )) error stop 40
       	if ( .not. precision_r4( real_d(4),     -0.0000009 )   ) error stop 41
       	if ( .not. precision_r4( real_d(3),     -3.404443E+37 )) error stop 42
       	if ( .not. precision_r4( real_d(2),     -0.115494E-37 )) error stop 43
       	if ( .not. precision_r4( real_d(1),     1.175494E-39  )) error stop 44
	
       	if ( char_d(1)   .ne.    '\f'    )                       error stop 45
       	if ( char_d(2)   .ne.    'N'     )                       error stop 46
       	if ( char_d(3)   .ne.    'o'     )                       error stop 47
       	if ( char_d(4)   .ne.    ','     )                       error stop 48
       	if ( char_d(5)   .ne.    ' '     )                       error stop 49
       	if ( char_d(6)   .ne.    'I'     )                       error stop 50
       	if ( char_d(7)   .ne.    '\''    )                       error stop 51
       	if ( char_d(8)   .ne.    "m"     )                       error stop 52
       	if ( char_d(9)   .ne.    ' '     )                       error stop 53
       	if ( char_d(10)  .ne.    'a'     )                       error stop 54
       	if ( char_d(11)  .ne.    ' '     )                       error stop 55
       	if ( char_d(12)  .ne.    'd'     )                       error stop 56
       	if ( char_d(13)  .ne.    'o'     )                       error stop 57
       	if ( char_d(14)  .ne.    'g'     )                       error stop 58
       	if ( char_d(15)  .ne.    '!'     )                       error stop 59
       	if ( char_d(16)  .ne.    '\n'    )                       error stop 60

   end subroutine

end program
