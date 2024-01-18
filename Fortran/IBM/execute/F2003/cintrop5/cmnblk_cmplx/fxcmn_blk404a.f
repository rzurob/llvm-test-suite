!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_cmplx.sh fxcmn_blk404a cxcmn_blk404a
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk404a fxcmn_blk404a.out
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
!*  DRIVER STANZA              : xlf95, xlc, gcc 
!*  REQUIRED COMPILER OPTIONS  : -qfloat=complexgcc (with gcc)
!*
!*  DESCRIPTION                : This test case will verify that 1-dimensional array variables
!*                               inside of common blocks are interoperable with C variables.
!*
!*                               Data type being tested: all types od COMPLEX
!*
!*                               Scope being tested:  main program
!*
!*                               Similar to fxcmn_blk404.f, but this testcase will
!*                               test complex(C_LONG_DOUBLE_COMPLEX) as well
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk404a
        use iso_c_binding
        implicit none

        logical precision_x8, precision_x16, precision_x32
	integer i

! ----------------------------------------------------------------------------
! Complex Declaration
!       - use default, KIND, LEN, INT, MIN
!       - use ISO_C_BINDING modules
! ----------------------------------------------------------------------------

        complex                       			cmplx_8a(3)
        complex(LEN('Kobi'))				cmplx_8b(3)
        complex*8 					cmplx_8c(3)
        complex(4 )					cmplx_8d(3)

        complex*16                      		cmplx_16a(3)
        complex(kind=MIN(8,19))        			cmplx_16b(3)
        complex( 8)         				cmplx_16c(3)
        double complex 					cmplx_16d(3)

        complex(C_FLOAT_COMPLEX)        		cmplx_FLOAT_COMPLEX_a(3)
        complex( C_FLOAT_COMPLEX )        		cmplx_FLOAT_COMPLEX_b(3)
        complex(C_FLOAT_COMPLEX )        		cmplx_FLOAT_COMPLEX_c(3)
        complex( C_FLOAT_COMPLEX)        		cmplx_FLOAT_COMPLEX_d(3)

        complex(C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_a(3)
        complex(  C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_b(3)
        complex(C_DOUBLE_COMPLEX  )       		cmplx_DOUBLE_COMPLEX_c(3)
        complex(  C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_d(3)

        complex(  C_LONG_DOUBLE_COMPLEX  )              cmplx_LONG_DOUBLE_COMPLEX_a(3)
        complex(  C_LONG_DOUBLE_COMPLEX)                cmplx_LONG_DOUBLE_COMPLEX_b(3)
        complex(C_LONG_DOUBLE_COMPLEX  )                cmplx_LONG_DOUBLE_COMPLEX_c(3)
        complex(C_LONG_DOUBLE_COMPLEX)                  cmplx_LONG_DOUBLE_COMPLEX_d(3)


! ----------------------------------------------------------------------------
! One COMMON statement with multiple common blocks in one BIND(C) statement
! ----------------------------------------------------------------------------

        common /blk_cmplx/      cmplx_8a, 	&
				cmplx_8b,	&
				cmplx_8c,	&
				cmplx_8d,	&
				cmplx_16a, 	&
				cmplx_16b,	&
				cmplx_16c,	&
				cmplx_16d,	&
				cmplx_LONG_DOUBLE_COMPLEX_a, 	&
				cmplx_LONG_DOUBLE_COMPLEX_b,	&
				cmplx_LONG_DOUBLE_COMPLEX_c,	&
				cmplx_LONG_DOUBLE_COMPLEX_d,	&
				cmplx_FLOAT_COMPLEX_a,	&
				cmplx_FLOAT_COMPLEX_b,	&
				cmplx_FLOAT_COMPLEX_c,	&
				cmplx_FLOAT_COMPLEX_d,	&
				cmplx_DOUBLE_COMPLEX_a,	&
				cmplx_DOUBLE_COMPLEX_b,	&
				cmplx_DOUBLE_COMPLEX_c,	&
				cmplx_DOUBLE_COMPLEX_d

        bind(c) ::   /blk_cmplx/


! ----------------------------------------------------------------------------
! Complex Initialization
! ----------------------------------------------------------------------------

	cmplx_8a		= ( 3.4,  3.4)
        cmplx_8b                = ( 3.4, -3.4)
        cmplx_8c		= (-1.17, -3.4)
        cmplx_8d                = (-3.4, -1.175)

        cmplx_16a		= ( 1.79D0,  1.79D0)
        cmplx_16b		= ( 1.79D0, -1.79D0)
        cmplx_16c		= (-2.22D0, -1.79D0)
        cmplx_16d		= ( 1.79D0, -2.22D0)

        cmplx_LONG_DOUBLE_COMPLEX_a		= ( 1.797693Q0,  1.797693Q0)
        cmplx_LONG_DOUBLE_COMPLEX_b      	= ( 1.797693Q0, -1.797693Q0)
        cmplx_LONG_DOUBLE_COMPLEX_c      	= (-2.225073Q0, -1.797693Q0)
        cmplx_LONG_DOUBLE_COMPLEX_d      	= ( 1.797693Q0, -2.225073Q0)

        cmplx_FLOAT_COMPLEX_a		= ( 3.4028,  3.4028)
        cmplx_FLOAT_COMPLEX_b		= ( 3.4028, -3.4028)
        cmplx_FLOAT_COMPLEX_c		= (-1.1754, -3.4028)
        cmplx_FLOAT_COMPLEX_d		= (-3.4028, -1.1754)

        cmplx_DOUBLE_COMPLEX_a		= ( 1.797693D0,  1.797693D0)
        cmplx_DOUBLE_COMPLEX_b		= ( 1.797693D0, -1.797693D0)
        cmplx_DOUBLE_COMPLEX_c		= (-2.225073D0, -1.797693D0)
        cmplx_DOUBLE_COMPLEX_d		= ( 1.797693D0, -2.225073D0)


! ----------------------------------------------------------------------------
! Complex Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------


      do i = 1, 3

        if ( .not. precision_x8 ( cmplx_8a(i)                , ( 3.4,  3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8b(i)                , ( 3.4, -3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8c(i)                , (-1.17, -3.4) )) error stop 5
        if ( .not. precision_x8 ( cmplx_8d(i)                , (-3.4, -1.175)))	error stop 5

        if ( .not. precision_x8 ( cmplx_16a(i)              , ( 1.79D0,  1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16b(i)              , ( 1.79D0, -1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16c(i)              , (-2.22D0, -1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16d(i)              , ( 1.79D0, -2.22D0) )) error stop 6

        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_a(i)              , ( 1.797693Q0,  1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_b(i)              , ( 1.797693Q0, -1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_c(i)              , (-2.225073Q0, -1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_d(i)              , ( 1.797693Q0, -2.225073Q0) )) error stop 7

        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_a(i)   , ( 3.4028,  3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_b(i)   , ( 3.4028, -3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_c(i)   , (-1.1754, -3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_d(i)   , (-3.4028, -1.1754) )) 	error stop 8

        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_a(i) , ( 1.797693D0,  1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_b(i) , ( 1.797693D0, -1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_c(i) , (-2.225073D0, -1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_d(i) , ( 1.797693D0, -2.225073D0) )) error stop 9

      end do


! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
       CALL CSUB_CMPLX()


! ----------------------------------------------------------------------------
! Complex Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------
print *, cmplx_8a


      do i = 1, 3

        if ( .not. precision_x8 ( cmplx_8a(i)               , (  300.119,   300.119 ) ))     error stop 60
        if ( .not. precision_x8 ( cmplx_8b(i)               , ( -300.119 , -300.119 ) ))     error stop 61
        if ( .not. precision_x8 ( cmplx_8c(i)               , (  1000.009 , -1000.009 ) ))   error stop 62
        if ( .not. precision_x8 ( cmplx_8d(i)              , ( -1000.009  , 1000.009 ) ))   error stop 63
     
        if ( .not. precision_x8 ( cmplx_16a(i)              , ( 1234300.11911D0 ,1234300.11911D0 ) ))     error stop 64
        if ( .not. precision_x8 ( cmplx_16b(i)              , ( -1234300.11911D0 ,-1234300.11911D0 ) ))   error stop 65
        if ( .not. precision_x8 ( cmplx_16c(i)              , ( 12341000.00911D0 ,-12341000.00911D0 ) ))    error stop 66
        if ( .not. precision_x8 ( cmplx_16d(i)              , ( -12341000.00911D0 ,12341000.00911D0 ) ))    error stop 67
    
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_a(i)              , (  987654321300.11998Q0  , 987654321300.11998Q0 ) ))     error stop 68
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_b(i)              , ( -987654321300.11998Q0  ,-987654321300.11998Q0 ) ))     error stop 69
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_c(i)              , (  9876543211000.00998Q0  ,-9876543211000.00998Q0 ) ))   error stop 70
        if ( .not. precision_x32 ( cmplx_LONG_DOUBLE_COMPLEX_d(i)              , ( -9876543211000.00998Q0  , 9876543211000.00998Q0 ) ))   error stop 71
    
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_a(i)   , (  300.119  , 300.119 ) ))       error stop 72
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_b(i)   , ( -300.119  ,-300.119 ) ))       error stop 73
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_c(i)   , (  1000.009 , -1000.009 ) ))     error stop 74
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_d(i)   , ( -1000.009 ,  1000.009 ) ))     error stop 75
    
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_a(i) , ( 1234300.11911D0 ,1234300.11911D0 ) ))      error stop 76
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_b(i) , ( -1234300.11911D0 ,-1234300.11911D0 ) ))    error stop 77
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_c(i) , ( 12341000.00911D0 ,-12341000.00911D0 ) ))     error stop 78
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_d(i) , ( -12341000.00911D0 ,12341000.00911D0 ) ))     error stop 79

      end do

    

end program

