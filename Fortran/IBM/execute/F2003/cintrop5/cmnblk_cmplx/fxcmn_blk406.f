!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  : -qfloat=complexgcc (with gcc), -qlongdouble (with xlc)
!*
!*  DESCRIPTION                : This test case will verify that 3-dimensional array variables
!*                               inside of common blocks are interoperable with C variables.
!*
!*                               Data type being tested: all types od COMPLEX
!*
!*                               Scope being tested:  main program
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk406
        use iso_c_binding
        implicit none

        logical precision_x8, precision_x16, precision_x32
	integer i, j, k

! ----------------------------------------------------------------------------
! Complex Declaration
!       - use default, KIND, LEN, INT, MIN
!       - use ISO_C_BINDING modules
! ----------------------------------------------------------------------------

        complex                       			cmplx_8a(2,2,2)
        complex(LEN('Kobi'))				cmplx_8b(2,2,2)
        complex*8 					cmplx_8c(2,2,2)
        complex(4 )					cmplx_8d(2,2,2)

        complex*16                      		cmplx_16a(2,2,2)
        complex(kind=MIN(8,19))        			cmplx_16b(2,2,2)
        complex( 8)         				cmplx_16c(2,2,2)
        double complex 					cmplx_16d(2,2,2)

	!**  C-code must be compiled with xlc -qlongdouble for complex*32
        complex(kind=b'10000')          		cmplx_32a(2,2,2)
        complex(kind=INT((12.4e0_8,6.5e0_8))+4 )     	cmplx_32b(2,2,2)
        complex(16)  					cmplx_32c(2,2,2)
        complex*32     					cmplx_32d(2,2,2)

        complex(C_FLOAT_COMPLEX)        		cmplx_FLOAT_COMPLEX_a(2,2,2)
        complex( C_FLOAT_COMPLEX )        		cmplx_FLOAT_COMPLEX_b(2,2,2)
        complex(C_FLOAT_COMPLEX )        		cmplx_FLOAT_COMPLEX_c(2,2,2)
        complex( C_FLOAT_COMPLEX)        		cmplx_FLOAT_COMPLEX_d(2,2,2)

        complex(C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_a(2,2,2)
        complex(  C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_b(2,2,2)
        complex(C_DOUBLE_COMPLEX  )       		cmplx_DOUBLE_COMPLEX_c(2,2,2)
        complex(  C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_d(2,2,2)


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
				cmplx_32a, 	&
				cmplx_32b,	&
				cmplx_32c,	&
				cmplx_32d,	&
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

        cmplx_8a               = ( 3.4,  3.4)
        cmplx_8b               = ( 3.4, -3.4)
        cmplx_8c               = (-1.17, -3.4)
        cmplx_8d               = (-3.4, -1.175)

        cmplx_16a              = ( 1.79D0,  1.79D0)
        cmplx_16b              = ( 1.79D0, -1.79D0)
        cmplx_16c              = (-2.22D0, -1.79D0)
        cmplx_16d              = ( 1.79D0, -2.22D0)

        cmplx_32a              = ( 1.797693Q0,  1.797693Q0)
        cmplx_32b              = ( 1.797693Q0, -1.797693Q0)
        cmplx_32c              = (-2.225073Q0, -1.797693Q0)
        cmplx_32d              = ( 1.797693Q0, -2.225073Q0)

        cmplx_FLOAT_COMPLEX_a  = ( 3.4028,  3.4028)
        cmplx_FLOAT_COMPLEX_b  = ( 3.4028, -3.4028)
        cmplx_FLOAT_COMPLEX_c  = (-1.1754, -3.4028)
        cmplx_FLOAT_COMPLEX_d  = (-3.4028, -1.1754)

        cmplx_DOUBLE_COMPLEX_a = ( 1.797693D0,  1.797693D0)
        cmplx_DOUBLE_COMPLEX_b = ( 1.797693D0, -1.797693D0)
        cmplx_DOUBLE_COMPLEX_c = (-2.225073D0, -1.797693D0)
        cmplx_DOUBLE_COMPLEX_d = ( 1.797693D0, -2.225073D0)



! ----------------------------------------------------------------------------
! Complex Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------


      do i = 1, 2
        do j = 1, 2
          do k = 1, 2

        if ( .not. precision_x8 ( cmplx_8a(k,j,i)                , ( 3.4,  3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8b(k,j,i)                , ( 3.4, -3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8c(k,j,i)                , (-1.17, -3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8d(k,j,i)                , (-3.4, -1.175)))	error stop 5

        if ( .not. precision_x8 ( cmplx_16a(k,j,i)              , ( 1.79D0,  1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16b(k,j,i)              , ( 1.79D0, -1.79D0) )) error stop 7
        if ( .not. precision_x8 ( cmplx_16c(k,j,i)              , (-2.22D0, -1.79D0) )) error stop 8
        if ( .not. precision_x8 ( cmplx_16d(k,j,i)              , ( 1.79D0, -2.22D0) )) error stop 9

        if ( .not. precision_x32 ( cmplx_32a(k,j,i)              , ( 1.797693Q0,  1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_32b(k,j,i)              , ( 1.797693Q0, -1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_32c(k,j,i)              , (-2.225073Q0, -1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_32d(k,j,i)              , ( 1.797693Q0, -2.225073Q0) )) error stop 7

        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_a(k,j,i)   , ( 3.4028,  3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_b(k,j,i)   , ( 3.4028, -3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_c(k,j,i)   , (-1.1754, -3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_d(k,j,i)   , (-3.4028, -1.1754) )) 	error stop 8

        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_a(k,j,i) , ( 1.797693D0,  1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_b(k,j,i) , ( 1.797693D0, -1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_c(k,j,i) , (-2.225073D0, -1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_d(k,j,i) , ( 1.797693D0, -2.225073D0) )) error stop 9

          end do
        end do
      end do


! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
       CALL CSUB_CMPLX()


! ----------------------------------------------------------------------------
! Complex Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------


      do i = 1, 2
        do j = 1, 2
          do k = 1, 2

        if ( .not. precision_x8 ( cmplx_8a(k,j,i)               , (  300.119,   300.119 ) ))     error stop 60
        if ( .not. precision_x8 ( cmplx_8b(k,j,i)               , ( -300.119 , -300.119 ) ))     error stop 61
        if ( .not. precision_x8 ( cmplx_8c(k,j,i)               , (  1000.009 , -1000.009 ) ))   error stop 62
        if ( .not. precision_x8 ( cmplx_8d(k,j,i)              , ( -1000.009  , 1000.009 ) ))   error stop 63

        if ( .not. precision_x8 ( cmplx_16a(k,j,i)              , ( 1234300.11911D0 ,1234300.11911D0 ) ))     error stop 64
        if ( .not. precision_x8 ( cmplx_16b(k,j,i)              , ( -1234300.11911D0 ,-1234300.11911D0 ) ))   error stop 65
        if ( .not. precision_x8 ( cmplx_16c(k,j,i)              , ( 12341000.00911D0 ,-12341000.00911D0 ) ))    error stop 66
        if ( .not. precision_x8 ( cmplx_16d(k,j,i)              , ( -12341000.00911D0 ,12341000.00911D0 ) ))    error stop 67

        if ( .not. precision_x32 ( cmplx_32a(k,j,i)              , (  987654321300.11998Q0  , 987654321300.11998Q0 ) ))     error stop 68
        if ( .not. precision_x32 ( cmplx_32b(k,j,i)              , ( -987654321300.11998Q0  ,-987654321300.11998Q0 ) ))     error stop 69
        if ( .not. precision_x32 ( cmplx_32c(k,j,i)              , (  9876543211000.00998Q0  ,-9876543211000.00998Q0 ) ))   error stop 70
        if ( .not. precision_x32 ( cmplx_32d(k,j,i)              , ( -9876543211000.00998Q0  , 9876543211000.00998Q0 ) ))   error stop 71

        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_a(k,j,i)   , (  300.119  , 300.119 ) ))       error stop 72
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_b(k,j,i)   , ( -300.119  ,-300.119 ) ))       error stop 73
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_c(k,j,i)   , (  1000.009 , -1000.009 ) ))     error stop 74
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_d(k,j,i)   , ( -1000.009 ,  1000.009 ) ))     error stop 75

        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_a(k,j,i) , ( 1234300.11911D0 ,1234300.11911D0 ) ))      error stop 76
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_b(k,j,i) , ( -1234300.11911D0 ,-1234300.11911D0 ) ))    error stop 77
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_c(k,j,i) , ( 12341000.00911D0 ,-12341000.00911D0 ) ))     error stop 78
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_d(k,j,i) , ( -12341000.00911D0 ,12341000.00911D0 ) ))     error stop 79

          end do
        end do
      end do




end program

