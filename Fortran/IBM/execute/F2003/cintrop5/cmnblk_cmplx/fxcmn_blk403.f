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
!*  REQUIRED COMPILER OPTIONS  : -qlongdouble (with xlc)
!*
!*  DESCRIPTION                : This test case will verify that scalar variables
!*                               inside of common blocks are interoperable with C variables.
!*
!*                               Data type being tested: all types od COMPLEX
!*
!*                               Scope being tested:  main program
!*
!*				 Test one COMMON statements with multiple
!*				 common blocks in one BIND(C) statement
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk403
        use iso_c_binding
        implicit none

        logical precision_x8, precision_x16, precision_x32

! ----------------------------------------------------------------------------
! Complex Declaration
!       - use default, KIND, LEN, INT, MIN
!       - use ISO_C_BINDING modules
! ----------------------------------------------------------------------------

        complex                       			cmplx_8a
        complex(LEN('Kobi'))				cmplx_8b
        complex*8 					cmplx_8c
        complex(4 )					cmplx_8d

        complex*16                      		cmplx_16a
        complex(kind=MIN(8,19))        			cmplx_16b
        complex( 8)         				cmplx_16c
        double complex 					cmplx_16d

	!**  C-code must be compiled with xlc -qlongdouble for complex*32
        complex(kind=b'10000')          		cmplx_32a
        complex(kind=INT((12.4e0_8,6.5e0_8))+4 )     	cmplx_32b
        complex(16)  					cmplx_32c
        complex*32     					cmplx_32d

        complex(C_FLOAT_COMPLEX)        		cmplx_FLOAT_COMPLEX_a
        complex( C_FLOAT_COMPLEX )        		cmplx_FLOAT_COMPLEX_b
        complex(C_FLOAT_COMPLEX )        		cmplx_FLOAT_COMPLEX_c
        complex( C_FLOAT_COMPLEX)        		cmplx_FLOAT_COMPLEX_d

        complex(C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_a
        complex(  C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_b
        complex(C_DOUBLE_COMPLEX  )       		cmplx_DOUBLE_COMPLEX_c
        complex(  C_DOUBLE_COMPLEX)       		cmplx_DOUBLE_COMPLEX_d


! ----------------------------------------------------------------------------
! One COMMON statements with multiple common blocks in one BIND(C) statement
! ----------------------------------------------------------------------------

       common /blk_cmplx_8a/                              cmplx_8a	&
        ,  /blk_cmplx_8b/                                 cmplx_8b      &
        ,  /blk_cmplx_8c/                                 cmplx_8c      &
        ,  /blk_cmplx_8d/                                 cmplx_8d      &
        ,  /blk_cmplx_16a/                                 cmplx_16a    &
        ,  /blk_cmplx_16b/                                 cmplx_16b    &
        ,  /blk_cmplx_16c/                                 cmplx_16c    &
        ,  /blk_cmplx_16d/                                 cmplx_16d    &
        ,  /blk_cmplx_32a/                                 cmplx_32a    &
        ,  /blk_cmplx_32b/                                 cmplx_32b    &
        ,  /blk_cmplx_32c/                                 cmplx_32c    &
        ,  /blk_cmplx_32d/                                 cmplx_32d    &
        ,  /blk_cmplx_FLOAT_COMPLEX_a/               cmplx_FLOAT_COMPLEX_a      &
        ,  /blk_cmplx_FLOAT_COMPLEX_b/               cmplx_FLOAT_COMPLEX_b      &
        ,  /blk_cmplx_FLOAT_COMPLEX_c/               cmplx_FLOAT_COMPLEX_c      &
        ,  /blk_cmplx_FLOAT_COMPLEX_d/               cmplx_FLOAT_COMPLEX_d      &
        ,  /blk_cmplx_DOUBLE_COMPLEX_a/               cmplx_DOUBLE_COMPLEX_a    &
        ,  /blk_cmplx_DOUBLE_COMPLEX_b/               cmplx_DOUBLE_COMPLEX_b    &
        ,  /blk_cmplx_DOUBLE_COMPLEX_c/               cmplx_DOUBLE_COMPLEX_c    &
        ,  /blk_cmplx_DOUBLE_COMPLEX_d/               cmplx_DOUBLE_COMPLEX_d



        bind(c) ::  			&
          /blk_cmplx_8a/		&
       ,  /blk_cmplx_8b/		&
       ,  /blk_cmplx_8c/		&
       ,  /blk_cmplx_8d/		&
       ,  /blk_cmplx_16a/		&
       ,  /blk_cmplx_16b/		&
       ,  /blk_cmplx_16c/		&
       ,  /blk_cmplx_16d/		&
       ,  /blk_cmplx_32a/		&
       ,  /blk_cmplx_32b/		&
       ,  /blk_cmplx_32c/		&
       ,  /blk_cmplx_32d/		&
       ,  /blk_cmplx_FLOAT_COMPLEX_a/	&
       ,  /blk_cmplx_FLOAT_COMPLEX_b/	&
       ,  /blk_cmplx_FLOAT_COMPLEX_c/	&
       ,  /blk_cmplx_FLOAT_COMPLEX_d/	&
       ,  /blk_cmplx_DOUBLE_COMPLEX_a/	&
       ,  /blk_cmplx_DOUBLE_COMPLEX_b/	&
       ,  /blk_cmplx_DOUBLE_COMPLEX_c/	&
       ,  /blk_cmplx_DOUBLE_COMPLEX_d/



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

        cmplx_32a		= ( 1.797693Q0,  1.797693Q0)
        cmplx_32b      		= ( 1.797693Q0, -1.797693Q0)
        cmplx_32c      		= (-2.225073Q0, -1.797693Q0)
        cmplx_32d      		= ( 1.797693Q0, -2.225073Q0)

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

        if ( .not. precision_x8 ( cmplx_8a                , ( 3.4,  3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8b                , ( 3.4, -3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8c                , (-1.17, -3.4) )) 	error stop 5
        if ( .not. precision_x8 ( cmplx_8d                , (-3.4, -1.175))) 	error stop 5

        if ( .not. precision_x8 ( cmplx_16a              , ( 1.79D0,  1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16b              , ( 1.79D0, -1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16c              , (-2.22D0, -1.79D0) )) error stop 6
        if ( .not. precision_x8 ( cmplx_16d              , ( 1.79D0, -2.22D0) )) error stop 6

        if ( .not. precision_x32 ( cmplx_32a              , ( 1.797693Q0,  1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_32b              , ( 1.797693Q0, -1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_32c              , (-2.225073Q0, -1.797693Q0) )) error stop 7
        if ( .not. precision_x32 ( cmplx_32d              , ( 1.797693Q0, -2.225073Q0) )) error stop 7

        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_a   , ( 3.4028,  3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_b   , ( 3.4028, -3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_c   , (-1.1754, -3.4028) )) 	error stop 8
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_d   , (-3.4028, -1.1754) )) 	error stop 8

        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_a , ( 1.797693D0,  1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_b , ( 1.797693D0, -1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_c , (-2.225073D0, -1.797693D0) )) error stop 9
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_d , ( 1.797693D0, -2.225073D0) )) error stop 9


! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
       CALL CSUB_CMPLX()


! ----------------------------------------------------------------------------
! Complex Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------
        if ( .not. precision_x8 ( cmplx_8a               , (  300.119,   300.119 ) ))     error stop 60
        if ( .not. precision_x8 ( cmplx_8b               , ( -300.119 , -300.119 ) ))     error stop 61
        if ( .not. precision_x8 ( cmplx_8c               , (  1000.009 , -1000.009 ) ))   error stop 62
        if ( .not. precision_x8 ( cmplx_8d               , ( -1000.009  , 1000.009 ) ))   error stop 63

        if ( .not. precision_x8 ( cmplx_16a              , ( 1234300.11911D0 ,1234300.11911D0 ) ))     error stop 64
        if ( .not. precision_x8 ( cmplx_16b              , ( -1234300.11911D0 ,-1234300.11911D0 ) ))   error stop 65
        if ( .not. precision_x8 ( cmplx_16c              , ( 12341000.00911D0 ,-12341000.00911D0 ) ))    error stop 66
        if ( .not. precision_x8 ( cmplx_16d              , ( -12341000.00911D0 ,12341000.00911D0 ) ))    error stop 67

        if ( .not. precision_x32 ( cmplx_32a              , (  987654321300.11998Q0  , 987654321300.11998Q0 ) ))     error stop 68
        if ( .not. precision_x32 ( cmplx_32b              , ( -987654321300.11998Q0  ,-987654321300.11998Q0 ) ))     error stop 69
        if ( .not. precision_x32 ( cmplx_32c              , (  9876543211000.00998Q0  ,-9876543211000.00998Q0 ) ))   error stop 70
        if ( .not. precision_x32 ( cmplx_32d              , ( -9876543211000.00998Q0  , 9876543211000.00998Q0 ) ))   error stop 71

        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_a   , (  300.119  , 300.119 ) ))       error stop 72
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_b   , ( -300.119  ,-300.119 ) ))       error stop 73
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_c   , (  1000.009 , -1000.009 ) ))     error stop 74
        if ( .not. precision_x8 ( cmplx_FLOAT_COMPLEX_d   , ( -1000.009 ,  1000.009 ) ))     error stop 75

        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_a , ( 1234300.11911D0 ,1234300.11911D0 ) ))      error stop 76
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_b , ( -1234300.11911D0 ,-1234300.11911D0 ) ))    error stop 77
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_c , ( 12341000.00911D0 ,-12341000.00911D0 ) ))     error stop 78
        if ( .not. precision_x16 ( cmplx_DOUBLE_COMPLEX_d , ( -12341000.00911D0 ,12341000.00911D0 ) ))     error stop 79


end program
