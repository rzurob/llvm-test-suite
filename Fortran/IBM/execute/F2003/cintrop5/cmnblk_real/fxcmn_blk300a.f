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
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that scalar variables of
!*                               REAL data types inside of common blocks do
!*                               interoperate with C variables
!*
!*                               Scope:  main program
!*
!*				 Similar to fxcmn_blk300.f; but this test case  will
!*				 test C_LONG_DOUBLE iso_c_binding module (so -qlongdouble
!*				 is not used, and real*16 is not tested).
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk300a
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

! ----------------------------------------------------------------------------
! Real Declaration
!     	- use KIND, MAX, LEN, INT, MIN
!	- use ISO_C_BINDING modules
! ----------------------------------------------------------------------------

	real (kind=o'004')			:: real_s4a
	real (LEN('Kobi'))			:: real_s4b
        real                       		:: real_s4c
        real (  4)                       	:: real_s4d

	real (kind=MAX(8, 7))			:: real_s8a
	real (kind=INT((4.4e0_8,6.5e0_8))+4 ) 	:: real_s8b
        real ( 8 )                   		:: real_s8c
        real (KIND=O'010')      :: real_s8d

 	REAL (C_FLOAT 			)	:: r_C_FLOAT_s4a
 	REAL (C_FLOAT 			)	:: r_C_FLOAT_s4b
 	REAL (C_FLOAT 			)	:: r_C_FLOAT_s4c
 	REAL (C_FLOAT 			)	:: r_C_FLOAT_s4d

 	REAL (C_DOUBLE 			)	:: r_C_DOUBLE_s8a
 	REAL (C_DOUBLE 			)	:: r_C_DOUBLE_s8b
 	REAL (C_DOUBLE 			)	:: r_C_DOUBLE_s8c
 	REAL (C_DOUBLE 			)	:: r_C_DOUBLE_s8d

 	REAL (C_LONG_DOUBLE 		)	:: r_C_LONG_DOUBLE_s8a
 	REAL (C_LONG_DOUBLE 		)	:: r_C_LONG_DOUBLE_s8b
 	REAL (C_LONG_DOUBLE 		)	:: r_C_LONG_DOUBLE_s8c
 	REAL (C_LONG_DOUBLE 		)	:: r_C_LONG_DOUBLE_s8d


        common /blk_real/       real_s4a, real_s4b, real_s4c, real_s4d, 	&
				real_s8a, real_s8b, real_s8c, real_s8d, 	&
                                r_C_FLOAT_s4a, r_C_FLOAT_s4b, r_C_FLOAT_s4c, r_C_FLOAT_s4d, 	&
				r_C_DOUBLE_s8a, r_C_DOUBLE_s8b, r_C_DOUBLE_s8c, r_C_DOUBLE_s8d, &
				r_C_LONG_DOUBLE_s8a, r_C_LONG_DOUBLE_s8b, r_C_LONG_DOUBLE_s8c, r_C_LONG_DOUBLE_s8d

        bind(c) ::   /blk_real/


! ----------------------------------------------------------------------------
! Real Initialization
!       - use max and min possible values for +ve and -ve numbers
! ----------------------------------------------------------------------------
        real_s4a 			=  3.402823E+38
        real_s4b 			=  1.175494E-38
        real_s4c                        = -3.402823E+38
        real_s4d                        = -1.175494E-38

        real_s8a 			=  1.797693D+308
        real_s8b 			=  2.225073D-308
        real_s8c                        = -1.797693D+308
        real_s8d                        = -2.225073D-308

        r_C_FLOAT_s4a			=  3.402823E+38
        r_C_FLOAT_s4b			=  1.175494E-38
        r_C_FLOAT_s4c			= -3.402823E+38
        r_C_FLOAT_s4d			= -1.175494E-38

        r_C_DOUBLE_s8a			=  1.797693D+308
        r_C_DOUBLE_s8b			=  2.225073D-308
        r_C_DOUBLE_s8c			= -1.797693D+308
        r_C_DOUBLE_s8d			= -2.225073D-308

        r_C_LONG_DOUBLE_s8a		=  1.797693Q+308
        r_C_LONG_DOUBLE_s8b		=  2.004168Q-292
        r_C_LONG_DOUBLE_s8c		= -1.797693Q+308
        r_C_LONG_DOUBLE_s8d		= -2.004168Q-292


        if ( .not. precision_r4 ( real_s4a                ,  3.402823E+38 )) error stop 10
        if ( .not. precision_r4 ( real_s4b                ,  1.175494E-38 )) error stop 11
        if ( .not. precision_r4 ( real_s4c                , -3.402823E+38 )) error stop 12
        if ( .not. precision_r4 ( real_s4d                , -1.175494E-38 )) error stop 13

        if ( .not. precision_r8 ( real_s8a                ,  1.797693D+308 )) error stop 14
        if ( .not. precision_r8 ( real_s8b                ,  2.225073D-308 )) error stop 15
        if ( .not. precision_r8 ( real_s8c                , -1.797693D+308 )) error stop 16
        if ( .not. precision_r8 ( real_s8d                , -2.225073D-308 )) error stop 17

        if ( .not. precision_r4 ( r_C_FLOAT_s4a           ,  3.402823E+38 )) error stop 22
        if ( .not. precision_r4 ( r_C_FLOAT_s4b           ,  1.175494E-38 )) error stop 23
        if ( .not. precision_r4 ( r_C_FLOAT_s4c           , -3.402823E+38 )) error stop 24
        if ( .not. precision_r4 ( r_C_FLOAT_s4d           , -1.175494E-38 )) error stop 25

        if ( .not. precision_r8 ( r_C_DOUBLE_s8a          ,  1.797693D+308 )) error stop 26
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b          ,  2.225073D-308 )) error stop 27
        if ( .not. precision_r8 ( r_C_DOUBLE_s8c          , -1.797693D+308 )) error stop 28
        if ( .not. precision_r8 ( r_C_DOUBLE_s8d          , -2.225073D-308 )) error stop 29

        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a    ,  1.797693Q+308 )) error stop 30
        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8b    ,  2.004168Q-292 )) error stop 31
        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8c    , -1.797693Q+308 )) error stop 32
        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8d    , -2.004168Q-292 )) error stop 33


	CALL CSUB_REAL()

        if ( .not. precision_r4 ( real_s4d               ,  3.402823E+38  )) error stop 40
        if ( .not. precision_r4 ( real_s4c               ,  1.175494E-38  )) error stop 41
        if ( .not. precision_r4 ( real_s4b               , -3.402823E+38  )) error stop 42
        if ( .not. precision_r4 ( real_s4a               , -1.175494E-38  )) error stop 43

        if ( .not. precision_r8 ( real_s8d               ,  1.797693D+308 )) error stop 44
        if ( .not. precision_r8 ( real_s8c               ,  2.225073D-308 )) error stop 45
        if ( .not. precision_r8 ( real_s8b               , -1.797693D+308 )) error stop 46
        if ( .not. precision_r8 ( real_s8a               , -2.225073D-308 )) error stop 47

        if ( .not. precision_r4 ( r_C_FLOAT_s4d          ,  3.402823E+38  )) error stop 48
        if ( .not. precision_r4 ( r_C_FLOAT_s4c          ,  1.175494E-38  )) error stop 49
        if ( .not. precision_r4 ( r_C_FLOAT_s4b          , -3.402823E+38  )) error stop 50
        if ( .not. precision_r4 ( r_C_FLOAT_s4a          , -1.175494E-38  )) error stop 51

        if ( .not. precision_r8 ( r_C_DOUBLE_s8d         ,  1.797693D+308 )) error stop 52
        if ( .not. precision_r8 ( r_C_DOUBLE_s8c         ,  2.225073D-308 )) error stop 53
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b         , -1.797693D+308 )) error stop 54
        if ( .not. precision_r8 ( r_C_DOUBLE_s8a         , -2.225073D-308 )) error stop 55

        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8d    ,  1.797693Q+308 )) error stop 56
        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8c    ,  2.004168Q-292 )) error stop 57
        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8b    , -1.797693Q+308 )) error stop 58
        if ( .not. precision_r16 ( r_C_LONG_DOUBLE_s8a    , -2.004168Q-292 )) error stop 59

end program
