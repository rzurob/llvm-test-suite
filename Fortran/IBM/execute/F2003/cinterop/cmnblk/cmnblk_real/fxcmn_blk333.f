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
!*  DESCRIPTION                : This test case will verify that variables of
!*				 REAL data types inside of common blocks do
!*				 interoperate with C variables inside Fortran  module called by main
!*				 program.
!*
!*                              This testcase will verify 1-D array variables.
!*
!*                               This test case requires C code to be compiled
!*                               with -qlongdouble option in order to support real*16.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module fmod1
	use iso_c_binding
        implicit none

! ----------------------------------------------------------------------------
! Real Array Declaration
!     	- use KIND, MAX, LEN, INT
!	- use ISO_C_BINDING modules
!       - use non-default lower bounds (i.e test array not starting from 1)
! ----------------------------------------------------------------------------

	integer*4 		:: i
	integer*4,parameter 	:: N=5

	real (kind=o'004'), DIMENSION(-5:-1)	:: real_s4a
	real (LEN('Kobi'))			:: real_s4b(N)
        real                        		:: real_s4c(-2:2)

	real (kind=MAX(8, 7)), DIMENSION(-5:-1)	:: real_s8a
	real (kind=INT((4.4e0_8,6.5e0_8))+4 ) 	:: real_s8b(N)
        real (KIND=O'010')      		:: real_s8c(-2:2)

	real (16  )	, DIMENSION(-5:-1)	:: real_s16a
	real (kind=b'10000'), DIMENSION(N) 	:: real_s16b
        real (kind=o'20')                    	:: real_s16c(-2:2)

 	REAL (C_FLOAT 	), DIMENSION(-5:-1)	:: r_C_FLOAT_s4a
 	REAL (C_FLOAT 	)			:: r_C_FLOAT_s4b(N)
 	REAL (C_FLOAT 	)			:: r_C_FLOAT_s4c(-2:2)

 	REAL ( C_DOUBLE ) , DIMENSION(-5:-1)	:: r_C_DOUBLE_s8a
 	REAL (C_DOUBLE 	), DIMENSION(N)		:: r_C_DOUBLE_s8b
 	REAL (C_DOUBLE 	)			:: r_C_DOUBLE_s8c(-2:2)


! ----------------------------------------------------------------------------
! COMMON and BIND(C) statements
! ----------------------------------------------------------------------------

        common /blk_real/       real_s4a, real_s4b, real_s4c,  			&
				real_s8a, real_s8b, real_s8c,  			&
				real_s16a, real_s16b, real_s16c, 		&
                                r_C_FLOAT_s4a, r_C_FLOAT_s4b, r_C_FLOAT_s4c,  	&
				r_C_DOUBLE_s8a, r_C_DOUBLE_s8b, r_C_DOUBLE_s8c


        bind(c) ::   /blk_real/

end module fmod1


program fxcmn_blk333
	use fmod1
	implicit none
	logical precision_r4, precision_r8, precision_r16


! ----------------------------------------------------------------------------
! Real Initialization
!       - use max and min possible values for +ve and -ve numbers
! ----------------------------------------------------------------------------

        real_s4a 			=  (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/)
        real_s4b 			=  -1.175494E-38
        real_s4c 			=  (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/)

        real_s8a 			=  (/1.797693D+308, -2.225073D-308, 0.0D0, 2.225073D-308, -1.797693D+308/)
        real_s8b 			=  (/1.797693D+308, -2.225073D-308, 0.0D0, 2.225073D-308, -1.797693D+308/)
        real_s8c                        = -1.797693D+308

        real_s16a 			=  (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308/)
        real_s16b                       = -2.225073Q-308
        real_s16c 			=  (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308/)

        r_C_FLOAT_s4a			=  (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/)
        r_C_FLOAT_s4b			=  1.175494E-38
        r_C_FLOAT_s4c			= -3.402823E+38

        r_C_DOUBLE_s8a			=  1.797693D+308
        r_C_DOUBLE_s8b			=  (/1.797693D+308, -2.225073D-308, 0.0D0, 2.225073D-308, -1.797693D+308/)
        r_C_DOUBLE_s8c			= -1.797693D+308


! ----------------------------------------------------------------------------
! Real Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------
        if ( .not. precision_r4 ( real_s4a(-5)             ,  3.402823E+38 )) error stop 10
        if ( .not. precision_r4 ( real_s4a(-4)             , -1.175494E-38 )) error stop 10
        if ( .not. precision_r4 ( real_s4a(-3)             ,  0.0          )) error stop 10
        if ( .not. precision_r4 ( real_s4a(-2)             ,  1.175494E-38 )) error stop 10
        if ( .not. precision_r4 ( real_s4a(-1)             , -3.402823E+38 )) error stop 10

        do i = 1, 5
           if (.not. precision_r4 ( real_s4b(i)            ,  -1.175494E-38 )) error stop 11
        end do

        if ( .not. precision_r4 ( real_s4c(-2)             ,  3.402823E+38 )) error stop 12
        if ( .not. precision_r4 ( real_s4c(-1)             , -1.175494E-38 )) error stop 12
        if ( .not. precision_r4 ( real_s4c(0)              ,  0.0          )) error stop 12
        if ( .not. precision_r4 ( real_s4c(1)              ,  1.175494E-38 )) error stop 12
        if ( .not. precision_r4 ( real_s4c(2)              , -3.402823E+38 )) error stop 12

        if ( .not. precision_r8 ( real_s8a(-5)             ,  1.797693D+308 )) error stop 13
        if ( .not. precision_r8 ( real_s8a(-4)             , -2.225073D-308 )) error stop 13
        if ( .not. precision_r8 ( real_s8a(-3)             ,  0.0D0         )) error stop 13
        if ( .not. precision_r8 ( real_s8a(-2)             ,  2.225073D-308 )) error stop 13
        if ( .not. precision_r8 ( real_s8a(-1)             , -1.797693D+308 )) error stop 13

        if ( .not. precision_r8 ( real_s8b(1)              ,  1.797693D+308 )) error stop 14
        if ( .not. precision_r8 ( real_s8b(2)              , -2.225073D-308 )) error stop 14
        if ( .not. precision_r8 ( real_s8b(3)              ,  0.0D0         )) error stop 14
        if ( .not. precision_r8 ( real_s8b(4)              ,  2.225073D-308 )) error stop 14
        if ( .not. precision_r8 ( real_s8b(5)              , -1.797693D+308 )) error stop 14

        do i = -2, 2
           if (.not. precision_r8 ( real_s8c(i)            , -1.797693D+308 )) error stop 15
        end do

        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-5)        ,  3.402823E+38  )) error stop 16
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-4)        , -1.175494E-38  )) error stop 16
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-3)        ,  0.0           )) error stop 16
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-2)        ,  1.175494E-38  )) error stop 16
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-1)        , -3.402823E+38  )) error stop 16

        do i = 1, 5
           if (.not. precision_r4 ( r_C_FLOAT_s4b(i)       ,  1.175494E-38  )) error stop 17
        end do

        do i = -2, 2
           if (.not. precision_r4 ( r_C_FLOAT_s4c(i)       , -3.402823E+38  )) error stop 18
        end do

        do i = -5, -1
           if (.not. precision_r8 ( r_C_DOUBLE_s8a(i)      , 1.797693D+308 ))  error stop 19
        end do


        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(1)       ,  1.797693D+308 )) error stop 20
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(2)       , -2.225073D-308 )) error stop 20
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(3)       ,  0.0D0         )) error stop 20
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(4)       ,  2.225073D-308 )) error stop 20
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(5)       , -1.797693D+308 )) error stop 20

        do i = -2, 2
           if ( .not. precision_r8 ( r_C_DOUBLE_s8c(i)    , -1.797693D+308 )) error stop 21
        end do

        if ( .not. precision_r16 ( real_s16a(-5)           ,  1.797693Q+308 )) error stop 22
        if ( .not. precision_r16 ( real_s16a(-4)           , -2.225073Q-308 )) error stop 22
        if ( .not. precision_r16 ( real_s16a(-3)           ,  0.0Q0 	    )) error stop 22
        if ( .not. precision_r16 ( real_s16a(-2)           ,  2.225073Q-308 )) error stop 22
        if ( .not. precision_r16 ( real_s16a(-1)           , -1.797693Q+308 )) error stop 22

        do i = 1, 5
           if (.not. precision_r16 ( real_s16b(i)   	   , -2.225073Q-308 )) error stop 23
        end do

        if ( .not. precision_r16 ( real_s16c(-2)           ,  1.797693Q+308 )) error stop 24
        if ( .not. precision_r16 ( real_s16c(-1)           , -2.225073Q-308 )) error stop 24
        if ( .not. precision_r16 ( real_s16c(0)            ,  0.0Q0         )) error stop 24
        if ( .not. precision_r16 ( real_s16c(1)            ,  2.225073Q-308 )) error stop 24
        if ( .not. precision_r16 ( real_s16c(2)            , -1.797693Q+308 )) error stop 24


! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
	CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

        if ( .not. precision_r4 ( real_s4a(-1)             ,  3.402823E+38 )) error stop 30
        if ( .not. precision_r4 ( real_s4a(-2)             , -1.175494E-38 )) error stop 31
        if ( .not. precision_r4 ( real_s4a(-3)             ,  0.0          )) error stop 32
        if ( .not. precision_r4 ( real_s4a(-4)             ,  1.175494E-38 )) error stop 33
        if ( .not. precision_r4 ( real_s4a(-5)             , -3.402823E+38 )) error stop 34

        do i = 1, 5
           if (.not. precision_r4 ( real_s4b(i)            ,  1.175494E-38 )) call zzrc(34+i)
        end do

        if ( .not. precision_r4 ( real_s4c(2)             ,  3.402823E+38 )) error stop 40
        if ( .not. precision_r4 ( real_s4c(1)             , -1.175494E-38 )) error stop 41
        if ( .not. precision_r4 ( real_s4c(0)             ,  0.0          )) error stop 42
        if ( .not. precision_r4 ( real_s4c(-1)            ,  1.175494E-38 )) error stop 43
        if ( .not. precision_r4 ( real_s4c(-2)            , -3.402823E+38 )) error stop 44

        if ( .not. precision_r8 ( real_s8a(-1)             ,  1.797693D+308 )) error stop 45
        if ( .not. precision_r8 ( real_s8a(-2)             , -2.225073D-308 )) error stop 46
        if ( .not. precision_r8 ( real_s8a(-3)             ,  0.0D0         )) error stop 47
        if ( .not. precision_r8 ( real_s8a(-4)             ,  2.225073D-308 )) error stop 48
        if ( .not. precision_r8 ( real_s8a(-5)             , -1.797693D+308 )) error stop 49

        if ( .not. precision_r8 ( real_s8b(5)              ,  1.797693D+308 )) error stop 50
        if ( .not. precision_r8 ( real_s8b(4)              , -2.225073D-308 )) error stop 51
        if ( .not. precision_r8 ( real_s8b(3)              ,  0.0D0         )) error stop 52
        if ( .not. precision_r8 ( real_s8b(2)              ,  2.225073D-308 )) error stop 53
        if ( .not. precision_r8 ( real_s8b(1)              , -1.797693D+308 )) error stop 54

        do i = -2, 2
           if (.not. precision_r8 ( real_s8c(i)            ,  1.797693D+308 )) call zzrc(54+3+i)
        end do

        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-1)        ,  3.402823E+38  )) error stop 60
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-2)        , -1.175494E-38  )) error stop 61
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-3)        ,  0.0           )) error stop 62
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-4)        ,  1.175494E-38  )) error stop 63
        if ( .not. precision_r4 ( r_C_FLOAT_s4a(-5)        , -3.402823E+38  )) error stop 64

        do i = 1, 5
           if (.not. precision_r4 ( r_C_FLOAT_s4b(i)      , -1.175494E-38  )) call zzrc(64+i)
        end do

        do i = -2, 2
           if (.not. precision_r4 ( r_C_FLOAT_s4c(i)      , 3.402823E+38   )) call zzrc(69+3+i)
        end do

        do i = -5, -1
           if (.not. precision_r8 ( r_C_DOUBLE_s8a(i)     , -1.797693D+308 )) call zzrc(74+6+i)
        end do

        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(5)       ,  1.797693D+308 )) error stop 80
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(4)       , -2.225073D-308 )) error stop 81
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(3)       ,  0.0D0         )) error stop 82
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(2)       ,  2.225073D-308 )) error stop 83
        if ( .not. precision_r8 ( r_C_DOUBLE_s8b(1)       , -1.797693D+308 )) error stop 84

        do i = -2, 2
           if ( .not. precision_r8 ( r_C_DOUBLE_s8c(i)    ,  1.797693D+308 )) call zzrc(84+3+i)
        end do

        if ( .not. precision_r16 ( real_s16a(-1)           ,  1.797693Q+308 )) error stop 90
        if ( .not. precision_r16 ( real_s16a(-2)           , -2.225073Q-308 )) error stop 91
        if ( .not. precision_r16 ( real_s16a(-3)           ,  0.0Q0         )) error stop 92
        if ( .not. precision_r16 ( real_s16a(-4)           ,  2.225073Q-308 )) error stop 93
        if ( .not. precision_r16 ( real_s16a(-5)           , -1.797693Q+308 )) error stop 94

        do i = 1, 5
           if (.not. precision_r16 ( real_s16b(i)          ,  2.225073Q-308 )) call zzrc(94+i)
        end do

        if ( .not. precision_r16 ( real_s16c(2)            ,  1.797693Q+308 )) error stop 100
        if ( .not. precision_r16 ( real_s16c(1)            , -2.225073Q-308 )) error stop 101
        if ( .not. precision_r16 ( real_s16c(0)            ,  0.0Q0         )) error stop 102
        if ( .not. precision_r16 ( real_s16c(-1)           ,  2.225073Q-308 )) error stop 103
        if ( .not. precision_r16 ( real_s16c(-2)           , -1.797693Q+308 )) error stop 104


end program
