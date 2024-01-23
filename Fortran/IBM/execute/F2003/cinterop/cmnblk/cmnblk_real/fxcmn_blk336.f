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
!*  DESCRIPTION                : This test case will verify that array variables of
!*				 REAL data types inside of common blocks do
!*				 interoperate with C variables.
!*
!*                               This testcase will
!*                               verify 3-dimensional array variables inside of
!*                               BIND(C) common block.
!*
!*				 BIND(c) common block inside of module called by main program.
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
!	- use ISO_C_BINDING modules
!       - use non-default lower bounds (i.e test array not starting from 1)
! ----------------------------------------------------------------------------

	integer*4 		:: i, j, k
	integer*4,parameter 	:: N=2

	real , DIMENSION(-2:-1,-2:-1,-2:-1)	:: real_s4			!* real*4

	real (kind=INT((4.4e0_8,6.5e0_8))+4 ) 	:: real_s8(N,N,N)		!* real*8

        real (kind=o'20')                    	:: real_s16(0:1,0:1,0:1)	!* real*16

 	REAL (c_float), DIMENSION(N,N,N)	:: r_c_float_s4

 	REAL (c_double)				:: r_c_double_s8(-2:-1,-2:-1,-2:-1)


	!*** Comparison matrices
        real            :: cmp_real_s4(8), tmp_real_s4(-2:-1,-2:-1,-2:-1)
        real(8)         :: cmp_real_s8(8) , tmp_real_s8(N,N,N)
        real(16)        :: cmp_real_s16(8), tmp_real_s16(0:1,0:1,0:1)
        REAL            :: cmp_r_c_float_s4(8), tmp_r_c_float_s4(N,N,N)
        REAL(8)         :: cmp_r_c_double_s8(8), tmp_r_c_double_s8(-2:-1,-2:-1,-2:-1)


	!*** Resultant matrices
        real            :: res_real_s4(8)
        real(8)         :: res_real_s8(8)
        real(16)        :: res_real_s16(8)
        REAL            :: res_r_c_float_s4(8)
        REAL(8)         :: res_r_c_double_s8(8)

! ----------------------------------------------------------------------------
! One COMMON statement with multiple common blocks in one BIND(C) statements
! ----------------------------------------------------------------------------

        common /blk_real/   real_s4, real_s8, real_s16, r_c_float_s4, r_c_double_s8

        bind(c) ::   /blk_real/

end module fmod1


program fxcmn_blk336
	use fmod1
	implicit none
        logical precision_r4, precision_r8, precision_r16


! ----------------------------------------------------------------------------
! Real Initialization
!       - use max and min possible values for +ve and -ve numbers
! ----------------------------------------------------------------------------

        real_s4 	=  RESHAPE( (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/), (/2,2,2/), (/1.0,-1.0,-2.9/))

        real_s8 	=  -2.225073D-308

        real_s16 	=  RESHAPE( (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308/), (/2,2,2/), (/1.0Q0,-1.0Q0,-2.9Q0/))

        r_c_float_s4	=  RESHAPE( (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/), (/2,2,2/), (/1.0,-1.0,-2.9/))

        r_c_double_s8   =  RESHAPE( (/1.797693D+308, -2.225073D-308, 0.0D0, 2.225073D-308, -1.797693D+308/), (/2,2,2/), (/1.00000D+308, -1.00000D+308, -1.00000D+100/))

       !*** Comparison matrices
        cmp_real_s4	     = (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38, 1.0, -1.0, -2.9/)
        cmp_real_s8 	     = (/-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308,-2.225073D-308/)
        cmp_real_s16	     = (/1.797693Q+308, -2.225073Q-308, 0.0Q0, 2.225073Q-308, -1.797693Q+308, 1.0Q0, -1.0Q0, -2.9Q0/)
        cmp_r_c_float_s4     = (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38,1.0, -1.0, -2.9/)
        cmp_r_c_double_s8    = (/1.797693D+308, -2.225073D-308, 0.0D0, 2.225073D-308, -1.797693D+308, 1.00000D+308, -1.00000D+308, -1.00000D+100/)

        !*** Resultant matrices
        res_real_s4          = RESHAPE(real_s4, (/8/))
        res_real_s8          = RESHAPE(real_s8, (/8/))
        res_real_s16	     = RESHAPE(real_s16, (/8/))
        res_r_c_float_s4     = RESHAPE(r_c_float_s4, (/8/))
        res_r_c_double_s8    = RESHAPE(r_c_double_s8,(/8/))


! ----------------------------------------------------------------------------
! Real Verification
!       - verify assigned values before passing to C
! ----------------------------------------------------------------------------

        do i = 1, 8
           if ( .not. precision_r4( res_real_s4(i)	,  cmp_real_s4(i) )) 		error stop 5
           if ( .not. precision_r8( res_real_s8(i)	,  cmp_real_s8(i) )) 		error stop 6
           if ( .not. precision_r16(res_real_s16(i)	,  cmp_real_s16(i) )) 		error stop 7
           if ( .not. precision_r4( res_r_c_float_s4(i)	,  cmp_r_c_float_s4(i) )) 	error stop 8
           if ( .not. precision_r8( res_r_c_double_s8(i),  cmp_r_c_double_s8(i) )) 	error stop 9
	end do


        !*** Temporary for comparison purposes (switching first dimension with the third)
        do k = -2, -1
           do j = -2, -1
              do i = -2, -1

                tmp_real_s4(i,j,k)              = real_s4(k,j,i)
                tmp_r_c_double_s8(i,j,k)        = r_c_double_s8(k,j,i)

                tmp_real_s8(i+3,j+3,k+3)        = real_s8(k+3,j+3,i+3)
                tmp_r_c_float_s4(i+3,j+3,k+3)   = r_c_float_s4(k+3,j+3,i+3)

                tmp_real_s16(i+2,j+2,k+2)       = real_s16(k+2,j+2,i+2)

            end do
          end do
        end do

! ----------------------------------------------------------------------------
!  Call to C subprogram
! ----------------------------------------------------------------------------
	CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
!       - verify values passed back from C
! ----------------------------------------------------------------------------

       !*** Comparison matrices
        cmp_real_s4             = RESHAPE( tmp_real_s4, (/8/))
        cmp_real_s8             = RESHAPE( tmp_real_s8, (/8/))
        cmp_real_s16            = RESHAPE( tmp_real_s16, (/8/))
        cmp_r_c_float_s4        = RESHAPE( tmp_r_c_float_s4, (/8/))
        cmp_r_c_double_s8       = RESHAPE( tmp_r_c_double_s8, (/8/))

        !*** Resultant matrices
        res_real_s4          = RESHAPE(real_s4, (/8/))
        res_real_s8          = RESHAPE(real_s8, (/8/))
        res_real_s16         = RESHAPE(real_s16, (/8/))
        res_r_c_float_s4     = RESHAPE(r_c_float_s4, (/8/))
        res_r_c_double_s8    = RESHAPE(r_c_double_s8,(/8/))

        do i = 1, 8
           if ( .not. precision_r4( res_real_s4(i)      ,  cmp_real_s4(i) ))            call zzrc(10+i)
           if ( .not. precision_r8( res_real_s8(i)      ,  cmp_real_s8(i) ))            call zzrc(20+i)
           if ( .not. precision_r16(res_real_s16(i)     ,  cmp_real_s16(i) ))           call zzrc(30+i)
           if ( .not. precision_r4( res_r_c_float_s4(i) ,  cmp_r_c_float_s4(i) ))       call zzrc(40+i)
           if ( .not. precision_r8( res_r_c_double_s8(i),  cmp_r_c_double_s8(i) ))      call zzrc(50+i)
        end do

end program
