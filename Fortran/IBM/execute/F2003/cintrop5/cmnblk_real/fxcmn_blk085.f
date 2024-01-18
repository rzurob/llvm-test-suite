!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk_qlngdbl.sh fxcmn_blk085 cxcmn_blk085
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk085 fxcmn_blk085.out
! %END
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
!*  DESCRIPTION                : This test case will verify that 1-dimensional array
!*                               variables inside of common blocks are interoperable
!*                               with C variables that are not inside of a structure.
!*
!*                               Data type being tested:  real
!*
!*                               Scope being tested:  main program
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk085
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

        integer*4               :: i
        integer*4,parameter     :: N=5


! ----------------------------------------------------------------------------
! Real Array Declaration
! ----------------------------------------------------------------------------
         real                                    :: real_s4a(-5:-1)

! ----------------------------------------------------------------------------
! One COMMON statement with one common block in one BIND(C) statement that has a binding label
! ----------------------------------------------------------------------------

         common /blk_real_s4a/          real_s4a

         bind(c, Name='_L_a_b_e_l_4_U') ::   /blk_real_s4a/

! ----------------------------------------------------------------------------
! Real Initialization
! ----------------------------------------------------------------------------

         real_s4a                        =  (/3.402823E+38, -1.175494E-38, 0.0, 1.175494E-38, -3.402823E+38/)

! ----------------------------------------------------------------------------
! Real Verification
! - verify assigned values before passing to C
! ----------------------------------------------------------------------------

         if ( .not. precision_r4 ( real_s4a(-5)             ,  3.402823E+38 )) error stop 10
         if ( .not. precision_r4 ( real_s4a(-4)             , -1.175494E-38 )) error stop 11
         if ( .not. precision_r4 ( real_s4a(-3)             ,  0.0          )) error stop 12
         if ( .not. precision_r4 ( real_s4a(-2)             ,  1.175494E-38 )) error stop 13
         if ( .not. precision_r4 ( real_s4a(-1)             , -3.402823E+38 )) error stop 14


! ----------------------------------------------------------------------------
! Call to C subprogram
! ----------------------------------------------------------------------------

       CALL CSUB_REAL()


! ----------------------------------------------------------------------------
! Real Verification
! - verify values passed back from C
! ----------------------------------------------------------------------------

         if ( .not. precision_r4 ( real_s4a(-1)             ,  3.402823E+38 )) error stop 20
         if ( .not. precision_r4 ( real_s4a(-2)             , -1.175494E-38 )) error stop 21
         if ( .not. precision_r4 ( real_s4a(-3)             ,  0.0          )) error stop 22
         if ( .not. precision_r4 ( real_s4a(-4)             ,  1.175494E-38 )) error stop 23
         if ( .not. precision_r4 ( real_s4a(-5)             , -3.402823E+38 )) error stop 24


end program
