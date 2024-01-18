!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/cmn_blk001.sh fxcmn_blk033a cxcmn_blk033
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: rm -f *.o *.mod fxcmn_blk033a fxcmn_blk033a.out
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
!*  REFERENCE                  : Feature 239812
!*
!*  DRIVER STANZA              : xlf95, xlc
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This test case will verify that a single variable inside of
!*                               BIND(C) common block with a binding label is interoperable
!*                               with a C variable that is not in a structure.
!*
!*                               Data type being tested: real*8
!*					
!*                               Test: BIND(C) common block in internal subroutine 
!*					
!* ===================================================================
!*  REVISION HISTORY					
!*  MM/DD/YY:  Init:  Comments:			
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk033a
	use iso_c_binding
        implicit none

	call intern_fsub()

   CONTAINS

   subroutine intern_fsub()
	use iso_c_binding
        implicit none
	logical precision_r4, precision_r8, precision_r16

	real (kind=INT((4.4e0_8,6.5e0_8))+4 ) 	:: Real1

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed characters
! ----------------------------------------------------------------------------
        common /blk1/    Real1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

        Real1  =  -( TINY(Real1) )  

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

!        Issue with the conversion of the literal into a binary number
!        !*** Verify values before passing to C; camparing with precision_r8 will cause comparison to fail.
!        if ( .not. precision_r4 ( Real1     ,  -2.225073D-308  )) error stop 5


	!***  Call to C subprogram
	CALL CSUB_REAL()


        !*** Verify values that were modified by C subprogram
        if ( .not. precision_r8 ( Real1     ,  1.797693D+308  )) error stop 10

   end subroutine

end program
