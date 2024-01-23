!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : February 13, 2004
!*
!*  REFERENCE                  : Feature 239812
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This test case will verify that a 1-dimensional array variable
!*                               inside of BIND(C) common block with a binding label is
!*                               interoperable with a C variable that is not in a structure.
!*
!*                               Data type being tested: logical(C_BOOL)
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


program fxcmn_blk052
	use iso_c_binding
        implicit none

	logical(C_BOOL)  :: log1(5)

! ----------------------------------------------------------------------------
!      COMMON block with one variable in it;
!      BIND(C) statement with a binding label in mixed logical(C_BOOL)s
! ----------------------------------------------------------------------------
        common /blk1/    log1
        bind(c, name='Bnd_Lbl15') ::       /blk1/

        log1  = (/.true., .false., .true., .false., .true./)

! ----------------------------------------------------------------------------
!      1) Check values before passing to C sub-program
!      2) Pass values into C sub-program
!      3) Check values passed back from C sub-program after modifications
! ----------------------------------------------------------------------------

        !*** Verify values before passing to C
        if ( log1(1)   .neqv.  .true.  ) 	error stop 5
        if ( log1(2)   .neqv.  .false. )    	error stop 6
        if ( log1(3)   .neqv.  .true.  )   	error stop 7
        if ( log1(4)   .neqv.  .false. )  	error stop 8
        if ( log1(5)   .neqv.  .true.  ) 	error stop 9



	!***  Call to C subprogram
	CALL CSUB1()


        !*** Verify values that were modified by C subprogram
        if ( log1(1)   .neqv.  .true.  )        error stop 10
        if ( log1(2)   .neqv.  .false. )        error stop 11
        if ( log1(3)   .neqv.  .false. )        error stop 12
        if ( log1(4)   .neqv.  .true.  )        error stop 13
        if ( log1(5)   .neqv.  .true.  )        error stop 14


end program

