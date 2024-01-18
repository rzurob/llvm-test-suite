
!*********************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 30, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_datatype()
!*
!*  REFERENCE                  : Feature 180920
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This testcase will verify that
!*				 ieee_support_datatype() works fine
!*				 for REAL variables.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	@process autodbl(none)
	program supdtype01
       	  use ieee_arithmetic

          real 			dtype_r(5)
          real*4  		dtype_r4(5)
          real*8  		dtype_r8(5)
          real*16 		dtype_r16(5)
          double precision  	dtype_dp(5)

	  dtype_r(1)	=	tiny(1.0)
          dtype_r(2)    =       5.0
          dtype_r(3)    =       huge(1.0)
          dtype_r(4)    =       tiny(1.0) - tiny(1.0)
          dtype_r(5)    =       huge(1.0) + tiny(1.0)

	  dtype_r4(1)	=	tiny(1.0)
          dtype_r4(2)    =       5.0
          dtype_r4(3)    =       huge(1.0)
          dtype_r4(4)    =       tiny(1.0) - tiny(1.0)
          dtype_r4(5)    =       huge(1.0) + tiny(1.0)

	  dtype_r8(1)	=	tiny(1.0d0)
          dtype_r8(2)    =       5.0d0
          dtype_r8(3)    =       huge(1.0d0)
          dtype_r8(4)    =       tiny(1.0d0) - tiny(1.0d0)
          dtype_r8(5)    =       huge(1.0d0) + tiny(1.0d0)

	  dtype_r16(1)	=	tiny(1.0q0)
          dtype_r16(2)   =       5.0q0
          dtype_r16(3)   =       huge(1.0q0)
          dtype_r16(4)   =       tiny(1.0q0) - tiny(1.0q0)
          dtype_r16(5)   =       huge(1.0q0) + tiny(1.0q0)

	  dtype_dp(1)	=	tiny(1.0d0)
          dtype_dp(2)    =       5.0d0
          dtype_dp(3)    =       huge(1.0d0)
          dtype_dp(4)    =       tiny(1.0d0) - tiny(1.0d0)
          dtype_dp(5)    =       huge(1.0d0) + tiny(1.0d0)

!****************************************************************************
!* Verify that REAL, REAL*4, REAL*8, and DOUBLE PRECISION data are
!* supported inside main program.
!* Syntax: IEEE_SUPPORT_DATATYPE(X)
!****************************************************************************

!* If X is absent, the result has the value false.
    	  if (ieee_support_datatype()) 				error stop 1

!* default REAL should result in true.
          if (.not. ieee_support_datatype(dtype_r))            	error stop 25

          if (.not. ieee_support_datatype(dtype_r(1)))		error stop 2
          if (.not. ieee_support_datatype(dtype_r(2)))		error stop 3
          if (.not. ieee_support_datatype(dtype_r(3)))		error stop 4
          if (.not. ieee_support_datatype(dtype_r(4)))		error stop 5
          if (.not. ieee_support_datatype(dtype_r(5)))		error stop 6

!* REAL*4 should result in true.
          if (.not. ieee_support_datatype(dtype_r4))  		error stop 75

          if (.not. ieee_support_datatype(dtype_r4(1)))  	error stop 7
          if (.not. ieee_support_datatype(dtype_r4(2)))         error stop 8
          if (.not. ieee_support_datatype(dtype_r4(3)))         error stop 9
          if (.not. ieee_support_datatype(dtype_r4(4)))         error stop 10
          if (.not. ieee_support_datatype(dtype_r4(5)))         error stop 11


!* REAL*8 should result in true.
          if (.not. ieee_support_datatype(dtype_r8))         	error stop 125

          if (.not. ieee_support_datatype(dtype_r8(1)))         error stop 12
          if (.not. ieee_support_datatype(dtype_r8(2)))         error stop 13
          if (.not. ieee_support_datatype(dtype_r8(3)))         error stop 14
          if (.not. ieee_support_datatype(dtype_r8(4)))         error stop 15
          if (.not. ieee_support_datatype(dtype_r8(5)))         error stop 16

!* REAL*16 should result in false.
          if (ieee_support_datatype(dtype_r16))           	error stop 175

          if (ieee_support_datatype(dtype_r16(1)))           	error stop 17
          if (ieee_support_datatype(dtype_r16(2)))           	error stop 18
          if (ieee_support_datatype(dtype_r16(3)))           	error stop 19
          if (ieee_support_datatype(dtype_r16(4)))           	error stop 20
          if (ieee_support_datatype(dtype_r16(5)))           	error stop 21

!* DOUBLE PRECISION should result in true.
          if (.not. ieee_support_datatype(dtype_dp))         	error stop 225

          if (.not. ieee_support_datatype(dtype_dp(1)))         error stop 22
          if (.not. ieee_support_datatype(dtype_dp(2)))         error stop 23
          if (.not. ieee_support_datatype(dtype_dp(3)))         error stop 24
          if (.not. ieee_support_datatype(dtype_dp(4)))         error stop 25
          if (.not. ieee_support_datatype(dtype_dp(5)))         error stop 26

	end program
