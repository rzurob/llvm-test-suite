!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: supdtype04.f
! %VERIFY:
! %STDIN:
! %STDOUT: supdtype04.out
! %EXECARGS:
! %POSTCMD: rm -f supdtype04.out
! %END
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
!*				 for REAL variables inside external
!*				 subroutine, when autodbl(dbl)
!*				 option is invoked.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	program supdtype04

	  interface
                subroutine ext_sub1()
                        use ieee_arithmetic
                end subroutine
          end interface

          call ext_sub1()

        end program

	@process autodbl(dbl)
        subroutine ext_sub1()
          use ieee_arithmetic

          real 			dtype_r(5)
          real*4  		dtype_r4(5)
          real*8  		dtype_r8(5)
          real*16 		dtype_r16(5)
          double precision  	dtype_dp(5)

!****************************************************************************
!* Verify that only REAL and REAL*4 data are supported.
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


!* REAL*8 should result in false.
          if (ieee_support_datatype(dtype_r8))         		error stop 125

          if (ieee_support_datatype(dtype_r8(1)))         	error stop 12
          if (ieee_support_datatype(dtype_r8(2)))         	error stop 13
          if (ieee_support_datatype(dtype_r8(3)))         	error stop 14
          if (ieee_support_datatype(dtype_r8(4)))         	error stop 15
          if (ieee_support_datatype(dtype_r8(5)))         	error stop 16

!* REAL*16 should result in false.
          if (ieee_support_datatype(dtype_r16))           	error stop 175

          if (ieee_support_datatype(dtype_r16(1)))           	error stop 17
          if (ieee_support_datatype(dtype_r16(2)))           	error stop 18
          if (ieee_support_datatype(dtype_r16(3)))           	error stop 19
          if (ieee_support_datatype(dtype_r16(4)))           	error stop 20
          if (ieee_support_datatype(dtype_r16(5)))           	error stop 21

!* DOUBLE PRECISION should result in false.
          if (ieee_support_datatype(dtype_dp))         		error stop 225

          if (ieee_support_datatype(dtype_dp(1)))        	error stop 22
          if (ieee_support_datatype(dtype_dp(2)))         	error stop 23
          if (ieee_support_datatype(dtype_dp(3)))         	error stop 24
          if (ieee_support_datatype(dtype_dp(4)))         	error stop 25
          if (ieee_support_datatype(dtype_dp(5)))         	error stop 26

	end subroutine
