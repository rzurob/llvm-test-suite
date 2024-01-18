!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90 -qstrict
! %GROUP: ieeemisc23.f
! %VERIFY:
! %STDIN:
! %STDOUT: ieeemisc23.out
! %EXECARGS:
! %POSTCMD: rm -f ieeemisc23.out
! %END
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*  TEST CASE TITLE            : ieeemisc23.f
!*
!*  PROGRAMMER                 : Kobi Vinayagamoorthy
!*  DATE                       : April 15, 2002
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : nint()
!*				 ieee_invalid
!*
!*  REFERENCE                  : Feature 180920
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This testcase verifies that when the
!*				 maximum integer is passed to the 
!*				 NINT function, ieee_invalid flag
!*				 is not set to true.  
!*				 The testcase also verifies that all other 
!*				 flags also remain clear and false.
!*
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

	program ieeemisc23
	  use ieee_arithmetic
	  implicit none

	  logical*4 flag_values(5)
	
          integer                 int0
          integer*1               int1
          integer*2               int2
          integer*4               int4
          integer*8               int8

!*  Maximum Integers
          integer(4), parameter :: imaxint_4 = z"7fffffff"
          integer(8), parameter :: imaxint_8 = z"7fffffffffffffff"
          
!*  NOTE: imaxint_4 can't be represented exactly in a 4-byte real.  The value
!*        in the real is an estimate that is slightly larger.
!*        imaxint_8 can't be represented exactly in an 8-byte real.  The value
!*        in the real is a slightly larger estimate.
!*        integer*8:  9223372036854775807
!*        real*8:   0.922337203685477581E+19
!*
!*        For MAXINT_4, we'll use the converted value from imaxint_4.  Since
!*        this value is slightly larger than the maximum integer, it will set
!*        the ieee_invalid flag.
!*
!*        (same thing for MAXINT_8)

       	  real(4), parameter :: MAXINT_4 = imaxint_4
          real(8), parameter :: MAXINT_8 = imaxint_8

!***********************************************************************
!*  Note:
!*    Exception flags:
!*       flag_values(1) = IEEE_OVERFLOW flag
!*       flag_values(2) = IEEE_DIVIDE_BY_ZERO flag
!*       flag_values(3) = IEEE_INVALID flag
!*       flag_values(4) = IEEE_UNDERFLOW flag
!*       flag_values(5) = IEEE_INEXACT flag
!*
!***********************************************************************

!***********************************************************************
!*  Check to see that maximum integer will not cause IEEE invalid exception 
!*  flag to signal. 
!***********************************************************************
!*  default integer and real(4) 
	  int0 = nint(MAXINT_4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)        	error stop 1
          if (flag_values(2) .neqv. .false.)            error stop 2
          if (flag_values(3) .neqv. .true.)             error stop 3
          if (flag_values(4) .neqv. .false.)            error stop 4
          if (flag_values(5) .neqv. .false.)            error stop 5
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(1) and real(4) 
	  int1 = nint(MAXINT_4,1)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 7
          if (flag_values(2) .neqv. .false.)            error stop 8
          if (flag_values(3) .neqv. .true.)             error stop 9
          if (flag_values(4) .neqv. .false.)            error stop 10
          if (flag_values(5) .neqv. .false.)            error stop 11
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(2) and real(4)  
          int2 = nint(MAXINT_4,2)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 13
          if (flag_values(2) .neqv. .false.)            error stop 14
          if (flag_values(3) .neqv. .true.)             error stop 15
          if (flag_values(4) .neqv. .false.)            error stop 16
          if (flag_values(5) .neqv. .false.)            error stop 17
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(4) and real(4) 
          int4 = nint(MAXINT_4,4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 19
          if (flag_values(2) .neqv. .false.)            error stop 20
          if (flag_values(3) .neqv. .true.)             error stop 21
          if (flag_values(4) .neqv. .false.)            error stop 22
          if (flag_values(5) .neqv. .false.)            error stop 23
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(8) and real(4) 
          int8 = nint(MAXINT_4,8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 25
          if (flag_values(2) .neqv. .false.)            error stop 26
          if (flag_values(3) .neqv. .false.)            error stop 27
          if (flag_values(4) .neqv. .false.)            error stop 28
          if (flag_values(5) .neqv. .false.)            error stop 29
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  default integer and real(8) 
          int0 = nint(MAXINT_8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 31
          if (flag_values(2) .neqv. .false.)            error stop 32
          if (flag_values(3) .neqv. .true.)             error stop 33
          if (flag_values(4) .neqv. .false.)            error stop 34
          if (flag_values(5) .neqv. .false.)            error stop 35
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(1) and real(8) 
          int1 = nint(MAXINT_8,1)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 37
          if (flag_values(2) .neqv. .false.)            error stop 38
          if (flag_values(3) .neqv. .true.)             error stop 39
          if (flag_values(4) .neqv. .false.)            error stop 40
          if (flag_values(5) .neqv. .false.)            error stop 41
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(2) and real(8) 
          int2 = nint(MAXINT_8,2)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 43
          if (flag_values(2) .neqv. .false.)            error stop 44
          if (flag_values(3) .neqv. .true.)             error stop 45
          if (flag_values(4) .neqv. .false.)            error stop 46
          if (flag_values(5) .neqv. .false.)            error stop 47
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(4) and real(8)
          int4 = nint(MAXINT_8,4)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 49
          if (flag_values(2) .neqv. .false.)            error stop 50
          if (flag_values(3) .neqv. .true.)             error stop 51
          if (flag_values(4) .neqv. .false.)            error stop 52
          if (flag_values(5) .neqv. .false.)            error stop 53
          
          call ieee_set_flag(ieee_all, .false.)

!* --------------------------------------------------------------------
!*  integer(8) and real(8)
          int8 = nint(MAXINT_8,8)
          call ieee_get_flag(ieee_all, flag_values)

          if (flag_values(1) .neqv. .false.)            error stop 55
          if (flag_values(2) .neqv. .false.)            error stop 56
          if (flag_values(3) .neqv. .true.)             error stop 57
          if (flag_values(4) .neqv. .false.)            error stop 58
          if (flag_values(5) .neqv. .false.)            error stop 59

!* --------------------------------------------------------------------
        end program
!=======================================================================

