! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sep. 24, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test the interoperability of drived type module
!*                              : variable which contains character, real, logical
!*                              : type variable. Variables are called by cmain.
!*                              : Use module iso_c_binding.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
use iso_c_binding
  type, bind(c) :: dt
    character(c_char) :: ch
    real(c_float) :: a
    logical(c_bool) :: lo
    real(c_double) :: b
  end type

end module

subroutine fsub(z)
  use mod
  type(dt) :: z

  logical precision_r4
  logical precision_r8




IF ( .not.precision_r4(z%a, 0.0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r8(z%b, 0.0D0) ) THEN
  ERROR STOP 52
END IF

IF ( z%ch .NE. 'a') THEN
  ERROR STOP 53
END IF

IF ( z%lo .NEQV. .true. ) THEN
  ERROR STOP 54
END IF

z = dt('b', 2.0, .false., 2.0D0)

end


