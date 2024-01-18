! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvr03 cxmdvr01
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!* ===================================================================
!*
!
!* DATE                         : Sep. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test the interoperability of drived type module
!*                              : variable which contains character, real, logical
!*                              : type variable. Use module iso_c_binding
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  use iso_c_binding
  type, bind(c) :: dt
    character(c_char) :: ch
    real(c_float) :: a
    logical(c_bool) :: lo
    real(c_double) :: b
  end type
  type(dt) :: z
end module


  use mod

  logical precision_r4
  logical precision_r8


  interface
    subroutine csub(x)
      use mod
      type(dt) :: x
    end subroutine
  end interface



  z = dt('a', 0.0, .true., 0.0D0)
    call csub(z)

IF ( .not.precision_r4(z%a, 2.0) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_r8(z%b, 2.0D0) ) THEN
  ERROR STOP 52
END IF

IF ( z%ch .NE. 'b') THEN
  ERROR STOP 53
END IF

IF ( z%lo .NEQV. .false. ) THEN
  ERROR STOP 54
END IF

end



