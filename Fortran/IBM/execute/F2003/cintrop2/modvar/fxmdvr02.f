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
!*                              : variable which contains complex type variable.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  type, bind(c) :: dt
    complex(4) :: a
    complex(8) :: b
  end type
  type(dt) :: z
end module


  use mod

  logical precision_x8
  logical precision_x16


  interface
    subroutine csub(x)
      use mod
      type(dt) :: x
    end subroutine
  end interface



  z = dt((0.0, 1.0), (0.0D0,1.0D0))
    call csub(z)

IF ( .not.precision_x8(z%a, (1.0,3.0)) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_x16(z%b, (1.0D0,3.0D0) )) THEN
  ERROR STOP 52
END IF



end


