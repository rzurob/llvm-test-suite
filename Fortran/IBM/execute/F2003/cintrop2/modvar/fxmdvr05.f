! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn01.sh fxmdvr05 cxmdvr05
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
!* XL Fortran Test Case                         INBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxmdvr05.f
!
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Sep. 24, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    : -qfree=f90
!*
!* DESCRIPTION                  : Test the interoperability of drived type module
!*                              : variable which contains character, real, logical
!*                              : type variable. Variables are called by cmain.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  type, bind(c) :: dt
    character :: ch
    real(4) :: a
    logical(1) :: lo
    real(8) :: b
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



