! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: ${TR_SRC}/fxmdvn04.sh fxmdvr06 cxmdvr06
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
!* TEST CASE TITLE              : fxmdvr06.f
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
!*                              : variable which contains complex
!*                              : type variable. Variables are called by cmain.
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  type, bind(c) :: dt
    
    complex(4) :: a
    
    complex(8) :: b
  end type
  
end module

subroutine fsub(z)
  use mod
  type(dt) :: z
  
  logical precision_x8
  logical precision_x16

  
 

IF ( .not.precision_x8(z%a, (0.0,1.0)) ) THEN
    ERROR STOP 51
END IF

IF ( .not.precision_x16(z%b, (0.0D0,1.0D0)) ) THEN
  ERROR STOP 52
END IF



z = dt((1.0,3.0),(1.0D0,3.0D0))
 
end



