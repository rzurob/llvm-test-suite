! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/runf.sh fxbind_c04ood 
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c03ood.f
!* TEST CASE TITLE              : BIND(C) for Fortran procedures 
!*
!* PROGRAMMER                   : Kan Tian
!* DATE                         : Jan, 7, 2004
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :Interoperable Procedure
!*                               contained in Module.
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf95
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*   - Test: Fun/Entry with BINC(C) attribute .
!*   - FORTRAN code only , the interoperable function is implemented
!*     in Fortran and called in Fortran.
!*   - External procedure ( Fun/Entry), the interfaces is implicit.
!*   - Both primary entry point and an alternate entry point have 
!*     bind(c) attribute.
!*   - Derived Type.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  01/07/04   KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE Personal_details
  use iso_c_binding 
  IMPLICIT NONE
  TYPE, BIND(C):: Person
     REAL:: Weight
     INTEGER :: Age
     CHARACTER :: Sex
  END TYPE Person
END MODULE Personal_details

module mderived
contains
SUBROUTINE eval3 ( x, result ) bind(c) ! Global-Scope SUBROUTINE with bind(c)
  !
  !     Declare calling arguments
  USE Personal_details
  IMPLICIT NONE
  Integer :: x
  TYPE (Person) ::  result ,a1
  ! Declare local variables
  ! The SAVE attribute forces the program
  ! to retain the value of a procedure variable
  ! from one call to the next. 
  TYPE (Person),save :: data 

  ! assign one instance of a derived type to 
  ! another instance of the same derived type.
  result = data
  result%age = result%age + x
  RETURN

  ! Entry INITL specifies the values of data
  ! to be used when evaluating the SUBROUTINE eval3 .

  ENTRY initl(a1)  bind(c) !Global-Scope SUBROUTINE-entry with bind(c)
  data = a1
  RETURN
END SUBROUTINE eval3
end module mderived

PROGRAM testentry
  USE Personal_details
  use assertmod
  use mderived
  IMPLICIT NONE
  TYPE (Person) :: Patient ,result
  INTEGER :: I
  logical :: test
  Patient%Weight = 60.0
  Patient%Age = 18
  Patient%Sex = 'F'

  CALL initl ( Patient)
  I = 2
  CALL eval3 (i, result )
  test = (result%Age .EQ. 20) .AND. (result%Weight .EQ.  60.0)
  call assert(test,'The result is not correct',9)

END PROGRAM testentry

