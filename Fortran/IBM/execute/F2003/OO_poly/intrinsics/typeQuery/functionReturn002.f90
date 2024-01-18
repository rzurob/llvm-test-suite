! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : A or MOLD is the return value of
!*                               intrinsic function null().
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program functionReturn002
use m
    type(Base) :: arg1
    type(Child) :: arg2
    class(*), pointer :: arg3 => null()
    class(*), pointer :: arg4 => null()
    allocate(Child::arg3)
    allocate(Child::arg4)

    if(extends_type_of(null(arg3), arg1)) error stop 1_4
    if(same_type_as(arg2, null(arg4))) error stop 2_4
end
