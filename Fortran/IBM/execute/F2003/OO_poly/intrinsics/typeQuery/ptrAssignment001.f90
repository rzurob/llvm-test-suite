! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ptrAssignment001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Use data pointer assignment to change
!*    the dynamic type. Polymorphic pointer.
!*
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
        integer :: i = 1
    end type

    type, extends(Base) :: Child
        character(10) :: c = "abc"
    end type

    type Container
        class(Base), pointer :: b => null()
    end type
end module

program ptrAssignment001
use m
    type(Container) :: x, y
    type(Base), target :: b1
    type(Child), target :: c1

    if(.NOT. extends_type_of(y%b, x%b)) error stop 1_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 2_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 3_4

    x%b => b1
    y%b => c1

    if(.NOT. extends_type_of(y%b, x%b)) error stop 4_4
    if(extends_type_of(x%b, y%b)) error stop 5_4
    if(same_type_as(y%b, x%b)) error stop 6_4

    y = x

    if(.NOT. extends_type_of(y%b, x%b)) error stop 7_4
    if(.NOT. extends_type_of(x%b, y%b)) error stop 8_4
    if(.NOT. same_type_as(y%b, x%b)) error stop 9_4
end
