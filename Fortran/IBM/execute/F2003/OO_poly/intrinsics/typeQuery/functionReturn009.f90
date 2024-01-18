! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn009.f
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
!*  DATE                       : 11/11/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Test that temporary object that is
!*    allocatable and is a function return shall be deallocated after
!*    execution of the innermost executable construct containing the
!*    function reference.
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
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type

    class(*), pointer :: b1 => null()

    contains

    function func1()
        class(Base), allocatable, target :: func1
        allocate(func1)
        b1 => func1
    end function
end module

program functionReturn009
use m
    if(extends_type_of(b1, Base(1))) error stop 1_4
    if(.NOT. extends_type_of(func1(), Base(1)) .AND. &
       .NOT. extends_type_of(b1, Base(1))) error stop 2_4

    b1 => null()
    if(same_type_as(b1, Base(1))) error stop 3_4
    if(.NOT. same_type_as(func1(), Base(1)) .AND. &
       .NOT. same_type_as(b1, Base(1))) error stop 4_4
end
