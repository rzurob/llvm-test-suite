! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn006.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/11/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : A or MOLD is the return value of a
!*                               type bound procedure call.
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

        contains

        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child
        character(10) :: c

        contains

        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base), intent(in) :: a
        class(Base), allocatable :: createBase
        allocate(Base::createBase)
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChild
        allocate(Child::createChild)
    end function
end module

program functionReturn006
use m
    class(Base), pointer :: ap1 => null()
    allocate(Base::ap1)

    if(.NOT. extends_type_of(Base(1), ap1%create())) error stop 1_4
    if(.NOT. extends_type_of(ap1%create(), Base(1))) error stop 2_4
    if(.NOT. same_type_as(ap1%create(), Base(1))) error stop 3_4

    deallocate(ap1)
    allocate(Child::ap1)
    if(.NOT. extends_type_of(Child(1,"a"), ap1%create())) error stop 4_4
    if(.NOT. extends_type_of(ap1%create(), Child(1,"a"))) error stop 5_4
    if(.NOT. same_type_as(ap1%create(), Child(1,"a"))) error stop 6_4
end
