! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn006.f
! %VERIFY: functionReturn006.out:functionReturn006.vf
! %STDIN:
! %STDOUT: functionReturn006.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is the return value of a type bound procedure call.
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
        integer j

        contains

        procedure, pass :: create => createChild
    end type

    type Base1
        integer k
        integer m
        integer n
    end type

    contains

    function createBase(a)
        class(Base), intent(in) :: a
        class(Base), allocatable :: createBase(:)
        allocate(createBase(21), SOURCE=(/ (Base(i), i=1,21) /))
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChild(:)
        allocate(createChild(20), SOURCE=(/ (Child(i,i+1), i=1,20) /))
    end function
end module

program functionReturn006
use m
    class(Base), allocatable :: a
    type(Base1) :: b1(2)
    b1 = (/ Base1(1,3,5), Base1(2,4,6) /)

    allocate(Base::a)
    associate(name1=>transfer(a%create(), (/Base1(1,1,1)/)))
        if(size(name1) .NE. 7) error stop 1_4
        print *, name1
    end associate

    deallocate(a)
    allocate(Child::a)

    select type(name1=>transfer(b1, a%create()))
        type is (Child)
            if(size(name1) .NE. 3) error stop 2_4
            print *, name1
        class default
            error stop 3_4
    end select
end
