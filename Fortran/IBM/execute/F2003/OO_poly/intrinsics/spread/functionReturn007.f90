! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn007.f
! %VERIFY: functionReturn007.out:functionReturn007.vf
! %STDIN:
! %STDOUT: functionReturn007.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is the return value of a type bound procedure call.
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

    contains

    function createBase(a)
        class(Base), intent(in) :: a
        class(Base), allocatable :: createBase(:)
        allocate(createBase(10), SOURCE=(/ (Base(i), i=1,10) /))
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChild(:)
        allocate(createChild(20), SOURCE=(/ (Child(i,-i), i=1,20) /))
    end function
end module

program functionReturn007
use m
    class(Base), allocatable :: a

    allocate(Base::a)

    select type(name1=>spread(a%create(), 2, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child::a)

    select type(name1=>spread(a%create(), 1, 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
