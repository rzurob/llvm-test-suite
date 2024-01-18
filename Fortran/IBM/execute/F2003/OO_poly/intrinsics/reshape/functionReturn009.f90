! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn009.f
! %VERIFY: functionReturn009.out:functionReturn009.vf
! %STDIN:
! %STDOUT: functionReturn009.out
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
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is the return value of a
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
        integer j

        contains

        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base), intent(in) :: a
        class(Base), allocatable :: createBase(:)
        print *, "create Base"
        allocate(createBase(20), SOURCE=(/ (Base(i), i=1,20) /))
    end function

    function createChild(a)
        class(Child), intent(in) :: a
        class(Base), allocatable :: createChild(:)
        print *, "create Child"
        allocate(createChild(20), SOURCE=(/ (Child(i,-i), i=1,20) /))
    end function
end module

program functionReturn009
use m
    class(Base), allocatable :: a
    class(Base), pointer :: b(:,:) => null()

    allocate(Base::a)
    allocate(b(3,5), SOURCE=reshape(a%create(), (/3,5/)))

    select type (b)
        type is (Base)
            print *, b
        class default
            error stop 1_4
    end select

    deallocate(a)
    deallocate(b)

    allocate(Child::a)
    allocate(b(3,5), SOURCE=reshape(a%create(), (/3,5/)))

    select type (b)
        type is (Child)
            print *, b
        class default
            error stop 2_4
    end select
end
