! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn011.f
! %VERIFY: functionReturn011.out:functionReturn011.vf
! %STDIN:
! %STDOUT: functionReturn011.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/11/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of an
!*    external function call. Test the SAVE attribute.
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
        integer j
    end type

    contains

    function func1()
        class(Base), allocatable, SAVE :: temp(:)
        class(Base), allocatable :: func1(:)
        if(.NOT. allocated(temp)) then
            print *, "create Base"
            allocate(temp(2), SOURCE=(/ (Base(i), i=1,2) /))
            allocate(func1(20), SOURCE=(/ (Base(i), i=1,20) /))
        else
            print *, "create Child"
            deallocate(temp)
            allocate(func1(20), SOURCE=(/ (Child(i,i-1), i=1,20) /))
        end if
    end function
end module

program functionReturn011
use m
    class(Base), pointer :: b1(:,:) => null()

    allocate(b1(3,5), SOURCE=reshape(func1(), (/3,5/)))

    select type (b1)
        type is (Base)
            print *, b1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1(5,4), SOURCE=reshape(func1(), (/5,4/)))

    select type (b1)
        type is (Child)
            print *, b1
        class default
            error stop 2_4
    end select
end
