! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn001.f
! %VERIFY: functionReturn001.out:functionReturn001.vf
! %STDIN:
! %STDOUT: functionReturn001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is the return value of an
!*                               internal function call.
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
end module

program functionReturn001
use m
    class(*), pointer :: b1(:,:) => null()

    print *, reshape(func1(), (/3,5/))
    allocate(b1(3,5), SOURCE=reshape(func2(), (/3,5/), &
     (/Base(-1),Base(-2)/), (/2,1/)))

    select type (b1)
        type is (Base)
            print *, b1
        class default
            error stop 1_4
    end select

    contains

    function func1()
        type(Base) :: func1(20)
        func1 = (/ (Base(i), i=1,20) /)
    end function

    function func2()
        class(Base), pointer :: func2(:)
        allocate(func2(10), SOURCE=(/ (Base(i), i=1,10) /))
    end function
end
