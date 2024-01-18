! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn004.f
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
!*  DATE                       : 12/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is the return value of an
!*                               external function call. Use associate
!*                               construct to check the return value.
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
        class(Base), pointer :: func1(:)
        allocate(func1(10), SOURCE=(/ (Child(i+1,i-1), i=1,10) /))
    end function
end module

program functionReturn004
use m
    class(Base), allocatable :: b1(:)
    allocate(b1(2), SOURCE=(/Child(-1,1),Child(-2,2)/))
    associate (c1 => reshape(func1(), (/3,5/), b1, (/2,1/)))
        if(same_type_as(c1, Base(1))) error stop 1_4
        if(.NOT. same_type_as(c1, Child(1,2))) error stop 2_4
    end associate
end
