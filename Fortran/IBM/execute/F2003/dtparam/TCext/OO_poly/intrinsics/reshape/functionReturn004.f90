! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn004.f
! opt variations: -ql -qreuse=none

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
!*  DATE                       : 12/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    function func1()
        class(Base(4)), pointer :: func1(:)
        allocate(func1(10), SOURCE=(/ (Child(4)(i+1,i-1), i=1,10) /))
    end function
end module

program functionReturn004
use m
    class(Base(4)), allocatable :: b1(:)
    allocate(b1(2), SOURCE=(/Child(4)(-1,1),Child(4)(-2,2)/))
    associate (c1 => reshape(func1(), (/3,5/), b1, (/2,1/)))
        if(same_type_as(c1, Base(4)(1))) error stop 1_4
        if(.NOT. same_type_as(c1, Child(4)(1,2))) error stop 2_4
    end associate
end
