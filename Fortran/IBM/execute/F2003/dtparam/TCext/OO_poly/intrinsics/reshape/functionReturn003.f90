! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn003.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn003.f
! %VERIFY: functionReturn003.out:functionReturn003.vf
! %STDIN:
! %STDOUT: functionReturn003.out
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
!*                               external function call. Use select
!*                               type to check the return value.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    contains

    function func1()
        class(Base(:,4)), pointer :: func1(:)
        allocate(func1(10), SOURCE=(/ (Child(20,4)(i+1,i-1), i=1,10) /))
    end function
end module

program functionReturn003
use m
    class(Base(:,4)), allocatable :: b1(:)
    allocate(b1(2), SOURCE=(/Child(20,4)(-1,1),Child(20,4)(-2,2)/))
    select type (c1 => reshape(func1(), (/3,5/), b1, (/2,1/)))
        type is (Child(*,4))
            print *, c1
        class default
            error stop 1_4
    end select
end
