! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/functionReturn001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is the return value of an internal function call.
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
end module

program functionReturn001
use m
    print *, spread(func1(), 2, 2)
    print *, size(spread(func1(), 2, 2))
    print *, shape(spread(func1(), 2, 2))

    select type(name1=>spread(func2(), 3, 2))
        type is (Child(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    contains

    function func1()
        type(Base(20,4)) :: func1(10)
        func1 = (/ (Base(20,4)(i), i=1,10) /)
    end function

    function func2()
        class(Base(:,4)), pointer :: func2(:,:)
        allocate(func2(3,4), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,12)/), &
         (/3,4/)))
    end function
end
