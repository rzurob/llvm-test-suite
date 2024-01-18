! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/functionReturn006.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i

        contains

        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j

        contains

        procedure, pass :: create => createChild
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      k
        integer(k2)      m
        integer(k2)      n
    end type

    contains

    function createBase(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBase(:)
        allocate(createBase(21), SOURCE=(/ (Base(20,4)(i), i=1,21) /))
    end function

    function createChild(a)
        class(Child(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChild(:)
        allocate(createChild(20), SOURCE=(/ (Child(20,4)(i,i+1), i=1,20) /))
    end function
end module

program functionReturn006
use m
    class(Base(:,4)), allocatable :: a
    type(Base1(20,4)) :: b1(2)
    b1 = (/ Base1(20,4)(1,3,5), Base1(20,4)(2,4,6) /)

    allocate(Base(20,4)::a)
    associate(name1=>transfer(a%create(), (/Base1(20,4)(1,1,1)/)))
        if(size(name1) .NE. 7) error stop 1_4
        print *, name1
    end associate

    deallocate(a)
    allocate(Child(20,4)::a)

    select type(name1=>transfer(b1, a%create()))
        type is (Child(*,4))
            if(size(name1) .NE. 3) error stop 2_4
            print *, name1
        class default
            error stop 3_4
    end select
end
