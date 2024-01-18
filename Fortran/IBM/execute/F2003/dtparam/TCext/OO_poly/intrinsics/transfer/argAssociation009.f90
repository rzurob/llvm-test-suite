! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/argAssociation009.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is a pointer or allocatable, poly, and is scalar.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    type Base1(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        integer(k2)          j
        type(Base(n2,k2)) :: k
        integer(k2)          m
    end type

    type, extends(Base1) :: Child1    ! (4,20)
        integer(k2) n
    end type
end module

program argAssociation009
use m
    class(Base(:,4)), pointer :: b
    class(Base1(4,:)), allocatable :: b1

    allocate(b, SOURCE=Child(20,4)(j=11,i=10))
    allocate(b1, SOURCE=Child1(4,20)(6, Base(20,4)(7), 8, 9))

    call sub1(b, b1)

    contains

    subroutine sub1(arg1, arg2)
        class(Base(:,4)), pointer :: arg1
        class(Base1(4,:)), allocatable :: arg2

        select type(name1=>transfer(arg1, arg2))
            type is (Child1(4,*))
                print *, name1%j, name1%k%i
            class default
                error stop 1_4
        end select

        select type(name1=>transfer(arg2, arg1, 2))
            type is (Child(*,4))
                print *, name1
            class default
                error stop 2_4
        end select
    end subroutine
end
