! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transfer/associate001.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is an associate name.
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program associate001
use m
    class(AbstractParent(4,:)), pointer :: ap1(:) => null()

    allocate(ap1(20), SOURCE=(/ (Base(4,20)(i),i=1,20) /))

    associate(name1=>ap1)
        associate(name2=>transfer(name1, Child(4,20)(8,9)))
            if(.NOT. same_type_as(name2, Child(4,20)(8,9))) error stop 1_4
            print *, name2
        end associate
    end associate

    associate(name1=>ap1(15:19))
        associate(name2=>transfer(name1, Child(4,20)(8,9)))
            if(.NOT.  same_type_as(name2, Child(4,20)(8,9))) error stop 2_4
            print *, name2
        end associate
    end associate
end
