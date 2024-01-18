! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/intrinsics/transfer/reshape002.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Function return of transfer is the SOURCE of reshape.
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

program reshape002
use m
    type(Child(4,20)) :: c1(5)

    c1 = (/(Child(4,20)(i,i-1),i=101,105)/)

    print *, reshape(transfer(c1, Base(4,20)(1), 10), (/2,6/), &
     (/Base(4,20)(-8)/), (/1,2/))
    print *, size(reshape(transfer(c1, Base(4,20)(1), 10), (/2,6/), &
     (/Base(4,20)(-8)/), (/1,2/)))

    associate(name1=>reshape(transfer(c1, Base(4,20)(1), 10), (/2,6/), &
     (/Base(4,20)(-8)/), (/1,2/)))
        if(.NOT. same_type_as(name1, Base(4,20)(1))) error stop 1_4
        if(size(name1) .NE. 12) error stop 2_4
    end associate
end
