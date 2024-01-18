! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/merge/typeBound001.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Non-poly
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
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
        contains
        procedure :: mergeWith
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type

    contains

    subroutine mergeWith(this, fs, mask)
        class(AbstractParent(*)), intent(in) :: this
        class(AbstractParent(*)), intent(in) :: fs(:)
        logical, intent(in) :: mask
        associate(name1=>merge(this, fs, mask))
            select type(name1)
                type is (Base(*,4))
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                type is (Child(*,4,4))
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        end associate
    end subroutine
end module

program typeBound001
use m
    type(Base(20,4)) :: b1
    type(Child(20,4,4)) :: c1

    b1%i = 7
    c1%i = 8
    c1%j = 9

    call b1%mergeWith((/Base(20,4)(1),Base(20,4)(2),Base(20,4)(3)/), .TRUE.)
    call c1%mergeWith((/Child(20,4,4)(1,-1),Child(20,4,4)(2,-2),Child(20,4,4)(3,-3)/), .FALSE.)
end
