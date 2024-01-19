! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/merge/typeBound002.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Poly
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
        class(AbstractParent(*)), intent(in) :: fs
        logical, intent(in) :: mask(:,:)
        select type(name1=>merge(this, fs, mask))
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
    end subroutine
end module

program typeBound002
use m
    class(Base(:,4)), pointer :: b1
    logical :: m1(3,2)

    m1 = reshape((/.FALSE.,.TRUE.,.FALSE.,.FALSE., &
     .TRUE.,.FALSE./), (/3,2/))

    allocate(b1, SOURCE=Base(20,4)(7))
    call b1%mergeWith(Base(20,4)(2), m1)

    deallocate(b1)
    allocate(b1, SOURCE=Child(20,4,4)(8,9))
    call b1%mergeWith(Child(20,4,4)(3,-3), m1)
end
