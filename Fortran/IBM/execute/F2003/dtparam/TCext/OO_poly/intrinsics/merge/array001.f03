! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/merge/array001.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is array element or array section
!*    Poly and unlimited poly
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program array001
use m
    class(Base(4)), allocatable :: b1(:,:)
    class(*), pointer :: u1(:,:)
    logical :: m1(2,3)

    allocate(b1(2,3), SOURCE=reshape((/(Child(4,4)(i,i+1),i=1,6)/), &
     (/2,3/)))
    allocate(u1(2,3), SOURCE=reshape((/(Child(4,4)(-i,i+1),i=1,11,2)/), &
     (/2,3/)))
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/2,3/))

    select type(name1=>merge(b1(2,3), u1, m1))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>merge(u1(:2,2:3), b1(:,:2), m1(1:2,2:3)))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end