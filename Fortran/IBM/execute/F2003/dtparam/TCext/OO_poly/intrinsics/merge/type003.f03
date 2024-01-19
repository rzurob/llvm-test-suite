! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/type003.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Declared types of TSOURCE and FSOURCE are different, but their
!*  dynamic types are the same.
!*    TSOURCE is scalar or array
!*    FSOURCE is scalar or array
!*    MASK is scalar or array
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
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 9
    end type
end module

program type003
use m
    class(Base(4)), pointer :: b1(:,:)
    class(*), allocatable :: b2(:,:)
    logical :: m1(3,2)

    allocate(b1(3,2), SOURCE=reshape((/(Base(4)(i),i=1,6)/),(/3,2/)))
    allocate(b2(3,2), SOURCE=reshape((/(Base(4)(i),i=101,106)/),(/3,2/)))
    m1 = reshape((/.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./),(/3,2/))

    select type(name1=>merge(b1, b2, m1))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(b1, b2)
    allocate(b1(3,2), SOURCE=reshape((/(Child(4)(i,-i),i=1,6)/),(/3,2/)))
    allocate(b2(3,2), SOURCE=reshape((/(Child(4)(i,-i),i=101,106)/),(/3,2/)))

    select type(name1=>merge(b2, b1, m1))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
