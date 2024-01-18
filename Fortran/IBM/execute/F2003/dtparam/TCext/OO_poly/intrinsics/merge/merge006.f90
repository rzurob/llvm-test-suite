! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=none -qdeferredlp /tstdev/OO_poly/intrinsics/merge/merge006.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/21/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar/array
!*    FSOURCE is scalar/array
!*    MASK is scalar
!*    Unlimited poly
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)   :: j = 9
    end type
end module

program merge006
use m
    class(*), pointer :: c1(:,:)
    class(*), allocatable :: c2

    allocate(c1(3,2), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,6)/), (/3,2/)))
    allocate(c2, SOURCE=Child(4,4)(4,-4))

    select type(name1=>merge(c1, c2, .TRUE.))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>merge(c2, c1, .TRUE.))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select

    deallocate(c1, c2)
    allocate(c1(4,5), SOURCE=reshape((/(Base(4)(i),i=1,20)/), (/4,5/)))
    allocate(c2, SOURCE=Base(4)(6))

    select type(name1=>merge(c2, c1, .FALSE.))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select

    select type(name1=>merge(c1, c2, .FALSE.))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 4_4
    end select

    deallocate(c1, c2)
    allocate(c1(3,3), SOURCE=reshape((/(i,i=11,19)/), (/3,3/)))
    allocate(c2, SOURCE=10)

    select type(name1=>merge(c2, c1, .FALSE.))
        type is (integer)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 5_4
    end select

    select type(name1=>merge(c1, c2, .FALSE.))
        type is (integer)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 6_4
    end select
end
