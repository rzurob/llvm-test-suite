! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/merge/merge003.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/21/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar
!*    FSOURCE is scalar
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

program merge003
use m
    class(*), pointer :: u1
    class(*), allocatable :: u2

    allocate(u1, SOURCE=Child(4,4)(3,-3))
    allocate(u2, SOURCE=Child(4,4)(4,-4))

    select type(name1=>merge(u1, u2, .TRUE.))
        type is (Child(4,4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>merge(u1, u2, .FALSE.))
        type is (Child(4,4))
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(u1, u2)
    allocate(u1, SOURCE=Base(4)(5))
    allocate(u2, SOURCE=Base(4)(6))

    select type(name1=>merge(u1, u2, .TRUE.))
        type is (Base(4))
            print *, name1
        class default
            error stop 3_4
    end select

    select type(name1=>merge(u1, u2, .FALSE.))
        type is (Base(4))
            print *, name1
        class default
            error stop 4_4
    end select

    deallocate(u1, u2)
    allocate(u1, SOURCE=7)
    allocate(u2, SOURCE=-7)

    select type(name1=>merge(u1, u2, .TRUE.))
        type is (integer)
            print *, name1
        class default
            error stop 5_4
    end select

    select type(name1=>merge(u1, u2, .FALSE.))
        type is (integer)
            print *, name1
        class default
            error stop 6_4
    end select
end
