! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/selectType004.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType004.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Query type inside an select type
!*    construct. Selector is whole array, has associate name.
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

    type, extends(Base) :: Child(n2)    ! (4,20,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program selectType004
use m
    class(*), pointer :: ap1(:,:) => null()
    class(AbstractParent(4,:)), allocatable :: ap2(:)

    allocate(Base(4,20)::ap1(3,5))
    allocate(Base(4,20)::ap2(8))

    select type(name1=>ap1)
        type is(Base(4,*))
            select type(name2=>ap2)
                type is(Base(4,*))
                    if(.NOT. extends_type_of(name2(1), name1(1:,1:2))) &
                     error stop 1_4
                    if(.NOT. same_type_as(name2(:2), name1(1,2))) &
                     error stop 2_4
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
