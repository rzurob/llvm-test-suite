! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/typeQuery/selectType001.f
! opt variations: -qnock -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType001.f
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
!*    construct. Selector is scalar.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child(k2,n1)    ! (4,1,10)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: c
    end type
end module

program selectType001
use m
    class(*), pointer :: ap1 => null()
    class(AbstractParent(4)), allocatable :: ap2

    allocate(Base(4)::ap1)
    allocate(Child(4,1,10)::ap2)

    select type(name1=>ap1)
        type is(Base(4))
            select type(name2=>ap2)
                type is(Child(4,1,*))
                    if(.NOT. extends_type_of(name2, name1)) error stop 1_4
                    if(same_type_as(name1, name2)) error stop 2_4
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
