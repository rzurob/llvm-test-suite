! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge003.f
! %VERIFY: merge003.out:merge003.vf
! %STDIN:
! %STDOUT: merge003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/21/2005
!*  ORIGIN                     :
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
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program merge003
use m
    class(*), pointer :: u1
    class(*), allocatable :: u2

    allocate(u1, SOURCE=Child(3,-3))
    allocate(u2, SOURCE=Child(4,-4))

    select type(name1=>merge(u1, u2, .TRUE.))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>merge(u1, u2, .FALSE.))
        type is (Child)
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(u1, u2)
    allocate(u1, SOURCE=Base(5))
    allocate(u2, SOURCE=Base(6))

    select type(name1=>merge(u1, u2, .TRUE.))
        type is (Base)
            print *, name1
        class default
            error stop 3_4
    end select

    select type(name1=>merge(u1, u2, .FALSE.))
        type is (Base)
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
