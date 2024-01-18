! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge002.f
! %VERIFY: merge002.out:merge002.vf
! %STDIN:
! %STDOUT: merge002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/21/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    TSOURCE is scalar
!*    FSOURCE is scalar
!*    MASK is scalar
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
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program merge002
use m
    class(Base), pointer :: c1
    class(Base), allocatable :: c2

    allocate(c1, SOURCE=Child(3,-3))
    allocate(c2, SOURCE=Child(4,-4))

    select type(name1=>merge(c1, c2, .TRUE.))
        type is (Child)
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>merge(c1, c2, .FALSE.))
        type is (Child)
            print *, name1
        class default
            error stop 2_4
    end select

    deallocate(c1, c2)
    allocate(c1, SOURCE=Base(5))
    allocate(c2, SOURCE=Base(6))

    select type(name1=>merge(c1, c2, .TRUE.))
        type is (Base)
            print *, name1
        class default
            error stop 3_4
    end select

    select type(name1=>merge(c1, c2, .FALSE.))
        type is (Base)
            print *, name1
        class default
            error stop 4_4
    end select
end
