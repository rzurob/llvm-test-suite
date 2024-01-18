! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType005.f
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
!*    construct. Selector is scalar, has associate name. Selector
!*    is unlimited polymorphic and is either unallocated allocatable
!*    or disassociated pointer.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program selectType005
use m
    class(*), pointer :: u1 => null()
    class(*), allocatable :: u2
    class(AbstractParent), allocatable :: ap1

    select type(name1=>u1)
        class default
            if(.NOT. extends_type_of(ap1, name1)) error stop 1_4
            if(extends_type_of(name1, ap1)) error stop 2_4
            if(same_type_as(ap1, name1)) error stop 3_4
    end select

    select type(name1=>u2)
        class default
            if(.NOT. extends_type_of(ap1, name1)) error stop 4_4
            if(extends_type_of(name1, ap1)) error stop 5_4
            if(same_type_as(ap1, name1)) error stop 6_4
    end select
end
