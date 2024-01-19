! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc511.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/04/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (abstract type in allocate statement;
!                               a test case derived from fdtio513a1.f)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure(printBase), deferred :: print
    end type

    interface
        subroutine printBase (b)
        import base
            class (base(4,*)), intent(in) :: b
        end subroutine
    end interface
end module

module m1
use m
    type, extends(base) :: child(k2)    ! (4,20,4)
        integer, kind            :: k2
        integer(k2), allocatable :: id

        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3(k3,n2)    ! (4,20,4,1,20)
        integer, kind             :: k3
        integer, len              :: n2
        character(kind=k3,len=n2) :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine printChild (b)
        class (child(4,*,4)), intent(in) :: b

        if (allocated (b%id)) then
            write (*, *) b%id
        end if
    end subroutine

    subroutine printGen3 (b)
        class (gen3(4,*,4,1,*)), intent(in) :: b

        if (allocated (b%id)) then
            write (*, '(i8,2a)') b%id, '; ', b%name
        else
            write (*, *) b%name
        end if
    end subroutine
end module


!program fdtio513a1
program falloc511
use m1
    class (base(4,20)), allocatable :: b1(:)

    allocate (b1(0:1), source=(/gen3(4,20,4,1,20)(1, 'xlftest'), gen3(4,20,4,1,20)(null(), 'team')/))

    call b1(0)%print
    call b1(1)%print
end
