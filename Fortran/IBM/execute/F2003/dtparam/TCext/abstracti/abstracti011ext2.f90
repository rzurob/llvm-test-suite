! GM DTP extension using:
! ftcx_dtp -qnok -qnol /tstdev/F2003/abstracti/functional/abstracti011.f

!************************************************************************
!* ======================================================================
!*
!*  TEST CASE NAME             : abstracti011ext2
!*
!*                               by Alberto Alvarez-Mesquida)
!*  DATE                       : 2007-10-17 (original: 02/20/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : ALLOCATE (abstract type in allocate statement;
!                               a test case derived from fdtio513a1.f)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*
!* =====================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base
        contains

        procedure(printBase), deferred :: print
    end type

    abstract interface
        subroutine printBase (b)
        import base
            class (base), intent(in) :: b
        end subroutine
    end interface
end module

module m1
use m
    type, extends(base) :: child(k1,n1)    ! (4,20)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: id

        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3
        character(n1) :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b

        if (allocated (b%id)) then
            write (*, *) b%id
        end if
    end subroutine

    subroutine printGen3 (b)
        class (gen3(4,*)), intent(in) :: b

        if (allocated (b%id)) then
            write (*, '(i8,2a)') b%id, '; ', b%name
        else
            write (*, *) b%name
        end if
    end subroutine
end module


program abstracti011ext2
use m1
    class (base), allocatable :: b1(:)

    allocate (b1(0:1), source=(/gen3(4,20)(1, 'xlftest'), gen3(4,20)(null(), 'team')/))

    call b1(0)%print
    call b1(1)%print
end
