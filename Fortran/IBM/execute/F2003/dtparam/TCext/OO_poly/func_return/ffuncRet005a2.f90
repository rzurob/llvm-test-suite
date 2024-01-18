! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/func_return/ffuncRet005a2.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 05/14/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function return (abstract type as the
!                               return declared type; generic overrides the
!                               structure constructor)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure(printBase), deferred :: print
    end type

    type, extends(base) :: child(k2)    ! (4,20,8)
        integer, kind :: k2
        integer(k2)      id

        contains

        procedure :: print => printChild
    end type

    interface base
        module procedure makeChildAlloc
    end interface

    interface
        subroutine printBase (b)
        import base
            class(base(4,*)), intent(in) :: b
        end subroutine
    end interface

    contains

    class(base(4,:)) function makeChildAlloc (id)
        integer(8), intent(in) :: id
        allocatable makeChildAlloc

        allocate (makeChildAlloc, source=child(4,20,8)(id))
    end function

    subroutine printChild (b)
        class (child(4,*,8)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type, extends(child) :: gen3    ! (4,20,8)
        character(n1) :: name

        contains

        procedure :: print => printGen3
    end type

    interface base
        module procedure makeGen3Alloc
    end interface

    contains

    subroutine printGen3 (b)
        class (gen3(4,*,8)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    class (base(4,:)) function makeGen3Alloc (id, name)
        allocatable makeGen3Alloc
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        allocate(makeGen3Alloc, source=gen3(4,20,8)(id, name))
    end function
end module

program ffuncRet005a2
use m1
    class (base(4,:)), allocatable :: b1, b2(:)

    allocate (b1, source=base(100_8))

    allocate (b2(0:2), source=(/base(1_8, 'test 01'), base(2_8, 'test 02'), &
            base(3_8, 'test 03')/))

    call b1%print

    call b2(0)%print
    call b2(1)%print
    call b2(2)%print

    associate (x => base(200_8, 'xlftest'))
        call x%print
    end associate
end
