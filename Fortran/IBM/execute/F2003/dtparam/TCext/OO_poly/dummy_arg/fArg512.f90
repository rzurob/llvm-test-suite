! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/dummy_arg/fArg512.f
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
!*  DATE                       : 04/07/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) for pointer
!                               dummy-arg just makes the actual-arg undefined;
!                               no finalization occurrs)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), allocatable :: x

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4,*)), intent(inout) :: b

        print *, 'finalizeBase'

        if (allocated (b%x)) deallocate (b%x)
    end subroutine

    subroutine test1 (b1)
        class (base(4,*)), pointer, intent(out) :: b1

        allocate (b1)
    end subroutine

    subroutine test2 (b1)
        type (base(4,*)), pointer, intent(out) :: b1

        allocate (b1)
    end subroutine
end module

program fArg512
use m
    class (base(4,20)), pointer :: b11
    type (base(4,20)), pointer :: b22

    allocate (b11, b22)

    allocate (b11%x, source= 100)
    allocate (base(4,20)::b22%x)

    print *, 'begin'

    call test1(b11)
    call test2 (b22)

    if ((.not. associated (b11)) .or. (.not. associated (b22))) error stop 1_4

    if (allocated (b11%x) .or. allocated(b22%x)) error stop 2_4

    print *, 'end'
end
