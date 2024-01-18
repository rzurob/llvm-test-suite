! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/dummy_arg/fArg009a5_1.f
! opt variations: -qck -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 05/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (VALUE attribute used in
!*                               the type-bound)
!*
!*                               2008-01-02: value attribute removed per defect
!*                               324210.1
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
        final :: finalizeBase
    end type

    type, extends(base) :: child    ! (20,4)
        character(n1) name

        contains

        procedure :: print => printChild
        final :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4)) :: b

        print *, b%id, b%name
    end subroutine

    subroutine finalizeChild (b)
        type(child(*,4)) :: b

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeBase (b)
        type (base(*,4)) :: b

        print *, 'finalizeBase'
    end subroutine
end module

program fArg009a5_1
use m
    class (base(:,4)), allocatable :: b1

    allocate (b1, source=base(20,4)(100))

    call b1%print

    deallocate (b1)

    allocate (b1, source=child(20,4)(-100, 'test 1'))

    call b1%print
end
