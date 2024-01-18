!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc028.f
! %VERIFY: falloc028.out:falloc028.vf
! %STDIN:
! %STDOUT: falloc028.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLCOATE (if a pointer is associated with an
!                               allocatable entity, the pointer shall not be
!                               deallocated)
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
    type base
        integer(4) id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base) b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base) b(*)

        print *, 'finalizeBaseRank1'
    end subroutine
end module

program falloc028
use m
    class (*), allocatable, target :: x1_alloc, x2_alloc(:)

    class (*), pointer :: x1_ptr, x2_ptr(:)

    integer(4) err(2)

    allocate (base::x1_alloc, x2_alloc(10))

    x1_ptr => x1_alloc
    x2_ptr => x2_alloc

    err = -1

    deallocate (x1_ptr, stat=err(1))
    deallocate (x2_ptr, stat=err(2))

    if (any (err /= 2)) error stop 1_4
end
