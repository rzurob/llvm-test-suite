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
! %GROUP: fimplct006.f
! %VERIFY: fimplct006.out:fimplct006.vf
! %STDIN:
! %STDOUT: fimplct006.out
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
!*  DATE                       : 04/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : IMPLICIT (implicit poly entities as function
!*                               return results)
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
        integer*4 :: id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: replicate => bReplicateBase
    end type

    type (base) :: b1_m = base(-1)
    contains

    subroutine printBase (b)
        implicit class (base) (b)
        intent(in) b

        print *, b%id
    end subroutine

    function bReplicateBase (b)
        implicit class (base) (b)

        intent(in) :: b
        pointer bReplicateBase

        allocate (bReplicateBase, source=b)
    end function
end module

module m1
use m, only : base
    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child) :: b
        intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fimplct006
use m, only : b1_m
use m1
    implicit class (base) (b)

    pointer b_ptr

    type (child) :: c1 = child (10, name = 'c1')

    type, extends (child) :: gen3
        logical*4 :: flag = .false.
    end type

    type (gen3) g1

    g1 = gen3 (name = 'g3', id = 100)

    b_ptr => c1%replicate()

    call b_ptr%print

    deallocate (b_ptr)

    b_ptr => g1%replicate()

    call b_ptr%print

    deallocate (b_ptr)

    b_ptr => b1_m%replicate()

    call b_ptr%print

    deallocate (b_ptr)
end
