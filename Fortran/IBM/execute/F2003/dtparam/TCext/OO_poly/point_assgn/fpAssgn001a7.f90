! GB DTP extension using:
! ftcx_dtp -qck -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn001a7.f
! opt variations: -qnock -ql -qreuse=none

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
! %GROUP: fpAssgn001a7.f
! %VERIFY: fpAssgn001a7.out:fpAssgn001a7.vf
! %STDIN:
! %STDOUT: fpAssgn001a7.out
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
!*  DATE                       : 02/25/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (base poly pointer
!*                               points to multi-children types)
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
    type base(k1)    ! (4)
        integer, kind        :: k1
        integer(k1), pointer :: flag
        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child1    ! (4)
        integer(k1) :: id

        contains

        procedure :: print => printChild1
    end type

    type, extends(base) :: child2(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild2
    end type

    class(base(4)), pointer :: b1_m
    class (child1(4)), allocatable, target :: c1_m
    type (child2(4,1,20)), target :: c2_m

    contains

    subroutine printBase(b)
        class (base(4)), intent(in) :: b
        print *, 'type base'
    end subroutine

    subroutine printChild1 (b)
        class (child1(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild2 (b)
        class (child2(4,1,*)), intent(in) :: b

        print *, b%name
    end subroutine
end module

program fpAssgn001a7
use m
    class (base(4)), pointer :: b_ptr

    type (child1(4)), target :: c1
    type (child2(4,1,20)), target :: c2

    c1%id = 10
    c2%name = 'c2'

    b_ptr => c1

    call b_ptr%print

    b_ptr => c2

    call b_ptr%print

    c2_m%name = 'c2_m'

    b_ptr => c2_m
    call b_ptr%print


    allocate (c1_m)
    c1_m%id = -1
    b_ptr => c1_m%base

    call b_ptr%print

    b_ptr => c1_m
    b1_m => b_ptr
    call b1_m%print
end
