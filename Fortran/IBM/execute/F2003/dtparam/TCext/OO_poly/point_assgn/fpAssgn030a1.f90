! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn030a1.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

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
! %GROUP: fpAssgn030a1.f
! %VERIFY: fpAssgn030a1.out:fpAssgn030a1.vf
! %STDIN:
! %STDOUT: fpAssgn030a1.out
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
!*  DATE                       : 05/11/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (pointer assignment
!                               occurs in ASSOCIATE construct; test the bounds
!                               and shape)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine resetBaseVal (b)
        class (base(*,4)), intent(inout) :: b

        b%id = -1
    end subroutine

end module

program fpAssgn030a1
use m
    class (base(:,4)), pointer :: b_ptr(:)
    class (base(:,4)), allocatable, target :: b_alloc (:)

    type (child(20,4,1)), target :: c1 (3:7)

    c1 = (/(child(20,4,1) (i, 'c1'), i=3,7)/)

    associate (x => c1)
        b_ptr => x

        if ((lbound(b_ptr,1) /= 3) .or. (ubound (b_ptr,1) /= 7)) error stop 1_4

        call b_ptr(6)%print
    end associate

    if (.not. associated (b_ptr, c1)) error stop 2_4

    allocate (b_alloc (2:8), source=child(20,4,1)(1,'b_alloc'))

    b_alloc%id = (/(i, i=2,8)/)

    associate (x => b_alloc)
        b_ptr => x

        if ((lbound(b_ptr,1) /= 2) .or. (ubound (b_ptr,1) /= 8)) error stop 3_4

        call b_ptr(5)%print
    end associate

    if (.not. associated (b_ptr, b_alloc)) error stop 4_4
end
