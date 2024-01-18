! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn030a.f
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
! %GROUP: fpAssgn030a.f
! %VERIFY: fpAssgn030a.out:fpAssgn030a.vf
! %STDIN:
! %STDOUT: fpAssgn030a.out
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
!*  DESCRIPTION                : data pointer assignment (happens in ASSOCIATE
!*                               construct; the associating entity assumes the
!*                               dynamic type of the selector)
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
        procedure :: reset => resetBaseVal
    end type

    type, extends(base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
        procedure :: reset => resetChildVal
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

    subroutine resetChildVal (b)
        class (child(*,4,1)), intent(inout) :: b

        b%id = -1
        b%name = 'default'
    end subroutine
end module

program fpAssgn030a
use m
    class (base(:,4)), pointer :: b_ptr, b_ptr1

    type (child(20,4,1)), target :: c1 = child(20,4,1) (1, 'c1')

    b_ptr => c1

    associate (x => b_ptr)
        call x%print

        b_ptr1 => x

        call b_ptr1%print

        call x%reset
    end associate

    if (.not. associated (b_ptr, b_ptr1)) error stop 1_4

    if (.not. associated (b_ptr1, c1)) error stop 2_4

    call b_ptr1%print
end
