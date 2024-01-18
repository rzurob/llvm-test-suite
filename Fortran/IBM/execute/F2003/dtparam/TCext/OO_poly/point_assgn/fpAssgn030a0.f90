! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/OO_poly/point_assgn/fpAssgn030a0.f
! opt variations: -qnock -ql

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
! %GROUP: fpAssgn030a0.f
! %VERIFY: fpAssgn030a0.out:fpAssgn030a0.vf
! %STDIN:
! %STDOUT: fpAssgn030a0.out
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = -1

        contains

        procedure :: print => printBase
        procedure :: reset => resetBaseVal
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
        procedure :: reset => resetChildVal
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine resetBaseVal (b)
        class (base(4)), intent(inout) :: b

        b%id = -1
    end subroutine

    subroutine resetChildVal (b)
        class (child(4,1,*)), intent(inout) :: b

        b%id = -1
        b%name = 'default'
    end subroutine
end module

program fpAssgn030a0
use m
    class (base(4)), pointer :: b_ptr

    type (child(4,1,20)), target :: c1 = child(4,1,20) (1, 'c1')

    associate (x => c1%base)
        call x%print

        b_ptr => x

        call b_ptr%print

        call b_ptr%reset

        call x%print
    end associate

    if (.not. associated (b_ptr, c1%base)) error stop 1_4

    b_ptr => c1

    call b_ptr%print
end
