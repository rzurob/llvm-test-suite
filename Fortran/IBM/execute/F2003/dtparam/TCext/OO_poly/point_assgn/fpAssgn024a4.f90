! GB DTP extension using:
! ftcx_dtp -qck -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn024a4.f
! opt variations: -qnock -qdeferredlp

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
! %GROUP: fpAssgn024a4.f
! %VERIFY: fpAssgn024a4.out:fpAssgn024a4.vf
! %STDIN:
! %STDOUT: fpAssgn024a4.out
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
!*  DATE                       : 03/29/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (non-poly-pointer
!*                               assigned to type-bound function which returns
!*                               poly pointer)
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
    type base(k1,n1)    ! (1,20)
        integer, kind             :: k1
        integer, len              :: n1
        character(kind=k1,len=n1) :: name

        contains

        procedure :: replicate => replicateBase
        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2)    ! (1,20,4)
        integer, kind :: k2
        integer(k2)   :: id

        contains

        procedure :: replicate => replicateChild
        procedure :: print => printChild
    end type

    type (child(1,20,4)) :: c1_m = child(1,20,4) ('c1_m', 10)

    contains

    subroutine printBase (b)
        class (base(1,*)), intent(in) :: b

        print *, b%name
    end subroutine

    subroutine printChild (b)
        class (child(1,*,4)), intent(in) :: b

        print *, b%name, b%id
    end subroutine

    function replicateBase (b)
        class (base(1,*)), intent(in) :: b
        class (base(1,20)), pointer :: replicateBase

        allocate (replicateBase)

        replicateBase%name = b%name
    end function

    function replicateChild (b)
        class (base(1,20)), pointer :: replicateChild
        class (child(1,*,4)), intent(in) :: b

        type (child(1,20,4)), pointer :: tmp

        allocate (tmp)

        tmp%name = b%name
        tmp%id = b%id

        replicateChild => tmp
    end function
end module

program fpAssgn024a4
use m
    class (base(1,20)), pointer :: b1
    type (base(1,20)), pointer :: b2

    type (child(1,20,4)) :: c1

    c1 = child(1,20,4) ('c1', 20)

    b1 => c1%replicate()

    call b1%print

    deallocate (b1)

    b2 => c1_m%replicate()

    call b2%print
end
