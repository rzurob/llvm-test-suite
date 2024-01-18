! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd509a.f
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
! %GROUP: ftpbnd509a.f
! %VERIFY: ftpbnd509a.out:ftpbnd509a.vf
! %STDIN:
! %STDOUT: ftpbnd509a.out
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
!*  DATE                       : 04/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type-bound (external procedure as type
!*                               bound; pass binding; overriding binding as
!*                               external procedure)
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
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

module m1
use m
    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name = ''

        contains

        procedure :: print => printChild
    end type

    private printChild

    interface
        subroutine printChild (b)
        import child
            class (child(*,4,1)), intent(in) :: b
        end subroutine
    end interface
end module

program ftpbnd509a
use m1
    type (base(20,4)), target :: b1
    type (child(20,4,1)), target :: c1

    class (base(:,4)), pointer :: b_ptr


    c1 = child(20,4,1) (20, 'c1_test')
    b1 = base(20,4) (10)

    call c1%print

    b_ptr => c1

    call b_ptr%print

    b_ptr => b1

    call b_ptr%print
end

subroutine printChild (b)
use m1
    class (child(*,4,1)), intent(in) :: b

    print *, b%id, b%name
end subroutine
