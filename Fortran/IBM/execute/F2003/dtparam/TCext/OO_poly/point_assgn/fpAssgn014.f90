! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/point_assgn/fpAssgn014.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn014.f
! %VERIFY: fpAssgn014.out:fpAssgn014.vf
! %STDIN:
! %STDOUT: fpAssgn014.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module data pointers'
!                               dynamic types)
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

        procedure, nopass :: print => printBase
    end type

    type, extends (base) :: child(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure, nopass :: print => printChild
    end type

    class (base(:,4)), pointer :: b1_m
    class (base(:,4)), pointer :: b2_m (:) => null()

    DATA b1_m /null()/

    class (base(:,4)), allocatable, target :: b3_m (:)

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program fpAssgn014
use m
    call b1_m%print
    call b2_m%print
    call b3_m%print

    allocate (b3_m(2:3), source=child(20,4,4,20)(1,'b3_m'))

    b1_m => b3_m (3)

    b2_m => b3_m (2:2)

    call b1_m%print
    call b2_m%print
    call b3_m%print

    deallocate (b3_m)

    call b3_m%print

    nullify (b2_m)

    call b2_m%print
end
