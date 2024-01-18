! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_tpbnd/specific/ftpbnd503a.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd503a.f
! %VERIFY: ftpbnd503a.out:ftpbnd503a.vf
! %STDIN:
! %STDOUT: ftpbnd503a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (correct binding invocation
!*                               with overriding binding called in inherited
!*                               binding)
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

        procedure, pass, non_overridable :: print => printBase
        procedure, nopass :: printHeader => printBaseHeader
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        call b%printHeader

        print *, 'id = ', b%id
    end subroutine

    subroutine printBaseHeader
        print *, 'base type'
    end subroutine
end module

module m1
use m
    type, extends (base) :: child    ! (20,4)

        contains

        procedure, nopass :: printHeader => printChildHeader
    end type

    contains

    subroutine printChildHeader

        print *, 'child type'
    end subroutine
end module

program ftpbnd503a
use m1
    class (base(:,4)), pointer :: b1
    type (child(20,4)), target :: c1 = child(20,4)(10)

    class (child(:,4)), pointer :: c_ptr

    call c1%base%print

    call c1%print

    b1 => c1
    c_ptr => c1

    call b1%print

    call c_ptr%print

    call c_ptr%base%print
end
