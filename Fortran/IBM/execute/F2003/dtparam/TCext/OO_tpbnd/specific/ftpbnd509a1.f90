! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd509a1.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/buildLib ftpbnd509a1_1.f90 libftpbnd509a1_1.a
! %COMPOPTS: -qfree=f90 -L./ -lftpbnd509a1_1
! %GROUP: ftpbnd509a1.f
! %VERIFY: ftpbnd509a1.out:ftpbnd509a1.vf
! %STDIN:
! %STDOUT: ftpbnd509a1.out
! %EXECARGS:
! %POSTCMD: rm -f libftpbnd509a1_1.a
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (external procedure as type
!*                               bound; pass binding; inherited binding as
!*                               external procedure; defined in lib)
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

module m1
use m
    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name = ''

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b)
        class (child(*,4,1)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program ftpbnd509a1
use m1
    type (base(20,4)), target :: b1
    type (child(20,4,1)), target :: c1

    class (base(:,4)), pointer :: b_ptr


    c1 = child(20,4,1) (20, 'c1_test')
    b1 = base(20,4) (10)

    b_ptr => c1

    call b_ptr%print

    b_ptr => b1

    call b_ptr%print
end
