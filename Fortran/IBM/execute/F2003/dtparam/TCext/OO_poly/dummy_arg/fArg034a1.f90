! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg034a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg034a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (change of dummy-arg will
!                               be seen in the procedure by actual arg for
!                               dummy-arg with TARGET attribute)
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
        integer(k1)   :: id
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

program fArg034a1
use m
    class (base(4)), pointer :: b1 (:)

    allocate (child(4,1,20) :: b1(2:11))

    b1%id = (/(i, i=2,11)/)

    call test1 (b1)

    if (any (b1%id /= (/(i,i=102, 111)/))) error stop 3_4

    deallocate (b1)

    contains

    subroutine test1 (b)
        class (base(4)), TARGET :: b (:)

        if (any (b1%id /= (/(i,i=2,11)/))) error stop 1_4

        b%id = b%id + 100

        if (any (b1%id /= (/(i,i=102, 111)/))) error stop 2_4
    end subroutine
end
