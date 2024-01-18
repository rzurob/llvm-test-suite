! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg604.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/28/2005
!*
!*  DESCRIPTION                : argument association (C544: assumed-size array
!                               with INTENT(OUT), allow derived type without
!                               default initialization)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fArg604
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i
    end type

    type (base(4)) :: b1 (10)

    call abc (b1(3))

    if (any (b1(3:5)%i /= (/1,2,3/))) error stop 1_4

    contains

    subroutine abc (b)
        class(base(4)), intent(out) :: b(*)

        b(1:3)%i = (/1,2,3/)
    end subroutine
    end
