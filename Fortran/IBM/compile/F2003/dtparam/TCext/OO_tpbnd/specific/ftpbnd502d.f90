! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd502d.f
! opt variations: -qnol

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2005
!*
!*  DESCRIPTION                : specific type bound (array section with vector
!                               subscript is not eligible to be used as
!                               actual-arg to be associated with dummy-arg
!                               having intent(out) or intent(inout) attr.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id
        contains

        procedure, pass :: assgn => assgnBase
    end type

    type (base(20,4)) :: b1_m(10)
    contains

    elemental subroutine assgnBase (b, i)
        class (base(*,4)), intent(inout) :: b
        integer, intent(in):: i
        b%id = i
    end subroutine

end module

program ftpbnd502a
use m

    type(base(20,4)) :: b1(10)
    integer*4 :: aSect(3) = (/1,2,3/)

    call b1(aSect)%assgn (1) !<-- this is illegal
end
