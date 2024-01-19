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
    type base
        integer id
        contains

        procedure, pass :: assgn => assgnBase
    end type

    type (base) :: b1_m(10)
    contains

    elemental subroutine assgnBase (b, i)
        class (base), intent(inout) :: b
        integer, intent(in):: i
        b%id = i
    end subroutine

end module

program ftpbnd502a
use m

    type(base) :: b1(10)
    integer*4 :: aSect(3) = (/1,2,3/)

    call b1(aSect)%assgn (1) !<-- this is illegal
end
