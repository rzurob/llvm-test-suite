!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 19, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1 1 Actual arguments, dummy arguments, and argument association
!*  The values of assumed type parameters of a dummy argument are assumed from
!*  the corresponding type parameters of the associated actual argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(x)
        integer, len :: x
        integer :: a(x)
        integer, allocatable :: b(:)
        integer, pointer :: c(:)
    end type
end module

use m
integer, parameter :: X=4,Y=5,Z=6
type(base(:)), allocatable :: dtobj
allocate(base(X) :: dtobj)
allocate(dtobj%b(Y))
allocate(dtobj%c(Z))

call sub(dtobj,X,Y,Z)

contains
    subroutine sub(dtobj,X,Y,Z)
        use m
        integer, intent(in) :: X,Y,Z
        type(base(*)) :: dtobj

        if (size(dtobj%a) <> X &
        .or. size(dtobj%b) <> Y &
        .or. size(dtobj%c) <> Z) then
            stop 1
        endif
    end subroutine
end
