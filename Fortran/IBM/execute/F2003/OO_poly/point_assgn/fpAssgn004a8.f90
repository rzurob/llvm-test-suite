!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn004a8.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (poly-pointer array
!*                               assigned to results from a call to NOPASS
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
    type base
        integer*4 :: id = 0

        contains

        procedure, nopass :: replicate => replicateBase
    end type

    class (base), pointer :: b1_m(:)

    private replicateBase, printBase

    contains

    !! this binding can be overridden, but need SELECT TYPE support
    !! skip testing in this case
    function replicateBase (b)
        class (base), pointer :: replicateBase (:)
        class (base), intent(in) :: b(:)

        allocate (replicateBase(size(b)))

        replicateBase%id = b%id
    end function
end module

program fpAssgn004a8
use m
    class (base), pointer :: b_ptr(:)
    type (base) :: b1(10), b2

    b1%id = (/(i, i=1,10)/)

    b_ptr => b1%replicate (b1)

    if (size (b_ptr) /= 10) error stop 1_4

    if (any (b_ptr%id /= b1%id)) error stop 2_4

    deallocate (b_ptr)

    b1_m => b2%replicate (b1(::2))

    if (size (b1_m) /= 5) error stop 3_4

    if (any (b1_m%id /= (/1,3,5,7,9/))) error stop 4_4

    deallocate (b1_m)
end
