!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr021a2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (default
!*                               initialization for pointer comp.; omitted in
!*                               the structure constructor)
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
        integer*4 :: id = 1
        type(base), pointer :: next => null()
    end type

    type base1
        type (base1), pointer :: next => null()
    end type
end module

program fconstr021a2
use m

    type (base) :: b1 = base ()
    type (base) :: b2 = base (id = 10)

    type (base1) :: bb1 = base1()
    type (base), pointer :: b_ptr

    if ((b1%id /= 1) .or. associated(b1%next)) error stop 1_4

    if ((b2%id /= 10) .or. associated(b2%next)) error stop 2_4

    if (associated(bb1%next)) error stop 3_4

    allocate(b_ptr)

    b_ptr = base ()

    if (associated(b_ptr%next) .or. (b_ptr%id /= 1)) error stop 4_4

    deallocate(b_ptr)
end
