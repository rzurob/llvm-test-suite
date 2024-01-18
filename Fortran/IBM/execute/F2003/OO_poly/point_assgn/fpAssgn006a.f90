!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn006a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (pointer assignment for
!*                               pointer component takes place during intrinsic
!*                               assignment of a derived type)
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
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'
    end type
end module

program fpAssgn006a
use m
    type container
        class (base), pointer :: data => null()
    end type

    type (container) :: co, co1

    type (base), target :: b1 = base (10)
    type (child), target :: c1

    type (container), pointer :: c_ptr1
    type (container), allocatable :: c_alloc

    co = container (data = c1)

    co1 = co

    if (.not. associated (co1%data, c1)) error stop 1_4
    if (.not. associated (co%data, c1)) error stop 2_4

    co = container (data = b1)

    co1 = co

    if ((.not. associated (co1%data, b1)) .or. (co1%data%id /= 10)) error stop 3_4
    if (.not. associated (co%data, b1)) error stop 4_4

    nullify (co%data)

    co1 = co

    if (associated(co%data) .or. associated(co1%data)) error stop 5_4

    allocate (c_ptr1, c_alloc)
    if (associated (c_ptr1%data) .or. associated (c_alloc%data)) error stop 6_4

    c_ptr1 =  co1

    if (associated (c_ptr1%data)) error stop 7_4

    co%data => c1

    c_alloc = co

    if (.not. associated (c_alloc%data, co%data)) error stop 8_4
    if (c_alloc%data%id /= 1) error stop 9_4
end
