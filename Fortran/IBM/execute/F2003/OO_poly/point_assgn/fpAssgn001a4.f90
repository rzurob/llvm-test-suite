! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (poly pointer assigned to
!*                               allocatable target; scalars)
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

        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child

        contains

        procedure, pass :: print => printChild
    end type

    class (base), allocatable, target :: b1_m
    class (child), allocatable, target :: c1_m

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'child, id =', b%id
    end subroutine
end module

use m
    class (base), pointer :: b_ptr
    class (child), pointer :: c_ptr

    type (base), allocatable, target :: b1
    type (child), allocatable, target :: c1

    class (child), allocatable, target :: c2

    allocate (c1, b1)

    c1 = child (base = base (id = 100))

    b_ptr => c1     ! non-poly allocatable target

    if (.not. associated (b_ptr, c1)) error stop 1_4

    call b_ptr%print

    b_ptr => b1     ! non-poly allocatable target

    if (.not. associated (b_ptr, b1)) error stop 2_4

    call b_ptr%print

    !! test assignment to c2
    allocate (c2)

    c2%id = 10

    c_ptr => c2     ! target is poly-allocatable
    b_ptr => c_ptr

    if ((.not. associated (b_ptr, c_ptr)) .or. (.not. associated (b_ptr,c2))) &
                error stop 3_4

    call b_ptr%print
    call c_ptr%print
    call c2%print


    b_ptr => c_ptr%base    ! back to non-poly target

    if (.not. associated (b_ptr, c2%base)) error stop 4_4
    call b_ptr%print
    call c2%base%print

    !! test module entities as targets

    allocate (b1_m, c1_m)

    b_ptr => c1_m           ! poly-target declared in module
    call b_ptr%print
    call c1_m%base%print

    if (.not. associated (b_ptr, c1_m)) error stop 5_4


    b1_m%id = -10

    b_ptr => b1_m           ! poly-target declared in module
    call b_ptr%print

    deallocate (b1_m, c1_m)
end
