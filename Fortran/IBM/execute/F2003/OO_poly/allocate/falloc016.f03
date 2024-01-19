! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (saved allocatable objects)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
        character(15) :: name
    end type

    class (base), save, allocatable :: b1_m, b2_m(:)
end module


integer(4) function allocationStat ()
use m
    allocationStat = 0

    if (allocated(b1_m))  allocationStat = allocationStat + 1

    if (allocated(b2_m))  allocationStat = allocationStat + 1
end function


subroutine allocateModuleData (howMany)
use m
    integer(4), intent(in) :: howMany

    if (howMany == 1)  allocate (b1_m)
    if (howMany == 2)  allocate (b2_m(1))
end subroutine


program falloc016
use m
    class (base), save, allocatable :: b1, b2(:)
    class (*), save, allocatable :: x, x1(:)

    integer(4) allocationStat

    !! test module data first
    if (allocationStat () /= 0) error stop 1_4

    call allocateModuleData (1)

    if (allocationStat () /= 1) error stop 2_4

    call allocateModuleData (2)

    deallocate (b1_m)

    if (allocationStat () /= 1) error stop 3_4

    if (allocated (b1_m) .or. .not. allocated (b2_m)) error stop 4_4

    !! test main program entities

    if (allocated(b1) .or. allocated(b2)) error stop 5_4

    if (allocated (x) .or. allocated (x1)) error stop 6_4

    call allocateX(x, 100.1d0)

    call allocateX1 (x1, (/1,2/))

    if ((.not. allocated (x)) .or. (.not. allocated(x1))) error stop 7_4

    if (size(x1) /= 2) error stop 8_4

    contains

    subroutine allocateX(x, src)
        class (*), allocatable, intent(out) :: x
        class (*), intent(in) :: src

        allocate (x, source=src)
    end subroutine

    subroutine allocateX1 (x, src)
        class (*), allocatable, intent(out) :: x(:)
        class (*), intent(in) :: src(:)

        allocate (x(size(src)), source=src)
    end subroutine
end
