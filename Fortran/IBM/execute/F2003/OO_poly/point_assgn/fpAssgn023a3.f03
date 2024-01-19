! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!                               array' finalization when deallocating the data
!                               pointer; use pointer assignment for the
!                               association; rank-one array)
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
        integer*4 :: id
    end type

     type, extends (base) :: child
        integer*4, pointer :: value => null()

        contains

        final :: finalizeChild
     end type

     class (base), pointer :: b_ptr (:)
     type (child), pointer :: c_ptr (:)

     contains

     subroutine finalizeChild (c)
        type (child), intent(inout) :: c(:)

        do i = 1, size (c)
            if (associated (c(i)%value)) then
                print *, 'deallocating c:', i

                deallocate (c(i)%value)
            end if
        end do
     end subroutine
end module

program fpAssgn023a3
use m
    class (*), pointer :: x(:)
    integer*4, pointer :: v1, v2

    type (child) :: c1(2)

    allocate (v1, v2)

    c1 = (/child (1, v1), child(2, v2)/)

    allocate (b_ptr (2:3), source=c1)
    allocate (c_ptr (3))

    allocate (c_ptr(3)%value)

    x => b_ptr

    deallocate (x)

    x => c_ptr

    deallocate (x)
end
