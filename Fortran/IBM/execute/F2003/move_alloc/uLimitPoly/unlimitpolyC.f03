! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/31/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               global entities declared in module,
!*                               have public, save, private attribute
!*                               FROM and TO are same object to test
!*                               TO has unallocated status
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
   end type

   class(*), save, public, allocatable :: old(:,:,:,:)
   class(*), save, private, allocatable :: new(:,:,:)

   contains
        subroutine sub()
            allocate( base :: new(10,10,10) )
            if ( .not. allocated(new) ) error stop 11

            call move_alloc(new, new)
            if ( allocated(new) ) error stop 21
        end subroutine
end module

program main
use m

    call sub

    allocate(integer :: old(1,1,1,1))
    if ( .not. allocated(old) ) error stop 11

    call move_alloc(old, old)
    if ( allocated(old) ) error stop 13
end
