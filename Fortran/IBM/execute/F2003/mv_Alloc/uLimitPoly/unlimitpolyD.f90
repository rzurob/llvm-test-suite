! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               FROM/TO are same object thr two dummy
!*                               arguments with the same object
!*                               FROM/TO has intent(inout) attribute
!*                               MOVE_ALLOC called in a external procedure
!*
!*                              20081219: the test case violates the aliasing
!                               rule in Fortran: it's required NOT to change an
!                               actual's value through means rather than
!                               the associated dummy.  -- JX
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

    class(*), allocatable :: o1(:,:,:,:,:,:)

    interface
           subroutine sub( arg1, arg2 )
               class(*), intent(inout), allocatable :: arg1(:,:,:,:,:,:)
               class(*), intent(inout), allocatable :: arg2(:,:,:,:,:,:)
        end subroutine
    end interface

end module

program main
use m

    class(*), dimension(:,:,:,:,:,:), allocatable :: p1

    allocate( character(8) ::  o1(2,2,2,2,2,2) )

    call sub (o1, p1)

    if ( allocated(o1) ) stop 21

    if (.not. allocated(p1)) stop 22

    if (any(shape(p1) /= [2,2,2,2,2,2])) stop 23

    select type (p1)
        type is (character(*))
            if (len(p1) /= 8) stop 24

        class default
            stop 25
    end select
end

subroutine sub( arg1, arg2 )

     class(*), intent(inout), allocatable :: arg1(:,:,:,:,:,:)
     class(*), intent(inout), allocatable :: arg2(:,:,:,:,:,:)

     call move_alloc(arg1, arg2)
end subroutine
