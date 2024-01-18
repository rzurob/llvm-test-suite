! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : polyPntToDTfmfinal.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : TO is of a poly type which has final subroutine
!*                               FROM is of nonpoly type of child
!*                               Pointer is of a poly type
!*                               defect 322595
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
        contains
            final :: finalA 
   end type

   type, extends(base) :: A
       class(base), allocatable :: x 
   end type 

   class(base), pointer :: p(:)

   contains 
       subroutine finalA(a)
            type( base), intent(inout) :: a(:)
            print *, "deallocting" 
       end subroutine
end module

use m

    type(A), target, allocatable :: from(:)
    class(base), target, allocatable :: to (:)

    allocate(from(3:5), source = (/ A(base()),A(base()),A(base()) /) ) 
   ! allocate(from(3:5), source = (/ (A(base()), i=1,3) /) ) 

    p => from

    call move_alloc(from, to)

    if ( allocated(from) ) stop 11
    if ( .not. allocated(to)) stop 13

    if ( lbound(to,1) /= 3 ) stop  21
    if ( ubound(to,1) /= 5 ) stop  22
    if ( .not. associated(p, to) ) stop 23

    deallocate(to)

end
