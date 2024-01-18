! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpolyI.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 05/24/2006
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
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               FROM and TO are dummy arg with intent(inout)
!*                               type LOGICAL
!*                               FROM is dummy arg, TO is global entity
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

    class(*), allocatable :: z1(:,:,:,:) 
  
    contains

        subroutine sub( arg, barg )

            class(*), intent(inout), allocatable :: arg(:,:,:,:)
            class(*), intent(inout), allocatable :: barg(:,:,:,:)

            call move_alloc(arg, barg)
            if ( allocated(arg) ) stop 9 
            if ( .not. allocated(barg) ) stop 12

            call move_alloc(barg,z1)

        end subroutine
end module

program main
use m

    class(*), allocatable :: x1(:,:,:,:), y1(:,:,:,:)
    integer i

    allocate(y1(1,2,2,2), source = reshape ((/ (max(i+i, i*i), i = -4,3) /), (/1,2,2,2/))) 
    allocate(x1(1,2,2,2), source = reshape ((/ (min(i+i, i*i), i = -4,3) /), (/1,2,2,2/))) 
    call sub (x1, y1)

    if ( allocated(x1) ) stop 21
    if ( allocated(y1) ) stop 23
    if ( .not. allocated(z1) ) stop 25

    select type(z1)
        type is (integer)
            print *, z1 
        class default
            stop 33 
    end select
end
