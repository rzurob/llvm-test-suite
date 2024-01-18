! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : pure2.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
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
!*                               test if move_alloc is pure procedure
!*                               move_alloc is called in a pure subroutine
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

    class(*), allocatable :: o1(:,:,:,:,:,:)
 
    interface 
         pure subroutine sub( arg1, arg2 )
               class(*), intent(inout), allocatable :: arg1(:,:,:,:,:,:)
               class(*), intent(inout), allocatable :: arg2(:,:,:,:,:,:)
        end subroutine
    end interface 

end module

program main
use m

    class(*), allocatable :: o2(:,:,:,:,:,:)

    allocate( character(8) ::  o1(2,2,2,2,2,2) )

    select type(O1)
         type is (character(*))
             o1(:,:,:,1,:,:)    =  'COMPILER'
             o1(:,:,:,2,:,:)    =  'compiler'
             o1(:,:,:,1,1,:)    =  'FORTRAN'
             o1(:,:,:,1,2,:)    =  'fortran'
         class default
    end select 
    call sub (o1, o2)

    if ( allocated(o1) ) stop 21
    if ( .not. allocated(o2) ) stop 31 
 
    select type(O2)
         type is (character(*))
             print *, shape(o2) 
             print *, o2(1,2,2,1,:,1)
         class default
    end select 
     
end

pure subroutine sub( arg1, arg2 )

     class(*), intent(inout), allocatable :: arg1(:,:,:,:,:,:)
     class(*), intent(inout), allocatable :: arg2(:,:,:,:,:,:)
   
     call move_alloc(arg1, arg2)
end subroutine
