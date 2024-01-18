! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : sameDT4both.f 
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
!*  DESCRIPTION                : FROM/TO are of an nonpoly DT 
!*                               move_alloc appears in internal proc 
!*                               TO is internal func name
!*                               FROM is global var 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type A
   integer :: i
end type 

integer, allocatable :: i(:)

type(A), allocatable :: k(:,:) 

allocate(i(10), source = (/ (j, j=1,10)  /) )
allocate(k(2,5), source=func(i))
print *, k%i

contains
   function func(i)
       class(*), intent(in) :: i(2,*)
       type(A), allocatable :: func(:,:)
     
       select type (i)
           type is (integer)
                allocate(k(2,5), source = reshape( (/ A(i(2,1)),A(i(2,2)), &
                A(i(2,3)), A(i(2,4)), A(i(2,5)), A(i(1,1)),  A(i(1,2)), & 
                A(i(1,3)), A(i( 1,4)), A(i(1,5)) /),  (/2, 5/)))
           class default
                stop 31
       end select 

       call move_alloc(k, func)

       if ( allocated(k)) stop 11
       if ( .not. allocated(func)) stop 13 
   end function 

   end
