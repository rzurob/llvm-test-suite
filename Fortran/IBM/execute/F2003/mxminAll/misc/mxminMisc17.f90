!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX*/MIN* with deferred shape array 
!* ===================================================================

program mxminMisc17

   character*3, pointer, dimension(:,:) :: x1, x2
   character*3, target :: y(2,3)
   integer      v(2)

   y = "ddd"

   x1 => y

   allocate(x2(2, 3))

   x2 = "ggg" 
  
   if(any(max(x1, x2)   .ne. 'ggg')) then
         error stop 1_4
   endif

   if(any(maxval(x1,dim=1) .ne. "ddd")) then
         error stop 2_4
   endif

   if(any(maxloc(x2) .ne. 1)) then
         error stop 3_4
   endif

   deallocate(x2)

end program mxminMisc17 


