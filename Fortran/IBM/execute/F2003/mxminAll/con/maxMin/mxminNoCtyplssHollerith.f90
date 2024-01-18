!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                :  TC to test max/min with Hollerith constant
!*                                with -qnoctyplss. 
!*   (315985)
!* ===================================================================

@process noctyplss

program mxminNoCtyplssHollerith 

   call sub_val(%val("a"))

   call sub_val(%val(min("a", "b")))

   call sub_val(%val("b")) 

   call sub_val(%val(max(1Ha, "b")))
   
end program mxminNoCtyplssHollerith 

   subroutine sub_val(%val(j))
       integer*4 j
       print '(z8)', j
   end subroutine
