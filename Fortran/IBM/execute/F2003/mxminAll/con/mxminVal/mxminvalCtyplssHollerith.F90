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
!*  DESCRIPTION                :  TC to test maxval/minval with Hollerith 
!*                                constant using -qctyplss.
!* ===================================================================

@process ctyplss

program mxminvalCtyplssHollerith 

   call sub_val1(%val("P"))

   call sub_val1(%val(minval((/"Z", "P"/))))

#if __BIG_ENDIAN__
   call sub_val1(%val(minval((/1HZ, 1HP/))))
#else
   call sub_val2(%val(minval((/1HZ, 1HP/))))
#endif	

end program mxminvalCtyplssHollerith 

   subroutine sub_ref(%ref(i))
       integer*4 i
       print '(z8)', i
   end subroutine

   subroutine sub_val1(%val(j))
       integer*4 j
       print '(z8)', j
   end subroutine

#if __LITTLE_ENDIAN__
   subroutine sub_val2(%val(j))
       integer*4 j
       print '(z8)', reverse_byte_order(j)
   end subroutine
#endif	

