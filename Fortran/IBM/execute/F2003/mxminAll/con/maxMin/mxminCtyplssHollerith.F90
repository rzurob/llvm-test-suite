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
!*                                with -qctyplss.
!* ===================================================================

@process ctyplss

program mxminCtyplssHollerith1 

   call sub_ref(%ref(min(1Ha, 1Hb)))

#if __BIG_ENDIAN__
   call sub_val1(%val("a"))
#else
   call sub_val2(%val("a"))
#endif	   

   call sub_val1(%val(min(1Ha, 1Hb)))

   call sub_val1(%val(min("a", "b")))

   call sub_val1(%val(max(1Ha//1Hb, 1Hb//1Hc)))

   call sub_val1(%val(max("ab", "bc")))

   call sub_val1(%val(max(1Ha, "b")))

   call sub_val1(%val(max("a", 1Hb)))

end program mxminCtyplssHollerith1 

#if __BIG_ENDIAN__
   subroutine sub_ref(%ref(i))
       integer*4 i
       print '(z8)', i
   end subroutine

   subroutine sub_val1(%val(j))
       integer*4 j
       print '(z8)', j
   end subroutine

   subroutine sub_val2(j)
       integer*4, value:: j
       print '(z8)', j
   end subroutine
#else
   subroutine sub_ref(%ref(i))
       integer*4 i
       print '(z8)', reverse_byte_order(i)
   end subroutine

   subroutine sub_val1(%val(j))
       integer*4 j
       print '(z8)', reverse_byte_order(j)
   end subroutine

   subroutine sub_val2(j)
       integer*1, value:: j
       print '(z8)', j
   end subroutine
#endif	




