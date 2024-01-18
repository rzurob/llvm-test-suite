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
!*  DESCRIPTION                : MAXVAL/MINVAL with zero-sized array
!*                               check kind, length and each character
!*                               of the string
!*
!* ===================================================================

program mxminvalConform02 

      character(3), parameter :: x(4:1) = "abc"

      if(kind(x) .ne. 1) then
          error stop 1_4
      endif

      if(len(maxval(x)) .ne. 3) then
          error stop 2_4
      endif

      if(ichar(maxval(x)(1:1)) .ne. 0) then
          error stop 3_4
      endif

      if(ichar(maxval(x)(2:2)) .ne. 0) then
          error stop 4_4
      endif

      if(ichar(maxval(x)(3:3)) .ne. 0) then
          error stop 5_4
      endif

      if(len(minval(x)) .ne. 3) then
          error stop 6_4
      endif

      if(ichar(minval(x)(1:1)) .ne. 127) then
          error stop 7_4
      endif

      if(ichar(minval(x)(2:2)) .ne. 127) then
          error stop 8_4
      endif

      if(ichar(minval(x)(3:3)) .ne. 127) then
          error stop 9_4
      endif

end program mxminvalConform02 

