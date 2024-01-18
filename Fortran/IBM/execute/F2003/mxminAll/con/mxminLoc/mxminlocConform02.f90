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
!*  DESCRIPTION                : MAXLOC/MINLOC with named constant(array
!*                               section) as its argument.
!* ===================================================================

program mxminlocConform02 

      character(3), parameter :: x(4:1, 5:4, -4:0) = "abc"
      character(3), parameter :: y(12, 8, 4) = "dbx"
      character*1, parameter::z(7) = (/"f", "s", "k", "j", "a", "b", "q"/)

      if(kind(x) .ne. 1) then
          error stop 1_4
      endif

      if(any(maxloc(x) .ne. 0 )) then
          error stop 2_4
      endif

      if(size(maxloc(y(5:11, 2:7, 2:3))) .ne. 3) then
           error stop 3_4
      endif

      if(any(shape(maxloc(y(5:11, 2:7, 2:3))) .ne. 3)) then
           error stop 4_4
      endif

      if (any(maxloc(y(5:11, 2:7, 2:3)) .ne. 1)) then
           error stop 5_4
      endif

      if(any(minloc(z) .ne. 5)) then
          error stop 6_4
      endif

      if(any(minloc(z(3:7)) .ne. 3)) then
          error stop 7_4
      endif

end program mxminlocConform02 

