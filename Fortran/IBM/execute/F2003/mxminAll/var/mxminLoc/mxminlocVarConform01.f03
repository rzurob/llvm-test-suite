!*  ===================================================================
!*
!*  DATE                       : 1/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with variable(array
!*                               section) as its argument.
!* ===================================================================

program mxminlocVarConform01

      character(3) :: x(4:1, 5:4, -4:0) = "abc"
      character(3) :: y(12, 8, 4) = "dbx"
      character*1 ::z(7) = (/"f", "s", "k", "j", "a", "b", "q"/)

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

end program mxminlocVarConform01

