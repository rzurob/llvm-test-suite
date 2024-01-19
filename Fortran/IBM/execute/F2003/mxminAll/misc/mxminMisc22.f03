!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* with zero-length char
!*
!* ===================================================================

program mxminMisc22

   character*(-1)   x_arr(3), y_arr(2,5), z_arr(3)

   if(len(maxval(x_arr)) .ne. len(minval(y_arr))) then
         error stop 1_4
   endif

   if(any(maxloc(x_arr) .ne. minloc(x_arr))) then
        error stop 2_4
   endif

   if(len(max(x_arr, x_arr)) .ne. len(min(x_arr, x_arr))) then
        error stop 3_4
   endif

   if(len(max("", '')) .ne. 0 ) then
        error stop 4_4
   endif

end program mxminMisc22

