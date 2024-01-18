! GB DTP extension using:
! ftcx_dtp -qk -qreuse=none /tstdev/F2003/mxminAll/misc/mxminMisc21.f
! opt variations: -qck -qnok -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXLOC/MINLOC with zero-sized array
!*
!*   (315021)
!* ===================================================================

program mxminMisc21

   type dt(k1,n1,n2)    ! (4,2,2)
     integer, kind :: k1
     integer, len  :: n1,n2
     character(n1)    z(4:1, 2:-4)
     character(n2)    zz(3:-1, 4:2, 9:2)
   end type

   character*(5)   x1_arr(3:1), x2_arr(1:0, 4:3), x3_arr(2:1,4:2,7:3)
   integer         v0, v1(1), v2(2), v3(3), v4(2), v5(3)

   type(dt(4,2,2)) :: obj

   v0 = 3
   v1 = 3
   v2 = 3
   v3 = 3
   v4 = 3
   v5 = 3

   v0 = maxloc(x1_arr, dim=1)
   v1 = maxloc(x1_arr)
   v2 = maxloc(x2_arr, mask= .true.)
   v3 = maxloc(x3_arr)
   v4 = maxloc(obj%z)
   v5 = maxloc(obj%zz)

   if (v0 .ne. 0) then
      error stop 1_4
   endif

   if (any(v1 .ne. 0)) then
      error stop 2_4
   endif

   if (any(v2 .ne. 0)) then
      error stop 3_4
   endif

   if (any(v3 .ne. 0)) then
      error stop 4_4
   endif

   if (any(v4 .ne. 0)) then
      error stop 5_4
   endif

   if (any(v5 .ne. 0)) then
      error stop 6_4
   endif

   v0 = 4
   v1 = 4
   v2 = 4
   v3 = 4
   v4 = 4
   v5 = 4

   v0 = minloc(x1_arr, dim=1)
   v1 = minloc(x1_arr)
   v2 = minloc(x2_arr)
   v3 = minloc(x3_arr, mask = .true.)
   v4 = minloc(obj%z, mask = .true.)
   v5 = minloc(obj%zz)

   if (v0 .ne. 0) then
      error stop 7_4
   endif

   if (any(v1 .ne. 0)) then
      error stop 8_4
   endif

   if (any(v2 .ne. 0)) then
      error stop 9_4
   endif

   if (any(v3 .ne. 0)) then
      error stop 10_4
   endif

   if (any(v4 .ne. 0)) then
      error stop 11_4
   endif

   if (any(v5 .ne. 0)) then
      error stop 12_4
   endif

end program mxminMisc21

