! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/mxminAll/misc/mxminMisc19.f
! opt variations: -qnock

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
!*
!* ===================================================================

program mxminMisc19 

   type dt(k1,n1)    ! (1,2)
     integer, kind             :: k1
     integer, len              :: n1
     character(kind=k1,len=n1)    zz(3:-1, 4:2, 9:2)
   end type 

   character*(5)   x1_arr(3:1), x2_arr(1:0, 4:3), x3_arr(2:1,4:2,7:3)
   character*5     v1, v2, v3(2), v4(2)

   type(dt(1,2)) :: obj 

   v1 = 'sssss'
   v2 = 'sssss'

   v1 = maxval(x1_arr, dim = 1, mask = .true.)
   v2 = minval(x1_arr, dim = 1, mask = .true.)

   if(ichar(v1) .ne. 0) then
        error stop 1_4
   endif

   if(ichar(v2) .ne. 127) then
        error stop 2_4
   endif
  
   if(len(maxval(x1_arr)) .ne. 5) then
        error stop 3_4
   endif

   if(len(minval(x1_arr)) .ne. 5) then
        error stop 4_4
   endif

   v3 = 'xxxxx'
   v4 = 'xxxxx'

   v3 = maxval(x2_arr, dim =1) 
   v4 = minval(x2_arr, dim =1)

   if(len(maxval(x2_arr)) .ne. 5) then
        error stop 7_4
   endif

   if(len(minval(x2_arr)) .ne. 5) then
        error stop 8_4
   endif

   v1 = 'hhhhh'
   v2 = 'hhhhh'

   v1 = maxval(pack(x3_arr, mask = .true.))
   v2 = minval(pack(x3_arr, mask = .true.)) 

   if(ichar(v1) .ne. 0) then
        error stop 9_4
   endif

   if(ichar(v2) .ne. 127) then
        error stop 10_4
   endif

   if(len(maxval(x2_arr)) .ne. 5) then
        error stop 11_4
   endif

   if(len(maxval(x2_arr)) .ne. 5) then
        error stop 12_4
   endif

   if (ichar(minval(obj%zz)) .ne. 127) then
      error stop 13_4
   endif

   if (ichar(maxval(obj%zz)) .ne. 0) then
      error stop 14_4
   endif

   if(len(maxval(obj%zz)) .ne. 2) then
      error stop 15_4
   endif

   if(len(minval(obj%zz)) .ne. 2) then
      error stop 16_4
   endif

end program mxminMisc19 

