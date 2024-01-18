!*  ===================================================================
!*
!*  DATE                       : June 2, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray in data stmt
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qcaf
!*
!*  DESCRIPTION                :
!*
!*    Test coarray objects initialized in module
!*    (378387)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m

   integer, save :: s_coarr[*]
   integer, save :: arr_coarr1(3)[*]
   integer, save :: arr_coarr2(3)[*]

   integer       :: me
   integer       :: images
   integer       :: next

   data s_coarr /-1/

   data arr_coarr1(1) /-1/
   data arr_coarr1(2) /-1/
   data arr_coarr1(3) /-1/

   data (arr_coarr2(i), i=1,3) /3*-2/

   end module

   use m

   if (s_coarr        .NE. -1)   stop 11
   if (ANY(arr_coarr1 .NE. -1) ) stop 12
   if (ANY(arr_coarr2 .NE. -2) ) stop 13

   me = this_image()
   images = num_images()

   if (me .NE. images) then
     next = me + 1
   else
     next = 1
   end if

   sync all

   if (s_coarr[next]         .NE. -1 ) stop 21
   s_coarr[next] = 1
   if (s_coarr[next]         .NE.  1 ) stop 22

   do i=1, 3
     if (arr_coarr1(i)[next] .NE. -1 ) stop 31
     arr_coarr1(i)[next] = 1
     if (arr_coarr1(i)[next] .NE.  1 ) stop 32

     if (arr_coarr2(i)[next] .NE. -2 ) stop 41
     arr_coarr2(i)[next] = 2
     if (arr_coarr2(i)[next] .NE.  2 ) stop 42
   end do

   sync all

   end

