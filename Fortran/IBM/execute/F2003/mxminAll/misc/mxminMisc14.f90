!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* with class obj.
!*
!* ===================================================================

module misc14

   type base
     character*3 bname
   end type

   type, extends (base) :: child
     character*3 :: cname
   end type

   class(child), allocatable :: charC(:)

end module misc14

program mxminMisc14
use misc14

   character*3 x, y

   x = 'fff'
   y = 'aaa'

   allocate(charC(3), source=(/child(min('aaa', 'zzz'), 'bbb'), child('ccc', &
   'ddd'), child('eee', max(x, y, x))/))

   if(MAXLOC(charC%bname, dim=1, mask = .true.) .ne. 3) then
        error stop 1_4
   endif

   if(MAXVAL(charC%bname, Dim = 1, Mask = .true.) .ne. 'eee')then
         error stop 2_4
   endif

   deallocate(charC)

end program mxminMisc14

