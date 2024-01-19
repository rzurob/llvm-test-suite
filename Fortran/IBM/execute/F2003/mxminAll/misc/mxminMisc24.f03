!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               recursive subprogram.
!*
!* ===================================================================

program mxminMisc24

   interface
          recursive character*7 function f_recu (arg) result (res)
             character*7 arg, str
          end function f_recu
   end interface

   character*7  x(2,3)

   x = 'dss__//'

   if(f_recu(max('aaaaaa ', 'bbbbbb ')) .ne. 'zzzzzzz') then
         error stop 1_4
   endif

   if(f_recu(minval(x, mask=.true.)) .ne. 'zzzzzzz') then
          error stop 2_4
   endif

end program mxminMisc24

recursive character*7 function f_recu (arg) result (res)
    character*7 arg, str
    if (arg(1:1) .eq. ' ') then
         res = 'z'
    else
         str = arg(2:)
         res = 'z' // f_recu(str)
    end if
end function f_recu

