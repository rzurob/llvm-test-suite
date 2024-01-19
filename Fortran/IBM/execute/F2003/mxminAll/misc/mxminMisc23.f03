!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as argument to procedure declaration
!*                               statement entity
!*
!* ===================================================================

program mxminMisc23

    character*3 x(12)

    procedure(character*3) ::  p_char

    procedure(integer) :: p_int

    x(1:5) = 'aaa'
    x(6:10) = 'zzz'
    x(11:12) = 'bbb'

    if(p_char(max('aaa', 'bbb')) .ne. 'bbb') then
         error stop 1_4
    endif

    if(p_int(maxloc(x,dim=1, mask = .true.)) .ne. 6) then
         error stop 2_4
    endif

    if(p_char(minval(x,dim=1, mask=.true.)) .ne. 'aaa') then
        error stop 3_4
    endif

end program mxminMisc23

   character*3 function p_char(arg)
          character*3 arg
          p_char = arg
   end function

   integer function p_int(arg)
          integer arg
          p_int = arg
   end function
