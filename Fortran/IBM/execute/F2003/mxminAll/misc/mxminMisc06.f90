!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               elemental subprogram with variable as
!*                               MAX/MIN argument
!*
!*                               use min/maxval (pack(array, mask))
!* ===================================================================

module misc6
    interface
       elemental function fun1(arg)
          character*3, intent(in):: arg
          character*3 fun1
       end function
    end interface

    interface max_min
       procedure fun1
       module procedure fun2
    end interface max_min

    contains
        elemental function fun2(arg)
          integer , intent(in)::arg
          integer fun2
          fun2 = arg
        end function
end module misc6

program mxminMisc06

  use misc6

  character*3 x(6)

  x(1:2) = 'abc'
  x(3:4) = 'xyz'
  x(5:6) = '   '

  if(max_min(maxval(x)) .ne. 'xyz') then
       error stop 1_4
  endif

  if(any(max_min(maxloc(x)) .ne. 3)) then
      error stop 2_4
  endif

  if(max_min(minval(x, dim=1, mask=.true.)) .ne. '  ') then
       error stop 3_4
  endif

  if(max_min(minloc(x, dim=1, mask=.true.)) .ne. 5) then ! result is scalar
      error stop 4_4
  endif

  if(any(max_min(max(x(1:2), x(3:4), x(5:6))) .ne. 'xyz')) then
        error stop 5_4
  endif

  if(any(max_min(min(x(1:2), x(3:4))) .ne. 'abc')) then
        error stop 6_4
  endif

  if(max_min(maxval(x, mask = .true.)) .ne. 'xyz') then
       error stop 7_4
  endif

  if(max_min(maxval(pack(x, mask = .true.))) .ne. 'xyz') then
       error stop 8_4
  endif

  if(ichar(max_min(minval(pack(x, mask = .false.)))) .ne. 127) then
        error stop 9_4
  endif

end program mxminMisc06

elemental function fun1(arg)
    character*3, intent(in):: arg
    character*3 fun1
    fun1 = arg
end function

