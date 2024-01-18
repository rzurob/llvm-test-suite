interface assignment (=)
  subroutine myassign(a,b)
    character(2), intent(out) :: a[*]
    integer, intent(in) :: b
  end subroutine
end interface

character(2), save :: c[*]
integer :: d = 2
c = d
if (c .ne. 'ab') then
  print *, c
  error stop 9
end if
end

subroutine myassign(a,b)
  character(2), intent(out) :: a[*]
  integer, intent(in) :: b
  if (b .eq. 2) then
    a = 'ab'
  else
    a = 'cd'
  end if
end subroutine
