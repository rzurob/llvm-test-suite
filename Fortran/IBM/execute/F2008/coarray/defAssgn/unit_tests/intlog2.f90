interface assignment (=)
  subroutine myassign(a,b)
    logical, intent(out) :: a
    integer, intent(in) :: b
  end subroutine
end interface

logical, save :: c[*]
integer i
integer :: d = 3

i = this_image()
c[i] = d

if (c[i]) then
  print *, c[i]
  error stop 9
end if
end

subroutine myassign(a,b)
  logical, intent(out) :: a
  integer, intent(in) :: b
  if (b .eq. 2) then
    a = .true.
  else
    a = .false.
  end if
end subroutine
