interface assignment (=)
  subroutine myassign(a,b)
    integer, intent(out) :: a[*]
    logical, intent(in) :: b
  end subroutine
end interface

integer, save :: c[*]
logical :: d = .true.

c = d
if (c /= 1) then
  print *, c
  error stop 9
end if
end

subroutine myassign(a,b)
  integer, intent(out) :: a[*]
  logical, intent(in) :: b
  if (b .eqv. .true.) then
    a = 1
  else
    a = 45
  end if
end subroutine
