interface assignment (=)
  subroutine myassign(a,b)
    integer, intent(out) :: a
    logical, intent(in) :: b
  end subroutine
end interface 

integer, save :: c[*]
integer i
logical :: d = .false.

i = this_image()
c[i] = d

if (c[i] /= 45) then
  print *, c[i]
  error stop 9
end if
end
  
subroutine myassign(a,b)
  integer, intent(out) :: a
  logical, intent(in) :: b
  if (b .eqv. .true.) then
    a = 1
  else
    a = 45
  end if
end subroutine 
