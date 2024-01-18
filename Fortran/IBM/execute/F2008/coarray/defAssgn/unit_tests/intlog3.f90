interface assignment (=)
  subroutine myassign(a,b)
    logical, intent(out) :: a
    integer, intent(in) :: b[*]
  end subroutine
end interface 

logical :: c
integer, save :: d[*] = 2
c = d

if (.not. c) then
  print *, c
  error stop 9
end if
end

subroutine myassign(a,b)
  logical, intent(out) :: a
  integer, intent(in) :: b[*]
  if (b .eq. 2) then
    a = .true.
  else
    a = .false.
  end if
end subroutine 
