interface assignment (=)
  subroutine myassign(a,b)
    logical, intent(out) :: a
    character(2), intent(in) :: b[*]
  end subroutine
end interface

logical :: c
character(2), save :: d[*] = 'ab'
c = d

if (.not. c) then
  print *, c
  error stop 9
end if
end

subroutine myassign(a,b)
  logical, intent(out) :: a
  character(2), intent(in) :: b[*]
  if (b .eq. 'ab') then
    a = .true.
  else
    a = .false.
  end if
end subroutine
