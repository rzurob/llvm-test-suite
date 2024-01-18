block
  integer :: n
  call sub(n)
  if (n .ne. 10) stop 1
end block

contains

  subroutine sub(n)
    integer, intent(inout) :: n
    n = 10
  end 
end
