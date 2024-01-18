do i = 1,3
  block
    integer, save :: j
    if (i .eq. 1) then
      j = 99
    else if (j .ne. 99) then
      stop 1
    end if
  end block
end do
end 
