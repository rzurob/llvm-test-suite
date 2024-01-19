      ! Error: NEWUNIT= specified twice.
      integer :: i = 10, j = 20
      open(newunit=i, status='scratch', newunit=j) ! error. recovery is to ignore newunit=j
      if (i >= 0) then
        print *, i
        stop 1
      endif
      close(i)

      if (j /= 20) then
        print *, j
        stop 2
      endif

      end
