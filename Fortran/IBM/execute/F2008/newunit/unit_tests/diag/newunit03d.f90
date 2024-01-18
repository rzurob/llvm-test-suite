      ! If an error occurs in the OPEN statement, the variable
      ! specified by the NEWUNIT= specifier should not be changed
      integer :: i = 10
      integer iostat
      open(newunit=i, file='doesnotexist', status='old', iostat=iostat)
      if (iostat == 0) then
        stop 1
      endif

      if (i /= 10) then
        print *, i
        stop 2
      endif
      end
