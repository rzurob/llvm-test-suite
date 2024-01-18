! F2008 allows atan to have two arguments (in which case it has the
! same result as atan2.)  Test atan with two arguments in
! non-constant expressions. (This tests the non-folding path in the
! compiler.)  Also, ensure that atan with one argument is still
! allowed.
      implicit none
      real(4) rx4, ry4, res4a, res4b
      real(8) rx8, ry8, res8a, res8b
      real(16) rx16, ry16, res16a, res16b

      rx4 = 1.5574077e0
      ry4 = 1.0e0
      res4a = atan(rx4)
      res4a = atan(ry4, rx4)
      res4b = atan2(ry4, rx4)
      if (res4a /= res4b) then
        print *, res4a
        print *, res4b
        error stop 1
      end if

      rx8 = 1.5574077d0
      ry8 = 1.0d0
      res8a = atan(rx8)
      res8a = atan(ry8, rx8)
      res8b = atan2(ry8, rx8)
      if (res8a /= res8b) then
        print *, res8a
        print *, res8b
        error stop 2
      end if

      rx16 = 1.5574077q0
      ry16 = 1.0q0
      res16a = atan(rx16)
      res16a = atan(ry16, rx16)
      res16b = atan2(ry16, rx16)
      if (res16a /= res16b) then
        print *, res16a
        print *, res16b
        error stop 3
      end if

      end
