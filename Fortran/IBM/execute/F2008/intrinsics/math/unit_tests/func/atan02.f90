! F2008 allows atan to have two arguments (in which case it has the
! same result as atan2.)  Test atan with two arguments in
! constant expressions. (This tests the folding path in the
! compiler.)  Also, ensure that atan with one argument is still
! allowed.
      implicit none

      real(4), parameter :: rx4 = 1.5574077e0
      real(4), parameter :: ry4 = 1.0e0
      real(4), parameter :: res4 = atan(rx4)
      real(4), parameter :: res4a = atan(ry4, rx4)
      real(4), parameter :: res4b = atan2(ry4, rx4)

      real(8), parameter :: rx8 = 1.5574077d0
      real(8), parameter :: ry8 = 1.0d0
      real(8), parameter :: res8 = atan(rx8)
      real(8), parameter :: res8a = atan(ry8, rx8)
      real(8), parameter :: res8b = atan2(ry8, rx8)

      real(16), parameter :: rx16 = 1.5574077q0
      real(16), parameter :: ry16 = 1.0q0
      real(16), parameter :: res16 = atan(rx16)
      real(16), parameter :: res16a = atan(ry16, rx16)
      real(16), parameter :: res16b = atan2(ry16, rx16)

      if (res4a /= res4b) then
        print *, res4a
        print *, res4b
        error stop 1
      end if

      if (res8a /= res8b) then
        print *, res8a
        print *, res8b
        error stop 2
      end if

      if (res16a /= res16b) then
        print *, res16a
        print *, res16b
        error stop 3
      end if

      end
