! Calls to maxloc and minloc with back present and different -qlanglvl settings.

      implicit none

      integer :: j1(1) = maxloc((/1_1, 3_1, 3_1, 2_1/), back = .true.)
      integer :: j2(1)

      j2 = maxloc((/1_1, 3_1, 3_1, 2_1/), back = .false.)

      end
