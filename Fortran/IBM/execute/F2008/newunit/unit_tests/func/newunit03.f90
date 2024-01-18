      ! Test newunit value reuse.
      ! This test depends on our internal implementation.

      implicit none
      integer i
      integer, parameter :: num_units = 128
      integer unit_numbers(num_units)
      integer j
      logical reused

      ! Open num_units to exhaust the fast units table
      do j = 1, num_units
        open(newunit=i, status='scratch')
        unit_numbers(j) = i
        print *, i
      end do

      ! close a unit
      close(unit_numbers(5))

      ! open a new unit
      open(newunit=i, status='scratch')
      print *, i

      ! was this unit number reused?
      reused = any(unit_numbers == i)
      if (.not. reused) then
        stop 1
      endif
      close(i)

      do j = 1, num_units
        close(unit_numbers(j))
      end do

      do j = 1, num_units
        if (unit_numbers(j) >= 0) then
          print *, j, unit_numbers(j)
          stop 2
        endif
      end do

      end
