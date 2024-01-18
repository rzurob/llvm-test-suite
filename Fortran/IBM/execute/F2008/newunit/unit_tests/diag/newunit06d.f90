      ! NEWUNIT= requires either STATUS='scratch' or FILE=
      character(10) :: bad_status = 'new'
      integer i
      integer :: iostat = 0
      integer, parameter :: new_unit_requires_status_scratch_iostat = 240

      open(newunit=i, status=bad_status, iostat=iostat) ! error
      if (iostat /= new_unit_requires_status_scratch_iostat) then
        print *, iostat
        stop 1
      endif

      end
