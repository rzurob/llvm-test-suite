! Compares extended-precision erfc_scaled results to saved true value files
! from Mathematica. If max allowable ulp errors are exceeded, stops and 
! prints error message. Nothing is printed for successful completion.

      implicit none

      real(16) :: ulperr
      real(16) :: x, res
      real(16) :: true
      
      character(*), parameter :: fname = "erfc_scaled09.dat"
      integer, parameter :: u = 8
      real(16), parameter :: MAXULPS = 1.0_16

      integer :: ios = 0, line = 0

      open(unit=u, status='old', file=fname, action='read', iostat=ios)
      if (ios .ne. 0) error stop 1

      do while(.not. is_iostat_end(ios))

        ! read in the input argument and the expected result
        read(u, *, iostat=ios) x, true
        line = line + 1

        ! compute erfc_scaled
        res = erfc_scaled(x)
        
        ! compute ulp error
        if ((res == true) .or. ((res .ne. res) .and. (true .ne. true))) then
            ! equal or both NaN
            ulperr = 0.0_16
        else
           ! For QP can't just compare low doubles as integers since the high
           ! and low doubles may have exponents greater than 53 apart, in
           ! which case the low bit is < 1 ulp.  Instead, compute absolute
           ! error and divide by 1 ulp.
           ulperr = (res - true)/spacing(true);
        end if
        
        if (abs(ulperr) .gt. MAXULPS) then
           print *, "line:", line, ", ulperr =", ulperr
           print *, "input:", x
           print *, "expected output :", true
           print *, "actual output   :", res
           error stop
        end if
        
      end do

      close(u)

      end

      
