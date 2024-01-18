! Compares double-precision erfc_scaled results to saved true value files
! from Mathematica. If max allowable ulp errors are exceeded, stops and 
! prints error message. Nothing is printed for successful completion.

      implicit none

      real(8) :: ulperr
      real(8) :: x, res
      real(16) :: true16
      real(8) :: f1, f2, true
      integer(8) :: i1, i2
      equivalence(f1, i1)
      equivalence(f2, i2)  
      
      character(*), parameter :: fname = "erfc_scaled08.dat"
      integer, parameter :: u = 8
      integer(8), parameter :: MAXULPS = 1

      integer :: ios = 0, line = 0

      open(unit=u, status='old', file=fname, action='read', iostat=ios)
      if (ios .ne. 0) error stop 1

      do while(.not. is_iostat_end(ios))

        ! read in the input argument and the expected result
        read(u, *, iostat=ios) x, true16
        line = line + 1

        ! convert verification data to double-precision
        true = true16

        ! compute erfc_scaled
        res = erfc_scaled(x)
        
        ! compute ulp error
        if ((res == true) .or. ((res .ne. res) .and. (true .ne. true))) then
            ! equal or both NaN
            ulperr = 0.0_8
        else
           f1 = res
           f2 = true
           ulperr = i1 - i2
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

      
