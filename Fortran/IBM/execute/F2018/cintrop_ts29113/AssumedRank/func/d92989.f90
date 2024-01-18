      interface
        subroutine sub_1(arg1) bind(c)
          character(1) , contiguous :: arg1(..)
        end subroutine
      end interface
      character(1), target :: c1(6,2)
      character(1), pointer :: c2(:,:)
      k = 0
      do i=1,2
        do j=1,6
          k = k + 1
          c1(j,i) = CHAR(k+64)
        end do
      end do
      c2 => c1(1:6:2,1:2)
      print *, "value on Fortran Side : ", c2
      call sub_1(c2)
      end program
