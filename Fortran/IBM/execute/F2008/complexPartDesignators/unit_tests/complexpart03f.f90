!* ===================================================================
!*
!* DATE                       : June 24, 2010
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Complex part designator
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              scalar complex part designators
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program complexpart03f

      complex, parameter :: cp = (1.0,2.0)

      complex     :: c
      complex     :: cc
      complex(4)  :: c4
      complex(8)  :: c8
      complex(16) :: c16

      complex, pointer     :: pc
      complex, target      :: tc
      complex, allocatable :: ac

      real     :: ra, rb
      real(4)  :: r4
      real(8)  :: r8
      real(16) :: r16

      real, pointer :: pr

      type base (l)
        integer, len :: l
        integer :: i(l)
        complex :: c
      end type

      type(base(10)), target     :: dtbt
      type(base(:)), allocatable :: dtba
      type(base(:)), pointer     :: dtbp

      common /data/ cc

      print *, "1:  ", cp%re, cp%im

      c%re = 3.0
      c%im = 4.0
      print *, "2:  ", c%re, c%im

      c%re = 5.0
      c%im = 6.0
      ra = c%re
      rb = c%im
      print *, "3:  ", ra, rb

      c%re = 7.0
      c%im = 8.0
      associate ( y => c%im, x => c%re )
        print *, "4:  ", x, y
      end associate

      r4  = 9.0
      r8  = 10.0
      r16 = 11.0
      c4%re = r4
      c4%im = r8
      c8%re = r8
      c8%im = r4
      c16%re = r16
      c16%im = r4

      print *, "5:  ", c4,  c4%re,  c4%im
      print *, "6:  ", c8,  c8%re,  c8%im
      print *, "7:  ", c16, c16%re, c16%im

      print *, "8:  ", c4%re%kind,  c4%im%kind
      print *, "9:  ", c8%re%kind,  c8%im%kind
      print *, "10: ", c16%re%kind, c16%im%kind

      tc%re = 12.0
      tc%im = 13.0

      pc => tc
      print *, "11: ", pc, pc%re, pc%im

      allocate (complex :: ac)

      ac%re = 14.0
      ac%im = 15.0

      print *, "12: ", ac, ac%re, ac%im

      data cc / (16.0,17.0) /
      print *, "13: ", cc, cc%re, cc%im

      tc%re = 18.0
      tc%im = 19.0

      pr => tc%re
      print *, "14: ", pr

      pr => tc%im
      print *, "15: ", pr

      dtbt%c%re = 20.0
      dtbt%c%im = 21.0
      print *, "16: ", dtbt%c, dtbt%c%re, dtbt%c%im

      allocate (base(10) :: dtba)
      dtba%c%re = 22.0
      dtba%c%im = 23.0
      print *, "17: ", dtba%c, dtba%c%re, dtba%c%im

      dtbp => dtbt
      dtbp%c%re = 24.0
      dtbp%c%im = 25.0
      print *, "18: ", dtbp%c, dtbp%c%re, dtbp%c%im

      deallocate(ac)
      allocate(ac, source=(26.0,27.0))
      print *, "19: ", ac, ac%re, ac%im

      tc%re = 28.0
      tc%im = 29.0
      deallocate(ac)
      allocate(ac, source=(tc%re, tc%im))
      print *, "20: ", ac, ac%re, ac%im

      pc => tc
      pc%re = 30.0
      pc%im = 31.0
      deallocate(ac)
      allocate(ac, source=(pc%re, pc%im))
      print *, "21: ", ac, ac%re, ac%im

      tc%re = 32.0
      tc%im = 33.0
      allocate(pr, source=pc%re)
      print *, "22: ", pr, pc%re

      deallocate(pr)
      allocate(pr, source=pc%im)
      print *, "23: ", pr, pc%im

      tc%re = 34.0
      tc%im = 35.0
      call sub1(24, tc%re, tc%im)

      tc%re = 36.0
      tc%im = 37.0
      call sub1(25, pc%re, pc%im)

      tc%re = 38.0
      tc%im = 39.0
      deallocate(ac)
      allocate(ac, source=(pc%re, pc%im))
      call sub1(26, ac%re, ac%im)

      deallocate(dtba)
      allocate (base(7) :: dtba)
      dtba%c%re = 40.0
      dtba%c%im = 41.0
      call sub2(27, dtba%c)

      tc%re = 42.0
      tc%im = 43.0
      call sub2(28, tc)

      contains

      subroutine sub1(i, r1, r2)
        integer :: i
        real    :: r1, r2
        write(*,FMT='(I3,":  ",F11.8," ",F11.8)') i, r1, r2
      end subroutine sub1

      subroutine sub2(i, c)
        integer :: i
        complex :: c
        write(*,FMT='(I3,":  ",F11.8," ",F11.8)') i, c%re, c%im
      end subroutine sub2

end
