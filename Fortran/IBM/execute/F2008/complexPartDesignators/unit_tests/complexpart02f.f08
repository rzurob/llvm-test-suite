!* ===================================================================
!*
!* DATE                       : June 24, 2010
!*
!* PRIMARY FUNCTIONS TESTED   : Complex part designator
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              array complex part designators
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program complexpart02f

      type base (l)
        integer, len :: l
        integer :: i(l)
        complex :: c
      end type

      type base2 (k,l,j)
        integer, kind :: k
        integer, len :: l,j
        character(l) :: c(j)
        complex(k) :: cmpl(j+l)
      end type

      type(base(10)), target :: bt(2)
      type(base(:)), allocatable :: ba(:)
      type(base(:)), pointer :: bp(:)

      class(base2 (8,:,:)), allocatable :: polyc(:)

      complex :: c(2)

      complex, pointer :: cpa(:)
      complex, target :: cta(3)
      complex, allocatable :: ca(:)

      real :: r(3)

      bt%c = (1.0,2.0)

      print *, "1:  ", bt%c, bt%c%im, bt%c%re

      bt%c = (3.0,4.0)

      allocate(base(10) :: ba(2))

      ba%c = (5.0,6.0)

      print *, "2:  ", bt%c, bt%c%im, bt%c%re
      print *, "3:  ", ba%c, ba%c%im, ba%c%re

      deallocate(ba)
      allocate(ba(2), source=bt)

      bp => bt

      print *, "4:  ", bp%c, bp%c%im, bp%c%re
      print *, "5:  ", ba%c, ba%c%im, ba%c%re

      c(1) = (7.0,8.0)
      c(2) = (9.0,10.0)

      print *, "6:  ", c,c(1)%re,c(1)%im,c(2)%re,c(2)%im
      print *, "7:  ", c%re,c%im

      allocate (base2(8,3,5) :: polyc(2))

      polyc(1)%cmpl = (11.0,12.0)
      polyc(1)%cmpl(7) = (13.0,14.0)
      polyc(2)%cmpl = (15.0,16.0)
      polyc(2)%cmpl(8) = (17.0,18.0)

      print *, "8:  ", polyc(1)%cmpl%im
      print *, "9:  ", polyc(2)%cmpl%im

      cta = (19.0,20.0)
      cpa => cta
      print *, "10: ", cpa, cpa%re, cpa%im

      call sub1(11, cpa%im)
      call sub1(12, cpa%re)
      call sub2(13, cpa%im)
      r = func1(cpa%im)
      print *, "14: ", r

      allocate(ca(3), source=(21.0,22.0))
      print *, "15: ", cpa, cpa%re, cpa%im

      call sub1(16, ca%im)
      call sub2(17, ca%im)
      r = func1(ca%im)
      print *, "18: ", r

      contains

      subroutine sub1(i, r)
      real :: r(:)
      integer :: a(3), i
      a(1) = r%kind * 2
      a(2) = LOC(r(2)) - LOC(r(1))
      a(3) = LOC(r(3)) - LOC(r(2))
      write(*,FMT='(I3,": ",3F12.8)') i, r
      print *, "    ", a
      end subroutine sub1

      subroutine sub2(i, r)
      real :: r(3)
      integer :: a(3), i
      a(1) = r%kind
      a(2) = LOC(r(2)) - LOC(r(1))
      a(3) = LOC(r(3)) - LOC(r(2))
      write(*,FMT='(I3,": ",3F12.8)') i, r
      print *, "    ", a
      end subroutine sub2

      elemental function func1(r)
      real, intent(in) :: r
      real :: func1
      func1 = r
      end function func1

      end
