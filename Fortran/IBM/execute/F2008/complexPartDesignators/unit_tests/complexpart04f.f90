!* ===================================================================
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE            : complexpart04f.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : June 24, 2010
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Complex part designator
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              array complex part designators
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program complexpart04f

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

      bt%c%re = 1.0
      bt%c%im = 2.0

      print *, "1:  ", bt%c, bt%c%im, bt%c%re

      bp => bt
      bp%c%re = 3.0
      bp%c%im = 4.0

      allocate(base(10) :: ba(2))

      ba%c%re = 5.0
      ba%c%im = 6.0

      print *, "2:  ", bt%c, bt%c%im, bt%c%re
      print *, "3:  ", ba%c, ba%c%im, ba%c%re

      deallocate(ba)
      allocate(ba(2), source=bt)

      print *, "4:  ", bp%c, bp%c%im, bp%c%re
      print *, "5:  ", ba%c, ba%c%im, ba%c%re

      c(1)%re = 7.0
      c(1)%im = 8.0
      c(2)%re = 9.0
      c(2)%im = 10.0

      print *, "6:  ", c,c(1)%re,c(1)%im,c(2)%re,c(2)%im
      print *, "7:  ", c%re,c%im

      allocate (base2(8,3,5) :: polyc(2))

      polyc(1)%cmpl%re = 11.0
      polyc(1)%cmpl%im = 12.0 
      polyc(1)%cmpl(7)%re = 13.0 
      polyc(1)%cmpl(7)%im = 14.0
      polyc(2)%cmpl%re = 15.0
      polyc(2)%cmpl%im = 16.0
      polyc(2)%cmpl(8)%re = 17.0
      polyc(2)%cmpl(8)%im = 18.0

      print *, "8:  ", polyc(1)%cmpl%im
      print *, "9:  ", polyc(2)%cmpl%im

      cta%re = 19.0
      cta%im = 20.0
      cpa => cta
      print *, "10: ", cpa, cpa%re, cpa%im

      call sub1(11, cpa%im)
      call sub1(12, cpa%re)
      call sub2(13, cpa%im)
      print *, "14: ", func1(cpa%im)

      allocate(ca(3), source=(0.0,0.0))
      ca%re = 21.0
      ca%im = 22.0
      print *, "15: ", cpa, cpa%re, cpa%im

      call sub1(16, ca%im)
      call sub2(17, ca%im)
      print *, "18: ", func1(ca%im)

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
