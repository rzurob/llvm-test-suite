! GM DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/ace/types/none/acetnone10t.f

!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-29 (original: 2006-08-14)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Build different size arrays and assign them and sections of them to
!*  eachother.  Work backwards and forwards, and in different strides.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetnone10textmod

  implicit none

  type :: derived(k1)    ! (4)
     integer, kind :: k1
     integer(k1)   :: ival
   contains
     procedure :: isNotEqual => derivedIsNotEqual
     generic :: operator(/=) => isNotEqual
  end type derived

contains

  logical elemental function derivedIsNotEqual(a, b)
    class (derived(4)), intent(in) :: a, b
    derivedIsNotEqual = a % ival /= b %ival
  end function derivedIsNotEqual

end module acetnone10textmod

program acetnone10text

  use acetnone10textmod
  implicit none

  type(derived(4)) :: darr(4), da2(3,4), dmulti(2,3,4,5,6,7), dmulti2(7,6,5,4,3,2)
  type(derived(4)) :: da1(size(da2)), d25(25), dflat(size(dmulti)), dempty(0)
  integer       :: i, j
  save          :: dflat

  da1     = derived(4)(-1)
  darr    = derived(4)(-2)
  da2     = derived(4)(-3)
  dmulti  = derived(4)(-4)
  dmulti2 = derived(4)(-5)
  dflat   = derived(4)(-6)

  print *, reshape((/ (derived(4)(i), i=1,12) /), (/3,4/))
  call test2(reshape((/ (derived(4)(i), i=1,12) /), (/3,4/)))
  da2 = reshape((/ (derived(4)(i), i=1,12) /), (/3,4/))
  print *, da2

  print *, (/ da2 /)
  call test1((/ da2 /))
  da1 = (/ da2 /)
  print *, da1

  print *, (/ da1 /)
  call test1((/ da1 /))
  da1 = (/ da1 /)
  print *, da1

  print *, (/ da2(2,:) /)
  call test1((/ da2(2,:) /))
  darr = (/ da2(2,:) /)
  print *, darr

  print *, (/ da2(:,2), derived(4)(100) /)
  call test1((/ da2(:,2), derived(4)(100) /))
  darr = (/ da2(:,2), derived(4)(100) /)
  print *, darr

  print *, (/ da2(:,:) /)
  call test1((/ da2(:,:) /))
  da1 = (/ da2(:,:) /)
  print *, da1

  print *, (/ da1(ubound(da1,1):lbound(da1,1):-1) /)
  call test1((/ da1(ubound(da1,1):lbound(da1,1):-1) /))
  da1 = (/ da1(ubound(da1,1):lbound(da1,1):-1) /)
  print *, da1

  print *, (/ da2(ubound(da2,1):lbound(da2,1):-1,ubound(da2,2):lbound(da2,2):-1) /)
  call test1((/ da2(ubound(da2,1):lbound(da2,1):-1,ubound(da2,2):lbound(da2,2):-1) /))
  da1 = (/ da2(ubound(da2,1):lbound(da2,1):-1,ubound(da2,2):lbound(da2,2):-1) /)
  print *, da1

  print *, (/ ((da2(i,j),j=ubound(da2,2),lbound(da2,2),-1),i=ubound(da2,1),lbound(da2,1),-1) /)
  call test1((/ ((da2(i,j),j=ubound(da2,2),lbound(da2,2),-1),i=ubound(da2,1),lbound(da2,1),-1) /))
  da1 = (/ ((da2(i,j),j=ubound(da2,2),lbound(da2,2),-1),i=ubound(da2,1),lbound(da2,1),-1) /)
  print *, da1

  print *, (/ ((da2(i,j),i=ubound(da2,1),lbound(da2,1),-1),j=ubound(da2,2),lbound(da2,2),-1) /)
  call test1((/ ((da2(i,j),i=ubound(da2,1),lbound(da2,1),-1),j=ubound(da2,2),lbound(da2,2),-1) /))
  da1 = (/ ((da2(i,j),i=ubound(da2,1),lbound(da2,1),-1),j=ubound(da2,2),lbound(da2,2),-1) /)
  print *, da1

  print *, (/ (da2(i,ubound(da2,2):lbound(da2,2):-1),i=ubound(da2,1),lbound(da2,1),-1) /)
  call test1((/ (da2(i,ubound(da2,2):lbound(da2,2):-1),i=ubound(da2,1),lbound(da2,1),-1) /))
  da1 = (/ (da2(i,ubound(da2,2):lbound(da2,2):-1),i=ubound(da2,1),lbound(da2,1),-1) /)
  print *, da1

  print *, (/ (da2(ubound(da2,1):lbound(da2,1):-1,j), j=ubound(da2,2),lbound(da2,2),-1) /)
  call test1((/ (da2(ubound(da2,1):lbound(da2,1):-1,j), j=ubound(da2,2),lbound(da2,2),-1) /))
  da1 = (/ (da2(ubound(da2,1):lbound(da2,1):-1,j), j=ubound(da2,2),lbound(da2,2),-1) /)
  print *, da1


  print *, (/ ((da1(i), i=1,12,j), j=1,4) /)
  call test1((/ ((da1(i), i=1,12,j), j=1,4) /))
  d25 = (/ ((da1(i), i=1,12,j), j=1,4) /)
  print *, d25

  print *, (/ (da1(::j), j=1,4) /)
  call test1((/ (da1(::j), j=1,4) /))
  d25 = (/ (da1(::j), j=1,4) /)
  print *, d25

  print *, (/ ((da1(i), i=1,12,j), j=1,0) /)
  call test1((/ ((da1(i), i=1,12,j), j=1,0) /))
  dempty = (/ ((da1(i), i=1,12,j), j=1,0) /)
  print *, dempty

  print *, (/ (da1(::j), j=1,0) /)
  call test1((/ (da1(::j), j=1,0) /))
  dempty = (/ (da1(::j), j=1,0) /)
  print *, dempty


  ! Test something a little larger:

  dflat = (/ (derived(4)(i),i=1,size(dflat)) /)
  dmulti = reshape(dflat, (/ 2,3,4,5,6,7 /))
  call test6(dmulti)
  call test6(reshape(dflat, (/ 2,3,4,5,6,7 /)))

  if( any(dflat /= (/ dmulti /)) ) stop 2

  dflat = (/ dmulti(ubound(dmulti,1):lbound(dmulti,1):-1, &
                    ubound(dmulti,2):lbound(dmulti,2):-1, &
                    ubound(dmulti,3):lbound(dmulti,3):-1, &
                    ubound(dmulti,4):lbound(dmulti,4):-1, &
                    ubound(dmulti,5):lbound(dmulti,5):-1, &
                    ubound(dmulti,6):lbound(dmulti,6):-1) /)
  dflat = (/ dflat(ubound(dflat,1):lbound(dflat,1):-1) /)

  dmulti2 = reshape(dflat, (/ 7,6,5,4,3,2 /))
  call test6(dmulti2)
  call test6(reshape(dflat, (/ 7,6,5,4,3,2 /)))

  if( any(dflat /= (/ dmulti /)) ) stop 3
  if( any(dflat /= (/ dmulti2 /)) ) stop 4
  if( any((/ dmulti /) /= (/ dmulti2 /)) ) stop 5

  print *, 'done'

contains

  subroutine test1(darr)
    type(derived(4)) :: darr(:)
    print *, darr
  end subroutine test1

  subroutine test2(darr)
    type(derived(4)) :: darr(:,:)
    print *, darr
  end subroutine test2

  subroutine test6(darr)
    type(derived(4)) :: darr(:,:,:,:,:,:)
    print *, darr(lbound(darr,1),lbound(darr,2),lbound(darr,3),lbound(darr,4),lbound(darr,5),lbound(darr,6)), &
             darr(ubound(darr,1),ubound(darr,2),ubound(darr,3),ubound(darr,4),ubound(darr,5),ubound(darr,6))
  end subroutine test6

end program acetnone10text
