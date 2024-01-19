!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2006-08-11
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : arrays and array sections as content (real)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Build different size arrays and assign them and sections of them to
!*  eachother.  Work backwards and forwards, and in different strides.
!*  Here we focus on real data.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone10a

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  real :: rarr(4), ra2(3,4), rmulti(2,3,4,5,6,7), rmulti2(7,6,5,4,3,2)
  real :: ra1(size(ra2)), r25(25), rflat(size(rmulti)), rempty(0)
  integer :: i, j
  save :: rflat

  ra1     = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)
  rarr    = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)
  ra2     = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)
  rmulti  = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)
  rmulti2 = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)
  rflat   = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)
  r25     = IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF)

1 format(g7.1,6000(" ",f0.2))

  print 1, reshape((/ (real(i), i=-4,5), IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF), IEEE_VALUE(0.0_4, IEEE_QUIET_NAN) /), (/3,4/))
  call test2(reshape((/ (real(i), i=-4,5), IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF), IEEE_VALUE(0.0_4, IEEE_QUIET_NAN) /), (/3,4/)))
  ra2 = reshape((/ (real(i), i=-4,5), IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF), IEEE_VALUE(0.0_4, IEEE_QUIET_NAN) /), (/3,4/))
  print 1, ra2

  print 1, (/ ra2 /)
  call test1((/ ra2 /))
  ra1 = (/ ra2 /)
  print 1, ra1

  print 1, (/ ra1 /)
  call test1((/ ra1 /))
  ra1 = (/ ra1 /)
  print 1, ra1

  print 1, (/ ra2(2,:) /)
  call test1((/ ra2(2,:) /))
  rarr = (/ ra2(2,:) /)
  print 1, rarr

  print 1, (/ epsilon(0.0_4), ra2(:,2) /)
  call test1((/ epsilon(0.0_4), ra2(:,2) /))
  rarr = (/ epsilon(0.0_4), ra2(:,2) /)
  print 1, rarr

  print 1, (/ ra2(:,:) /)
  call test1((/ ra2(:,:) /))
  ra1 = (/ ra2(:,:) /)
  print 1, ra1

  print 1, (/ ra1(ubound(ra1,1):lbound(ra1,1):-1) /)
  call test1((/ ra1(ubound(ra1,1):lbound(ra1,1):-1) /))
  ra1 = (/ ra1(ubound(ra1,1):lbound(ra1,1):-1) /)
  print 1, ra1

  print 1, (/ ra2(ubound(ra2,1):lbound(ra2,1):-1,ubound(ra2,2):lbound(ra2,2):-1) /)
  call test1((/ ra2(ubound(ra2,1):lbound(ra2,1):-1,ubound(ra2,2):lbound(ra2,2):-1) /))
  ra1 = (/ ra2(ubound(ra2,1):lbound(ra2,1):-1,ubound(ra2,2):lbound(ra2,2):-1) /)
  print 1, ra1

  print 1, (/ ((ra2(i,j),j=ubound(ra2,2),lbound(ra2,2),-1),i=ubound(ra2,1),lbound(ra2,1),-1) /)
  call test1((/ ((ra2(i,j),j=ubound(ra2,2),lbound(ra2,2),-1),i=ubound(ra2,1),lbound(ra2,1),-1) /))
  ra1 = (/ ((ra2(i,j),j=ubound(ra2,2),lbound(ra2,2),-1),i=ubound(ra2,1),lbound(ra2,1),-1) /)
  print 1, ra1

  print 1, (/ ((ra2(i,j),i=ubound(ra2,1),lbound(ra2,1),-1),j=ubound(ra2,2),lbound(ra2,2),-1) /)
  call test1((/ ((ra2(i,j),i=ubound(ra2,1),lbound(ra2,1),-1),j=ubound(ra2,2),lbound(ra2,2),-1) /))
  ra1 = (/ ((ra2(i,j),i=ubound(ra2,1),lbound(ra2,1),-1),j=ubound(ra2,2),lbound(ra2,2),-1) /)
  print 1, ra1

  print 1, (/ (ra2(i,ubound(ra2,2):lbound(ra2,2):-1),i=ubound(ra2,1),lbound(ra2,1),-1) /)
  call test1((/ (ra2(i,ubound(ra2,2):lbound(ra2,2):-1),i=ubound(ra2,1),lbound(ra2,1),-1) /))
  ra1 = (/ (ra2(i,ubound(ra2,2):lbound(ra2,2):-1),i=ubound(ra2,1),lbound(ra2,1),-1) /)
  print 1, ra1

  print 1, (/ (ra2(ubound(ra2,1):lbound(ra2,1):-1,j), j=ubound(ra2,2),lbound(ra2,2),-1) /)
  call test1((/ (ra2(ubound(ra2,1):lbound(ra2,1):-1,j), j=ubound(ra2,2),lbound(ra2,2),-1) /))
  ra1 = (/ (ra2(ubound(ra2,1):lbound(ra2,1):-1,j), j=ubound(ra2,2),lbound(ra2,2),-1) /)
  print 1, ra1


  print 1, (/ ((ra1(i), i=1,12,j), j=1,4) /)
  call test1((/ ((ra1(i), i=1,12,j), j=1,4) /))
  r25 = (/ ((ra1(i), i=1,12,j), j=1,4) /)
  print 1, r25

  print 1, (/ (ra1(::j), j=1,4) /)
  call test1((/ (ra1(::j), j=1,4) /))
  r25 = (/ (ra1(::j), j=1,4) /)
  print 1, r25

  print 1, (/ ((ra1(i), i=1,12,j), j=1,0) /)
  call test1((/ ((ra1(i), i=1,12,j), j=1,0) /))
  rempty = (/ ((ra1(i), i=1,12,j), j=1,0) /)
  print 1, rempty

  print 1, (/ (ra1(::j), j=1,0) /)
  call test1((/ (ra1(::j), j=1,0) /))
  rempty = (/ (ra1(::j), j=1,0) /)
  print 1, rempty


  ! Test something a little larger:

  rflat = (/ (i,i=1,size(rflat)) /)
  rmulti = reshape(rflat, (/ 2,3,4,5,6,7 /))
  call test6(rmulti)
  call test6(reshape(rflat, (/ 2,3,4,5,6,7 /)))

  if( any(rflat /= (/ rmulti /)) ) error stop 2

  rflat = (/ rmulti(ubound(rmulti,1):lbound(rmulti,1):-1, &
                    ubound(rmulti,2):lbound(rmulti,2):-1, &
                    ubound(rmulti,3):lbound(rmulti,3):-1, &
                    ubound(rmulti,4):lbound(rmulti,4):-1, &
                    ubound(rmulti,5):lbound(rmulti,5):-1, &
                    ubound(rmulti,6):lbound(rmulti,6):-1) /)
  rflat = (/ rflat(ubound(rflat,1):lbound(rflat,1):-1) /)

  rmulti2 = reshape(rflat, (/ 7,6,5,4,3,2 /))
  call test6(rmulti2)
  call test6(reshape(rflat, (/ 7,6,5,4,3,2 /)))

  if( any(rflat /= (/ rmulti /)) ) error stop 3
  if( any(rflat /= (/ rmulti2 /)) ) error stop 4
  if( any((/ rmulti /) /= (/ rmulti2 /)) ) error stop 5

  print *, 'done'

contains

  subroutine test1(rarr)
    real :: rarr(:)
1   format(g7.1,6000(" ",f0.2))
    print 1, rarr
  end subroutine test1

  subroutine test2(rarr)
    real :: rarr(:,:)
1   format(g7.1,6000(" ",f0.2))
    print 1, rarr
  end subroutine test2

  subroutine test6(rarr)
    real :: rarr(:,:,:,:,:,:)
1   format(g7.1,6000(" ",f0.2))
    print 1, rarr(lbound(rarr,1),lbound(rarr,2),lbound(rarr,3),lbound(rarr,4),lbound(rarr,5),lbound(rarr,6)), &
             rarr(ubound(rarr,1),ubound(rarr,2),ubound(rarr,3),ubound(rarr,4),ubound(rarr,5),ubound(rarr,6))
  end subroutine test6

end program acetnone10a
