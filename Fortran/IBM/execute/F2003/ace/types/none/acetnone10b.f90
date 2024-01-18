!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : acetnone10b
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : arrays and array sections as content (complex)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
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
!*  Here we focus on complex data.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone10b

  use, intrinsic :: ieee_features
  use, intrinsic :: ieee_arithmetic
  implicit none

  complex :: zarr(4), za2(3,4), zmulti(2,3,4,5,6,7), zmulti2(7,6,5,4,3,2)
  complex :: za1(size(za2)), z25(25), zflat(size(zmulti)), zempty(0)
  integer :: i, j
  save :: zflat

  za1     = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))
  zarr    = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))
  za2     = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))
  zmulti  = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))
  zmulti2 = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))
  zflat   = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))
  z25     = (IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF), IEEE_VALUE(0.0_4, IEEE_NEGATIVE_INF))

  print *, reshape((/ (cmplx(i,1.0/i), i=-4,5), &
       (IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF),IEEE_VALUE(0.0_4, IEEE_QUIET_NAN)), &
       (IEEE_VALUE(0.0_4, IEEE_QUIET_NAN),IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF)) /), (/3,4/))
  call test2(reshape((/ (cmplx(i,1.0/i), i=-4,5), &
       (IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF),IEEE_VALUE(0.0_4, IEEE_QUIET_NAN)), &
       (IEEE_VALUE(0.0_4, IEEE_QUIET_NAN),IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF)) /), (/3,4/)))
  za2 = reshape((/ (cmplx(i,1.0/i), i=-4,5), &
       (IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF),IEEE_VALUE(0.0_4, IEEE_QUIET_NAN)), &
       (IEEE_VALUE(0.0_4, IEEE_QUIET_NAN),IEEE_VALUE(0.0_4, IEEE_POSITIVE_INF)) /), (/3,4/))
  print *, za2

  print *, (/ za2 /)
  call test1((/ za2 /))
  za1 = (/ za2 /)
  print *, za1

  print *, (/ za1 /)
  call test1((/ za1 /))
  za1 = (/ za1 /)
  print *, za1

  print *, (/ za2(2,:) /)
  call test1((/ za2(2,:) /))
  zarr = (/ za2(2,:) /)
  print *, zarr

  print *, (/ (epsilon(0.0_4),1/epsilon(0.0_4)), za2(:,2) /)
  call test1((/ (epsilon(0.0_4),1/epsilon(0.0_4)), za2(:,2) /))
  zarr = (/ (epsilon(0.0_4),1/epsilon(0.0_4)), za2(:,2) /)
  print *, zarr

  print *, (/ za2(:,:) /)
  call test1((/ za2(:,:) /))
  za1 = (/ za2(:,:) /)
  print *, za1

  print *, (/ za1(ubound(za1,1):lbound(za1,1):-1) /)
  call test1((/ za1(ubound(za1,1):lbound(za1,1):-1) /))
  za1 = (/ za1(ubound(za1,1):lbound(za1,1):-1) /)
  print *, za1

  print *, (/ za2(ubound(za2,1):lbound(za2,1):-1,ubound(za2,2):lbound(za2,2):-1) /)
  call test1((/ za2(ubound(za2,1):lbound(za2,1):-1,ubound(za2,2):lbound(za2,2):-1) /))
  za1 = (/ za2(ubound(za2,1):lbound(za2,1):-1,ubound(za2,2):lbound(za2,2):-1) /)
  print *, za1

  print *, (/ ((za2(i,j),j=ubound(za2,2),lbound(za2,2),-1),i=ubound(za2,1),lbound(za2,1),-1) /)
  call test1((/ ((za2(i,j),j=ubound(za2,2),lbound(za2,2),-1),i=ubound(za2,1),lbound(za2,1),-1) /))
  za1 = (/ ((za2(i,j),j=ubound(za2,2),lbound(za2,2),-1),i=ubound(za2,1),lbound(za2,1),-1) /)
  print *, za1

  print *, (/ ((za2(i,j),i=ubound(za2,1),lbound(za2,1),-1),j=ubound(za2,2),lbound(za2,2),-1) /)
  call test1((/ ((za2(i,j),i=ubound(za2,1),lbound(za2,1),-1),j=ubound(za2,2),lbound(za2,2),-1) /))
  za1 = (/ ((za2(i,j),i=ubound(za2,1),lbound(za2,1),-1),j=ubound(za2,2),lbound(za2,2),-1) /)
  print *, za1

  print *, (/ (za2(i,ubound(za2,2):lbound(za2,2):-1),i=ubound(za2,1),lbound(za2,1),-1) /)
  call test1((/ (za2(i,ubound(za2,2):lbound(za2,2):-1),i=ubound(za2,1),lbound(za2,1),-1) /))
  za1 = (/ (za2(i,ubound(za2,2):lbound(za2,2):-1),i=ubound(za2,1),lbound(za2,1),-1) /)
  print *, za1

  print *, (/ (za2(ubound(za2,1):lbound(za2,1):-1,j), j=ubound(za2,2),lbound(za2,2),-1) /)
  call test1((/ (za2(ubound(za2,1):lbound(za2,1):-1,j), j=ubound(za2,2),lbound(za2,2),-1) /))
  za1 = (/ (za2(ubound(za2,1):lbound(za2,1):-1,j), j=ubound(za2,2),lbound(za2,2),-1) /)
  print *, za1


  print *, (/ ((za1(i), i=1,12,j), j=1,4) /)
  call test1((/ ((za1(i), i=1,12,j), j=1,4) /))
  z25 = (/ ((za1(i), i=1,12,j), j=1,4) /)
  print *, z25

  print *, (/ (za1(::j), j=1,4) /)
  call test1((/ (za1(::j), j=1,4) /))
  z25 = (/ (za1(::j), j=1,4) /)
  print *, z25

  print *, (/ ((za1(i), i=1,12,j), j=1,0) /)
  call test1((/ ((za1(i), i=1,12,j), j=1,0) /))
  zempty = (/ ((za1(i), i=1,12,j), j=1,0) /)
  print *, zempty

  print *, (/ (za1(::j), j=1,0) /)
  call test1((/ (za1(::j), j=1,0) /))
  zempty = (/ (za1(::j), j=1,0) /)
  print *, zempty


  ! Test something a little larger:

  zflat = (/ (cmplx(i,1.0/i),i=1,size(zflat)) /)
  zmulti = reshape(zflat, (/ 2,3,4,5,6,7 /))
  call test6(zmulti)
  call test6(reshape(zflat, (/ 2,3,4,5,6,7 /)))

  if( any(zflat /= (/ zmulti /)) ) stop 2

  zflat = (/ zmulti(ubound(zmulti,1):lbound(zmulti,1):-1, &
                    ubound(zmulti,2):lbound(zmulti,2):-1, &
                    ubound(zmulti,3):lbound(zmulti,3):-1, &
                    ubound(zmulti,4):lbound(zmulti,4):-1, &
                    ubound(zmulti,5):lbound(zmulti,5):-1, &
                    ubound(zmulti,6):lbound(zmulti,6):-1) /)
  zflat = (/ zflat(ubound(zflat,1):lbound(zflat,1):-1) /)

  zmulti2 = reshape(zflat, (/ 7,6,5,4,3,2 /))
  call test6(zmulti2)
  call test6(reshape(zflat, (/ 7,6,5,4,3,2 /)))

  if( any(zflat /= (/ zmulti /)) ) stop 3
  if( any(zflat /= (/ zmulti2 /)) ) stop 4
  if( any((/ zmulti /) /= (/ zmulti2 /)) ) stop 5

  print *, 'done'

contains

  subroutine test1(zarr)
    complex :: zarr(:)
    print *, zarr
  end subroutine test1

  subroutine test2(zarr)
    complex :: zarr(:,:)
    print *, zarr
  end subroutine test2

  subroutine test6(zarr)
    complex :: zarr(:,:,:,:,:,:)
    print *, zarr(lbound(zarr,1),lbound(zarr,2),lbound(zarr,3),lbound(zarr,4),lbound(zarr,5),lbound(zarr,6)), &
             zarr(ubound(zarr,1),ubound(zarr,2),ubound(zarr,3),ubound(zarr,4),ubound(zarr,5),ubound(zarr,6))
  end subroutine test6

end program acetnone10b
