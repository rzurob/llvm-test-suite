!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2006-08-10
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : arrays and array sections as content
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
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone10

  implicit none

  integer :: iarr(4), ia2(3,4), imulti(2,3,4,5,6,7), imulti2(7,6,5,4,3,2)
  integer :: i, j
  integer :: ia1(size(ia2)), i25(25), iflat(size(imulti)), iempty(0)
  save :: iflat

  ia1     = -1
  iarr    = -2
  ia2     = -3
  imulti  = -4
  imulti2 = -5
  iflat   = -6

  print *, reshape((/ (i, i=1,12) /), (/3,4/))
  call test2(reshape((/ (i, i=1,12) /), (/3,4/)))
  ia2 = reshape((/ (i, i=1,12) /), (/3,4/))
  print *, ia2

  print *, (/ ia2 /)
  call test1((/ ia2 /))
  ia1 = (/ ia2 /)
  print *, ia1

  print *, (/ ia1 /)
  call test1((/ ia1 /))
  ia1 = (/ ia1 /)
  print *, ia1

  print *, (/ ia2(2,:) /)
  call test1((/ ia2(2,:) /))
  iarr = (/ ia2(2,:) /)
  print *, iarr

  print *, (/ ia2(:,2), 100 /)
  call test1((/ ia2(:,2), 100 /))
  iarr = (/ ia2(:,2), 100 /)
  print *, iarr

  print *, (/ ia2(:,:) /)
  call test1((/ ia2(:,:) /))
  ia1 = (/ ia2(:,:) /)
  print *, ia1

  print *, (/ ia1(ubound(ia1,1):lbound(ia1,1):-1) /)
  call test1((/ ia1(ubound(ia1,1):lbound(ia1,1):-1) /))
  ia1 = (/ ia1(ubound(ia1,1):lbound(ia1,1):-1) /)
  print *, ia1

  print *, (/ ia2(ubound(ia2,1):lbound(ia2,1):-1,ubound(ia2,2):lbound(ia2,2):-1) /)
  call test1((/ ia2(ubound(ia2,1):lbound(ia2,1):-1,ubound(ia2,2):lbound(ia2,2):-1) /))
  ia1 = (/ ia2(ubound(ia2,1):lbound(ia2,1):-1,ubound(ia2,2):lbound(ia2,2):-1) /)
  print *, ia1

  print *, (/ ((ia2(i,j),j=ubound(ia2,2),lbound(ia2,2),-1),i=ubound(ia2,1),lbound(ia2,1),-1) /)
  call test1((/ ((ia2(i,j),j=ubound(ia2,2),lbound(ia2,2),-1),i=ubound(ia2,1),lbound(ia2,1),-1) /))
  ia1 = (/ ((ia2(i,j),j=ubound(ia2,2),lbound(ia2,2),-1),i=ubound(ia2,1),lbound(ia2,1),-1) /)
  print *, ia1

  print *, (/ ((ia2(i,j),i=ubound(ia2,1),lbound(ia2,1),-1),j=ubound(ia2,2),lbound(ia2,2),-1) /)
  call test1((/ ((ia2(i,j),i=ubound(ia2,1),lbound(ia2,1),-1),j=ubound(ia2,2),lbound(ia2,2),-1) /))
  ia1 = (/ ((ia2(i,j),i=ubound(ia2,1),lbound(ia2,1),-1),j=ubound(ia2,2),lbound(ia2,2),-1) /)
  print *, ia1

  print *, (/ (ia2(i,ubound(ia2,2):lbound(ia2,2):-1),i=ubound(ia2,1),lbound(ia2,1),-1) /)
  call test1((/ (ia2(i,ubound(ia2,2):lbound(ia2,2):-1),i=ubound(ia2,1),lbound(ia2,1),-1) /))
  ia1 = (/ (ia2(i,ubound(ia2,2):lbound(ia2,2):-1),i=ubound(ia2,1),lbound(ia2,1),-1) /)
  print *, ia1

  print *, (/ (ia2(ubound(ia2,1):lbound(ia2,1):-1,j), j=ubound(ia2,2),lbound(ia2,2),-1) /)
  call test1((/ (ia2(ubound(ia2,1):lbound(ia2,1):-1,j), j=ubound(ia2,2),lbound(ia2,2),-1) /))
  ia1 = (/ (ia2(ubound(ia2,1):lbound(ia2,1):-1,j), j=ubound(ia2,2),lbound(ia2,2),-1) /)
  print *, ia1


  print *, (/ ((ia1(i), i=1,12,j), j=1,4) /)
  call test1((/ ((ia1(i), i=1,12,j), j=1,4) /))
  i25 = (/ ((ia1(i), i=1,12,j), j=1,4) /)
  print *, i25

  print *, (/ (ia1(::j), j=1,4) /)
  call test1((/ (ia1(::j), j=1,4) /))
  i25 = (/ (ia1(::j), j=1,4) /)
  print *, i25

  print *, (/ ((ia1(i), i=1,12,j), j=1,0) /)
  call test1((/ ((ia1(i), i=1,12,j), j=1,0) /))
  iempty = (/ ((ia1(i), i=1,12,j), j=1,0) /)
  print *, iempty

  print *, (/ (ia1(::j), j=1,0) /)
  call test1((/ (ia1(::j), j=1,0) /))
  iempty = (/ (ia1(::j), j=1,0) /)
  print *, iempty


  ! Test something a little larger:

  iflat = (/ (i,i=1,size(iflat)) /)
  imulti = reshape(iflat, (/ 2,3,4,5,6,7 /))
  call test6(imulti)
  call test6(reshape(iflat, (/ 2,3,4,5,6,7 /)))

  if( any(iflat /= (/ imulti /)) ) stop 2

  iflat = (/ imulti(ubound(imulti,1):lbound(imulti,1):-1, &
                    ubound(imulti,2):lbound(imulti,2):-1, &
                    ubound(imulti,3):lbound(imulti,3):-1, &
                    ubound(imulti,4):lbound(imulti,4):-1, &
                    ubound(imulti,5):lbound(imulti,5):-1, &
                    ubound(imulti,6):lbound(imulti,6):-1) /)
  iflat = (/ iflat(ubound(iflat,1):lbound(iflat,1):-1) /)

  imulti2 = reshape(iflat, (/ 7,6,5,4,3,2 /))
  call test6(imulti2)
  call test6(reshape(iflat, (/ 7,6,5,4,3,2 /)))

  if( any(iflat /= (/ imulti /)) ) stop 3
  if( any(iflat /= (/ imulti2 /)) ) stop 4
  if( any((/ imulti /) /= (/ imulti2 /)) ) stop 5

  print *, 'done'

contains

  subroutine test1(iarr)
    integer :: iarr(:)
    print *, iarr
  end subroutine test1

  subroutine test2(iarr)
    integer :: iarr(:,:)
    print *, iarr
  end subroutine test2

  subroutine test6(iarr)
    integer :: iarr(:,:,:,:,:,:)
    print *, iarr(lbound(iarr,1),lbound(iarr,2),lbound(iarr,3),lbound(iarr,4),lbound(iarr,5),lbound(iarr,6)), &
             iarr(ubound(iarr,1),ubound(iarr,2),ubound(iarr,3),ubound(iarr,4),ubound(iarr,5),ubound(iarr,6))
  end subroutine test6

end program acetnone10
