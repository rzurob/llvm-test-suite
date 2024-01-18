!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : acetnone10e
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-08-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : arrays and array sections as content (logical)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : array section, logical
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Build different size arrays and assign them and sections of them to
!*  eachother.  Work backwards and forwards, and in different strides.
!*  Here we focus on logical data.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone10e

  implicit none

  integer :: i, j, npchars
  ! create a 5x4 array with AND, OR, EQV, XOR and "implies" columns
  logical :: left(4), right(4), a2(4,5), multi(2,3,4,5,6,7), multi2(7,6,5,4,3,2)
  logical :: asrc(size(a2)), a1(size(a2)), arr(size(a2,2)), a25(25), flat(size(multi)), empty(0)

  integer(8) :: i8

  save :: flat
  equivalence (a1,a2)

  left   = (/ .false., .false., .true., .true. /)
  right  = (/ .false., .true., .false., .true. /)
  a2(:,1) = left .or. right
  a2(:,2) = left .and. right
  a2(:,3) = left .eqv. right
  a2(:,4) = left .neqv. right
  a2(:,5) = (.not. left) .or. right

  ! a2 should now be F T T T F F F T T F F T F T T F T T F T, i.e.:
  !   F F T F T
  !   T F F T T
  !   T F F T F
  !   T T T F T

  asrc = a1

  a1     = .false.
  a2     = .false.
  multi  = .false.
  multi2 = .false.
  flat   = .false.
  a25    = .false.

  print *, "A:>", reshape((/ (asrc(i), i=1,20) /), (/4,5/)),"<"
  call test2(reshape((/ (asrc(i), i=1,20) /), (/4,5/)))
  a2 = reshape((/ (asrc(i), i=1,20) /), (/4,5/))
  print *, "  >",a2,"<"

  print *, "B:>", (/ a2 /),"<"
  call test1((/ a2 /))
  a1 = (/ a2 /)
  print *, "  >",a1,"<"

  print *, "C:>", (/ a1 /),"<"
  call test1((/ a1 /))
  a1 = (/ a1 /)
  print *, "  >",a1,"<"

  print *, "D:>", (/ a2(2,:) /),"<"
  call test1((/ a2(2,:) /))
  arr = (/ a2(2,:) /)
  print *, "  >",arr,"<"

  print *, "E:>", (/ .true., a2(:,2) /),"<"
  call test1((/ .true., a2(:,2) /))
  arr = (/ .true., a2(:,2) /)
  print *, "  >",arr,"<"

  print *, "F:>", (/ a2(:,:) /),"<"
  call test1((/ a2(:,:) /))
  a1 = (/ a2(:,:) /)
  print *, "  >",a1,"<"

  print *, "G:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /) .EQV. (/ asrc /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /) .EQV. (/ asrc /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /) .EQV. (/ asrc /)
  print *, "  >",a1,"<"

  print *, "H:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) .EQV. asrc /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1) .EQV. asrc /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) .EQV. asrc /)
  print *, "  >",a1,"<"

  print *, "J:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /)
  print *, "  >",a1,"<"

  print *, "K:>", (/ a2(ubound(a2,1):lbound(a2,1):-1,ubound(a2,2):lbound(a2,2):-1) /),"<"
  call test1((/ a2(ubound(a2,1):lbound(a2,1):-1,ubound(a2,2):lbound(a2,2):-1) /))
  a1 = (/ a2(ubound(a2,1):lbound(a2,1):-1,ubound(a2,2):lbound(a2,2):-1) /)
  print *, "  >",a1,"<"

  print *, "L:>", (/ ((a2(i,j),j=ubound(a2,2),lbound(a2,2),-1),i=ubound(a2,1),lbound(a2,1),-1) /),"<"
  call test1((/ ((a2(i,j),j=ubound(a2,2),lbound(a2,2),-1),i=ubound(a2,1),lbound(a2,1),-1) /))
  a1 = (/ ((a2(i,j),j=ubound(a2,2),lbound(a2,2),-1),i=ubound(a2,1),lbound(a2,1),-1) /)
  print *, "  >",a1,"<"

  print *, "M:>", (/ ((a2(i,j),i=ubound(a2,1),lbound(a2,1),-1),j=ubound(a2,2),lbound(a2,2),-1) /),"<"
  call test1((/ ((a2(i,j),i=ubound(a2,1),lbound(a2,1),-1),j=ubound(a2,2),lbound(a2,2),-1) /))
  a1 = (/ ((a2(i,j),i=ubound(a2,1),lbound(a2,1),-1),j=ubound(a2,2),lbound(a2,2),-1) /)
  print *, "  >",a1,"<"

  print *, "N:>", (/ (a2(i,ubound(a2,2):lbound(a2,2):-1),i=ubound(a2,1),lbound(a2,1),-1) /),"<"
  call test1((/ (a2(i,ubound(a2,2):lbound(a2,2):-1),i=ubound(a2,1),lbound(a2,1),-1) /))
  a1 = (/ (a2(i,ubound(a2,2):lbound(a2,2):-1),i=ubound(a2,1),lbound(a2,1),-1) /)
  print *, "  >",a1,"<"

  print *, "P:>", (/ (a2(ubound(a2,1):lbound(a2,1):-1,j), j=ubound(a2,2),lbound(a2,2),-1) /),"<"
  call test1((/ (a2(ubound(a2,1):lbound(a2,1):-1,j), j=ubound(a2,2),lbound(a2,2),-1) /))
  a1 = (/ (a2(ubound(a2,1):lbound(a2,1):-1,j), j=ubound(a2,2),lbound(a2,2),-1) /)
  print *, "  >",a1,"<"


  print *, "Q:>", (/ ((asrc(i), i=1,12,j), j=1,4) /),"<"
  call test1((/ ((asrc(i), i=1,12,j), j=1,4) /))
  a25 = (/ ((asrc(i), i=1,12,j), j=1,4) /)
  print *, "  >",a25,"<"

  print *, "R:>", (/ (asrc(::j), j=1,4) /),"<"
  call test1((/ (asrc(::j), j=1,4) /))
  block
    logical a42(42)
    a42 = (/ (asrc(::j), j=1,4) /)
    print *, "  >",a42,"<"
  end block

  print *, "S:>", (/ ((asrc(i), i=1,12,j), j=1,0) /),"<"
  call test1((/ ((asrc(i), i=1,12,j), j=1,0) /))
  empty = (/ ((asrc(i), i=1,12,j), j=1,0) /)
  print *, "  >",empty,"<"

  print *, "T:>", (/ (asrc(::j), j=1,0) /),"<"
  call test1((/ (asrc(::j), j=1,0) /))
  empty = (/ (asrc(::j), j=1,0) /)
  print *, "  >",empty,"<"

  ! We don't care what values flat has, we just need to initialise it in an interesting way.
  flat = (/ (ibits(transfer(sin(real(i)/size(flat)),i),16,1) == 1, i=1,size(flat)) /)
  multi = reshape(flat, (/ 2,3,4,5,6,7 /))
  call test6(multi)
  call test6(reshape(flat, (/ 2,3,4,5,6,7 /)))

  if( any(flat .neqv. (/ multi /)) ) stop 2

  flat = (/ multi(ubound(multi,1):lbound(multi,1):-1, &
                  ubound(multi,2):lbound(multi,2):-1, &
                  ubound(multi,3):lbound(multi,3):-1, &
                  ubound(multi,4):lbound(multi,4):-1, &
                  ubound(multi,5):lbound(multi,5):-1, &
                  ubound(multi,6):lbound(multi,6):-1) /)
  flat = (/ flat(ubound(flat,1):lbound(flat,1):-1) /)

  multi2 = reshape(flat, (/ 7,6,5,4,3,2 /))
  call test6(multi2)
  call test6(reshape(flat, (/ 7,6,5,4,3,2 /)))

  if( any(flat .neqv. (/ multi /)) ) stop 3
  if( any(flat .neqv. (/ multi2 /)) ) stop 4
  if( any((/ multi /) .neqv. (/ multi2 /)) ) stop 5

  print *, 'done'

contains

  subroutine test1(arr)
    logical :: arr(:)
    print *, "  >", arr, "<"
  end subroutine test1

  subroutine test2(arr)
    logical :: arr(:,:)
    print *, "  >", arr, "<"
  end subroutine test2

  subroutine test6(arr)
    logical :: arr(:,:,:,:,:,:)
    print *, "  >", arr(lbound(arr,1),lbound(arr,2),lbound(arr,3),lbound(arr,4),lbound(arr,5),lbound(arr,6)), &
             arr(ubound(arr,1),ubound(arr,2),ubound(arr,3),ubound(arr,4),ubound(arr,5),ubound(arr,6)), "<"
  end subroutine test6
  
end program acetnone10e
