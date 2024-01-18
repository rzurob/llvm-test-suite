!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2006-08-11
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : arrays and array sections as content (character)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : array section, character
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Build different size arrays and assign them and sections of them to
!*  eachother.  Work backwards and forwards, and in different strides.
!*  Here we focus on character data.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetnone10c

  implicit none

  integer :: i, j, npchars
  character(3) :: arr(4), a2(3,4), multi(2,3,4,5,6,7), multi2(7,6,5,4,3,2)
  character(3) :: asrc(size(a2)), a1(size(a2)), a25(25), flat(size(multi)), empty(0)
  character(1) :: ch1(36)
  save :: flat
  equivalence (ch1,asrc)

  a1     = '#'
  arr    = '#'
  a2     = '#'
  multi  = '#'
  multi2 = '#'
  flat   = '#'
  a25    = '#'

  ! asrc/ch1 are "abcdefghijklmnopqrstuvwxyz0123456789", i.e.,
  ! asrc is "abc","def","ghi","jkl","mno","pqr","stu","vwx","yz0","123","456","789"
  ! and ch1 is "a","b","c",...,"9"
  ! reversed, asrc is "789","456","123","yz0","vwx","stu","pqr","mno","jkl","ghi","def","abc"
  ch1 = (/ (char(97+i),i=0,25), (char(48+i),i=0,9) /)

  print *, "A:>", reshape((/ (asrc(i), i=1,12) /), (/3,4/)),"<"
  call test2(reshape((/ (asrc(i), i=1,12) /), (/3,4/)))
  a2 = reshape((/ (asrc(i), i=1,12) /), (/3,4/))
  print *, "  >",a2,"<"

  ! a2 is now:
  !  abc jkl stu 123
  !  def mno vwx 456
  !  ghi pqr yz0 789


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

  print *, "E:>", (/ 'xxx', a2(:,2) /),"<"
  call test1((/ 'xxx', a2(:,2) /))
  arr = (/ 'xxx', a2(:,2) /)
  print *, "  >",arr,"<"

  print *, "F:>", (/ a2(:,:) /),"<"
  call test1((/ a2(:,:) /))
  a1 = (/ a2(:,:) /)
  print *, "  >",a1,"<"

  print *, "G:>", (/ a2(:,:)(2:) /),"<"
  call test1((/ a2(:,:)(2:) /))
  a1 = (/ a2(:,:)(2:) /)
  print *, "  >",a1,"<"

  print *, "H:>", (/ a2(:,:)(3:) // a2(:,:)(:1) /),"<"
  call test1((/ a2(:,:)(3:) // a2(:,:)(:1) /))
  a1 = (/ a2(:,:)(3:) // a2(:,:)(:1) /)
  print *, "  >", a1, "<"

  print *, "J:>", (/ a2(:,:)(3:1) /), '<'
  call test1((/ a2(:,:)(3:1) /))
  a1 = (/ a2(:,:)(3:1) /)
  print *, "  >", a1, "<"

  print *, "K:>", (/ asrc(:)(3:) /),"<"
  call test1((/ asrc(:)(3:) /))
  a1 = (/ asrc(:)(3:) /)
  print *, "  >",a1,"<"

  print *, "L:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) /)
  print *, "  >",a1,"<"

  print *, "M:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) /)
  print *, "  >",a1,"<"

  print *, "N:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(:1) /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(:1) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(:1) /)
  print *, "  >",a1,"<"

  print *, "O:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) /) // (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) /) // (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) /) // (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) /)
  print *, "  >",a1,"<"

  print *, "P:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) // asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) // asrc(ubound(asrc,1):lbound(asrc,1):-1)(:1) /), "<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) // asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) // asrc(ubound(asrc,1):lbound(asrc,1):-1)(:1) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1)(3:) // asrc(ubound(asrc,1):lbound(asrc,1):-1)(2:2) // asrc(ubound(asrc,1):lbound(asrc,1):-1)(:1) /)
  print *, "  >",a1,"<"

  print *, "Q:>", (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /),"<"
  call test1((/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /))
  a1 = (/ asrc(ubound(asrc,1):lbound(asrc,1):-1) /)
  print *, "  >",a1,"<"

  print *, "R:>", (/ a2(ubound(a2,1):lbound(a2,1):-1,ubound(a2,2):lbound(a2,2):-1) /),"<"
  call test1((/ a2(ubound(a2,1):lbound(a2,1):-1,ubound(a2,2):lbound(a2,2):-1) /))
  a1 = (/ a2(ubound(a2,1):lbound(a2,1):-1,ubound(a2,2):lbound(a2,2):-1) /)
  print *, "  >",a1,"<"

  print *, "S:>", (/ ((a2(i,j),j=ubound(a2,2),lbound(a2,2),-1),i=ubound(a2,1),lbound(a2,1),-1) /),"<"
  call test1((/ ((a2(i,j),j=ubound(a2,2),lbound(a2,2),-1),i=ubound(a2,1),lbound(a2,1),-1) /))
  a1 = (/ ((a2(i,j),j=ubound(a2,2),lbound(a2,2),-1),i=ubound(a2,1),lbound(a2,1),-1) /)
  print *, "  >",a1,"<"

  print *, "T:>", (/ ((a2(i,j),i=ubound(a2,1),lbound(a2,1),-1),j=ubound(a2,2),lbound(a2,2),-1) /),"<"
  call test1((/ ((a2(i,j),i=ubound(a2,1),lbound(a2,1),-1),j=ubound(a2,2),lbound(a2,2),-1) /))
  a1 = (/ ((a2(i,j),i=ubound(a2,1),lbound(a2,1),-1),j=ubound(a2,2),lbound(a2,2),-1) /)
  print *, "  >",a1,"<"

  print *, "U:>", (/ (a2(i,ubound(a2,2):lbound(a2,2):-1),i=ubound(a2,1),lbound(a2,1),-1) /),"<"
  call test1((/ (a2(i,ubound(a2,2):lbound(a2,2):-1),i=ubound(a2,1),lbound(a2,1),-1) /))
  a1 = (/ (a2(i,ubound(a2,2):lbound(a2,2):-1),i=ubound(a2,1),lbound(a2,1),-1) /)
  print *, "  >",a1,"<"

  print *, "V:>", (/ (a2(ubound(a2,1):lbound(a2,1):-1,j), j=ubound(a2,2),lbound(a2,2),-1) /),"<"
  call test1((/ (a2(ubound(a2,1):lbound(a2,1):-1,j), j=ubound(a2,2),lbound(a2,2),-1) /))
  a1 = (/ (a2(ubound(a2,1):lbound(a2,1):-1,j), j=ubound(a2,2),lbound(a2,2),-1) /)
  print *, "  >",a1,"<"


  print *, "W:>", (/ ((asrc(i), i=1,12,j), j=1,4) /),"<"
  call test1((/ ((asrc(i), i=1,12,j), j=1,4) /))
  a25 = (/ ((asrc(i), i=1,12,j), j=1,4) /)
  print *, "  >",a25,"<"

  print *, "X:>", (/ (asrc(::j), j=1,4) /),"<"
  call test1((/ (asrc(::j), j=1,4) /))
  a25 = (/ (asrc(::j), j=1,4) /)
  print *, "  >",a25,"<"

  print *, "Y:>", (/ ((asrc(i), i=1,12,j), j=1,0) /),"<"
  call test1((/ ((asrc(i), i=1,12,j), j=1,0) /))
  empty = (/ ((asrc(i), i=1,12,j), j=1,0) /)
  print *, "  >",empty,"<"

  print *, "Z:>", (/ (asrc(::j), j=1,0) /),"<"
  call test1((/ (asrc(::j), j=1,0) /))
  empty = (/ (asrc(::j), j=1,0) /)
  print *, "  >",empty,"<"


  ! Test something a little larger:
  npchars = 126 + 1 - 33
  ! this is '!!' to 'VZ':
  flat = (/ (achar(33+mod(i/npchars,npchars)) // achar(33+mod(i,npchars)),i=0,size(flat)-1) /)
  multi = reshape(flat, (/ 2,3,4,5,6,7 /))
  call test6(multi)
  call test6(reshape(flat, (/ 2,3,4,5,6,7 /)))

  if( any(flat /= (/ multi /)) ) stop 2

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

  if( any(flat /= (/ multi /)) ) stop 3
  if( any(flat /= (/ multi2 /)) ) stop 4
  if( any((/ multi /) /= (/ multi2 /)) ) stop 5

  print *, 'done'

contains

  subroutine test1(arr)
    character(*) :: arr(:)
    print *, "  >", arr, "<"
  end subroutine test1

  subroutine test2(arr)
    character(*) :: arr(:,:)
    print *, "  >", arr, "<"
  end subroutine test2

  subroutine test6(arr)
    character(*) :: arr(:,:,:,:,:,:)
    print *, "  >", arr(lbound(arr,1),lbound(arr,2),lbound(arr,3),lbound(arr,4),lbound(arr,5),lbound(arr,6)), &
             arr(ubound(arr,1),ubound(arr,2),ubound(arr,3),ubound(arr,4),ubound(arr,5),ubound(arr,6)), "<"
  end subroutine test6

end program acetnone10c
