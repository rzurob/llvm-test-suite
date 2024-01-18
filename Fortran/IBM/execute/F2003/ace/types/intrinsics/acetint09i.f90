!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acetint09i
!*
!*  DATE                       : 2006-10-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : data pointers in AC's with intrinsic type specifiers (integer)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Include initialised data pointer references in AC's and later test their
!*  values, to verify that they are allowed and have the correct value.
!*  The data pointers refer to variables apart from the array as well as to
!*  members and sections of the array.  Pointers and variables of different
!*  kinds are also used.
!*  After the pointers are set up and independent variables initialised, we
!*  create AC's, making sure that different pointers appear in different
!*  positions, and that some references are into parts of the array which are
!*  to be changed.  Section 7.4.1.3 of the standard tells us that "The execution
!*  of the assignment shall have the same effect as if the evaluation of all
!*  operations in expr [the RHS] and variable [the LHS] occurred before any
!*  portion of variable is defined by the assignment."  To verify this, we
!*  print the expected values before the assignment and the actual values after.
!*  We also use some pointers of different kinds, to verify that appropriate
!*  conversions are made.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program acetint09i

  implicit none

  integer(1), target  :: itK1,        iarrK1(4)
  integer(2), target  :: itK2,        iarrK2(4)
  integer(4), target  :: itK4, it2K4, iarrK4(4), iarrD3K4(3,3,3)
  integer(8), target  :: itK8,        iarrK8(4)

  integer(1), pointer :: ipK1,               ipaK1(:)
  integer(2), pointer :: ipK2,               ipaK2(:)
  integer(4), pointer :: ipK4, ip2K4, ip3K4, ipaK4(:), ipaD3K4(:,:,:)
  integer(8), pointer :: ipK8,               ipaK8(:)

  integer :: i, j, k, tmp1, tmp2

  itK4  =  5
  it2K4 =  6
  ipK4  => itK4
  ip2K4 => it2K4
  ip3K4 => iarrK4(2)
  ipaK4 => iarrK4(2:3)

  print *, 'a.p', (/ (i, i=1,4,1) /)                          ! 1,2,3,4
  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'a  ', iarrK4

  print *, 'b p', (/ itK4, ipK4, ip2K4, ip3K4 /)              ! 5,5,6,2
  iarrK4 = (/ itK4, ipK4, ip2K4, ip3K4 /)                     ! 5,5,6,2
  print *, 'b  ', iarrK4

  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'b8p', (/ itK4, ipK4, ip2K4, ip3K4 /)              ! 5,5,6,2
  iarrK8 = (/ itK4, ipK4, ip2K4, ip3K4 /)                     ! 5,5,6,2
  print *, 'b8 ', iarrK8

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'bip', (/ integer(4):: itK4, ipK4, ip2K4, ip3K4 /) ! 5,5,6,2
  iarrK4 = (/ integer(4):: itK4, ipK4, ip2K4, ip3K4 /)        ! 5,5,6,2
  print *, 'b.i', iarrK4


  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'c p', (/ ip3K4, itK4, ip2K4, ipK4 /)              ! 2,5,6,5
  iarrK4 = (/ ip3K4, itK4, ip2K4, ipK4 /)                     ! 2,5,6,5
  print *, 'c  ', iarrK4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'c.p', (/ integer(4):: ip3K4, itK4, ip2K4, ipK4 /) ! 2,5,6,5
  iarrK4 = (/ integer(4):: ip3K4, itK4, ip2K4, ipK4 /)        ! 2,5,6,5
  print *, 'c.i', iarrK4


  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'd p', (/ itK4, ip3K4, ip2K4, ipK4 /)              ! 5,2,6,5
  iarrK4 = (/ itK4, ip3K4, ip2K4, ipK4 /)                     ! 5,2,6,5
  print *, 'd  ', iarrK4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'd.p', (/ integer(4):: itK4, ip3K4, ip2K4, ipK4 /) ! 5,2,6,5
  iarrK4 = (/ integer(4):: itK4, ip3K4, ip2K4, ipK4 /)        ! 5,2,6,5
  print *, 'd.i', iarrK4


  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'e p', (/ ipaK4, ipaK4 /)                          ! 2,3,2,3
  iarrK4 = (/ ipaK4, ipaK4 /)                                 ! 2,3,2,3
  print *, 'e  ', iarrK4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'e.p', (/ integer(4):: ipaK4, ipaK4 /)             ! 2,3,2,3
  iarrK4 = (/ integer(4):: ipaK4, ipaK4 /)                    ! 2,3,2,3
  print *, 'e.i', iarrK4


  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'f p', (/ (ipaK4, i=1,2) /)                        ! 2,3,2,3
  iarrK4 = (/ (ipaK4, i=1,2) /)                               ! 2,3,2,3
  print *, 'f  ', iarrK4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'f.p', (/ integer(4):: (ipaK4, i=1,2) /)           ! 2,3,2,3
  iarrK4 = (/ integer(4):: (ipaK4, i=1,2) /)                  ! 2,3,2,3
  print *, 'f.i', iarrK4


  ipaK4 => iarrK4(3:2:-1)

  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'g p', (/ ipaK4, ipaK4 /)                          ! 3,2,3,2
  iarrK4 = (/ ipaK4, ipaK4 /)                                 ! 3,2,3,2
  print *, 'g  ', iarrK4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'g.p', (/ integer(4):: ipaK4, ipaK4 /)             ! 3,2,3,2
  iarrK4 = (/ integer(4):: ipaK4, ipaK4 /)                    ! 3,2,3,2
  print *, 'g.i', iarrK4


  iarrK4 = (/ (i, i=1,4,1) /)                                 ! 1,2,3,4
  print *, 'h p', (/ (ipaK4, i=1,2) /)                        ! 3,2,3,2
  iarrK4 = (/ (ipaK4, i=1,2) /)                               ! 3,2,3,2
  print *, 'h  ', iarrK4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'h.p', (/ integer(4):: (ipaK4, i=1,2) /)           ! 3,2,3,2
  iarrK4 = (/ integer(4):: (ipaK4, i=1,2) /)                  ! 3,2,3,2
  print *, 'h.i', iarrK4


  itK1 = huge(itK1);  ipK1 => itK1
  itK2 = huge(itK2);  ipK2 => itK2
  itK8 = huge(itK8);  ipK8 => itK8


  print *, 'i.p', [ integer(8):: ipK8, ipK1, ipK2, ip2K4 ]    ! bignum,127,32767,6
  iarrK8 = [ integer(8):: ipK8, ipK1, ipK2, ip2K4 ]           ! bignum,127,32767,6
  print *, 'i.i', iarrK8

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'j.p', [ integer(4):: (iarrK8(i),i=3,4) ]          ! prints 32767,6
  ipaK4 = [ integer(4):: (iarrK8(i),i=3,4) ]                  ! 32767,6
  print *, 'j.i', ipaK4                                       ! prints 32767,6
  print *, 'j.2', iarrK4                                      ! prints 1,32767,6,4

  iarrK4 = (/ integer(4):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'k.p', [ integer(4):: ipaK4(2:1:-1) ]              ! prints 32767,6
  ipaK4 = [ integer(4):: ipaK4(2:1:-1) ]                      ! 32767,6
  print *, 'k.i', ipaK4                                       ! prints 32767,6

  iarrK8 = (/ integer(8):: (i, i=1,4,1) /)                    ! 1,2,3,4
  print *, 'l.p', [ integer(8):: ipaK4, ipaK4 ]               ! 32767,6,32767,6
  iarrK8 = [ integer(8):: ipaK4, ipaK4 ]                      ! 32767,6,32767,6
  print *, 'l.i', iarrK8

  iarrK1 = [integer(1):: (10+i, i=1,4)];  ipaK1 => iarrK1(1:1)
  iarrK2 = [integer(2):: (20+i, i=1,4)];  ipaK2 => iarrK2(2:2)
  iarrK4 = [integer(4):: (40+i, i=1,4)];  ipaK4 => iarrK4(3:3)
  iarrK8 = [integer(8):: (80+i, i=1,4)];  ipaK8 => iarrK8(4:4)

  print *, 'm.p', [integer(1):: ipaK1, ipaK2, ipaK4, ipaK8]   ! 11,22,43,84
  iarrK1 = [integer(1):: ipaK1, ipaK2, ipaK4, ipaK8]          ! 11,22,43,84
  print *, 'm.i', iarrK1

  iarrK1 = [integer(1):: (10+i, i=1,4)]
  print *, 'n.p', [integer(1):: ipaK8, ipaK4, ipaK2, ipaK1]   ! 84,43,22,11
  iarrK1 = [integer(1):: ipaK8, ipaK4, ipaK2, ipaK1]          ! 84,43,22,11
  print *, 'n.i', iarrK1

  iarrD3K4 = reshape([integer(4):: (((i*100+j*10+k,i=1,3),j=1,3),k=1,3)], [3,3,3])
  ipaD3K4 => iarrD3K4(1:3:2,1:3:2,1:1)                        ! 1,1,1; 3,1,1; 1,3,1; 3,3,1
  print *, "o.p", [integer(4):: ipaD3K4], [integer(2):: ipaD3K4]
  iarrK4 = [integer(4):: ipaD3K4]
  iarrK2 = [integer(2):: ipaD3K4]
  print *, "o.i", iarrK4, iarrK2

end program acetint09i
