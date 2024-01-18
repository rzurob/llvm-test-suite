!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAAArrayFunction007
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign allocated DTP array (container) with allocatable components returned from function
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArray007 (<-dtpIAABasic007<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  In a function, we assign values to the return variable and outside, assign the
!*  returned value to a variable.  Here, we have types with kind and length individually
!*  or together.  The function is defined in the module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArrayFunction007mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer :: iarr(l) = 0
  end type dl

  type containera(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)), allocatable :: dkvar
     type(dl(l)), allocatable :: dlvar
  end type containera

contains

  function funk2a(i)
    integer, parameter :: k = 2
    type(dk(k)), allocatable :: funk2a
    integer(k) :: i
    funk2a = dk(k)(i)
  end function funk2a

  function funk8a(i)
    integer, parameter :: k = 8
    type(dk(k)), allocatable :: funk8a
    integer(k) :: i
    funk8a = dk(k)(i)
  end function funk8a

  function funk2(i)
    integer, parameter :: k = 2
    type(dk(k)) :: funk2
    integer(k) :: i
    funk2 = dk(k)(i)
  end function funk2

  function funk8(i)
    integer, parameter :: k = 8
    type(dk(k)) :: funk8
    integer(k) :: i
    funk8 = dk(k)(i)
  end function funk8

  function funla(l,str,iarr)
    type(dl(:)), allocatable :: funla
    character(*) :: str
    integer :: l, iarr(*)
    funla = dl(l)(str,iarr(1:l))
  end function funla

  function funl(l,str,iarr)
    type(dl(l)) :: funl
    character(*) :: str
    integer :: l, iarr(*)
    funl = dl(l)(str,iarr(:l))
  end function funl

  function func2a(l,i,str,iarr)
    integer, parameter :: k = 2
    type(containera(k,:)), allocatable :: func2a
    integer :: l, iarr(*)
    integer(k) :: i
    character(*) :: str
    func2a = containera(k,l)(funk2a(i), funla(l,str,iarr))
  end function func2a

  function func8a(l,i,str,iarr)
    integer, parameter :: k = 8
    type(containera(k,:)), allocatable :: func8a
    integer :: l, iarr(*)
    integer(k) :: i
    character(*) :: str
    func8a = containera(k,l)(funk8a(i), funla(l,str,iarr))
  end function func8a

  function func2(l,i,str,iarr)
    integer, parameter :: k = 2
    type(containera(k,l)) :: func2
    integer :: l, iarr(*)
    integer(k) :: i
    character(*) :: str
    func2 = containera(k,l)(funk2(i), funl(l,str,iarr))
  end function func2

  function func8(l,i,str,iarr)
    integer, parameter :: k = 8
    type(containera(k,l)) :: func8
    integer :: l, iarr(*)
    integer(k) :: i
    character(*) :: str
    func8 = containera(k,l)(funk8(i), funl(l,str,iarr))
  end function func8

end module dtpIAAArrayFunction007mod



program dtpIAAArrayFunction007

  use dtpIAAArrayFunction007mod
  implicit none

  type(containera(2,:)), allocatable :: o1(:), o2(:), o3(:)
  type(containera(2,4)) :: o4
  type(containera(8,:)), allocatable :: v1(:), v2(:), v3(:)
  type(containera(8,4)) :: v4
  integer :: i

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  print *, allocated(o1), allocated(o2), allocated(o3)

  print *, "o1 = [func2(2,34_2,'ab',[35,36])]"
  o1 = [func2(2,34_2,'ab',[35,36])]

  print *, "o2 = [func2(2,37_2,'cd',[38,39])]"
  o2 = [func2(2,37_2,'cd',[38,39])]

  print *, "o1 = o2 {o1=", allocated(o1), (o1(i)%dkvar%ivar, o1(i)%dlvar%chvar, o1(i)%dlvar%iarr, i=1,size(o1)), &
           ", o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2)), "}"
  o1 = o2

  print *, "o3 = [func2(3,40_2,'efg',[41,42,43]),func2(3,-40_2,'gfe',[-41,-42,-43])]"
  o3 = [func2(3,40_2,'efg',[41,42,43]),func2(3,-40_2,'gfe',[-41,-42,-43])]

  print *, "o2 = o3 {o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2)), &
           ", o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3)), "}"
  o2 = o3

  print *, "o4 = func2(4,60_2,'wxyz',[61,62,63,64])"
  o4 = func2(4,60_2,'wxyz',[61,62,63,64])

  print *, "o3 = o4 {o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3)), &
           ", o4=", o4%dkvar%ivar, o4%dlvar%chvar, o4%dlvar%iarr, "}"
  o3 = o4

  ! Repeat the above with a containera of kind 4
  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = [func8(2,44_8,'AB',[45,46])]"
  v1 = [func8(2,44_8,'AB',[45,46])]

  print *, "v2 = [func8(2,47_8,'CD',[48,49])]"
  v2 = [func8(2,47_8,'CD',[48,49])]

  print *, "v1 = v2 {v1=", allocated(v1), (v1(i)%dkvar%ivar, v1(i)%dlvar%chvar, v1(i)%dlvar%iarr, i=1,size(v1)), &
           ", v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2)), "}"
  v1 = v2

  print *, "v3 = [func8(3,50_8,'EFG',[51,52,53]),func8(3,-47_8,'PQR',[-48,-49,-50])]"
  v3 = [func8(3,50_8,'EFG',[51,52,53]),func8(3,-47_8,'PQR',[-48,-49,-50])]

  print *, "v2 = v3 {v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2)), &
           ", v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3)), "}"
  v2 = v3

  print *, "v4 = func8(4,80_8,'wxyz',[81,82,83,84])"
  v4 = func8(4,80_8,'wxyz',[81,82,83,84])

  print *, "v3 = v4 {v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3)), &
           ", v4=", v4%dkvar%ivar, v4%dlvar%chvar, v4%dlvar%iarr, "}"
  v3 = v4

  print *, allocated(o1), o1%k, o1%l, (len(o1(i)%dlvar%chvar), size(o1(i)%dlvar%iarr), o1(i)%dkvar%ivar, o1(i)%dlvar%chvar, o1(i)%dlvar%iarr, i=1,size(o1))
  print *, allocated(o2), o2%k, o2%l, (len(o2(i)%dlvar%chvar), size(o2(i)%dlvar%iarr), o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2))
  print *, allocated(o3), o3%k, o3%l, (len(o3(i)%dlvar%chvar), size(o3(i)%dlvar%iarr), o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3))

  print *, allocated(v1), v1%k, v1%l, (len(v1(i)%dlvar%chvar), size(v1(i)%dlvar%iarr), v1(i)%dkvar%ivar, v1(i)%dlvar%chvar, v1(i)%dlvar%iarr, i=1,size(v1))
  print *, allocated(v2), v2%k, v2%l, (len(v2(i)%dlvar%chvar), size(v2(i)%dlvar%iarr), v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2))
  print *, allocated(v3), v3%k, v3%l, (len(v3(i)%dlvar%chvar), size(v3(i)%dlvar%iarr), v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3))

  deallocate(o1, o2, o3, v1, v2, v3)

  ! repeat with allocatable-returning function
  print *, allocated(o1), allocated(o2), allocated(o3)

  print *, "o1 = [func2a(2,34_2,'ab',[35,36])]"
  o1 = [func2a(2,34_2,'ab',[35,36])]

  print *, "o2 = [func2a(2,37_2,'cd',[38,39])]"
  o2 = [func2a(2,37_2,'cd',[38,39])]

  print *, "o1 = o2 {o1=", allocated(o1), (o1(i)%dkvar%ivar, o1(i)%dlvar%chvar, o1(i)%dlvar%iarr, i=1,size(o1)), &
           ", o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2)), "}"
  o1 = o2

  print *, "o3 = [func2a(3,40_2,'efg',[41,42,43]),func2a(3,-40_2,'gfe',[-41,-42,-43])]"
  o3 = [func2a(3,40_2,'efg',[41,42,43]),func2a(3,-40_2,'gfe',[-41,-42,-43])]

  print *, "o2 = o3 {o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2)), &
           ", o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3)), "}"
  o2 = o3

  print *, "o4 = func2a(4,60_2,'wxyz',[61,62,63,64])"
  o4 = func2a(4,60_2,'wxyz',[61,62,63,64])

  print *, "o3 = o4 {o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3)), &
           ", o4=", o4%dkvar%ivar, o4%dlvar%chvar, o4%dlvar%iarr, "}"
  o3 = o4

  ! Repeat the above with a containera of kind 4
  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = [func8a(2,44_8,'AB',[45,46])]"
  v1 = [func8a(2,44_8,'AB',[45,46])]

  print *, "v2 = [func8a(2,47_8,'CD',[48,49])]"
  v2 = [func8a(2,47_8,'CD',[48,49])]

  print *, "v1 = v2 {v1=", allocated(v1), (v1(i)%dkvar%ivar, v1(i)%dlvar%chvar, v1(i)%dlvar%iarr, i=1,size(v1)), &
           ", v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2)), "}"
  v1 = v2

  print *, "v3 = [func8a(3,50_8,'EFG',[51,52,53]),func8a(3,-47_8,'PQR',[-48,-49,-50])]"
  v3 = [func8a(3,50_8,'EFG',[51,52,53]),func8a(3,-47_8,'PQR',[-48,-49,-50])]

  print *, "v2 = v3 {v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2)), &
           ", v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3)), "}"
  v2 = v3

  print *, "v4 = func8a(4,80_8,'wxyz',[81,82,83,84])"
  v4 = func8a(4,80_8,'wxyz',[81,82,83,84])

  print *, "v3 = v4 {v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3)), &
           ", v4=", v4%dkvar%ivar, v4%dlvar%chvar, v4%dlvar%iarr, "}"
  v3 = v4

  print *, allocated(o1), o1%k, o1%l, (len(o1(i)%dlvar%chvar), size(o1(i)%dlvar%iarr), o1(i)%dkvar%ivar, o1(i)%dlvar%chvar, o1(i)%dlvar%iarr, i=1,size(o1))
  print *, allocated(o2), o2%k, o2%l, (len(o2(i)%dlvar%chvar), size(o2(i)%dlvar%iarr), o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2))
  print *, allocated(o3), o3%k, o3%l, (len(o3(i)%dlvar%chvar), size(o3(i)%dlvar%iarr), o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3))

  print *, allocated(v1), v1%k, v1%l, (len(v1(i)%dlvar%chvar), size(v1(i)%dlvar%iarr), v1(i)%dkvar%ivar, v1(i)%dlvar%chvar, v1(i)%dlvar%iarr, i=1,size(v1))
  print *, allocated(v2), v2%k, v2%l, (len(v2(i)%dlvar%chvar), size(v2(i)%dlvar%iarr), v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2))
  print *, allocated(v3), v3%k, v3%l, (len(v3(i)%dlvar%chvar), size(v3(i)%dlvar%iarr), v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3))

end program dtpIAAArrayFunction007
