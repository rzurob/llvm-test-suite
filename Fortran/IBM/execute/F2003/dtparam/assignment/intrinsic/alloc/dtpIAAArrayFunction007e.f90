!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign allocated DTP (container) with allocatable components returned from elemental functions
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArrayFunction007 (<-dtpIAAArray007<-dtpIAABasic007<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  In a function, we assign values to the return variable and outside, assign the
!*  returned value to a variable.  Here, we have types with kind and length individually
!*  or together.  The function is defined in the module.
!*  Here the functions are elemental.
!
!   Fixes by JX 20100111: elemental function results can NOT have non-constant
!   length type parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArrayFunction007emod

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

  type intArray(k,l) ! integer array wrapper - to get around limitation on arrays as arguments to elemental functions
     integer, kind :: k
     integer, len  :: l
     integer(k) :: iarr(l)
  end type intArray

contains

  elemental function funk2(i)
    integer, parameter :: k = 2
    type(dk(k)) :: funk2
    integer(k), intent(in) :: i
    funk2 = dk(k)(i)
  end function funk2

  elemental function funk8(i)
    integer, parameter :: k = 8
    type(dk(k)) :: funk8
    integer(k), intent(in) :: i
    funk8 = dk(k)(i)
  end function funk8

  elemental function funl2(l,str,i_arr)
    integer, intent(in) :: l
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(dl(2)) :: funl2
    funl2 = dl(2)(str,i_arr%iarr)
  end function

  elemental function funl3(l,str,i_arr)
    integer, intent(in) :: l
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(dl(3)) :: funl3
    funl3 = dl(3)(str,i_arr%iarr)
  end function

  elemental function funl4(l,str,i_arr)
    integer, intent(in) :: l
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(dl(4)) :: funl4
    funl4 = dl(4)(str,i_arr%iarr)
  end function

  elemental function func22(l,i,str,i_arr)
    integer, parameter :: k = 2
    integer, intent(in) :: l
    integer(k), intent(in) :: i
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(containera(k,2)) :: func22
    func22 = containera(k,2)(funk2(i), funl2(l,str,i_arr)) !!
  end function

  elemental function func23(l,i,str,i_arr)
    integer, parameter :: k = 2
    integer, intent(in) :: l
    integer(k), intent(in) :: i
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(containera(k,3)) :: func23
    func23 = containera(k,3)(funk2(i), funl3(l,str,i_arr)) !!
  end function

  elemental function func24(l,i,str,i_arr)
    integer, parameter :: k = 2
    integer, intent(in) :: l
    integer(k), intent(in) :: i
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(containera(k,4)) :: func24
    func24 = containera(k,4)(funk2(i), funl4(l,str,i_arr)) !!
  end function

  elemental function func82(l,i,str,i_arr)
    integer, parameter :: k = 8
    integer, intent(in) :: l
    integer(k), intent(in) :: i
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(containera(k,2)) :: func82
    func82 = containera(k,2)(funk8(i), funl2(l,str,i_arr)) !!
  end function

  elemental function func83(l,i,str,i_arr)
    integer, parameter :: k = 8
    integer, intent(in) :: l
    integer(k), intent(in) :: i
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(containera(k,3)) :: func83
    func83 = containera(k,3)(funk8(i), funl3(l,str,i_arr)) !!
  end function

  elemental function func84(l,i,str,i_arr)
    integer, parameter :: k = 8
    integer, intent(in) :: l
    integer(k), intent(in) :: i
    character(*), intent(in) :: str
    type(intArray(4,*)), intent(in) :: i_arr
    type(containera(k,4)) :: func84
    func84 = containera(k,4)(funk8(i), funl4(l,str,i_arr)) !!
  end function

end module dtpIAAArrayFunction007emod



program dtpIAAArrayFunction007e

  use dtpIAAArrayFunction007emod
  implicit none

  type(containera(2,:)), allocatable :: o1(:), o2(:), o3(:)
  type(containera(2,4)) :: o4
  type(containera(8,:)), allocatable :: v1(:), v2(:), v3(:)
  type(containera(8,4)) :: v4
  integer :: i

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  print *, allocated(o1), allocated(o2), allocated(o3)

  print *, "o1 = func2([2],[34_2],['ab'],[intArray(4,2)([35,36])])"
  o1 = func22([2],[34_2],['ab'],[intArray(4,2)([35,36])])

  print *, "o2 = func2([2],[37_2],'cd',intArray(4,2)([38,39]))"
  o2 = func22([2],[37_2],'cd',intArray(4,2)([38,39]))

  print *, "o1 = o2 {o1=", allocated(o1), (o1(i)%dkvar%ivar, o1(i)%dlvar%chvar, o1(i)%dlvar%iarr, i=1,size(o1)), &
           ", o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2)), "}"
  o1 = o2

  print *, "o3 = func2(3,[40_2,-40_2],['efg','gfe'],[intArray(4,3)([41,42,43]), intArray(4,3)([-41,-42,-43])])"
  o3 = func23(3,[40_2,-40_2],['efg','gfe'],[intArray(4,3)([41,42,43]), intArray(4,3)([-41,-42,-43])])

  print *, "o2 = o3 {o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2)), &
           ", o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3)), "}"
  o2 = o3

  print *, "o4 = func2(4,60_2,'wxyz',intArray(4,4)([61,62,63,64]))"
  o4 = func24(4,60_2,'wxyz',intArray(4,4)([61,62,63,64]))

  print *, "o3 = o4 {o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3)), &
           ", o4=", o4%dkvar%ivar, o4%dlvar%chvar, o4%dlvar%iarr, "}"

  deallocate (o3)
  allocate (containera(2,4) :: o3(2))

  o3 = o4

  ! Repeat the above with a containera of kind 4
  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = func8([2],44_8,'AB',intArray(4,2)([45,46]))"
  v1 = func82([2],44_8,'AB',intArray(4,2)([45,46]))

  print *, "v2 = func8(2,[47_8],'CD',intArray(4,2)([48,49]))"
  v2 = func82(2,[47_8],'CD',intArray(4,2)([48,49]))

  print *, "v1 = v2 {v1=", allocated(v1), (v1(i)%dkvar%ivar, v1(i)%dlvar%chvar, v1(i)%dlvar%iarr, i=1,size(v1)), &
           ", v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2)), "}"
  v1 = v2

  print *, "v3 = func8(3,[50_8,-47_8],['EFG','PQR'],[intArray(4,3)([51,52,53]),intArray(4,3)([-48,-49,-50])])"
  v3 = func83(3,[50_8,-47_8],['EFG','PQR'],[intArray(4,3)([51,52,53]),intArray(4,3)([-48,-49,-50])])

  print *, "v2 = v3 {v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2)), &
           ", v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3)), "}"
  v2 = v3

  print *, "v4 = func8(4,80_8,'wxyz',intArray(4,4)([81,82,83,84]))"
  v4 = func84(4,80_8,'wxyz',intArray(4,4)([81,82,83,84]))

  print *, "v3 = v4 {v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3)), &
           ", v4=", v4%dkvar%ivar, v4%dlvar%chvar, v4%dlvar%iarr, "}"

  deallocate (v3)
  allocate (containera(8,4) :: v3(2))

  v3 = v4

  print *, allocated(o1), o1%k, o1%l, (len(o1(i)%dlvar%chvar), size(o1(i)%dlvar%iarr), o1(i)%dkvar%ivar, o1(i)%dlvar%chvar, o1(i)%dlvar%iarr, i=1,size(o1))
  print *, allocated(o2), o2%k, o2%l, (len(o2(i)%dlvar%chvar), size(o2(i)%dlvar%iarr), o2(i)%dkvar%ivar, o2(i)%dlvar%chvar, o2(i)%dlvar%iarr, i=1,size(o2))
  print *, allocated(o3), o3%k, o3%l, (len(o3(i)%dlvar%chvar), size(o3(i)%dlvar%iarr), o3(i)%dkvar%ivar, o3(i)%dlvar%chvar, o3(i)%dlvar%iarr, i=1,size(o3))

  print *, allocated(v1), v1%k, v1%l, (len(v1(i)%dlvar%chvar), size(v1(i)%dlvar%iarr), v1(i)%dkvar%ivar, v1(i)%dlvar%chvar, v1(i)%dlvar%iarr, i=1,size(v1))
  print *, allocated(v2), v2%k, v2%l, (len(v2(i)%dlvar%chvar), size(v2(i)%dlvar%iarr), v2(i)%dkvar%ivar, v2(i)%dlvar%chvar, v2(i)%dlvar%iarr, i=1,size(v2))
  print *, allocated(v3), v3%k, v3%l, (len(v3(i)%dlvar%chvar), size(v3(i)%dlvar%iarr), v3(i)%dkvar%ivar, v3(i)%dlvar%chvar, v3(i)%dlvar%iarr, i=1,size(v3))

end program dtpIAAArrayFunction007e
