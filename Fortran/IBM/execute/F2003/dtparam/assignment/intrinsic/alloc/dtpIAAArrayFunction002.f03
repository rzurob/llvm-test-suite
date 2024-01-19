!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign allocated DTP array (len) returned from function
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArray002 (<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  In a function, we assign values to the return variable and outside, assign the
!*  returned value to a variable.  Here, we have a type with len.
!*  The function is defined in the module.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArrayFunction002mod

  implicit none
  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer      :: ivar(l) = 0
  end type dl

contains

  function funla(l,str,iarr)
    type(dl(:)), allocatable :: funla
    character(*) :: str
    integer :: l, iarr(*)
    funla = dl(l)(str,iarr)
  end function funla

  function funl(l,str,iarr)
    type(dl(l)) :: funl
    character(*) :: str
    integer :: l, iarr(*)
    funl = dl(l)(str,iarr)
  end function funl

end module dtpIAAArrayFunction002mod



program dtpIAAArrayFunction002

  use dtpIAAArrayFunction002mod
  implicit none

  type(dl(:)), allocatable :: v1(:), v2(:), v3(:)
  type(dl(4)) :: v4

  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = [funl(1,'a',[1234321])]"
  v1 = [funl(1,'a',[1234321])]

  print *, "v2 = v1 {v1=", allocated(v1), v1, "}"
  v2 = v1

  print *, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  v1 = v2

  print *, "v3 = [funl(3,'def',[44444,55555,66666]), funl(3,'ghi',[77777,88888,99999])]"
  v3 = [funl(3,'def',[44444,55555,66666]), funl(3,'ghi',[77777,88888,99999])]

  print *, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  v2 = v3

  print *, "v3 = v1 {v3=", allocated(v3), v3, ", v1=", allocated(v1), v1, "}"
  v3 = v1

  print *, "v4 = funl(4,'jklm',[4040404,3030303,2020202,1010101])"
  v4 = funl(4,'jklm',[4040404,3030303,2020202,1010101])

  print *, "v1 = v4 {v1=", allocated(v1), v1, ", v4=", v4, "}"
  v1 = v4

  print *, allocated(v1), v1%l, len(v1%chvar), v1
  print *, allocated(v2), v2%l, len(v2%chvar), v2
  print *, allocated(v3), v3%l, len(v3%chvar), v3

  deallocate(v1, v2, v3)

  ! repeat with allocatable-returning function
  print *, allocated(v1), allocated(v2), allocated(v3)

  print *, "v1 = [funla(1,'a',[1234321])]"
  v1 = [funla(1,'a',[1234321])]

  print *, "v2 = v1 {v1=", allocated(v1), v1, "}"
  v2 = v1

  print *, "v1 = v2 {v1=", allocated(v1), v1, ", v2=", allocated(v2), v2, "}"
  v1 = v2

  print *, "v3 = [funla(3,'def',[44444,55555,66666]), funla(3,'ghi',[77777,88888,99999])]"
  v3 = [funla(3,'def',[44444,55555,66666]), funla(3,'ghi',[77777,88888,99999])]

  print *, "v2 = v3 {v2=", allocated(v2), v2, ", v3=", allocated(v3), v3, "}"
  v2 = v3

  print *, "v3 = v1 {v3=", allocated(v3), v3, ", v1=", allocated(v1), v1, "}"
  v3 = v1

  print *, "v4 = funla(4,'jklm',[4040404,3030303,2020202,1010101])"
  v4 = funla(4,'jklm',[4040404,3030303,2020202,1010101])

  print *, "v1 = v4 {v1=", allocated(v1), v1, ", v4=", v4, "}"
  v1 = v4

  print *, allocated(v1), v1%l, len(v1%chvar), v1
  print *, allocated(v2), v2%l, len(v2%chvar), v2
  print *, allocated(v3), v3%l, len(v3%chvar), v3

end program dtpIAAArrayFunction002
