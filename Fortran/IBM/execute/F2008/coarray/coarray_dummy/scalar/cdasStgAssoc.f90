!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cdasStgAssoc
!*
!*  PROGRAMMER                 : dforster
!*  DATE                       : 2010-11-17
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments - scalar
!*  SECONDARY FUNCTIONS TESTED : storage association
!*  ADAPTED FROM               : -
!*
!*  DESCRIPTION
!*
!*  Array elements can also represent the start of an array; we test this.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program cdasStgAssoc
  implicit none
  integer, save :: array(9)[*]
  integer :: myNumber, j
  myNumber = this_image()
  array = [(myNumber+j,j=0,size(array)-1)]
  call printSome(array(1), 4) ! 2 3 4 5
  call printSome(array(3), 5) ! 4 5 6 7 8
  call printSome(array(7), 3) ! 8 9 10
  call printSomeMore(array(1), 4)
  call printSomeMore(array(3), 5)
  call printSomeMore(array(7), 3)

contains

  subroutine printSome(arg,n)
    integer :: arg(*)[*]
    integer, intent(in) :: n
    integer :: i
    if (myNumber == 2) then
       print *, (arg(i),i=1,n)
    end if
  end subroutine printSome

  subroutine printSomeMore(arg,n)
    integer :: arg(*)
    integer, intent(in) :: n
    integer :: i
    if (myNumber == 2) then
       print *, (arg(i),i=1,n)
    end if
  end subroutine printSomeMore

end program cdasStgAssoc
