!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAAtypicalPassedObjectPosition
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : passed object at a different position in the call sequence
!*
!*  REFERENCE                  : Feature Number 357495
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Declare a type with typebound procedures using assumed type parameters which
!*  expect the passed object in different positions in the call sequence.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAAtypicalPassedObjectPositionMod

  implicit none

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name(nStudents)
     integer(snBytes) :: studentNumber(nStudents)
   contains
     procedure, pass(this) :: print => printWorkshop
  end type Workshop

contains

  ! print participants "from" to "to" in "this" workshop
  subroutine printWorkshop(from, to, this)
    class (Workshop(4,*,*,*)) :: this
    integer, intent(in) :: from, to
    integer :: i
    print *, "Printing", from, "to", to, "of", this%nStudents, "students (", this%longestFirstName, this%longestLastName, ")"
    do i = from, to
       print *, i, this%name(i), ":", this%studentNumber(i)
    end do
  end subroutine printWorkshop

end module dtpATPDAAtypicalPassedObjectPositionMod

program dtpATPDAAtypicalPassedObjectPosition

  use :: dtpATPDAAtypicalPassedObjectPositionMod
  implicit none
  type(Workshop(4,4,6,8)) :: w1
  type(Workshop(4,3,8,16)) :: w2

  w1 = Workshop(4,4,6,8)(["Jonas  Salk    ","Albert Einstein","Marie  Curie   ","Isaac  Newton  "], [1234567,2345678,3456789,4567890])
  w2 = Workshop(4,3,8,16)(["Pyotr    Tschaikovsky    ","Nicolai  Rimsky-Korsakov ","Modest   Mussorgsky      "], [1234567,2345678,3456789])

  call w1 % print(1,2)
  call w2 % print(2,3)

end program dtpATPDAAtypicalPassedObjectPosition
