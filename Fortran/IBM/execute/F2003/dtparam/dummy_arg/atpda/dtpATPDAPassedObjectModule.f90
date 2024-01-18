!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAPassedObjectModule
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : passed object procedure
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
!*  Declare a type with multiple KIND and LEN parameters, and invoke a typebound
!*  procedure on an object, using assumed type parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAPassedObjectModuleMod

  implicit none

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name(nStudents)
     integer(snBytes) :: studentNumber(nStudents)
   contains
     procedure, pass :: print => printWorkshop
  end type Workshop

contains

  subroutine printWorkshop(this)
    class (Workshop(4,*,*,*)) :: this
    integer :: i
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       print *, i, this%name(i), ":", this%studentNumber(i)
    end do
  end subroutine printWorkshop

end module dtpATPDAPassedObjectModuleMod

program dtpATPDAPassedObjectModule

  use :: dtpATPDAPassedObjectModuleMod
  implicit none
  type(Workshop(4,4,6,8)) :: w1
  type(Workshop(4,3,8,16)) :: w2

  w1 = Workshop(4,4,6,8)(["Jonas  Salk    ","Albert Einstein","Marie  Curie   ","Isaac  Newton  "], [1234567,2345678,3456789,4567890])
  w2 = Workshop(4,3,8,16)(["Pyotr    Tschaikovsky    ","Nicolai  Rimsky-Korsakov ","Modest   Mussorgsky      "], [1234567,2345678,3456789])

  call w1 % print
  call w2 % print

end program dtpATPDAPassedObjectModule
