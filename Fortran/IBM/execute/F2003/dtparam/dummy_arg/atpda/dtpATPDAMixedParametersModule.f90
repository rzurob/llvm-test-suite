!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 357495
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Declare a type with multiple KIND and LEN parameters, and allow some of them to be assumed type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAMixedParametersModuleMod

  implicit none

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name(nStudents)
     integer(snBytes) :: studentNumber(nStudents)
  end type Workshop

contains

  subroutine printClass(wkshop)
    type (Workshop(4,*,*,*)) :: wkshop
    integer :: i
    print *, "There are", wkshop%nStudents, "students with first names of", wkshop%longestFirstName, &
             "or fewer characters, and last names of", wkshop%longestLastName, "or fewer characters"
    do i = 1, wkshop%nStudents
       print *, i, wkshop%name(i), ":", wkshop%studentNumber(i)
    end do
  end subroutine printClass

end module dtpATPDAMixedParametersModuleMod

program dtpATPDAMixedParametersModule

  use :: dtpATPDAMixedParametersModuleMod
  implicit none
  type(Workshop(4,4,6,8)) :: w1
  type(Workshop(4,3,8,16)) :: w2

  w1 = Workshop(4,4,6,8)(["Jonas  Salk    ","Albert Einstein","Marie  Curie   ","Isaac  Newton  "], [1234567,2345678,3456789,4567890])
  w2 = Workshop(4,3,8,16)(["Pyotr    Tschaikovsky    ","Nicolai  Rimsky-Korsakov ","Modest   Mussorgsky      "], [1234567,2345678,3456789])

  call printClass(w1)
  call printClass(w2)

end program dtpATPDAMixedParametersModule
