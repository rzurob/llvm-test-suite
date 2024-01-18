!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAGenericPassedObjectModule
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : passed object generic procedure
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
!*  Declare a type with multiple KIND and LEN parameters, and invoke a generic
!*  typebound procedure on an object, using assumed type parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAGenericPassedObjectModuleMod

  implicit none

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name(nStudents)
     integer(snBytes) :: studentNumber(nStudents)
   contains
     procedure, pass :: print4 => printWorkshop4
     procedure, pass :: print2 => printWorkshop2
     procedure, pass :: print1 => printWorkshop1
     generic :: print => print4, print2, print1
  end type Workshop

contains

  subroutine printWorkshop4(this)
    class (Workshop(4,*,*,*)) :: this
    integer :: i
    print *, "Long student numbers:"
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       print *, i, this%name(i), ":", this%studentNumber(i)
    end do
  end subroutine printWorkshop4

  subroutine printWorkshop2(this)
    class (Workshop(2,*,*,*)) :: this
    integer :: i
    print *, "Mid-length student numbers:"
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       print *, i, this%name(i), ":", this%studentNumber(i)
    end do
  end subroutine printWorkshop2

  subroutine printWorkshop1(this)
    class (Workshop(1,*,*,*)) :: this
    integer :: i
    print *, "Short student numbers:"
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       print *, i, this%name(i), ":", this%studentNumber(i)
    end do
  end subroutine printWorkshop1

end module dtpATPDAGenericPassedObjectModuleMod

program dtpATPDAGenericPassedObjectModule

  use :: dtpATPDAGenericPassedObjectModuleMod
  implicit none
  type(Workshop(4,4,6,8)) :: w4_1
  type(Workshop(4,3,8,16)) :: w4_2

  type(Workshop(2,6,3,3)) :: w2_1
  type(Workshop(2,1,11,6)) :: w2_2

  type(Workshop(1,9,0,0)) :: w1_1
  type(Workshop(1,3,4,4)) :: w1_2

  integer :: i

  w4_1 = Workshop(4,4,6,8)(["Jonas  Salk    ","Albert Einstein","Marie  Curie   ","Isaac  Newton  "], [1234567,2345678,3456789,4567890])
  w4_2 = Workshop(4,3,8,16)(["Pyotr    Tschaikovsky    ","Nicolai  Rimsky-Korsakov ","Modest   Mussorgsky      "], [1234567,2345678,3456789])

  w2_1 = Workshop(2,6,3,3)([(repeat(achar(iachar('a')+i),7),i=1,6)],[(5**i,i=1,6)])
  w2_2 = Workshop(2,1,11,6)(["Heironymous Bosch "],[32767])

  w1_1 = Workshop(1,9,0,0)([(achar(iachar('z')-i),i=1,9)],[(i**2,i=1,9)])
  w1_2 = Workshop(1,3,4,4)(['John John','Jane Joan','Xing Lang'], [-127, 0 , 127])

  call w4_1 % print
  call w4_2 % print

  call w2_1 % print
  call w2_2 % print

  call w1_1 % print
  call w1_2 % print

end program dtpATPDAGenericPassedObjectModule
