!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
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
!*  Declare a type with multiple KIND and LEN parameters, and invoke a generic
!*  typebound procedure on an object, using assumed type parameters.
!*  This differs from dtpATPDAGenericPassedObjectModule in that the single Workshop
!*  type is replaced with a class hierarchy terminating in the Workshop type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAInheritanceModuleMod

  implicit none

  type, abstract :: StudentEvent(snBytes)
     integer, kind :: snBytes
   contains
     procedure, pass :: print4 => printStudentEvent4
     procedure, pass :: print2 => printStudentEvent2
     procedure, pass :: print1 => printStudentEvent1
     generic :: print => print4, print2, print1
  end type StudentEvent

  type, extends(StudentEvent) :: CataloguedEvent (nStudents)
     integer, len :: nStudents
     integer(snBytes) :: studentNumber(nStudents)
   contains
     procedure, pass :: print4 => printCataloguedEvent4
     procedure, pass :: print2 => printCataloguedEvent2
     procedure, pass :: print1 => printCataloguedEvent1
  end type CataloguedEvent

  type, extends(CataloguedEvent) :: Workshop(longestFirstName,longestLastName)
     integer, len :: longestFirstName, longestLastName
     character(longestFirstName + longestLastName + 1) :: name(nStudents)
   contains
     procedure, pass :: print4 => printWorkshop4
     procedure, pass :: print2 => printWorkshop2
     procedure, pass :: print1 => printWorkshop1
  end type Workshop

contains

  subroutine printStudentEvent4(this)
    class (StudentEvent(4)) :: this
    print *, "Long Student Event object"
  end subroutine printStudentEvent4

  subroutine printStudentEvent2(this)
    class (StudentEvent(2)) :: this
    print *, "Middle Student Event object"
  end subroutine printStudentEvent2

  subroutine printStudentEvent1(this)
    class (StudentEvent(1)) :: this
    print *, "Short Student Event object"
  end subroutine printStudentEvent1

  subroutine printCataloguedEvent4(this)
    class (CataloguedEvent(4,*)) :: this
    print *, "Long CE student numbers:", this%nStudents, this%studentNumber
  end subroutine printCataloguedEvent4

  subroutine printCataloguedEvent2(this)
    class (CataloguedEvent(2,*)) :: this
    print *, "Mid-length CE student numbers:", this%nStudents, this%studentNumber
  end subroutine printCataloguedEvent2

  subroutine printCataloguedEvent1(this)
    class (CataloguedEvent(1,*)) :: this
    print *, "Short CE student numbers:", this%nStudents, this%studentNumber
  end subroutine printCataloguedEvent1

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

end module dtpATPDAInheritanceModuleMod

program dtpATPDAInheritanceModule

  use :: dtpATPDAInheritanceModuleMod
  implicit none
  type(Workshop(4,4,6,8)), target :: w4_1
  type(Workshop(4,3,8,16)), target :: w4_2

  type(Workshop(2,6,3,3)), target :: w2_1
  type(Workshop(2,1,11,6)), target :: w2_2

  type(Workshop(1,9,0,0)), target :: w1_1
  type(Workshop(1,3,4,4)), target :: w1_2

  type(CataloguedEvent(4,4)), target :: ce4_1
  type(CataloguedEvent(4,3)), target :: ce4_2

  type(CataloguedEvent(2,6)), target :: ce2_1
  type(CataloguedEvent(2,1)), target :: ce2_2

  type(CataloguedEvent(1,9)), target :: ce1_1
  type(CataloguedEvent(1,3)), target :: ce1_2

  class (StudentEvent(1)), pointer :: se1
  class (StudentEvent(2)), pointer :: se2
  class (StudentEvent(4)), pointer :: se4

  integer :: i

  w4_1 = Workshop(4,4,6,8)([1234567,2345678,3456789,4567890], ["Jonas  Salk    ","Albert Einstein","Marie  Curie   ","Isaac  Newton  "])
  w4_2 = Workshop(4,3,8,16)([1234567,2345678,3456789], ["Pyotr    Tschaikovsky    ","Nicolai  Rimsky-Korsakov ","Modest   Mussorgsky      "])

  w2_1 = Workshop(2,6,3,3)([(5**i,i=1,6)], [(repeat(achar(iachar('a')+i),7),i=1,6)])
  w2_2 = Workshop(2,1,11,6)([32767], ["Heironymous Bosch "])

  w1_1 = Workshop(1,9,0,0)([(i**2,i=1,9)], [(achar(iachar('z')-i),i=1,9)])
  w1_2 = Workshop(1,3,4,4)([-127, 0 , 127], ['John John','Jane Joan','Xing Lang'])

  call w4_1 % print
  call w4_2 % print

  call w2_1 % print
  call w2_2 % print

  call w1_1 % print
  call w1_2 % print

  call w4_1 % CataloguedEvent % print
  call w4_2 % CataloguedEvent % print
  call w2_1 % CataloguedEvent % print
  call w2_2 % CataloguedEvent % print
  call w1_1 % CataloguedEvent % print
  call w1_2 % CataloguedEvent % print

  ce4_1 = w4_1 % CataloguedEvent
  ce4_2 = w4_2 % CataloguedEvent
  ce2_1 = w2_1 % CataloguedEvent
  ce2_2 = w2_2 % CataloguedEvent
  ce1_1 = w1_1 % CataloguedEvent
  ce1_2 = w1_2 % CataloguedEvent

  call ce4_1 % print
  call ce4_2 % print

  call ce2_1 % print
  call ce2_2 % print

  call ce1_1 % print
  call ce1_2 % print

end program dtpATPDAInheritanceModule
