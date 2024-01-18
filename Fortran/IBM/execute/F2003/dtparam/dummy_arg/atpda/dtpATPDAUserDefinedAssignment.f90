!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : user-defined assignment
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
!*  Declare and use a type with different assignment operators involving assumed type parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAUserDefinedAssignmentMod

  implicit none

  type Student(snBytes,longestFirstName,longestLastName)
     integer, len :: longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name
     integer(snBytes) :: studentNumber
   contains
     procedure, pass :: printStudent2
     procedure, pass :: printStudent4
     generic :: print => printStudent2, printStudent4
     procedure, pass :: studentAssignFromString2
     procedure, pass :: studentAssignFromNumber2
     procedure, pass :: studentAssignCopy2
     procedure, pass :: studentAssignFromString4
     procedure, pass :: studentAssignFromNumber4
     procedure, pass :: studentAssignCopy4
     generic :: assignment(=) => studentAssignFromString2, studentAssignFromNumber2, studentAssignCopy2, &
                                 studentAssignFromString4, studentAssignFromNumber4, studentAssignCopy4
  end type Student

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     type(Student(snBytes, longestFirstName, longestLastName)) :: student(nStudents)
   contains
     procedure, pass :: printWorkshop2
     procedure, pass :: printWorkshop4
     generic :: print => printWorkshop2, printWorkshop4
     procedure, pass :: workshopAssignFromStudents2
     procedure, pass :: workshopAssignCopy2
     procedure, pass :: workshopAssignFromStudents4
     procedure, pass :: workshopAssignCopy4
     generic :: assignment(=) => workshopAssignFromStudents2, workshopAssignCopy2, &
                                 workshopAssignFromStudents4, workshopAssignCopy4
  end type Workshop


contains

  elemental subroutine studentAssignFromString2(sink, src)
    class(Student(2,*,*)), intent(inout) :: sink
    character(*), intent(in) :: src
    sink % name = src
  end subroutine studentAssignFromString2

  elemental subroutine studentAssignFromNumber2(sink, src)
    class(Student(2,*,*)), intent(inout) :: sink
    integer(2), intent(in) :: src
    sink % studentNumber = src
  end subroutine studentAssignFromNumber2

  elemental subroutine studentAssignCopy2(sink, src)
    class(Student(2,*,*)), intent(inout) :: sink
    class(Student(2,*,*)), intent(in) :: src

    sink % name = src % name
    sink % studentNumber = src % studentNumber
  end subroutine studentAssignCopy2

  subroutine workshopAssignFromStudents2(sink, src)
    class(Workshop(2,*,*,*)), intent(inout) :: sink
    class(Student(2,*,*)), intent(in) :: src(:)
    if (sink % nStudents /= size(src)) error stop 32
    sink % student = src
  end subroutine workshopAssignFromStudents2

  elemental subroutine workshopAssignCopy2(sink, src)
    class(Workshop(2,*,*,*)), intent(inout) :: sink
    class(Workshop(2,*,*,*)), intent(in) :: src
    if (sink % nStudents == src % nStudents)  sink % student = src % student
  end subroutine workshopAssignCopy2


  elemental subroutine studentAssignFromString4(sink, src)
    class(Student(4,*,*)), intent(inout) :: sink
    character(*), intent(in) :: src
    sink % name = src
  end subroutine studentAssignFromString4

  elemental subroutine studentAssignFromNumber4(sink, src)
    class(Student(4,*,*)), intent(inout) :: sink
    integer(4), intent(in) :: src
    sink % studentNumber = src
  end subroutine studentAssignFromNumber4

  elemental subroutine studentAssignCopy4(sink, src)
    class(Student(4,*,*)), intent(inout) :: sink
    class(Student(4,*,*)), intent(in) :: src

    sink % name = src % name
    sink % studentNumber = src % studentNumber
  end subroutine studentAssignCopy4

  subroutine workshopAssignFromStudents4(sink, src)
    class(Workshop(4,*,*,*)), intent(inout) :: sink
    class(Student(4,*,*)), intent(in) :: src(:)
    if (sink % nStudents /= size(src)) error stop 32
    sink % student = src
  end subroutine workshopAssignFromStudents4

  elemental subroutine workshopAssignCopy4(sink, src)
    class(Workshop(4,*,*,*)), intent(inout) :: sink
    class(Workshop(4,*,*,*)), intent(in) :: src
    if (sink % nStudents == src % nStudents)  sink % student = src % student
  end subroutine workshopAssignCopy4


  subroutine printWorkshop4(this)
    class (Workshop(4,*,*,*)) :: this
    integer :: i
    print *, "Long student numbers:"
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       call this%student(i) % print
    end do
  end subroutine printWorkshop4

  subroutine printWorkshop2(this)
    class (Workshop(2,*,*,*)) :: this
    integer :: i
    print *, "Mid-length student numbers:"
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       call this%student(i) % print
    end do
  end subroutine printWorkshop2

  subroutine printStudent4(this)
    class (Student(4,*,*)) :: this
    print *, this%name, ":", this%studentNumber
  end subroutine printStudent4

  subroutine printStudent2(this)
    class (Student(2,*,*)) :: this
    print *, this%name, ":", this%studentNumber
  end subroutine printStudent2

end module dtpATPDAUserDefinedAssignmentMod

program dtpATPDAUserDefinedAssignment

  use :: dtpATPDAUserDefinedAssignmentMod
  implicit none
  type(Workshop(4,4,6,8)) :: w4_1, w4_1a
  type(Workshop(4,3,8,16)) :: w4_2

  type(Workshop(2,6,3,3)) :: w2_1, w2_1a
  type(Workshop(2,1,11,6)) :: w2_2

  integer :: i

  w4_1 = Workshop(4,4,6,8)([Student(4,6,8)("Jonas  Salk    ",1234567), &
                            Student(4,6,8)("Albert Einstein",2345678), &
                            Student(4,6,8)("Marie  Curie   ",3456789), &
                            Student(4,6,8)("Isaac  Newton  ",4567890)])
  w4_2 = Workshop(4,3,8,16)([Student(4,8,16)("Pyotr    Tschaikovsky    ",1234567), &
                             Student(4,8,16)("Nicolai  Rimsky-Korsakov ",2345678), &
                             Student(4,8,16)("Modest   Mussorgsky      ",3456789)])

  w2_1 = Workshop(2,6,3,3)([(Student(2,3,3)(repeat(achar(iachar('a')+i),7),5**i),i=1,6)])
  w2_2 = Workshop(2,1,11,6)([Student(2,11,6)("Heironymous Bosch ",32767)])

  call w4_1 % print
  call w4_2 % print

  print *
  call w2_1 % print
  call w2_2 % print

  w4_1 % student(1) = "Alex   Borodin"
  w4_1 % student(3) = "Anders Angstrom"
  w4_1 % student(2) = 999999999
  w4_1 % student(3) = 555555555

  w4_2 % student(2) = w4_1 % student(1)

  print *
  call w4_1 % print
  call w4_2 % print

  w4_2 = w4_1 % student(1:3)
  w4_1a = w4_1

  print *
  call w4_2 % print
  call w4_1 % print
  call w4_1a % print

  print *

  w2_1 % student(1) = "#######"
  w2_1 % student(3) = "$$$$$$$"
  w2_1 % student(2) = 9999_2
  w2_1 % student(3) = 5555_2

  w2_2 % student(1) = w2_1 % student(2)

  print *
  call w2_1 % print
  call w2_2 % print

  w2_2 = w2_1 % student(1:1)
  w2_1a = w2_1

  print *
  call w2_2 % print
  call w2_1 % print
  call w2_1a % print
  print *, "done"

end program dtpATPDAUserDefinedAssignment
