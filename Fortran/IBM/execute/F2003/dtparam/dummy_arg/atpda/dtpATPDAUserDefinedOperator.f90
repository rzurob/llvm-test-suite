!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAUserDefinedOperator
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : user-defined operators
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
!*  Declare and use a type with different operators involving assumed type parameters.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAUserDefinedOperatorMod

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
     procedure, pass :: studentSimilarity2
     procedure, pass :: studentSimilarity4
     generic :: operator(.similarity.) => studentSimilarity2, studentSimilarity4
  end type Student

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     type(Student(snBytes, longestFirstName, longestLastName)) :: student(nStudents)
   contains
     procedure, pass :: printWorkshop2
     procedure, pass :: printWorkshop4
     generic :: print => printWorkshop2, printWorkshop4
     procedure, pass :: workshopSimilarity2
     procedure, pass :: workshopSimilarity4
     generic :: operator(.similarity.) => workshopSimilarity2, workshopSimilarity4
     procedure, pass :: workshopContains2
     procedure, pass :: workshopContains4
     procedure, pass :: workshopContainsInt2
     procedure, pass :: workshopContainsInt4
     generic :: operator(.contains.) => workshopContains2, workshopContains4, &
                                        workshopContainsInt2, workshopContainsInt4
  end type Workshop


contains

  elemental function workshopSimilarity4(this, that)
    class(Workshop(4,*,*,*)), intent(in) :: this, that
    integer(4) :: workshopSimilarity4
    integer(4) :: i, j, l
    workshopSimilarity4 = 0
    l = max(this%longestFirstName, that%longestFirstName)
    do i = 1, this%nStudents
       do j = 1, that%nStudents
          if ((this%student(i) .similarity. that%student(j)) > l) workshopSimilarity4 = workshopSimilarity4 + 1
       end do
    end do
  end function workshopSimilarity4

  elemental function workshopSimilarity2(this, that)
    class(Workshop(2,*,*,*)), intent(in) :: this, that
    integer(4) :: workshopSimilarity2
    integer(4) :: i, j, l
    workshopSimilarity2 = 0
    l = max(this%longestFirstName, that%longestFirstName)
    do i = 1, this%nStudents
       do j = 1, that%nStudents
          if ((this%student(i) .similarity. that%student(j)) > l) workshopSimilarity2 = workshopSimilarity2 + 1
       end do
    end do
  end function workshopSimilarity2


  elemental function studentSimilarity4(this, that)
    class(Student(4,*,*)), intent(in) :: this, that
    integer(4) :: studentSimilarity4
    integer :: i
    do i = 1, len(this%name)
       if (this%name(i:i) /= that%name(i:i)) exit
    end do
    studentSimilarity4 = i - 1
  end function studentSimilarity4

  elemental function studentSimilarity2(this, that)
    class(Student(2,*,*)), intent(in) :: this, that
    integer(4) :: studentSimilarity2
    integer(4) :: i
    do i = 1, len(this%name)
       if (this%name(i:i) /= that%name(i:i)) exit
    end do
    studentSimilarity2 = i - 1
  end function studentSimilarity2


  elemental logical function workshopContains4(this, name)
    class(Workshop(4,*,*,*)), intent(in) :: this
    character(*), intent(in) :: name
    workshopContains4 = any(index(this%student%name,name) /= 0)
  end function workshopContains4

  elemental logical function workshopContains2(this, name)
    class(Workshop(2,*,*,*)), intent(in) :: this
    character(*), intent(in) :: name
    workshopContains2 = any(index(this%student%name,name) /= 0)
  end function workshopContains2


  elemental logical function workshopContainsInt4(this, num)
    class(Workshop(4,*,*,*)), intent(in) :: this
    integer(4), intent(in) :: num
    workshopContainsInt4 = any(this%student%studentNumber == num)
  end function workshopContainsInt4

  elemental logical function workshopContainsInt2(this, num)
    class(Workshop(2,*,*,*)), intent(in) :: this
    integer(2), intent(in) :: num
    workshopContainsInt2 = any(this%student%studentNumber == num)
  end function workshopContainsInt2


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
    print *, this%name, "|", this%studentNumber
  end subroutine printStudent2

end module dtpATPDAUserDefinedOperatorMod

program dtpATPDAUserDefinedOperator

  use :: dtpATPDAUserDefinedOperatorMod
  implicit none
  type(Workshop(4,4,6,8)) :: w4_1, w4_1a
  type(Workshop(4,3,6,12)) :: w4_2

  type(Workshop(2,6,3,3)) :: w2_1, w2_1a
  type(Workshop(2,1,11,6)) :: w2_2

  type(Student(4,9,10)) :: sArr1(3)
  type(Student(4,9,12)) :: sArr2(3)
  type(Workshop(4,3,9,10)) :: w4_3
  type(Workshop(4,3,9,12)) :: w4_4

  integer :: i

  w4_1 = Workshop(4,4,6,8)([Student(4,6,8)("Jonas  Salk    ",1234567), &
                            Student(4,6,8)("Alex   Borodin ",2345678), &
                            Student(4,6,8)("Hilde  v.Bingen",3456789), &
                            Student(4,6,8)("Isaac  Newton  ",4567890)])
  w4_2 = Workshop(4,3,6,12)([Student(4,6,12)("Pyotr  Tschaikovsky",1234567), & 
                             Student(4,6,12)("Alex   Borodin     ",2345678), &
                             Student(4,6,12)("Hilde  von Bingen  ",3456789)])

  w2_1 = Workshop(2,6,3,3)([(Student(2,3,3)(repeat(achar(iachar('a')+i),7),5**i),i=1,6)])
  w2_2 = Workshop(2,1,11,6)([Student(2,11,6)("Heironymous Bosch ",32767)])

  call w4_1 % print
  call w4_2 % print

  write(*,'(a)') ''
  call w2_1 % print
  call w2_2 % print

  sArr1 = [Student(4,9,10)("Jonas     Salk      ",1234567), &
           Student(4,9,10)("Alexander Borodin   ",2345678), &
           Student(4,9,10)("Hildegard von Bingen",3456789)]

  sArr2 = [Student(4,9,12)("Alexander Borodin     ",234578), &
           Student(4,9,12)("Antoine   Lavoisier   ",123467), &
           Student(4,9,12)("Hildegard von Bingen  ",345689)]

  print *, (sArr1 .similarity. sArr2)

  w4_3 % student = sArr1
  w4_4 % student = sArr2
  print *, (w4_3 .similarity. w4_4) ! should be 2
  print *, (w4_3%student(2) .similarity. w4_4%student(1)), (w4_3%student(3) .similarity. w4_4%student(3))

  print *, (w4_1 .similarity. w4_2)
  print *, w4_1 .contains. "Jonas"
  print *, w4_1 .contains. "Salk"
  print *, w4_2 .contains. 2345678

  print *, (w2_2 .similarity. w2_1)
  print *, w2_1 .contains. "Modest"
  print *, w2_2 .contains. 32767_2

  print *, "done"

end program dtpATPDAUserDefinedOperator
