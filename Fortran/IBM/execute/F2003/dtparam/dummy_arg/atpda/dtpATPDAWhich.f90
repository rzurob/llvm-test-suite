!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAWhich
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : which argument is assumed-type parameter
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
!*  Invoke procedures in which different type parameters are assumed-type:
!*  the first (type(Workshop(4,*,6,8))), second (type(Workshop(4,3,*,8))), third,
!*  or some combination.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAWhichMod

  implicit none

  type Student(snBytes,longestFirstName,longestLastName)
     integer, len :: longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name
     integer(snBytes) :: studentNumber
   contains
     procedure, pass :: printStudent4
     generic :: print => printStudent4
  end type Student

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     type(Student(snBytes, longestFirstName, longestLastName)) :: student(nStudents)
   contains
     procedure, pass :: printWorkshop4
     generic :: print => printWorkshop4
  end type Workshop


contains

  subroutine none(this)
    class(Workshop(4,4,6,8)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine none

  subroutine first(this)
    class(Workshop(4,*,6,8)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine first

  subroutine second(this)
    class(Workshop(4,4,*,8)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine second

  subroutine third(this)
    class(Workshop(4,4,6,*)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine third

  subroutine firstAndSecond(this)
    class(Workshop(4,*,*,8)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine firstAndSecond

  subroutine firstAndThird(this)
    class(Workshop(4,*,6,*)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine firstAndThird

  subroutine secondAndThird(this)
    class(Workshop(4,4,*,*)), intent(in) :: this
    print *, this%nStudents, this% longestFirstName, this % longestLastName
  end subroutine secondAndThird

  ! All three
  integer function nCharacters(this)
    class(Workshop(4,*,*,*)), intent(in) :: this
    nCharacters = this%nStudents * (this%longestFirstName + this%longestLastName + 1)
  end function nCharacters

  ! Both
  elemental integer function nameLen(this)
    class(Student(4,*,*)), intent(in) :: this
    nameLen = this%longestFirstName + this%longestLastName + 1
  end function nameLen

  subroutine noneSt(this)
    class(Student(4,6,8)), intent(in) :: this
    print *, this% longestFirstName, this % longestLastName
  end subroutine noneSt

  subroutine firstSt(this)
    class(Student(4,*,8)), intent(in) :: this
    print *, this% longestFirstName, this % longestLastName
  end subroutine firstSt

  subroutine secondSt(this)
    class(Student(4,6,*)), intent(in) :: this
    print *, this% longestFirstName, this % longestLastName
  end subroutine secondSt


  subroutine printWorkshop4(this)
    class (Workshop(4,*,*,*)) :: this
    integer :: i
    print *, "There are", this%nStudents, "students with first names of", this%longestFirstName, &
             "or fewer characters, and last names of", this%longestLastName, "or fewer characters"
    do i = 1, this%nStudents
       call this%student(i) % print
    end do
  end subroutine printWorkshop4

  subroutine printStudent4(this)
    class (Student(4,*,*)) :: this
    print *, this%name, ":", this%studentNumber
  end subroutine printStudent4

end module dtpATPDAWhichMod

program dtpATPDAWhich

  use :: dtpATPDAWhichMod
  implicit none
  type(Workshop(4,4,6,8))  :: w4_1
  type(Workshop(4,3,6,12)) :: w4_2
  type(Workshop(4,4,3,8))  :: w4_3
  type(Workshop(4,1,6,8))  :: w4_4
  type(Workshop(4,4,6,3))  :: w4_5
  type(Student(4,9,10)) :: sArr1(3)
  type(Student(4,9,12)) :: sArr2(3)

  integer :: i

  w4_1 = Workshop(4,4,6,8) ([Student(4,6,8) ("Jonas  Salk    ",1234567), &
                             Student(4,6,8) ("Alex   Borodin ",2345678), &
                             Student(4,6,8) ("Hilde  v.Bingen",3456789), &
                             Student(4,6,8) ("Isaac  Newton  ",4567890)])
  w4_2 = Workshop(4,3,6,12)([Student(4,6,12)("Pyotr  Tschaikovsky",1234567), & 
                             Student(4,6,12)("Alex   Borodin     ",2345678), &
                             Student(4,6,12)("Hilde  von Bingen  ",3456789)])
  w4_3 = Workshop(4,4,3,8) ([Student(4,3,8) ("Ptr Tschaiko",1234567), & 
                             Student(4,3,8) ("Alx Borodin ",2345678), &
                             Student(4,3,8) ("Ser Prokofie",4567890), &
                             Student(4,3,8) ("Hld v.Bingen",3456789)])
  w4_4 = Workshop(4,1,6,8) ([Student(4,6,8) ("Pyotr  Tschaiko",1234567)])
  w4_5 = Workshop(4,4,6,3) ([Student(4,6,3) ("Pyotr  Tsc",1534567), &
                             Student(4,6,3) ("Jonas  Sal",1254567), &
                             Student(4,6,3) ("Hilde  v.B",1235567), &
                             Student(4,6,3) ("Isaac  New",1234557)])

  call w4_1 % print
  call w4_2 % print
  call w4_3 % print
  call w4_4 % print
  call w4_5 % print

  sArr1 = [Student(4,9,10)("Jonas     Salk      ",1234567), &
           Student(4,9,10)("Alexander Borodin   ",2345678), &
           Student(4,9,10)("Hildegard von Bingen",3456789)]

  sArr2 = [Student(4,9,12)("Alexander Borodin     ",234578), &
           Student(4,9,12)("Antoine   Lavoisier   ",123467), &
           Student(4,9,12)("Hildegard von Bingen  ",345689)]

  call none(w4_1)
  call first(w4_1)
  call second(w4_1)
  call third(w4_1)
  call firstAndSecond(w4_1)
  call firstAndThird(w4_1)
  call secondAndThird(w4_1)

  call noneSt(w4_1%student(1))
  call firstSt(w4_1%student(1))
  call secondSt(w4_1%student(1))

  print *, nCharacters(w4_1), nCharacters(w4_2), "<", nameLen(sArr1), "|", nameLen(sArr2(2)), "|", nameLen(w4_4%student), ">"

  call first(w4_4)
  call second(w4_3)
  call third(w4_5)
  call firstAndSecond(w4_3)
  call firstAndThird(w4_2)
  call secondAndThird(w4_3)

  call firstSt(w4_3%student(1))
  call secondSt(w4_2%student(1))

  print *, "done"

end program dtpATPDAWhich
