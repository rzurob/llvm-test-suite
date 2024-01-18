!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAElemental
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : elemental procedures
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
!*  As dtpATPDAPure, but use elemental procedures applied to scalars and arrays.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAElementalMod

  implicit none

  type Student(snBytes,longestFirstName,longestLastName)
     integer, len :: longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name
     integer(snBytes) :: studentNumber
  end type Student

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     type(Student(snBytes, longestFirstName, longestLastName)) :: student(nStudents)
  end type Workshop

contains

  ! print participants "from" to "to" in "this" workshop
  subroutine printWorkshop3(from, to, this)
    class (Workshop(4,*,*,*)), intent(in) :: this
    integer, intent(in) :: from, to
    integer :: i
    print *, "Printing", from, "to", to, "of", this%nStudents, "students (", this%longestFirstName, this%longestLastName, ")"
    print '(a)', studentPrintImage(this%student(from:to))
  end subroutine printWorkshop3

  elemental character(40) function studentPrintImage(pupil)
    type(Student(4,*,*)), intent(in) :: pupil
    write(studentPrintImage,*) '  ', pupil%name, ":", pupil%studentNumber
  end function studentPrintImage

  subroutine printWorkshop1(this)
    class (Workshop(4,*,*,*)), intent(in) :: this
    call printWorkshop3(1, this%nStudents, this)
  end subroutine printWorkshop1

  elemental subroutine update(this, fname, lname)
    type (Student(4,*,*)), intent(inout) :: this
    character(*), intent(in) :: fname, lname
    this%name = fname // ' ' // lname
  end subroutine update

  elemental subroutine zap(this)
    type (Student(4,*,*)), intent(out) :: this
  end subroutine zap

  elemental subroutine replace(this,that)
    type (Student(4,*,*)), intent(out) :: this
    type (Student(4,*,*)), intent(in) :: that
    this = that
  end subroutine replace

end module dtpATPDAElementalMod

program dtpATPDAElemental

  use :: dtpATPDAElementalMod
  implicit none
  type(Workshop(4,4,6,8)) :: w1, w1a
  type(Workshop(4,3,8,16)) :: w2, w2a

  w1 = Workshop(4,4,6,8)([Student(4,6,8)("Jonas  Salk    ",1234567), &
                          Student(4,6,8)("Albert Einstein",2345678), &
                          Student(4,6,8)("Marie  Curie   ",3456789), &
                          Student(4,6,8)("Isaac  Newton  ",4567890)])
  w2 = Workshop(4,3,8,16)([Student(4,8,16)("Pyotr    Tschaikovsky    ",1234567), & 
                           Student(4,8,16)("Nicolai  Rimsky-Korsakov ",2345678), &
                           Student(4,8,16)("Modest   Mussorgsky      ",3456789)])

  print *, "Partial lists:"
  call printWorkshop3(1,2,w1)
  call printWorkshop3(2,3,w2)

  print *, "Complete lists:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

  call replace(w1a%student, w1%student)
  call replace(w2a%student, w2%student)

  print *, "Targets after replace:"
  call printWorkshop1(w1a)
  call printWorkshop1(w2a)

  print *, "Sources after replace:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

  call update(w1%student(3:3), "Ernest", "Rutherford")
  call update(w2%student(1:1), "Wolfgang", "Mozart")

  print *, "Targets after update:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

  call zap(w1%student)
  call zap(w2%student)

  print *, "Targets after zap:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

end program dtpATPDAElemental
