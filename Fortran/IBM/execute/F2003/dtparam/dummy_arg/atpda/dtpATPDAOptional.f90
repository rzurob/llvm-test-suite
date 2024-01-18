!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDAOptional
!*
!*  DATE                       : 2008-10-15
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : optional assumed-type parameter arguments
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
!*  Invoke procedures in which the assumed-type parameter is optional.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDAOptionalMod

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

  subroutine opt1(this)
    class(Workshop(4,*,*,*)), intent(in), optional :: this
    if (present(this)) then
       print *, "opt1: one argument", this%nStudents, this% longestFirstName, this % longestLastName
    else
       print *, "opt1: no argument"
    end if
  end subroutine opt1

  subroutine opt2(this,that)
    class(Workshop(4,*,*,*)), intent(in), optional :: this
    class(Workshop(4,*,*,*)), intent(in), optional :: that
    integer :: nPresent
    nPresent = 0

    if (present(this)) then
       nPresent = nPresent + 1
       print *, "opt2: first argument", this%nStudents, this% longestFirstName, this % longestLastName
    end if

    if (present(that)) then
       nPresent = nPresent + 1
       print *, "opt2: second argument", that%nStudents, that% longestFirstName, that % longestLastName
    end if

    print *, "opt2:", nPresent, "argument(s)"

  end subroutine opt2

end module dtpATPDAOptionalMod

program dtpATPDAOptional

  use :: dtpATPDAOptionalMod
  implicit none
  type(Workshop(4,4,6,8))  :: w4_1
  type(Workshop(4,3,6,12)) :: w4_2

  w4_1 = Workshop(4,4,6,8) ([Student(4,6,8) ("Jonas  Salk    ",1234567), &
                             Student(4,6,8) ("Alex   Borodin ",2345678), &
                             Student(4,6,8) ("Hilde  v.Bingen",3456789), &
                             Student(4,6,8) ("Isaac  Newton  ",4567890)])
  w4_2 = Workshop(4,3,6,12)([Student(4,6,12)("Pyotr  Tschaikovsky",1234567), &
                             Student(4,6,12)("Alex   Borodin     ",2345678), &
                             Student(4,6,12)("Hilde  von Bingen  ",3456789)])

  ! First invoke procedures with parameters:
  call opt1(w4_1)
  call opt2(w4_1, w4_2)

  ! Now drop one parameter:
  call opt2(w4_1)
  call opt2(this=w4_2)
  call opt2(that=w4_1)

  ! Now no optional parameters:
  call opt1
  call opt2

  print *, "done"

end program dtpATPDAOptional
