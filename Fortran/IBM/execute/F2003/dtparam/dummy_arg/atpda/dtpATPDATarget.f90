!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpATPDATarget
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-15
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Assumed type parameters and dummy arguments
!*
!*  SECONDARY FUNCTIONS TESTED : target
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
!*  Declare an assumed-type object to be a TARGET of a pointer, point the pointer
!*  at the target, and access the object via the pointer.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpATPDATargetMod

  implicit none

  type Workshop(snBytes,nStudents,longestFirstName,longestLastName)
     integer, len :: nStudents, longestFirstName, longestLastName
     integer, kind :: snBytes
     character(longestFirstName + longestLastName + 1) :: name(nStudents) = ''
     integer(snBytes) :: studentNumber(nStudents) = 0
  end type Workshop

contains

  ! print participants "from" to "to" in "this" workshop
  subroutine printWorkshop3(from, to, this)
    class (Workshop(4,*,*,*)), target, intent(in) :: this
    class (Workshop(4,:,:,:)), pointer :: pThis
    integer, intent(in) :: from, to
    integer :: i

    pThis => this
    print *, "Printing", from, "to", to, "of", pThis%nStudents, "students (", pThis%longestFirstName, pThis%longestLastName, ")"
    do i = from, to
       print *, i, pThis%name(i), ":", pThis%studentNumber(i)
    end do
  end subroutine printWorkshop3

  subroutine printWorkshop1(this)
    class (Workshop(4,*,*,*)), intent(in), target :: this
    class (Workshop(4,:,:,:)), pointer :: pThis

    pThis => this
    call printWorkshop3(1, pThis%nStudents, pThis)
  end subroutine printWorkshop1

  subroutine update(this, inx, fname, lname)
    type (Workshop(4,*,*,*)), target, intent(inout) :: this
    integer, intent(in) :: inx
    character(*), intent(in) :: fname, lname
    type (Workshop(4,:,:,:)), pointer :: pThis

    pThis => this
    pThis%name(inx) = fname // ' ' // lname
  end subroutine update

  subroutine zap(this)
    type (Workshop(4,*,*,*)), intent(out), target :: this
  end subroutine zap

  subroutine replace(this,that)
    type (Workshop(4,*,*,*)), intent(out), target :: this
    type (Workshop(4,*,*,*)), intent(in), target :: that
    type (Workshop(4,:,:,:)), pointer :: pThis, pThat
    pThis => this
    pThat => that
    pThis = pThat
  end subroutine replace

end module dtpATPDATargetMod

program dtpATPDATarget

  use :: dtpATPDATargetMod
  implicit none
  type(Workshop(4,4,6,8)) :: w1, w1a
  type(Workshop(4,3,8,16)) :: w2, w2a

  w1 = Workshop(4,4,6,8)(["Jonas  Salk    ","Albert Einstein","Marie  Curie   ","Isaac  Newton  "], [1234567,2345678,3456789,4567890])
  w2 = Workshop(4,3,8,16)(["Pyotr    Tschaikovsky    ","Nicolai  Rimsky-Korsakov ","Modest   Mussorgsky      "], [1234567,2345678,3456789])

  print *, "Partial lists:"
  call printWorkshop3(1,2,w1)
  call printWorkshop3(2,3,w2)

  print *, "Complete lists:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

  call replace(w1a, w1)
  call replace(w2a, w2)

  print *, "Targets after replace:"
  call printWorkshop1(w1a)
  call printWorkshop1(w2a)

  print *, "Sources after replace:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

  call update(w1, 3, "Ernest", "Rutherford")
  call update(w2, 1, "Wolfgang", "Mozart")

  print *, "Targets after update:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

  call zap(w1)
  call zap(w2)

  print *, "Targets after zap:"
  call printWorkshop1(w1)
  call printWorkshop1(w2)

end program dtpATPDATarget
