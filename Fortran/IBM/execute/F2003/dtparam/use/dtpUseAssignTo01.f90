!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseAssignTo01
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : assignment
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Declare types with parameters (kind and len) in a module and USE
!*  them indirectly in another module via rename.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseAssignTo01BaseMod
  implicit none

  type :: Base
     integer :: iComp
   contains
     procedure, private :: baseAssign
     generic :: assignment(=) => baseAssign
  end type Base

  type :: Other (k)
     integer, kind :: k
     logical(k) :: lComp
  end type Other

  type :: LenBase(l)
     integer, len :: l
     integer :: iArray(l)
   contains
     procedure, private :: lenBaseAssign
     generic :: assignment(=) => lenBaseAssign
  end type LenBase

  interface assignment (=)
    module procedure other8Assign
    module procedure other4Assign
    module procedure other2Assign
  end interface assignment (=)

contains

  subroutine baseAssign (left, right)
    class (Base), intent(inout) :: left
    class (Base), intent(in)    :: right
    print *, "in baseAssign:", left % iComp, "<=", right % iComp
    left % iComp = right % iComp
  end subroutine

  subroutine lenBaseAssign (left, right)
    class (LenBase(*)), intent(inout) :: left
    class (LenBase(*)), intent(in)    :: right
    print *, "in lenBaseAssign:" , left % l, right % l
    print *, "in lenBaseAssign:", left % iArray, "<=", right % iArray
    if (left % l /= right % l) then
       print *, "Oops! Length mismatch:", left % l, "<=", right % l
       stop 10
    end if
    left % iArray = right % iArray
  end subroutine

  subroutine other8Assign (left, right)
    class (Other(8)), intent(inout) :: left
    class (*), intent(in)    :: right
    print *, "in other8Assign:", left % lComp
    select type (right)
    class is (Other(8)); left % lComp = right % lComp
    class is (Other(4)); left % lComp = right % lComp
    class is (Other(2)); left % lComp = right % lComp
    class default;    print *, "other8Assign unknown type"; stop 11
    end select
  end subroutine

  subroutine other4Assign (left, right)
    class (Other(4)), intent(inout) :: left
    class (*), intent(in)    :: right
    print *, "in other4Assign:", left % lComp
    select type (right)
    class is (Other(8)); left % lComp = right % lComp
    class is (Other(4)); left % lComp = right % lComp
    class is (Other(2)); left % lComp = right % lComp
    class default;    print *, "other8Assign unknown type"; stop 12
    end select
  end subroutine

  subroutine other2Assign (left, right)
    class (Other(2)), intent(inout) :: left
    class (*), intent(in)    :: right
    print *, "in other2Assign:", left % lComp
    select type (right)
    class is (Other(8)); left % lComp = right % lComp
    class is (Other(4)); left % lComp = right % lComp
    class is (Other(2)); left % lComp = right % lComp
    class default;    print *, "other8Assign unknown type"; stop 13
    end select
  end subroutine

end module dtpUseAssignTo01BaseMod

module dtpUseAssignTo01Mod

  use :: dtpUseAssignTo01BaseMod, only: Base, Base2 => Other, Base3 => LenBase, assignment(=)
  implicit none

end module dtpUseAssignTo01Mod

program dtpUseAssignTo01
  use dtpUseAssignTo01Mod
  implicit none

  type (Base)     :: ba, bb
  type (Base2(4)) :: b24a, b24b
  type (Base3(3)) :: b33a, b33b

  ba  %iComp  = 42
  b24a%lComp  = .true.
  b33a%iArray = [3, 99, 127]

  bb  %iComp  = 19
  b24b%lComp  = .false.
  b33b%iArray = [-5, -10, -25]

  print *, ba, b24a, b33a
  print *, bb, b24b, b33b

  print *, "start assign"
  ba    = bb
  print *, "after assign 1"
  b24a  = b24b
  print *, "after assign 2"
  b33a  = b33b
  print *, "end assign"

  print *, ba, b24a, b33a
  print *, bb, b24b, b33b

end program dtpUseAssignTo01
