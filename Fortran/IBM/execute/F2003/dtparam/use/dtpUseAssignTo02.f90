!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseAssignTo02
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
!*  assignment to a parameterised type
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseAssignTo02BaseMod
  implicit none

  type :: Base
     integer :: iComp
   contains
     procedure, private :: assignSub => baseAssign
     generic :: assignment(=) => assignSub
  end type Base

  type :: Other (k)
     integer, kind :: k
     logical(k) :: lComp
  end type Other

  type :: LenBase(l)
     integer, len :: l
     integer :: iArray(l)
   contains
     procedure, private :: assignSub => lenBaseAssign
     generic :: assignment(=) => assignSub
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

end module dtpUseAssignTo02BaseMod

module dtpUseAssignTo02Mod

  use :: dtpUseAssignTo02BaseMod, only: Base, Base2 => Other, Base3 => LenBase, assignment(=)
  implicit none

  type, extends(Base) :: Derived(k)
     integer, kind :: k
     integer(k) :: iCompD
   contains
     procedure, private :: assignSub => derivedAssign4
     procedure, private :: assignSub8 => derivedAssign8
     generic :: assignment(=) => assignSub, assignSub8
  end type

  type, extends(Derived) :: Derived2
     integer(k) :: iCompD2
   contains
     procedure, private :: assignSub => derived2Assign4
     procedure, private :: assignSub8 => derived2Assign8
  end type

  type, extends(Derived2) :: Derived3(l)
     integer, len :: l
     character(l) :: cCompD3
   contains
     procedure, private :: assignSub => derived3Assign4
     procedure, private :: assignSub8 => derived3Assign8
  end type

contains

  subroutine derivedAssign4 (left, right)
    class (Derived(4)), intent(inout) :: left
    class (Base), intent(in)    :: right
    select type(right)
    class is (Derived(4))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
    class is (Derived(8))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
    class is (Base)
       print *, "Can't assign object of parent type to object of child type (Derived(4))", left % iComp
    class default
       print *, "Can't assign unknown to Derived(4)", left % iComp
       stop 14
    end select
  end subroutine derivedAssign4

  subroutine derivedAssign8 (left, right)
    class (Derived(8)), intent(inout) :: left
    class (Base), intent(in)    :: right
    select type(right)
    class is (Derived(4))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
    class is (Derived(8))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
    class is (Base)
       print *, "Can't assign object of parent type to object of child type (Derived(8))", left % iComp
    class default
       print *, "Can't assign unknown to Derived(8)", left % iComp
       stop 15
    end select
  end subroutine derivedAssign8


  subroutine derived2Assign4 (left, right)
    class (Derived2(4)), intent(inout) :: left
    class (Base), intent(in)    :: right
    select type(right)
    class is (Derived2(4))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
    class is (Derived2(8))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
    class is (Base)
       print *, "Can't assign object of parent type to object of child type (Derived2(4))", left % iComp
    class default
       print *, "Can't assign unknown to Derived2(4)", left % iComp
       stop 16
    end select
  end subroutine derived2Assign4


  subroutine derived2Assign8 (left, right)
    class (Derived2(8)), intent(inout) :: left
    class (Base), intent(in)    :: right
    select type(right)
    class is (Derived2(4))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
    class is (Derived2(8))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
    class is (Base)
       print *, "Can't assign object of parent type to object of child type (Derived2(8))", left % iComp
    class default
       print *, "Can't assign unknown to Derived2(8)", left % iComp
       stop 17
    end select
  end subroutine derived2Assign8

  subroutine derived3Assign4 (left, right)
    class (Derived3(4,*)), intent(inout) :: left
    class (Base), intent(in)    :: right
    select type(right)
    class is (Derived3(4,*))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
       left % cCompD3 = right % cCompD3
    class is (Derived3(8,*))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
       left % cCompD3 = right % cCompD3
    class is (Base)
       print *, "Can't assign object of parent type to object of child type (Derived3(4,*))", left % iComp
    class default
       print *, "Can't assign unknown to Derived3(4,*)", left % iComp
       stop 18
    end select
  end subroutine derived3Assign4

  subroutine derived3Assign8 (left, right)
    class (Derived3(8,*)), intent(inout) :: left
    class (Base), intent(in)    :: right
    select type(right)
    class is (Derived3(4,*))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
       left % cCompD3 = right % cCompD3
    class is (Derived3(8,*))
       left % iComp  = right % iComp
       left % iCompD = right % iCompD
       left % iCompD2 = right % iCompD2
       left % cCompD3 = right % cCompD3
    class is (Base)
       print *, "Can't assign object of parent type to object of child type (Derived3(8,*))", left % iComp
    class default
       print *, "Can't assign unknown to Derived3(8,*)", left % iComp
       stop 19
    end select
  end subroutine derived3Assign8

end module dtpUseAssignTo02Mod

program dtpUseAssignTo02
  use dtpUseAssignTo02Mod
  implicit none

  type (Derived(4))    :: d4a, d4b, d4c
  type (Derived2(4))   :: d24a, d24b
  type (Derived3(4,3)) :: d343a, d343b, d343c

  type (Derived(8))    :: d8a, d8b, d8c
  type (Derived2(8))   :: d28a, d28b
  type (Derived3(8,3)) :: d383a, d383a1, d383b

  d343c%icomp = 0
  d4a %iComp  = 42;       d4a %iCompD = 45
  d24a%iComp  = 142;      d24a%iCompD = 56;       d24a%iCompD2 = 1234
  d343a%iComp = 11142;    d343a%iCompD= 11242;    d343a%iCompD2= 15242;    d343a%cCompD3= "this is a test"

  d4b %iComp  = 19;       d4b %iCompD = 17
  d24b%iComp  = 119;      d24b%iCompD = 29;       d24b%iCompD2 = 1293
  d343b%iComp = 11119;    d343b%iCompD= 11942;    d343b%iCompD2= 15242;    d343b%cCompD3= "another test"

  d8a %iComp  = 82;       d8a %iCompD = 85
  d28a%iComp  = 182;      d28a%iCompD = 567324_8; d28a%iCompD2 = 1238787
  d383a%iComp = 11182897; d383a%iCompD= 112821_8; d383a%iCompD2= 15282234; d383a%cCompD3= "still another"

  d8b %iComp  = 89;       d8b %iCompD = 174571_8
  d28b%iComp  = 189;      d28b%iCompD = 293487_8; d28b%iCompD2 = 1293656
  d383b%iComp = 11889876; d383b%iCompD= 119823_8; d383b%iCompD2= 15282345; d383b%cCompD3= "done now."


  print *, d4a, d24a, d343a
  print *, d4b, d24b, d343b

  print *, "Simple assignments"
  d4a   = d4b
  d24a  = d24b
  d343a = d343b
  print *, d4a, d24a, d343a
  print *, d4b, d24b, d343b
  d8a   = d8b
  d28a  = d28b
  d383a = d383b
  print *, d8a, d28a, d383a
  print *, d8b, d28b, d383b

  print *, "Cross-kind assignments"
  d4a   = d8b
  d24a  = d28b
  d343a = d383b
  print *, d4a, d24a, d343a
  print *, d8b, d28b, d383b
  d8a   = d4b
  d28a  = d24b
  d383a = d343b
  print *, d8a, d28a, d383a
  print *, d4b, d24b, d343b

  print *, "Child type assignments"
  d4a   = d24b
  d4c   = d343b
  d24a  = d343b
  print *, d4a, d24a, d343a, d4c
  print *, d4b, d24b, d343b

  print *, "Error assignments"
  d24a  = d4b
  d343c = d8b
  d343a = d24b

  print *, d4a, d343a
  print *, d4b, d24b, d8b

  print *, "end"

end program dtpUseAssignTo02
