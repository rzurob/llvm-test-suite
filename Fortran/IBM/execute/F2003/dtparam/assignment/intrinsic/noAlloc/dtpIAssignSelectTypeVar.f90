!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAssignSelectTypeVar
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-12-01
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign to var in select type
!*
!*  REFERENCE                  : Feature Number 358785
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
!*  Modify value of a var passed in to a subroutine as a polymorphic INOUT parameter
!*  according to the type determined via SELECT TYPE.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignSelectTypeVarmod

  implicit none

  type Base
     character(1) :: bfld = 'b'
  end type Base

  type, extends(Base) :: Child(l)
    integer, len :: l
    character(l) :: chfld
  end type Child

  type, extends(Child) :: GChild(k)
    integer, kind :: k
    integer(k) :: ifld
  end type GChild

  type, extends(GChild) :: GGChild
    type(GChild(l,k)) :: gfld
  end type GGChild

end module dtpIAssignSelectTypeVarmod


program dtpIAssignSelectTypeVar

  use dtpIAssignSelectTypeVarmod
  implicit none

  type(Base) :: v1
  type(Child(3)) :: v2
  type(GChild(5,4)) :: v3
  type(GChild(2,8)) :: v4
  type(GGChild(6,4)) :: v5
  type(GGChild(1,8)) :: v6


  v1 = Base('Q')
  v2 = Child(3)('A','abc')
  v3 = GChild(5,4)('D', 'defgh', 123456789)
  v4 = GChild(2,8)('G', 'ij', 44332211)
  v5 = GGChild(6,4)('J', 'klmnop', 123456789_4, GChild(6,4)('M','qrstuv',123454321_4))
  v6 = GGChild(1,8)('P', 'w', 123456789123456789_8, GChild(1,8)('S','x',111222333444555_8))

  call testAssign(v1)
  call testAssign(v2)
  call testAssign(v3)
  call testAssign(v4)
  call testAssign(v5)
  call testAssign(v6)

  print *, 'done'

contains

  subroutine testAssign(arg)
    class(Base), intent(inout) :: arg
    call test(arg)
    call assign(arg)
    call test(arg)
    print *
  end subroutine testAssign


  recursive subroutine test(arg)
    class(Base), intent(inout) :: arg

    select type (arg)
    type is (Base);         print *, 'found Base:', arg, '<'
    type is (Child(*));     print *, 'found Child(*):', arg, arg%l, '<'
    type is (GChild(*,4));  print *, 'found GChild(*,4):', arg, arg%l, arg%k, '<'
    type is (GChild(*,8));  print *, 'found GChild(*,8):', arg, arg%l, arg%k, '<'
    type is (GGChild(*,4)); print *, 'found GGChild(*,4):', arg, arg%l, arg%k, '<'; call test(arg%gfld)
    type is (GGChild(*,8)); print *, 'found GGChild(*,8):', arg, arg%l, arg%k, '<'; call test(arg%gfld)
    class default
      print *, 'Unknown type'
      stop 2
    end select
  end subroutine test


  function shiftCharsLeft(ch,ch1)
    character(*) :: ch
    character(1) :: ch1
    character(len(ch)) :: shiftCharsLeft
    integer :: e
    e = len(ch)
    shiftCharsLeft(1:e-1) = ch(2:e)
    shiftCharsLeft(e:e) = ch1
  end function shiftCharsLeft


  function shiftCharsRight(ch,ch1)
    character(*) :: ch
    character(1) :: ch1
    character(len(ch)) :: shiftCharsRight
    integer :: e
    e = len(ch)
    shiftCharsRight(2:e) = ch(1:e-1)
    shiftCharsRight(1:1) = ch1
  end function shiftCharsRight


  subroutine assign(arg)
    class(Base), intent(inout) :: arg

    select type (arg)

    type is (Base)
             arg = Base(achar(iachar(arg%bfld)+6))

    type is (Child(*))
             arg = Child(arg%l)(achar(iachar(arg%bfld)+5), shiftCharsLeft(arg%chfld,'+'))

    type is (GChild(*,4))
             arg = GChild(arg%l,4)(achar(iachar(arg%bfld)+3), shiftCharsLeft(arg%chfld,'%'), arg%ifld + 100)

    type is (GChild(*,8))
             arg = GChild(arg%l,8)(achar(iachar(arg%bfld)+4), shiftCharsLeft(arg%chfld,'*'), arg%ifld - 100)

    type is (GGChild(*,4))
             arg = GGChild(arg%l,4)(achar(iachar(arg%bfld)+1), shiftCharsLeft(arg%chfld,'#'), arg%ifld + 1, &
                                    GChild(arg%gfld%l,4)(achar(iachar(arg%gfld%bfld)-1), shiftCharsRight(arg%gfld%chfld,'#'), arg%gfld%ifld - 1))
    type is (GGChild(*,8))
             arg = GGChild(arg%l,8)(achar(iachar(arg%bfld)+2), shiftCharsLeft(arg%chfld,'@'), arg%ifld + 10, &
                                    GChild(arg%gfld%l,8)(achar(iachar(arg%gfld%bfld)-2), shiftCharsRight(arg%chfld,'@'), arg%gfld%ifld - 10))

    class default
      print *, 'Unknown type'
      stop 2

    end select

  end subroutine assign

end program dtpIAssignSelectTypeVar
