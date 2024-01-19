!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-12-01
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : assign from expr in select type
!*
!*  REFERENCE                  : Feature Number 358785
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Pass a polymorphic IN parameter to a subroutine and according to its type,
!*  determined via SELECT TYPE, modify the value of another parameter.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignSelectTypeExprmod

  implicit none

  type Base
     character(1) :: bfld = ''
  end type Base

  type, extends(Base) :: Child(l)
    integer, len :: l
    character(l) :: chfld = ''
  end type Child

  type, extends(Child) :: GChild(k)
    integer, kind :: k
    integer(k) :: ifld = 0
  end type GChild

  type, extends(GChild) :: GGChild
    type(GChild(l,k)) :: gfld
  end type GGChild

end module dtpIAssignSelectTypeExprmod


program dtpIAssignSelectTypeExpr

  use dtpIAssignSelectTypeExprmod
  implicit none

  type(Base) :: v1, b1
  type(Child(3)) :: v2, b2
  type(GChild(5,4)) :: v3, b3
  type(GChild(2,8)) :: v4, b4
  type(GGChild(6,4)) :: v5, b5
  type(GGChild(1,8)) :: v6, b6


  v1 = Base('Q')
  v2 = Child(3)('A','abc')
  v3 = GChild(5,4)('D', 'defgh', 123456789_4)
  v4 = GChild(2,8)('G', 'ij', 11223344332211_8)
  v5 = GGChild(6,4)('J', 'klmnop', 123456789_4, GChild(6,4)('M','qrstuv',123454321_4))
  v6 = GGChild(1,8)('P', 'w', 123456789123456789_8, GChild(1,8)('S','x',111222333444555_8))

  call testAssign(v1,b1,b2,b3,b4,b5,b6)
  call testAssign(v2,b1,b2,b3,b4,b5,b6)
  call testAssign(v3,b1,b2,b3,b4,b5,b6)
  call testAssign(v4,b1,b2,b3,b4,b5,b6)
  call testAssign(v5,b1,b2,b3,b4,b5,b6)
  call testAssign(v6,b1,b2,b3,b4,b5,b6)

  print *, 'done'

contains

  subroutine testAssign(arg,a1,a2,a3,a4,a5,a6)
    class(Base), intent(in) :: arg
    type(Base) :: a1
    type(Child(*)) :: a2
    type(GChild(*,4)) :: a3
    type(GChild(*,8)) :: a4
    type(GGChild(*,4)) :: a5
    type(GGChild(*,8)) :: a6
    print *,'<a1:',a1,'a2:',a2,'a3:',a3,'a4:',a4,'a5:',a5,'a6:',a6
    call assign(arg,a1,a2,a3,a4,a5,a6)
    print *,'=a1:',a1,'a2:',a2,'a3:',a3,'a4:',a4,'a5:',a5,'a6:',a6
    a1 = Base()
    a2%bfld = ''; a2%chfld = ''
    a3%bfld = ''; a3%chfld = ''; a3%ifld = 0
    a4%bfld = ''; a4%chfld = ''; a4%ifld = 0
    a5%bfld = ''; a5%chfld = ''; a5%ifld = 0; a5%gfld%bfld = ''; a5%gfld%chfld = ''; a5%gfld%ifld = 0
    a6%bfld = ''; a6%chfld = ''; a6%ifld = 0; a6%gfld%bfld = ''; a6%gfld%chfld = ''; a6%gfld%ifld = 0
    print *,'>a1:',a1,'a2:',a2,'a3:',a3,'a4:',a4,'a5:',a5,'a6:',a6
    print *
  end subroutine testAssign


  subroutine assign(arg,a1,a2,a3,a4,a5,a6)
    class(Base), intent(in) :: arg
    type(Base) :: a1
    type(Child(*)) :: a2
    type(GChild(*,4)) :: a3
    type(GChild(*,8)) :: a4
    type(GGChild(*,4)) :: a5
    type(GGChild(*,8)) :: a6

    select type (arg)
    type is (GGChild(*,8));  a6 = arg
    type is (GGChild(*,4));  a5 = arg
    type is (GChild(*,8));   a4 = arg
    type is (GChild(*,4));   a3 = arg
    type is (Child(*));      a2 = arg
    type is (Base);          a1 = arg
    class default;           print *, 'Unknown type'; stop 2
    end select

  end subroutine assign

end program dtpIAssignSelectTypeExpr
