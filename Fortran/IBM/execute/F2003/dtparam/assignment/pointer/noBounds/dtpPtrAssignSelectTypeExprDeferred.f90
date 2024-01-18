!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignSelectTypeExprDeferred
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-12-01
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : assign from expr in select type using pointers with deferred length parameters
!*
!*  REFERENCE                  : Feature Number 360669
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
!*  Pass a polymorphic IN parameter to a subroutine and according to its type,
!*  determined via SELECT TYPE, modify the value of another parameter.
!*  
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignSelectTypeExprDeferredmod

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

end module dtpPtrAssignSelectTypeExprDeferredmod


program dtpPtrAssignSelectTypeExprDeferred

  use dtpPtrAssignSelectTypeExprDeferredmod
  implicit none

  type(Base), target :: v1, b1
  type(Child(3)), target :: v2, b2
  type(GChild(5,4)), target :: v3, b3
  type(GChild(2,8)), target :: v4, b4
  type(GGChild(6,4)), target :: v5, b5
  type(GGChild(1,8)), target :: v6, b6

  class(Base), pointer :: vp

  type(Base), pointer :: b1p
  type(Child(3)), pointer :: b2p
  type(GChild(5,4)), pointer :: b3p
  type(GChild(2,8)), pointer :: b4p
  type(GGChild(6,4)), pointer :: b5p
  type(GGChild(1,8)), pointer :: b6p


  v1 = Base('Q')
  v2 = Child(3)('A','abc')
  v3 = GChild(5,4)('D', 'defgh', 123456789_4)
  v4 = GChild(2,8)('G', 'ij', 11223344332211_8)
  v5 = GGChild(6,4)('J', 'klmnop', 123456789_4, GChild(6,4)('M','qrstuv',123454321_4))
  v6 = GGChild(1,8)('P', 'w', 123456789123456789_8, GChild(1,8)('S','x',111222333444555_8))

  b1p => b1
  b2p => b2
  b3p => b3
  b4p => b4
  b5p => b5
  b6p => b6
  
  vp => v1
  call testAssign(vp,b1p,b2p,b3p,b4p,b5p,b6p)
  vp => v2
  call testAssign(vp,b1p,b2p,b3p,b4p,b5p,b6p)
  vp => v3
  call testAssign(vp,b1p,b2p,b3p,b4p,b5p,b6p)
  vp => v4
  call testAssign(vp,b1p,b2p,b3p,b4p,b5p,b6p)
  vp => v5
  call testAssign(vp,b1p,b2p,b3p,b4p,b5p,b6p)
  vp => v6
  call testAssign(vp,b1p,b2p,b3p,b4p,b5p,b6p)

  print *, 'done'

contains

  subroutine testAssign(arg,a1,a2,a3,a4,a5,a6)
    class(Base), intent(in), pointer :: arg
    type(Base), pointer :: a1
    type(Child(*)), pointer :: a2
    type(GChild(*,4)), pointer :: a3
    type(GChild(*,8)), pointer :: a4
    type(GGChild(*,4)), pointer :: a5
    type(GGChild(*,8)), pointer :: a6
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
    class(Base), intent(in), pointer :: arg
    type(Base), pointer :: a1
    type(Child(*)), pointer :: a2
    type(GChild(*,4)), pointer :: a3
    type(GChild(*,8)), pointer :: a4
    type(GGChild(*,4)), pointer :: a5
    type(GGChild(*,8)), pointer :: a6

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

end program dtpPtrAssignSelectTypeExprDeferred
