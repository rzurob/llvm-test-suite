!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-11-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment without Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : manipulate DTP variables with user-defined operators and assign
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
!*  In module procedures, assign values to a function result of a parameterised
!*  derived type for which there is no user-defined assignment, and verify that
!*  the type parameters and data values are as expected.  To make things
!*  interesting, we make the type parameters dependent on the arguments.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAssignFromUserDefinedOperatormod

  type dl (l)
     integer, len :: l
     character(l) :: chfld
  end type dl

  type :: dk (k)
     integer, kind :: k
     integer(k) :: ifld
  end type dk

  type :: dkl (k, l)
     integer, kind :: k
     integer, len :: l
     type(dl(l)) :: dlfld
     type(dk(k)) :: dkfld(l)
  end type dkl

  interface operator(.cat.)
     module procedure chCat
     module procedure dlCat
     module procedure dklCat22
     module procedure dklCat44
  end interface

  interface operator(.tweak.)
     module procedure iTweak11
     module procedure iTweak22
     module procedure dkTweak22
     module procedure dkTweak44
     module procedure dklTweak22
     module procedure dklTweak44
  end interface

contains


  function chCat(c1,c2)
    implicit none
    character(*), intent(in) :: c1, c2
    type(Dl(len(c1)+len(c2))) :: chCat
    character(100) :: chtmp1, chtmp2
    character(1) :: ch1tmp1(100), ch1tmp2(100)
    equivalence (chtmp1,ch1tmp1), (chtmp2,ch1tmp2)
    chtmp1 = c1
    chtmp2 = c2
    ch1tmp1(len(c1)+1:len(c1)+len(c2)) = ch1tmp2
    chCat = Dl(len(c1)+len(c2))(chtmp1)
  end function chCat


  function dlCat(c1,c2)
    implicit none
    type(Dl(*)), intent(in) :: c1, c2
    type(Dl(c1%l+c2%l)) :: dlCat
    character(100) :: chtmp1, chtmp2
    character(1) :: ch1tmp1(100), ch1tmp2(100)
    equivalence (chtmp1,ch1tmp1), (chtmp2,ch1tmp2)
    chtmp1 = c1%chfld
    chtmp2 = c2%chfld
    ch1tmp1(c1%l+1:c1%l+c2%l) = ch1tmp2
    dlCat = Dl(c1%l+c2%l)(chtmp1)
  end function dlCat


  elemental function map2To4DK(a)
    type(Dk(2)), intent(in) :: a
    type(Dk(4)) :: map2To4DK
    map2To4DK%ifld = a%ifld
  end function map2To4DK


  elemental function map4To8DK(a)
    type(Dk(4)), intent(in) :: a
    type(Dk(8)) :: map4To8DK
    map4To8DK%ifld = a%ifld
  end function map4To8DK


  function dklCat22(c1,c2)
    implicit none
    type(Dkl(2,*)), intent(in) :: c1, c2
    type(Dkl(c1%k+c2%k,c1%l+c2%l)) :: dklCat22
    character(100) :: chtmp1, chtmp2
    character(1) :: ch1tmp1(100), ch1tmp2(100)
    equivalence (chtmp1,ch1tmp1), (chtmp2,ch1tmp2)
    chtmp1 = c1%dlfld%chfld
    chtmp2 = c2%dlfld%chfld
    ch1tmp1(c1%l+1:c1%l+c2%l) = ch1tmp2
    dklCat22 = Dkl(c1%k+c2%k,c1%l+c2%l)(DL(c1%l+c2%l)(chtmp1), map2To4DK([c1%dkfld, c2%dkfld]))
  end function dklCat22


  function dklCat44(c1,c2)
    implicit none
    type(Dkl(4,*)), intent(in) :: c1, c2
    type(Dkl(c1%k+c2%k,c1%l+c2%l)) :: dklCat44
    character(100) :: chtmp1, chtmp2
    character(1) :: ch1tmp1(100), ch1tmp2(100)
    equivalence (chtmp1,ch1tmp1), (chtmp2,ch1tmp2)
    chtmp1 = c1%dlfld%chfld
    chtmp2 = c2%dlfld%chfld
    ch1tmp1(c1%l+1:c1%l+c2%l) = ch1tmp2
    dklCat44 = Dkl(c1%k+c2%k,c1%l+c2%l)(DL(c1%l+c2%l)(chtmp1), map4To8DK([c1%dkfld, c2%dkfld]))
  end function dklCat44


  function iTweak11(m,n)
    implicit none
    integer(1), intent(in) :: m, n
    type(Dk(kind(m)+kind(n))) :: iTweak11
!    iTweak11%ifld = int(m,iTweak11%k) + int(n,iTweak11%k)
    iTweak11%ifld = int(m,kind(m)+kind(n)) + int(n,kind(m)+kind(n))
  end function iTweak11


  function iTweak22(m,n)
    implicit none
    integer(2), intent(in) :: m, n
    type(Dk(kind(m)+kind(n))) :: iTweak22
    iTweak22%ifld = int(m,kind(m)+kind(n)) + int(n,kind(m)+kind(n))
  end function iTweak22


  function dkTweak22(d1,d2)
    implicit none
    type(Dk(2)), intent(in) :: d1, d2
    type(Dk(d1%k+d2%k)) :: dkTweak22
    dkTweak22%ifld = int(d1%ifld,kind(d1%ifld)+kind(d2%ifld)) + int(d2%ifld,kind(d1%ifld)+kind(d2%ifld))
  end function dkTweak22


  function dkTweak44(d1,d2)
    implicit none
    type(Dk(4)), intent(in) :: d1, d2
    type(Dk(d1%k+d2%k)) :: dkTweak44
    dkTweak44%ifld = int(d1%ifld,kind(d1%ifld)+kind(d2%ifld)) + int(d2%ifld,kind(d1%ifld)+kind(d2%ifld))
  end function dkTweak44


  function dklTweak44(d1,d2)
    implicit none
    type(Dkl(4,*)), intent(in) :: d1, d2
    type(Dkl(d1%k+d2%k,d1%l+d2%l)) :: dklTweak44
    character(100) :: chtmp1, chtmp2
    character(1) :: ch1tmp1(100), ch1tmp2(100)
    equivalence (chtmp1,ch1tmp1), (chtmp2,ch1tmp2)

    chtmp1 = d1%dlfld%chfld
    chtmp2 = d2%dlfld%chfld
    ch1tmp1(d1%l+1:d1%l+d2%l) = ch1tmp2

    dklTweak44%dkfld%ifld  = [d1%dkfld%ifld, d2%dkfld%ifld]
    dklTweak44%dlfld = DL(d1%l+d2%l)(chtmp1)

  end function dklTweak44


  function dklTweak22(d1,d2)
    implicit none
    type(Dkl(2,*)), intent(in) :: d1, d2
    type(Dkl(d1%k+d2%k,d1%l+d2%l)) :: dklTweak22
    character(100) :: chtmp1, chtmp2
    character(1) :: ch1tmp1(100), ch1tmp2(100)
    equivalence (chtmp1,ch1tmp1), (chtmp2,ch1tmp2)

    chtmp1 = d1%dlfld%chfld
    chtmp2 = d2%dlfld%chfld
    ch1tmp1(d1%l+1:d1%l+d2%l) = ch1tmp2

    dklTweak22%dkfld%ifld  = [d1%dkfld%ifld, d2%dkfld%ifld]
    dklTweak22%dlfld = DL(d1%l+d2%l)(chtmp1)

  end function dklTweak22


  subroutine dump(label,arg)
    implicit none
    character(*) :: label
    class(*), intent(in) :: arg
    select type(arg)
    type is (dl(*));     print *, label, ' DL', arg%l, len(arg%chfld), '>', arg%chfld, '<'
    type is (dk(2));     print *, label, ' DK', arg%k, kind(arg%ifld), arg%ifld
    type is (dk(4));     print *, label, ' DK', arg%k, kind(arg%ifld), arg%ifld
    type is (dk(8));     print *, label, ' DK', arg%k, kind(arg%ifld), arg%ifld
    type is (dkl(2,*));  print *, label, ' DKL', arg%k, arg%dkfld%k, kind(arg%dkfld%ifld), &
                                  arg%l, arg%dlfld%l, len(arg%dlfld%chfld), arg%dkfld%ifld, '>', arg%dlfld%chfld, '<'
    type is (dkl(4,*));  print *, label, ' DKL', arg%k, arg%dkfld%k, kind(arg%dkfld%ifld), &
                                  arg%l, arg%dlfld%l, len(arg%dlfld%chfld), arg%dkfld%ifld, '>', arg%dlfld%chfld, '<'
    type is (dkl(8,*));  print *, label, ' DKL', arg%k, arg%dkfld%k, kind(arg%dkfld%ifld), &
                                  arg%l, arg%dlfld%l, len(arg%dlfld%chfld), arg%dkfld%ifld, '>', arg%dlfld%chfld, '<'
    class default;       print *, label, ' Error in dump: unknown type'; stop 2
    end select
  end subroutine dump


end module dtpIAssignFromUserDefinedOperatormod


program dtpIAssignFromUserDefinedOperator

  use :: dtpIAssignFromUserDefinedOperatormod
  implicit none

  integer :: i

  type(dl(2)) :: dl2
  type(dl(3)) :: dl3
  type(dl(5)) :: dl5
  type(dl(6)) :: dl6, dl6a

  type(dk(2)) :: dk2
  type(dk(4)) :: dk4
  type(dk(8)) :: dk8

  type(dkl(2,5)) :: dkl25
  type(dkl(4,6)) :: dkl46
  type(dkl(8,1)) :: dkl81

  save :: dl2, dl5, dl6, dk4, dkl46

	call dump('A', ''.cat.'')
  dl2 = ('' .cat. 'uv');	call dump('B',dl2)
  dl3 = ('wx' .cat. 'y');	call dump('C',dl3)
  dl5 = ('abc' .cat. 'de');	call dump('D',dl5)
  dl6 = ('fghi' .cat. 'jk'); 	call dump('E',dl6)
  dl6a = ('lmnopq' .cat. '');	call dump('F',dl6a)

  dl5 = (dl2 .cat. dl3);        	call dump('G',dl5)
  dl6 = (dl3 .cat. dl3);        	call dump('H',dl6)

  dl6 = (('r' .cat. 's') .cat. ('t' .cat. '')) .cat. dl3;  	call dump('I',dl6)

  dkl25 = DKL(2,5)(dl5,[(dk(2)(i),i=1,5)]);               	call dump('J',dkl25)
  dkl46 = DKL(4,6)(dl6,[(dk(4)(i**2),i=10000,10005)]);    	call dump('K',dkl46)
  dkl81 = DKL(8,1)(DL(1)('z'),dk(8)(12345678987654321_8));	call dump('L',dkl81)

  dkl46 = dkl25 .cat. dkl(2,1)(dl(1)('A'),dk(2)(99));  	call dump('M',dkl46)
  dkl81 = (dkl(4,0)(dl(0)(''),[dk(4)::]) &
           .cat. dkl(4,1)(dl(1)('B'),[dk(4)(31415)]));	call dump('N',dkl81)

  dk2 = (120_1 .tweak. 120_1);      	call dump('O',dk2)
  dk4 = (30120_2 .tweak. 19210_2);      	call dump('P',dk4)

  dk8 = dk4 .tweak. ((110_1 .tweak. 99_1) .tweak. dk(2)(12345));	call dump('Q',dk8)

  dkl46 = dkl(4,6)(dl(6)(''), dk(4)(0));	call dump('R',dkl46)

  dkl46 = dkl25 .tweak. dkl(2,1)(dl(1)('A'),dk(2)(99));  	call dump('S',dkl46)
  dkl81 = (dkl(4,1)(dl(1)('C'),[dk(4)(89765432)]) &
           .tweak. dkl(4,0)(dl(0)(''),[dk(4)::]));	call dump('T',dkl81)

  print *, 'done'

end program dtpIAssignFromUserDefinedOperator

