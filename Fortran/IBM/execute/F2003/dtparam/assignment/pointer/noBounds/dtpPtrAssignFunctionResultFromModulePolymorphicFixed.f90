!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignFunctionResultFromModulePolymorphicFixed
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-11-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : assign polymorphic module function results with fixed-length type parameters
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
!*  In module procedures, assign values to a function result of a polymorphic
!*  parameterised derived type for which there is no user-defined assignment,
!*  and verify that the type parameters and data values are as expected.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpPtrAssignFunctionResultFromModulePolymorphicFixedmod

  type base (l)
     integer, len :: l
     character(l) :: ch
  end type base

  type, extends(base) :: derived (k)
     integer, kind :: k
     real(k) :: rfld
     logical(k) :: lfld
     integer(k) :: ifld(l)
  end type derived

  type, extends(derived) :: d2 (k2, l2)
     integer, kind :: k2
     integer, len :: l2
     integer(k2) :: iarr(l,l2)
     type(derived(l2,k2)) :: der
  end type d2

contains


  function baseFun(n)
    implicit none
    integer, intent(in) :: n
    integer :: i
    class(Base(:)), pointer :: baseFun
    type(Base(3)) :: baseFun1 = Base(3)('abc')
    type(Base(5)) :: baseFun2 = Base(5)('abcde')
    type(Derived(3,4)) :: baseFun1D = Derived(3,4)('ghi',9.321_4,.true.,[1112131415,2142728292,1031323334])
    type(Derived(5,8)) :: baseFun2D = Derived(5,8)('jklmn',3.14159265D05,.true., &
                                                   [1213141516171_8,2829202122232_8,3435363738393_8,4041424344454_8,5657585950515_8])
    type(D2(3,4,8,3)) :: baseFun1D2 = D2(3,4,8,3)('jkl',6.3_4,.false.,[-1012141618,-2123252729,-1323436383], &
                                                  reshape([(1311_2*i,i=1,9)],[3,3]), &
                                                  derived(3,8)('xyz',12245.61820D34,.true.,[72543618204264_8,0_8,-113355777951331_8]))
    type(D2(5,8,4,5)) :: baseFun2D2 = D2(5,8,4,5)('opqrs',9.27456381D-12,.true., &
                                                  [-1012141618101_8,-2325272921232_8,-3931373335393_8,-4947453414947_8,-5557535951505_8], &
                                                  reshape([(1131_2*i,i=1,25)],[5,5]),derived(5,4)('vwxyz',9.12344E-12,.true.,[-12456780_4,0,0,0,0]))

    target :: baseFun1, baseFun2, baseFun1D, baseFun2D, baseFun1D2, baseFun2D2
    save   :: baseFun1, baseFun2, baseFun1D, baseFun2D, baseFun1D2, baseFun2D2

    select case (n)
    case(1);  baseFun => baseFun1
    case(2);  baseFun => baseFun2
    case(3);  baseFun => baseFun1D
    case(4);  baseFun => baseFun2D
    case(5);  baseFun => baseFun1D2
    case(6);  baseFun => baseFun2D2
    end select
  end function baseFun

  function derivedFun4(n)
    implicit none
    integer, intent(in) :: n
    integer :: i
    class(Derived(:,4)), pointer :: derivedFun4
    type(Derived(3,4)) :: derivedFun4a = Derived(3,4)('def',4.1_4,.true.,[1111911111,2122292222,1333393333])
    type(D2(3,4,8,3)) ::  derivedFun4D2 = D2(3,4,8,3)('mno',123.456_4,.true.,[-1111111112,-2122222223,-1333333334], &
                                                      reshape([(1113_2*i,i=1,9)],[3,3]), &
                                                      derived(3,8)('uvw',11246.81321D34,.true.,[7654321234567_8,0_8,-12345677654321_8]))
    target :: derivedFun4a, derivedFun4D2 
    save   :: derivedFun4a, derivedFun4D2
    select case (n)
    case(1); derivedFun4 => derivedFun4a
    case(2); derivedFun4 => derivedFun4D2
    end select
  end function derivedFun4


  function derivedFun8(n)
    implicit none
    integer, intent(in) :: n
    integer :: i
    class(Derived(:,8)), pointer :: derivedFun8
    type(Derived(5,8)) :: derivedFun8a = Derived(5,8)('defgh',1.23456789D11,.true., &
                                                      [1111118111111_8,2222282222222_8,3333833333333_8,4444484444444_8,5555855555555_8])
    type(D2(5,8,4,5)) :: derivedFun8D2 = D2(5,8,4,5)('qrstu',1.23456789D12,.false., &
                                                     [-1111111111311_8,-2222222232222_8,-3333233333333_8,-4444444434444_8,-5555555535555_8], &
                                                     reshape([(1114_2*i,i=1,25)],[5,5]),derived(5,4)('hijkl',109.654E-12,.true.,[-12341118_4,0,0,0,0]))
    target :: derivedFun8a, derivedFun8D2 
    save   :: derivedFun8a, derivedFun8D2
    select case (n)
    case(1); derivedFun8 => derivedFun8a
    case(2); derivedFun8 => derivedFun8D2
    end select
  end function derivedFun8


  function d2Fun48()
    implicit none
    integer :: i
    class(D2(:,4,8,:)), pointer :: d2Fun48
    type(D2(3,4,8,2)) :: d2Fun48a = D2(3,4,8,2)('ghi',5.9_4,.false.,[-1111111111,-2122222222,-1333333333], &
                                                reshape([(1211_2*i,i=1,6)],[3,2]), &
                                                derived(2,8)('xz',11987.81321D34,.true.,[76543111134567_8,-123456787654321_8]))
    target :: d2Fun48a
    save   :: d2Fun48a
    d2Fun48 => d2Fun48a
  end function d2Fun48


  function d2Fun84()
    implicit none
    integer :: i
    class(D2(:,8,4,:)), pointer :: d2Fun84
    type(D2(5,8,4,1)) :: d2Fun84a = D2(5,8,4,1)('defij',9.87654321D-12,.true., &
                                                [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8], &
                                                reshape([(1171_2*i,i=1,5)],[5,1]),derived(1,4)('y',9.87654E-12,.true.,[-12345678_4]))
    target :: d2Fun84a
    save   :: d2Fun84a
    d2Fun84 => d2Fun84a
  end function d2Fun84


  subroutine testBase1(b)
    implicit none
    type(Base(*)), intent(in), pointer :: b
    print *, b
    if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'abc') stop 2
  end subroutine testBase1


  subroutine testBase2(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'abcde') stop 3
  end subroutine testBase2


  subroutine testDerived4(d)
    implicit none
    type(Derived(*,4)), intent(in) :: d
    logical(4) :: precision_r4
    external :: precision_r4
    print *, d
    if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'def' .or. .not.d%lfld &
         .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
         .or. any(d%ifld /= [1111911111,2122292222,1333393333]) .or. .not.precision_r4(d%rfld,4.1_4)) stop 4
  end subroutine testDerived4


  subroutine testDerived8(d)
    implicit none
    type(Derived(*,8)), intent(in) :: d
    logical(4) :: precision_r8
    external :: precision_r8
    print *, d
    if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'defgh' .or. .not.d%lfld &
         .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
         .or. any(d%ifld /= [1111118111111_8,2222282222222_8,3333833333333_8,4444484444444_8,5555855555555_8]) &
         .or. .not.precision_r8(d%rfld,1.23456789D11)) stop 5
  end subroutine testDerived8


  subroutine testBase1D(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'ghi') stop 6
  end subroutine testBase1D

  subroutine testBase1D2(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'jkl') stop 7
  end subroutine testBase1D2

  subroutine testBase2D(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'jklmn') stop 8
  end subroutine testBase2D


  subroutine testBase2D2(b)
    implicit none
    type(Base(*)), intent(in) :: b
    print *, b
    if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'opqrs') stop 9
  end subroutine testBase2D2


  subroutine testDerived4D2(d)
    implicit none
    type(Derived(*,4)), intent(in) :: d
    logical(4) :: precision_r4
    external :: precision_r4
    print *, d
    if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'mno' .or. .not.d%lfld &
         .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
         .or. any(d%ifld /= [-1111111112,-2122222223,-1333333334]) .or. .not.precision_r4(d%rfld,123.456_4)) stop 10
  end subroutine testDerived4D2


  subroutine testDerived8D2(d)
    implicit none
    type(Derived(*,8)), intent(in) :: d
    logical(4) :: precision_r8
    external :: precision_r8
    print *, d
    if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'qrstu' .or. d%lfld &
         .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
         .or. any(d%ifld /= [-1111111111311_8,-2222222232222_8,-3333233333333_8,-4444444434444_8,-5555555535555_8]) &
	 .or. .not.precision_r8(d%rfld,1.23456789D12)) stop 11
  end subroutine testDerived8D2


  subroutine testD2_48(d2v)
    implicit none
    type(D2(*,4,8,*)), intent(in) :: d2v
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    print *, d2v
    if (d2v%l /= 3 .or. d2v%k /= 4 .or. d2v%k2 /= 8 .or. d2v%l2 /= 2 &
         .or. len(d2v%ch) /= 3 .or. d2v%ch /= 'ghi' .or. d2v%lfld &
         .or. size(d2v%ifld) /= 3 .or. kind(d2v%lfld) /= 4 .or. kind(d2v%ifld) /= 4 .or. kind(d2v%rfld) /= 4 &
         .or. any(d2v%ifld /= [-1111111111,-2122222222,-1333333333]) .or. .not.precision_r4(d2v%rfld,5.9_4) &
         .or. kind(d2v%iarr) /= 8 .or. any(ubound(d2v%iarr) /= [3,2]) &
         .or. any([d2v%iarr] /= [1211,2422,3633,4844,6055,7266])) stop 12
    if (d2v%der%l /= 2 .or. d2v%der%k /= 8 .or. len(d2v%der%ch) /= 2 &
         .or. d2v%der%ch /= 'xz' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 2 &
         .or. kind(d2v%der%lfld) /= 8 .or. kind(d2v%der%ifld) /= 8 .or. kind(d2v%der%rfld) /= 8 &
         .or. any(d2v%der%ifld /= [76543111134567_8,-123456787654321_8]) &
         .or. .not.precision_r8(d2v%der%rfld,11987.81321D34)) stop 13
  end subroutine testD2_48


  subroutine testD2_84(d2v)
    implicit none
    type(D2(*,8,4,*)), intent(in) :: d2v
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    print *, d2v
    if (d2v%l /= 5 .or. d2v%k /= 8 .or. d2v%k2 /= 4 .or. d2v%l2 /= 1 &
         .or. len(d2v%ch) /= 5 .or. d2v%ch /= 'defij' .or. .not.d2v%lfld &
         .or. size(d2v%ifld) /= 5 .or. kind(d2v%lfld) /= 8 .or. kind(d2v%ifld) /= 8 .or. kind(d2v%rfld) /= 8 &
         .or. any(d2v%ifld /= [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8]) &
         .or. .not.precision_r8(d2v%rfld,9.87654321D-12) &
         .or. kind(d2v%iarr) /= 4 .or. any(ubound(d2v%iarr) /= [5,1]) &
         .or. any([d2v%iarr] /= [1171,2342,3513,4684,5855])) stop 14
    if (d2v%der%l /= 1 .or. d2v%der%k /= 4 .or. len(d2v%der%ch) /= 1 &
         .or. d2v%der%ch /= 'y' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 1 &
         .or. kind(d2v%der%lfld) /= 4 .or. kind(d2v%der%ifld) /= 4 .or. kind(d2v%der%rfld) /= 4 &
         .or. any(d2v%der%ifld /= [-12345678_4]) &
         .or. .not.precision_r4(d2v%der%rfld,9.87654E-12)) stop 15
  end subroutine testD2_84


  subroutine testBase1CP(b)
    implicit none
    class(Base(:)), intent(in), pointer :: b
    select type(b)
    type is (Base(*))
      print *, b
      if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'abc') stop 2
    class default
      print *, 'Unknown type in testBase1CP'
      stop 102
    end select
  end subroutine testBase1CP


  subroutine testBase2CP(b)
    implicit none
    class(Base(*)), intent(in) :: b
    select type(b)
    type is (Base(*))
      print *, b
      if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'abcde') stop 3
    class default
      print *, 'Unknown type in testBase2CP'
      stop 103
    end select
  end subroutine testBase2CP


  subroutine testDerived4CP(d)
    implicit none
    class(Derived(*,4)), intent(in) :: d
    logical(4) :: precision_r4
    external :: precision_r4
    select type(d)
    type is (Derived(*,4))
      print *, d
      if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'def' .or. .not.d%lfld &
           .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
           .or. any(d%ifld /= [1111911111,2122292222,1333393333]) .or. .not.precision_r4(d%rfld,4.1_4)) stop 4
    class default
      print *, 'Unknown type in testDerived4CP'
      stop 104
    end select
  end subroutine testDerived4CP


  subroutine testDerived8CP(d)
    implicit none
    class(Derived(*,8)), intent(in) :: d
    logical(4) :: precision_r8
    external :: precision_r8
    select type(d)
    type is (Derived(*,8))
      print *, d
      if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'defgh' .or. .not.d%lfld &
           .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
           .or. any(d%ifld /= [1111118111111_8,2222282222222_8,3333833333333_8,4444484444444_8,5555855555555_8]) &
           .or. .not.precision_r8(d%rfld,1.23456789D11)) stop 5
    class default
      print *, 'Unknown type in testDerived8CP'
      stop 105
    end select
  end subroutine testDerived8CP


  subroutine testBase1DCP(b)
    implicit none
    class(Base(*)), intent(in) :: b
    select type(b)
    class is (Base(*))
      print *, b%l, len(b%ch), b%ch
      if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'ghi') stop 6
    class default
      print *, 'Unknown type in testBase1DCP'
      stop 106
    end select
  end subroutine testBase1DCP

  subroutine testBase1D2CP(b)
    implicit none
    class(Base(*)), intent(in) :: b
    select type(b)
    class is (Base(*))
      print *, b%l, len(b%ch), b%ch
      if (b%l /= 3 .or. len(b%ch) /= 3 .or. b%ch /= 'jkl') stop 7
    class default
      print *, 'Unknown type in testBase1D2CP'
      stop 107
    end select
  end subroutine testBase1D2CP

  subroutine testBase2DCP(b)
    implicit none
    class(Base(*)), intent(in) :: b
    select type(b)
    class is (Base(*))
      print *, b%l, len(b%ch), b%ch
      if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'jklmn') stop 8
    class default
      print *, 'Unknown type in testBase2DCP'
      stop 108
    end select
  end subroutine testBase2DCP


  subroutine testBase2D2CP(b)
    implicit none
    class(Base(*)), intent(in) :: b
    select type(b)
    class is (Base(*))
      print *, b%l, len(b%ch), b%ch
      if (b%l /= 5 .or. len(b%ch) /= 5 .or. b%ch /= 'opqrs') stop 9
    class default
      print *, 'Unknown type in testBase2D2CP'
      stop 109
    end select
  end subroutine testBase2D2CP


  subroutine testDerived4D2CP(d)
    implicit none
    class(Derived(*,4)), intent(in) :: d
    logical(4) :: precision_r4
    external :: precision_r4
    select type(d)
    class is (Derived(*,4))
      print *, d%l, len(d%ch), d%ch, d%l, d%k, len(d%ch), d%ch, d%lfld, size(d%ifld), &
               kind(d%lfld), kind(d%ifld), kind(d%rfld), d%ifld, d%rfld
      if (d%l /= 3 .or. d%k /= 4 .or. len(d%ch) /= 3 .or. d%ch /= 'mno' .or. .not.d%lfld &
           .or. size(d%ifld) /= 3 .or. kind(d%lfld) /= 4 .or. kind(d%ifld) /= 4 .or. kind(d%rfld) /= 4 &
           .or. any(d%ifld /= [-1111111112,-2122222223,-1333333334]) .or. .not.precision_r4(d%rfld,123.456_4)) stop 10
    class default
      print *, 'Unknown type in testDerived4D2CP'
      stop 110
    end select
  end subroutine testDerived4D2CP


  subroutine testDerived8D2CP(d)
    implicit none
    class(Derived(*,8)), intent(in) :: d
    logical(4) :: precision_r8
    external :: precision_r8
    select type(d)
    class is (Derived(*,8))
      print *, d%l, len(d%ch), d%ch, d%l, d%k, len(d%ch), d%ch, d%lfld, size(d%ifld), &
               kind(d%lfld), kind(d%ifld), kind(d%rfld), d%ifld, d%rfld
      if (d%l /= 5 .or. d%k /= 8 .or. len(d%ch) /= 5 .or. d%ch /= 'qrstu' .or. d%lfld &
           .or. size(d%ifld) /= 5 .or. kind(d%lfld) /= 8 .or. kind(d%ifld) /= 8 .or. kind(d%rfld) /= 8 &
           .or. any(d%ifld /= [-1111111111311_8,-2222222232222_8,-3333233333333_8,-4444444434444_8,-5555555535555_8]) &
           .or. .not.precision_r8(d%rfld,1.23456789D12)) stop 11
    class default
      print *, 'Unknown type in testDerived8D2CP'
      stop 111
    end select
  end subroutine testDerived8D2CP


  subroutine testD2_48CP(d2v)
    implicit none
    class(D2(*,4,8,*)), intent(in) :: d2v
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    select type(d2v)
    class is (D2(*,4,8,*))
      print *, d2v%l, len(d2v%ch), d2v%ch, d2v%l, d2v%k, len(d2v%ch), d2v%ch, d2v%lfld, size(d2v%ifld), &
               kind(d2v%lfld), kind(d2v%ifld), kind(d2v%rfld), d2v%ifld, d2v%rfld,  kind(d2v%iarr), ubound(d2v%iarr), d2v%iarr, &
               d2v%der%l, d2v%der%k, len(d2v%der%ch), d2v%der%ch, d2v%der%lfld, size(d2v%der%ifld), &
               kind(d2v%der%lfld), kind(d2v%der%ifld), kind(d2v%der%rfld), d2v%der%ifld, d2v%der%rfld
      if (d2v%l /= 3 .or. d2v%k /= 4 .or. d2v%k2 /= 8 .or. d2v%l2 /= 2 &
           .or. len(d2v%ch) /= 3 .or. d2v%ch /= 'ghi' .or. d2v%lfld &
           .or. size(d2v%ifld) /= 3 .or. kind(d2v%lfld) /= 4 .or. kind(d2v%ifld) /= 4 .or. kind(d2v%rfld) /= 4 &
           .or. any(d2v%ifld /= [-1111111111,-2122222222,-1333333333]) .or. .not.precision_r4(d2v%rfld,5.9_4) &
           .or. kind(d2v%iarr) /= 8 .or. any(ubound(d2v%iarr) /= [3,2]) &
           .or. any([d2v%iarr] /= [1211,2422,3633,4844,6055,7266])) stop 12
      if (d2v%der%l /= 2 .or. d2v%der%k /= 8 .or. len(d2v%der%ch) /= 2 &
           .or. d2v%der%ch /= 'xz' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 2 &
           .or. kind(d2v%der%lfld) /= 8 .or. kind(d2v%der%ifld) /= 8 .or. kind(d2v%der%rfld) /= 8 &
           .or. any(d2v%der%ifld /= [76543111134567_8,-123456787654321_8]) &
           .or. .not.precision_r8(d2v%der%rfld,11987.81321D34)) stop 13
    class default
      print *, 'Unknown type in testD2_48CP'
      stop 113
    end select
  end subroutine testD2_48CP


  subroutine testD2_84CP(d2v)
    implicit none
    class(D2(*,8,4,*)), intent(in) :: d2v
    logical(4) :: precision_r4, precision_r8
    external :: precision_r4, precision_r8
    select type(d2v)
    class is (D2(*,8,4,*))
      print *, d2v%l, len(d2v%ch), d2v%ch, d2v%l, d2v%k, len(d2v%ch), d2v%ch, d2v%lfld, size(d2v%ifld), &
               kind(d2v%lfld), kind(d2v%ifld), kind(d2v%rfld), d2v%ifld, d2v%rfld,  kind(d2v%iarr), ubound(d2v%iarr), d2v%iarr, &
               d2v%der%l, d2v%der%k, len(d2v%der%ch), d2v%der%ch, d2v%der%lfld, size(d2v%der%ifld), &
               kind(d2v%der%lfld), kind(d2v%der%ifld), kind(d2v%der%rfld), d2v%der%ifld, d2v%der%rfld
      if (d2v%l /= 5 .or. d2v%k /= 8 .or. d2v%k2 /= 4 .or. d2v%l2 /= 1 &
           .or. len(d2v%ch) /= 5 .or. d2v%ch /= 'defij' .or. .not.d2v%lfld &
           .or. size(d2v%ifld) /= 5 .or. kind(d2v%lfld) /= 8 .or. kind(d2v%ifld) /= 8 .or. kind(d2v%rfld) /= 8 &
           .or. any(d2v%ifld /= [-1111111111111_8,-2222222222222_8,-3333333333333_8,-4444444444444_8,-5555555555555_8]) &
           .or. .not.precision_r8(d2v%rfld,9.87654321D-12) &
           .or. kind(d2v%iarr) /= 4 .or. any(ubound(d2v%iarr) /= [5,1]) &
           .or. any([d2v%iarr] /= [1171,2342,3513,4684,5855])) stop 14
      if (d2v%der%l /= 1 .or. d2v%der%k /= 4 .or. len(d2v%der%ch) /= 1 &
           .or. d2v%der%ch /= 'y' .or. .not.d2v%der%lfld .or. size(d2v%der%ifld) /= 1 &
           .or. kind(d2v%der%lfld) /= 4 .or. kind(d2v%der%ifld) /= 4 .or. kind(d2v%der%rfld) /= 4 &
           .or. any(d2v%der%ifld /= [-12345678_4]) &
           .or. .not.precision_r4(d2v%der%rfld,9.87654E-12)) stop 15
    class default
      print *, 'Unknown type in testD2_84CP'
      stop 115
    end select
  end subroutine testD2_84CP

end module dtpPtrAssignFunctionResultFromModulePolymorphicFixedmod


program dtpPtrAssignFunctionResultFromModulePolymorphicFixed

  use :: dtpPtrAssignFunctionResultFromModulePolymorphicFixedmod
  implicit none

  type(base(3)), pointer      :: b_3p
  type(derived(3,4)), pointer :: d_34p
  type(d2(3,4,8,2)), pointer  :: d2_3482p

  type(base(5)), pointer      :: b_5p
  type(derived(5,8)), pointer :: d_58p
  type(d2(5,8,4,1)), pointer  :: d2_5841p

  print *, 'testBase1CP(baseFun(1))'
  call testBase1CP(baseFun(1))
  b_3p => baseFun(1)
  call testBase1(b_3p)

  print *, 'testBase1DCP(baseFun(3))'
  call testBase1DCP(baseFun(3))
  b_3p => baseFun(3)
  call testBase1D(b_3p)

  print *, 'testBase1D2CP(baseFun(5))'
  call testBase1D2CP(baseFun(5))
  b_3p => baseFun(5)
  call testBase1D2(b_3p)

  print *, 'testBase2CP(baseFun(2))'
  call testBase2CP(baseFun(2))
  b_5p => baseFun(2)
  call testBase2(b_5p)

  print *, 'testBase2DCP(baseFun(4))'
  call testBase2DCP(baseFun(4))
  b_5p => baseFun(4)
  call testBase2D(b_5p)

  print *, 'testBase2D2CP(baseFun(6))'
  call testBase2D2CP(baseFun(6))
  b_5p => baseFun(6)
  call testBase2D2(b_5p)

  print *, 'testDerived4CP(derivedFun4(1))'
  call testDerived4CP(derivedFun4(1))
  d_34p => derivedFun4(1)
  call testDerived4(d_34p)

  print *, 'testDerived4D2CP(derivedFun4(2))'
  call testDerived4D2CP(derivedFun4(2))
  d_34p => derivedFun4(2)
  call testDerived4D2(d_34p)

  print *, 'testDerived8CP(derivedFun8(1))'
  call testDerived8CP(derivedFun8(1))
  d_58p => derivedFun8(1)
  call testDerived8(d_58p)

  print *, 'testDerived8D2CP(derivedFun8(2))'
  call testDerived8D2CP(derivedFun8(2))
  d_58p => derivedFun8(2)
  call testDerived8D2(d_58p)

  print *, 'testD2_48CP(d2Fun48())'
  call testD2_48CP(d2Fun48())
  d2_3482p => d2Fun48()
  call testD2_48(d2_3482p)

  print *, 'testD2_84CP(d2Fun84())'
  call testD2_84CP(d2Fun84())
  d2_5841p => d2Fun84()
  call testD2_84(d2_5841p)

  print *, 'done'

end program dtpPtrAssignFunctionResultFromModulePolymorphicFixed
