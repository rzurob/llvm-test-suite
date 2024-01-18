!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseTypeSpecAllocatable01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-10-07
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : allocatable with no type specifier
!*
!*  REFERENCE                  : Feature Number 355310
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
!*  Define allocatable variables and arrays, and verify that they are allocated
!*  correctly when created with a type specifier, i.e., correct kind, len, content.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseTypeSpecAllocatable01Basemod

  implicit none
  
  type :: Root
     integer :: iComp = -1
  end type Root

end module dtpUseTypeSpecAllocatable01Basemod

module dtpUseTypeSpecAllocatable01mod

  use :: dtpUseTypeSpecAllocatable01Basemod, only: Base => Root
  implicit none

  type, extends(Base) :: Der(k)
     integer, kind :: k
     integer(k) :: iCompD = -2
  end type

  type, extends(Der) :: Der2
     integer(k) :: iCompD2 = -3
  end type

  type, extends(Der2) :: Der3(l)
     integer, len :: l
     character(l) :: cCompD3 = 'abcdefghijklmnopqrstuvwxyz'
  end type

end module dtpUseTypeSpecAllocatable01mod


program dtpUseTypeSpecAllocatable01

  use :: dtpUseTypeSpecAllocatable01mod, Derived => Der, Derived2 => Der2, Derived3 => Der3
  implicit none

  type (Base)                       :: ba
  type (Base), allocatable          :: bb
  class(Base), allocatable          :: bc, barr(:)

  type (Derived(4))                 :: d4a
  type (Derived(4)), allocatable    :: d4b
  class(Derived(4)), allocatable    :: d4c, d4arr(:)
  type (Derived2(4))                :: d24a
  type (Derived2(4)), allocatable   :: d24b
  class(Derived2(4)), allocatable   :: d24c, d24arr(:)
  type (Derived3(4,3))              :: d343a
  type (Derived3(4,:)), allocatable :: d34b
  class(Derived3(4,:)), allocatable :: d34c, d34arr(:)

  type (Derived(8))                 :: d8a
  type (Derived(8)), allocatable    :: d8b
  class(Derived(8)), allocatable    :: d8c, d8arr(:)
  type (Derived2(8))                :: d28a
  type (Derived2(8)), allocatable   :: d28b
  class(Derived2(8)), allocatable   :: d28c, d28arr(:)
  type (Derived3(8,3))              :: d383a
  type (Derived3(8,:)), allocatable :: d38b
  class(Derived3(8,:)), allocatable :: d38c, d38arr(:)

  ba  %iComp = 1
  d4a %iComp = 2;   d4a%iCompD = 3;
  d24a%iComp = 4;   d24a%iCompD = 5;   d24a%iCompD2 = 6
  d343a%iComp = 7;  d343a%iCompD = 8;  d343a%iCompD2 = 9;  d343a%cCompD3 = 'Earl'
  d8a%iComp = 10;   d8a%iCompD = 11;
  d28a%iComp = 12;  d28a%iCompD = 13;  d28a%iCompD2 = 14
  d383a%iComp = 15; d383a%iCompD = 16; d383a%iCompD2 = 17; d383a%cCompD3 = 'Mar'

  print *, ba
  print *, d4a, d4a % k
  print *, d24a, d24a % k
  print *, d343a, d343a % k, d343a % l
  print *, d8a, d8a % k
  print *, d28a, d28a % k
  print *, d383a, d383a % k, d383a % l

  ! test compatibility of type-spec with var type
  allocate(Base:: bb);             bb  %iComp = 18
  call describe("Base:: bb",  bb)
  allocate(Derived(4):: d4b);      d4b %iComp = 19;  d4b%iCompD = 20;
  call describe("Derived(4):: d4b", d4b)
  allocate(Derived2(4):: d24b);    d24b%iComp = 21;  d24b%iCompD = 22;  d24b%iCompD2 = 23
  call describe("Derived2(4):: d24b", d24b)
  allocate(Derived3(4,3):: d34b);  d34b%iComp = 24;   d34b%iCompD = 25; d34b%iCompD2 = 26; d34b%cCompD3 = 'Wot'
  call describe("Derived3(4,3):: d34b", d34b)
  allocate(Derived(8):: d8b);      d8b%iComp = 27;   d8b%iCompD = 28;
  call describe("Derived(8):: d8b", d8b)
  allocate(Derived2(8):: d28b);    d28b%iComp = 29;  d28b%iCompD = 30;  d28b%iCompD2 = 31
  call describe("Derived2(8):: d28b", d28b)
  allocate(Derived3(8,3):: d38b); d38b%iComp = 32;   d38b%iCompD = 33; d38b%iCompD2 = 34; d38b%cCompD3 = 'Gorilla?'
  call describe("Derived3(8,3):: d38b", d38b)

  ! test compatibility of type-spec with var class
  allocate(Base:: bc);             bc  %iComp = 39
  call describe("Base:: bc",  bc)
  allocate(Derived(4):: d4c);      d4c %iComp = 40;  d4c%iCompD = 41;
  call describe("Derived(4):: d4c", d4c)
  allocate(Derived2(4):: d24c);    d24c%iComp = 42;  d24c%iCompD = 43;  d24c%iCompD2 = 44
  call describe("Derived2(4):: d24c", d24c)
  allocate(Derived3(4,3):: d34c);  d34c%iComp = 45;  d34c%iCompD = 46;  d34c%iCompD2 = 47; d34c%cCompD3 = 'Unquiet'
  call describe("Derived3(4,3):: d34c", d34c)
  allocate(Derived(8):: d8c);      d8c%iComp = 48;   d8c%iCompD = 49;
  call describe("Derived(8):: d8c", d8c)
  allocate(Derived2(8):: d28c);    d28c%iComp = 51;  d28c%iCompD = 52;  d28c%iCompD2 = 53
  call describe("Derived2(8):: d28c", d28c)
  allocate(Derived3(8,3):: d38c);  d38c%iComp = 54;  d38c%iCompD = 55;  d38c%iCompD2 = 56; d38c%cCompD3 = 'Slumbers'
  call describe("Derived3(8,3):: d38c", d38c)

  deallocate(d8b,d38b)
  ! auto-allocate
  d8b  = Derived(8)(353689, 17452771_8)
  call describe("auto d8b", d8b)
  d38b = Derived3(8,3)(118898333, 1198228293_8, 1303132345, "to the")
  call describe("auto d38b", d38b)
  deallocate(d8b, d38b)

  ! allocate child types:
  allocate(Derived(4) :: barr(3));    call aDescribe("barr(3) as Derived(4)", barr);    deallocate(barr)
  allocate(Derived(8) :: barr(2));    call aDescribe("barr(2) as Derived(8)", barr);    deallocate(barr)
  allocate(Derived2(4) :: barr(2));   call aDescribe("barr(2) as Derived2(4)", barr);   deallocate(barr)
  allocate(Derived2(8) :: barr(3));   call aDescribe("barr(3) as Derived2(8)", barr);   deallocate(barr)
  allocate(Derived3(4,3) :: barr(4)); call aDescribe("barr(4) as Derived3(4,3)", barr); deallocate(barr)
  allocate(Derived3(8,3) :: barr(5)); call aDescribe("barr(5) as Derived3(8,3)", barr); deallocate(barr)

  allocate(Derived(4) :: d4arr(3));    call aDescribe("d4arr(3) as Derived(4)", d4arr);    deallocate(d4arr)
  allocate(Derived2(4) :: d4arr(2));   call aDescribe("d4arr(2) as Derived2(4)", d4arr);   deallocate(d4arr)
  allocate(Derived3(4,3) :: d4arr(4)); call aDescribe("d4arr(4) as Derived3(4,3)", d4arr); deallocate(d4arr)
  allocate(Derived(8) :: d8arr(2));    call aDescribe("d8arr(2) as Derived(8)", d8arr);    deallocate(d8arr)
  allocate(Derived2(8) :: d8arr(3));   call aDescribe("d8arr(3) as Derived2(8)", d8arr);   deallocate(d8arr)
  allocate(Derived3(8,3) :: d8arr(5)); call aDescribe("d8arr(5) as Derived3(8,3)", d8arr); deallocate(d8arr)

  allocate(Derived2(4) :: d24arr(2));   call aDescribe("d24arr(2) as Derived2(4)", d24arr);   deallocate(d24arr)
  allocate(Derived3(4,3) :: d24arr(4)); call aDescribe("d24arr(4) as Derived3(4,3)", d24arr); deallocate(d24arr)
  allocate(Derived2(8) :: d28arr(3));   call aDescribe("d28arr(3) as Derived2(8)", d28arr);   deallocate(d28arr)
  allocate(Derived3(8,3) :: d28arr(5)); call aDescribe("d28arr(5) as Derived3(8,3)", d28arr); deallocate(d28arr)

  allocate(Derived3(4,3) :: d34arr(5)); call aDescribe("d34arr(5) as Derived3(4,3)", d34arr); deallocate(d34arr)
  allocate(Derived3(8,3) :: d38arr(5)); call aDescribe("d38arr(5) as Derived3(8,3)", d38arr); deallocate(d38arr)

contains

  subroutine describe(descrip, this)
    character(*), intent(in) :: descrip
    class(*), intent(in) :: this
    character(255) :: out
    select type(this)

    type is (integer(1))
      write(out,*) "I(1)", this
    type is (integer(2))
      write(out,*) "I(2)", this
    type is (integer(4))
      write(out,*) "I(4)", this
    type is (integer(8))
      write(out,*) "I(8)", this

    type is (logical(1))
      write(out,*) "L(1)", this
    type is (logical(2))
      write(out,*) "L(2)", this
    type is (logical(4))
      write(out,*) "L(4)", this
    type is (logical(8))
      write(out,*) "L(8)", this

    type is (real(4))
      write(out,*) "R(4)", this
    type is (real(8))
      write(out,*) "R(8)", this

    type is (complex(4))
      write(out,*) "Z(4)", this
    type is (complex(8))
      write(out,*) "Z(8)", this

    type is (character(*))
      write(out,*) "ch(", len(this), ") >", this, "<"

    type is (Base)
      write(out,*) "Base", this

    type is (Derived(4))
      write(out,*) "Derived(4)", this
    type is (Derived(8))
      write(out,*) "Derived(8)", this

    type is (Derived2(4))
      write(out,*) "Derived2(4)", this
    type is (Derived2(8))
      write(out,*) "Derived2(8)", this

    type is (Derived3(4,*))
      write(out,*) "Derived3(4,*)", this
    type is (Derived3(8,*))
      write(out,*) "Derived3(8,*)", this

    class default
      out = 'Unknown type'

    end select

    print *, trim(descrip), ":", trim(out)
  end subroutine describe

  subroutine aDescribe(descrip, this)
    character(*), intent(in) :: descrip
    class(*), intent(in) :: this(:)
    character(255) :: out
    select type(this)

    type is (integer(1))
      write(out,*) "I(1)", this
    type is (integer(2))
      write(out,*) "I(2)", this
    type is (integer(4))
      write(out,*) "I(4)", this
    type is (integer(8))
      write(out,*) "I(8)", this

    type is (logical(1))
      write(out,*) "L(1)", this
    type is (logical(2))
      write(out,*) "L(2)", this
    type is (logical(4))
      write(out,*) "L(4)", this
    type is (logical(8))
      write(out,*) "L(8)", this

    type is (real(4))
      write(out,*) "R(4)", this
    type is (real(8))
      write(out,*) "R(8)", this

    type is (complex(4))
      write(out,*) "Z(4)", this
    type is (complex(8))
      write(out,*) "Z(8)", this

    type is (character(*))
      write(out,*) "ch(", len(this), ") >", this, "<"

    type is (Base)
      write(out,*) "Base", this

    type is (Derived(4))
      write(out,*) "Derived(4)", this
    type is (Derived(8))
      write(out,*) "Derived(8)", this

    type is (Derived2(4))
      write(out,*) "Derived2(4)", this
    type is (Derived2(8))
      write(out,*) "Derived2(8)", this

    type is (Derived3(4,*))
      write(out,*) "Derived3(4,*)", this
    type is (Derived3(8,*))
      write(out,*) "Derived3(8,*)", this

    class default
      out = 'Unknown type'

    end select

    print *, trim(descrip), ":", trim(out)
  end subroutine aDescribe

end program dtpUseTypeSpecAllocatable01
