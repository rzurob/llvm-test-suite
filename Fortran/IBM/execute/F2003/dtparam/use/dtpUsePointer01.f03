!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-08
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : use pointers to DTP objects
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
!*  Types defined in a module and USEd elsewhere will be declared as TARGETs,
!*  and POINTERs, and manipulated, including pointer assignment and assignment
!*  via a pointer.  We can also sneak in a little test of the TRANSFER intrinsic.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUsePointer01BaseMod

  implicit none

  type :: Basic
     integer :: iComp = -1
  end type Basic

end module dtpUsePointer01BaseMod

module dtpUsePointer01Mod

  use :: dtpUsePointer01BaseMod, only: Base => Basic
  implicit none


  type, extends(Base) :: Der(k)
     integer, kind :: k
     real(k) :: rCompD = -2.2
  end type

  type, extends(Der) :: Der2
     integer(k) :: iCompD2 = -3
  end type

  type, extends(Der2) :: Der3(l)
     integer, len :: l
     character(l) :: cCompD3 = 'abcdefghijklmnopqrstuvwxyz'
  end type

  type(Der3(8,9)), save  :: dmod38
  type(Der3(4,10)), save :: dmod34

end module dtpUsePointer01Mod


program dtpUsePointer01

  use :: dtpUsePointer01Mod, only: Base, Derived => Der, Derived2 => Der2, Derived3 => Der3, dmod38, dmod34
  implicit none

  type (Base), target          :: ba

  type (Derived(4)), target    :: d4a
  type (Derived2(4)), target   :: d24a
  type (Derived3(4,3)), target :: d343a

  type (Derived(8)), target    :: d8a
  type (Derived2(8)), target   :: d28a
  type (Derived3(8,3)), target :: d383a

  type (Base), pointer          :: bp

  type (Derived(4)), pointer    :: d4p
  type (Derived2(4)), pointer   :: d24p
  type (Derived3(4,:)), pointer :: d343p

  type (Derived(8)), pointer    :: d8p
  type (Derived2(8)), pointer   :: d28p
  type (Derived3(8,:)), pointer :: d383p

  class(Base), pointer          :: bc

  class(Derived(4)), pointer    :: d4c
  class(Derived2(4)), pointer   :: d24c
  class(Derived3(4,3)), pointer :: d343c

  class(Derived(8)), pointer    :: d8c
  class(Derived2(8)), pointer   :: d28c
  class(Derived3(8,:)), pointer :: d383c

  ba%iComp    = 37

  d4a %iComp  = 42;       d4a %rCompD = 45
  d24a%iComp  = 142;      d24a%rCompD = 56;           d24a%iCompD2 = 1234
  d343a%iComp = 11142;    d343a%rCompD= 11242;        d343a%iCompD2= 15242;     d343a%cCompD3= "this is a test"

  d8a %iComp  = 82;       d8a %rCompD = 85
  d28a%iComp  = 182;      d28a%rCompD = 567324_8;     d28a%iCompD2 = 1238787
  d383a%iComp = 11182897; d383a%rCompD= 112821_8;     d383a%iCompD2= 15282234;  d383a%cCompD3= "wowee"

  print *, "ba:",   ba, ba
  print *, "d4a:",  d4a, d4a % k
  print *, "d24a",  d24a, d24a % k
  print *, "d343a", d343a, d343a % k, d343a % l
  print *, "d8a",   d8a, d8a % k
  print *, "d28a",  d28a, d28a % k
  print *, "d383a", d383a, d383a % k, d383a % l

  bp => ba
  d4p => d4a
  d24p => d24a
  d343p => d343a
  d8p => d8a
  d28p => d28a
  d383p => d383a

  print *, "bp:",   bp, bp
  print *, "d4p:",  d4p, d4p % k
  print *, "d24p",  d24p, d24p % k
  print *, "d343p", d343p, d343p % k, d343p % l
  print *, "d8p",   d8p, d8p % k
  print *, "d28p",  d28p, d28p % k
  print *, "d383p", d383p, d383p % k, d383p % l

  bp = Base(88)
  d4p = Derived(4)(20,21.1)
  d24p = Derived2(4)(22,23.1,38)
  d343p = Derived3(4,3)(24,25.9,26,'abcdef')
  d8p = Derived(8)(33,34.1)
  d28p = Derived2(8)(35,36.39,37)
  d383p = Derived3(8,3)(30,3140.41,32,'mnopqrstuv')

  print *, "bp:",   bp, bp
  print *, "d4p:",  d4p, d4p % k
  print *, "d24p",  d24p, d24p % k
  print *, "d343p", d343p, d343p % k, d343p % l
  print *, "d8p",   d8p, d8p % k
  print *, "d28p",  d28p, d28p % k
  print *, "d383p", d383p, d383p % k, d383p % l

  bc    => bp;    print *, "bc    => bp", transfer(bc, ba)
  bc    => d4p;   print *, "bc    => d4p", transfer(bc,d4a)
  bc    => d8p;   print *, "bc    => d8p", transfer(bc,d8a)
  bc    => d24p;  print *, "bc    => d24p", transfer(bc,d24a)
  bc    => d28p;  print *, "bc    => d28p", transfer(bc,d28a)
  bc    => d343p; print *, "bc    => d343p", transfer(bc,d343a)
  bc    => d383p; print *, "bc    => d383p", transfer(bc,d383a)

  d24c  => d24p;  print *, "d24c  => d24p", transfer(d24c,d24a)
  d28c  => d28p;  print *, "d28c  => d28p", transfer(d28c,d28a)
  d24c  => d343p; print *, "d24c  => d343p", transfer(d24c,d343a)
  d28c  => d383p; print *, "d28c  => d383p", transfer(d28c,d383a)

  d4c   => d4p;   print *, "d4c   => d4p", transfer(d4c,d4a)
  d4c   => d24p;  print *, "d4c   => d24p", transfer(d4c,d24a)
  d4c   => d343p; print *, "d4c   => d343p", transfer(d4c,d343a)

  d8c   => d8p;   print *, "d8c   => d8p", transfer(d8c,d8a)
  d8c   => d28p;  print *, "d8c   => d28p", transfer(d8c,d28a)
  d8c   => d383p; print *, "d8c   => d383p", transfer(d8c,d383a)

  d343c => d343p; print *, "d343c => d343p", transfer(d343c,d343a)
  d383c => d383p; print *, "d383c => d383p", transfer(d383c,d383a)

  print *, 'done'

end program dtpUsePointer01
