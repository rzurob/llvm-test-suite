!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : AC
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
!*  Array constructors
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789


module dtpUseAC01BaseMod
  implicit none

  type :: Base
     integer :: iComp
  end type Base

end module dtpUseAC01BaseMod

module dtpUseAC01Mod

  use :: dtpUseAC01BaseMod, only: Base
  implicit none

  type, extends(Base) :: Derived(k)
     integer, kind :: k
     integer(k) :: iCompD
  end type

  type, extends(Derived) :: Derived2
     integer(k) :: iCompD2
  end type

  type, extends(Derived2) :: Derived3(l)
     integer, len :: l
     character(l) :: cCompD3
  end type

end module dtpUseAC01Mod

program dtpUseAC01
  use dtpUseAC01Mod
  implicit none

  type (Base)          :: ba, bb, bc, barr(3)

  type (Derived(4))    :: d4a, d4b, d4c, d4arr(3)
  type (Derived2(4))   :: d24a, d24b, d24arr(2)
  type (Derived3(4,3)) :: d343a, d343b, d343c, d343arr(3)

  type (Derived(8))    :: d8a, d8b, d8c, d8arr(3)
  type (Derived2(8))   :: d28a, d28b, d28arr(2)
  type (Derived3(8,3)) :: d383a, d383b, d383c, d383arr(3)

  ba%iComp    = 37
  bb%iComp    = 38
  bc%iComp    = 39

  d4a %iComp  = 42;       d4a %iCompD = 45
  d24a%iComp  = 142;      d24a%iCompD = 56;           d24a%iCompD2 = 1234
  d343a%iComp = 11142;    d343a%iCompD= 11242;        d343a%iCompD2= 15242;     d343a%cCompD3= "this is a test"

  d4b %iComp  = 19;       d4b %iCompD = 17
  d24b%iComp  = 119;      d24b%iCompD = 29;           d24b%iCompD2 = 1293
  d343b%iComp = 11119;    d343b%iCompD= 11942;        d343b%iCompD2= 15242;     d343b%cCompD3= "another test"

  d4c %iComp  = 1209;     d4c %iCompD = 1721
  d343c%iComp = 1123119;  d343c%iCompD= 119244222;    d343c%iCompD2= 152526242; d343c%cCompD3= "not yet done"

  d8a %iComp  = 82;       d8a %iCompD = 85
  d28a%iComp  = 182;      d28a%iCompD = 567324_8;     d28a%iCompD2 = 1238787
  d383a%iComp = 11182897; d383a%iCompD= 112821_8;     d383a%iCompD2= 15282234;  d383a%cCompD3= "wowee"

  d8b %iComp  = 89;       d8b %iCompD = 174571_8
  d28b%iComp  = 189;      d28b%iCompD = 293487_8;     d28b%iCompD2 = 1293656
  d383b%iComp = 11889876; d383b%iCompD= 119823_8;     d383b%iCompD2= 15282345;  d383b%cCompD3= "almost done."

  d8c %iComp  = 353689;   d8c %iCompD = 17452771_8
  d383c%iComp = 118898333;d383c%iCompD= 1198228293_8; d383c%iCompD2= 1303132345;d383c%cCompD3= "done now."

  barr    = [Base          :: ba, bb, bc]
  d4arr   = [Derived(4)    :: d4a, d4b, d4c]
  d24arr  = [Derived2(4)   :: d24a, d24b]
  d343arr = [Derived3(4,3) :: d343a, d343b, d343c]
  d8arr   = [Derived(8)    :: d8a, d8b, d8c]
  d28arr  = [Derived2(8)   :: d28a, d28b]
  d383arr = [Derived3(8,3) :: d383a, d383b, d383c]

  print *, barr,   [Base          :: ba, bb, bc]
  print *, d4arr,  [Derived(4)    :: d4a, d4b, d4c]
  print *, d24arr, [Derived2(4)   :: d24a, d24b]
  print *, d343arr,[Derived3(4,3) :: d343a, d343b, d343c]
  print *, d8arr,  [Derived(8)    :: d8a, d8b, d8c]
  print *, d28arr, [Derived2(8)   :: d28a, d28b]
  print *, d383arr,[Derived3(8,3) :: d383a, d383b, d383c]

  print *, [Base ::         Base(37), Base(38), Base(39)]

  print *, [Derived(4) ::   Derived(4)(42,45), Derived(4)(19, 17), Derived(4)(1209, 1721)]
  print *, [Derived2(4)::   Derived2(4)(142 , 56, 1234), Derived2(4)(119, 29, 1293)]
  print *, [Derived3(4,3):: Derived3(4,3)(11142, 11242, 15242, "this is a test"), &
                            Derived3(4,3)(11119, 11942, 15242, "another test"), &
                            Derived3(4,3)(1123119, 119244222, 152526242, "not yet done")]
  print *, [Derived(8)::    Derived(8)(82, 85), Derived(8)(89, 174571_8), Derived(8)(353689, 17452771_8)]
  print *, [Derived2(8)::   Derived2(8)(182, 567324_8, 1238787), Derived2(8)(189, 293487_8, 1293656)]
  print *, [Derived3(8,3):: Derived3(8,3)(11182897, 112821_8, 15282234, "wowee"), &
                            Derived3(8,3)(11889876, 119823_8, 15282345, "almost done."), &
                            Derived3(8,3)(118898333, 1198228293_8, 1303132345, "done now.")]

  print *, "end"

end program dtpUseAC01
