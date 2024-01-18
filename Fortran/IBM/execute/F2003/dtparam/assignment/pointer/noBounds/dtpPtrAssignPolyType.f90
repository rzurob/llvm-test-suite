!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpPtrAssignPolyType
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-03
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment without Lower Bounds Specification or Remap
!*
!*  SECONDARY FUNCTIONS TESTED : dynamic type of polymorphic data-pointer assumes that of data-target
!*
!*  REFERENCE                  : Feature Number 360669
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : polymorphic, dynamic type, target
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  - create family of types, target instances of each type, and polymorphic pointers to each type in the family
!*  - pointer-assign the pointers to different targets, and test the dynamic type of each using SAME_TYPE_AS
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program dtpPtrAssignPolyType

  implicit none

  type, abstract :: ModeOfTransport(l)
     integer, len :: l
     character(l) :: name
  end type ModeOfTransport

  type, abstract, extends(ModeOfTransport) :: WheeledTransport (k)
    integer, kind :: k
    integer (k) :: nwheels
  end type WheeledTransport

  type, abstract, extends(ModeOfTransport) :: NonWheeledTransport
  end type NonWheeledTransport

  type, extends(NonWheeledTransport) :: Travois
  end type Travois

  type, extends(WheeledTransport) :: Automobile
    integer (k) :: ndoors
    character(l) :: colour
  end type Automobile

  type, extends(WheeledTransport) :: Cart (k2)
    integer, kind :: k2
    integer (k2) :: length, width
  end type Cart
  
  type, extends(Automobile) :: Car
    integer (k) :: npassengers
  end type Car

  type, extends(Automobile) :: Truck (k2)
    integer, kind :: k2
    integer (k2) :: maximumLoad
  end type Truck

  ! #ModeOfTransport (l) -+-- #NonWheeledTransport  ---- Travois
  !                       |
  !                       +-- #WheeledTransport (k) -+-- Cart (k2)
  !                                                  |
  !                                                  +-- Automobile -+-- Car
  !                                                                  |
  !                                                                  +-- Truck (k2)

  integer, parameter :: SMALL = 1
  integer, parameter :: MED   = 2

  ! abstract:
  class(*), pointer                         :: ultdp => null(), ultdp2 => null()
  class(ModeOfTransport(:)), pointer        :: motp  => null(), motp2  => null()
  class(NonWheeledTransport(:)), pointer    :: nwtp  => null(), nwtp2  => null()
  class(WheeledTransport(:,SMALL)), pointer :: wtp   => null(), wtp2   => null()

  ! concrete:
  class(Travois(:)), pointer                :: travp => null(), travp2 => null()
  class(Cart(:,SMALL,SMALL)), pointer       :: c1p   => null(), c1p2   => null()
  class(Automobile(:,SMALL)), pointer       :: ap    => null(), ap2    => null()
  class(Car(:,SMALL)), pointer              :: c2p   => null(), c2p2   => null()
  class(Truck(:,SMALL,SMALL)), pointer      :: trp   => null(), trp2   => null()

  type(Travois(8)), target                  :: travt
  type(Cart(5,SMALL,SMALL)), target         :: c1t
  type(Automobile(3,SMALL)), target         :: at
  type(Car(9,SMALL)), target                :: c2t
  type(Truck(8,SMALL,SMALL)), target        :: trt

  class(WheeledTransport(:,MED)), pointer   :: wtmp  => null(), wtmp2  => null()
  class(Cart(:,MED,MED)), pointer           :: c1mp  => null(), c1mp2   => null()
  class(Automobile(:,MED)), pointer         :: amp   => null(), amp2    => null()
  class(Car(:,MED)), pointer                :: c2mp  => null(), c2mp2   => null()
  class(Truck(:,MED,MED)), pointer          :: trmp  => null(), trmp2   => null()

  type(Travois(3)), target                  :: travmt
  type(Cart(7,MED,MED)), target             :: c1mt
  type(Automobile(6,MED)), target           :: amt
  type(Car(1,MED)), target                  :: c2mt
  type(Truck(9,MED,MED)), target            :: trmt


  ultdp => travt
  if (.not.SAME_TYPE_AS(ultdp,travt) .or. .not.SAME_TYPE_AS(ultdp,travp2)) stop 2
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtp2) .or. SAME_TYPE_AS(ultdp,c2p2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,c1p2) .or. SAME_TYPE_AS(ultdp,trp2) .or. SAME_TYPE_AS(ultdp,ap2)) stop 3

  motp => travt
  if (.not.SAME_TYPE_AS(motp,travt) .or. .not.SAME_TYPE_AS(motp,travp2)) stop 4
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtp2) .or. SAME_TYPE_AS(motp,c2p2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,c1p2) .or. SAME_TYPE_AS(motp,trp2) .or. SAME_TYPE_AS(motp,ap2)) stop 5

  nwtp => travt
  if (.not.SAME_TYPE_AS(nwtp,travt) .or. .not.SAME_TYPE_AS(nwtp,travp2)) stop 6
  if (SAME_TYPE_AS(nwtp,ultdp2) .or. SAME_TYPE_AS(nwtp,motp2) .or. SAME_TYPE_AS(nwtp,wtp2) .or. SAME_TYPE_AS(nwtp,c2p2) &
      .or. SAME_TYPE_AS(nwtp,nwtp2) .or. SAME_TYPE_AS(nwtp,c1p2) .or. SAME_TYPE_AS(nwtp,trp2) .or. SAME_TYPE_AS(nwtp,ap2)) stop 7

  travp => travt
  if (.not.SAME_TYPE_AS(travp,travt) .or. .not.SAME_TYPE_AS(travp,travp2)) stop 8
  if (SAME_TYPE_AS(travp,ultdp2) .or. SAME_TYPE_AS(travp,motp2) .or. SAME_TYPE_AS(travp,wtp2) .or. SAME_TYPE_AS(travp,c2p2) &
      .or. SAME_TYPE_AS(travp,nwtp2) .or. SAME_TYPE_AS(travp,c1p2) .or. SAME_TYPE_AS(travp,trp2) .or. SAME_TYPE_AS(travp,ap2)) stop 9


  ultdp => c1t
  if (.not.SAME_TYPE_AS(ultdp,c1t) .or. .not.SAME_TYPE_AS(ultdp,c1p2)) stop 10
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtp2) .or. SAME_TYPE_AS(ultdp,c2p2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2) .or. SAME_TYPE_AS(ultdp,trp2) .or. SAME_TYPE_AS(ultdp,ap2)) stop 11

  motp => c1t
  if (.not.SAME_TYPE_AS(motp,c1t) .or. .not.SAME_TYPE_AS(motp,c1p2)) stop 12
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtp2) .or. SAME_TYPE_AS(motp,c2p2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2) .or. SAME_TYPE_AS(motp,trp2) .or. SAME_TYPE_AS(motp,ap2)) stop 13

  wtp => c1t
  if (.not.SAME_TYPE_AS(wtp,c1t) .or. .not.SAME_TYPE_AS(wtp,c1p2)) stop 14
  if (SAME_TYPE_AS(wtp,ultdp2) .or. SAME_TYPE_AS(wtp,motp2) .or. SAME_TYPE_AS(wtp,wtp2) .or. SAME_TYPE_AS(wtp,c2p2) &
      .or. SAME_TYPE_AS(wtp,nwtp2) .or. SAME_TYPE_AS(wtp,travp2) .or. SAME_TYPE_AS(wtp,trp2) .or. SAME_TYPE_AS(wtp,ap2)) stop 15


  ultdp => at
  if (.not.SAME_TYPE_AS(ultdp,at) .or. .not.SAME_TYPE_AS(ultdp,ap2)) stop 16
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtp2) .or. SAME_TYPE_AS(ultdp,c2p2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2) .or. SAME_TYPE_AS(ultdp,trp2) .or. SAME_TYPE_AS(ultdp,c1p2)) stop 17

  motp => at
  if (.not.SAME_TYPE_AS(motp,at) .or. .not.SAME_TYPE_AS(motp,ap2)) stop 18
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtp2) .or. SAME_TYPE_AS(motp,c2p2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2) .or. SAME_TYPE_AS(motp,trp2) .or. SAME_TYPE_AS(motp,c1p2)) stop 19

  wtp => at
  if (.not.SAME_TYPE_AS(wtp,at) .or. .not.SAME_TYPE_AS(wtp,ap2)) stop 20
  if (SAME_TYPE_AS(wtp,ultdp2) .or. SAME_TYPE_AS(wtp,motp2) .or. SAME_TYPE_AS(wtp,wtp2) .or. SAME_TYPE_AS(wtp,c2p2) &
      .or. SAME_TYPE_AS(wtp,nwtp2) .or. SAME_TYPE_AS(wtp,travp2) .or. SAME_TYPE_AS(wtp,trp2) .or. SAME_TYPE_AS(wtp,c1p2)) stop 21

  ap => at
  if (.not.SAME_TYPE_AS(ap,at) .or. .not.SAME_TYPE_AS(ap,ap2)) stop 22
  if (SAME_TYPE_AS(ap,ultdp2) .or. SAME_TYPE_AS(ap,motp2) .or. SAME_TYPE_AS(ap,wtp2) .or. SAME_TYPE_AS(ap,c2p2) &
      .or. SAME_TYPE_AS(ap,nwtp2) .or. SAME_TYPE_AS(ap,travp2) .or. SAME_TYPE_AS(ap,trp2) .or. SAME_TYPE_AS(ap,c1p2)) stop 23


  ultdp => c2t
  if (.not.SAME_TYPE_AS(ultdp,c2t) .or. .not.SAME_TYPE_AS(ultdp,c2p2)) stop 24
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtp2) .or. SAME_TYPE_AS(ultdp,ap2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2) .or. SAME_TYPE_AS(ultdp,trp2) .or. SAME_TYPE_AS(ultdp,c1p2)) stop 25

  motp => c2t
  if (.not.SAME_TYPE_AS(motp,c2t) .or. .not.SAME_TYPE_AS(motp,c2p2)) stop 26
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtp2) .or. SAME_TYPE_AS(motp,ap2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2) .or. SAME_TYPE_AS(motp,trp2) .or. SAME_TYPE_AS(motp,c1p2)) stop 27

  wtp => c2t
  if (.not.SAME_TYPE_AS(wtp,c2t) .or. .not.SAME_TYPE_AS(wtp,c2p2)) stop 28
  if (SAME_TYPE_AS(wtp,ultdp2) .or. SAME_TYPE_AS(wtp,motp2) .or. SAME_TYPE_AS(wtp,wtp2) .or. SAME_TYPE_AS(wtp,ap2) &
      .or. SAME_TYPE_AS(wtp,nwtp2) .or. SAME_TYPE_AS(wtp,travp2) .or. SAME_TYPE_AS(wtp,trp2) .or. SAME_TYPE_AS(wtp,c1p2)) stop 29

  ap => c2t
  if (.not.SAME_TYPE_AS(ap,c2t) .or. .not.SAME_TYPE_AS(ap,c2p2)) stop 30
  if (SAME_TYPE_AS(ap,ultdp2) .or. SAME_TYPE_AS(ap,motp2) .or. SAME_TYPE_AS(ap,wtp2) .or. SAME_TYPE_AS(ap,ap2) &
      .or. SAME_TYPE_AS(ap,nwtp2) .or. SAME_TYPE_AS(ap,travp2) .or. SAME_TYPE_AS(ap,trp2) .or. SAME_TYPE_AS(ap,c1p2)) stop 31

  c2p => c2t
  if (.not.SAME_TYPE_AS(c2p,c2t) .or. .not.SAME_TYPE_AS(c2p,c2p2)) stop 32
  if (SAME_TYPE_AS(c2p,ultdp2) .or. SAME_TYPE_AS(c2p,motp2) .or. SAME_TYPE_AS(c2p,wtp2) .or. SAME_TYPE_AS(c2p,ap2) &
      .or. SAME_TYPE_AS(c2p,nwtp2) .or. SAME_TYPE_AS(c2p,travp2) .or. SAME_TYPE_AS(c2p,trp2) .or. SAME_TYPE_AS(c2p,c1p2)) stop 33


  ultdp => trt
  if (.not.SAME_TYPE_AS(ultdp,trt) .or. .not.SAME_TYPE_AS(ultdp,trp2)) stop 34
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(ultdp,wtp2) .or. SAME_TYPE_AS(ultdp,c1p2) .or. SAME_TYPE_AS(ultdp,ap2) .or. SAME_TYPE_AS(ultdp,c2p2) &
      ) stop 35

  motp => trt
  if (.not.SAME_TYPE_AS(motp,trt) .or. .not.SAME_TYPE_AS(motp,trp2)) stop 36
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(motp,wtp2) .or. SAME_TYPE_AS(motp,c1p2) .or. SAME_TYPE_AS(motp,ap2) .or. SAME_TYPE_AS(motp,c2p2) &
      ) stop 37

  wtp => trt
  if (.not.SAME_TYPE_AS(wtp,trt) .or. .not.SAME_TYPE_AS(wtp,trp2)) stop 38
  if (SAME_TYPE_AS(wtp,ultdp2) .or. SAME_TYPE_AS(wtp,motp2) &
      .or. SAME_TYPE_AS(wtp,nwtp2) .or. SAME_TYPE_AS(wtp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(wtp,wtp2) .or. SAME_TYPE_AS(wtp,c1p2) .or. SAME_TYPE_AS(wtp,ap2) .or. SAME_TYPE_AS(wtp,c2p2) &
      ) stop 39

  ap => trt
  if (.not.SAME_TYPE_AS(ap,trt) .or. .not.SAME_TYPE_AS(ap,trp2)) stop 40
  if (SAME_TYPE_AS(ap,ultdp2) .or. SAME_TYPE_AS(ap,motp2) &
      .or. SAME_TYPE_AS(ap,nwtp2) .or. SAME_TYPE_AS(ap,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(ap,wtp2) .or. SAME_TYPE_AS(ap,c1p2) .or. SAME_TYPE_AS(ap,ap2) .or. SAME_TYPE_AS(ap,c2p2) &
      ) stop 41

  trp => trt
  if (.not.SAME_TYPE_AS(trp,trt) .or. .not.SAME_TYPE_AS(trp,trp2)) stop 42
  if (SAME_TYPE_AS(trp,ultdp2) .or. SAME_TYPE_AS(trp,motp2) &
      .or. SAME_TYPE_AS(trp,nwtp2) .or. SAME_TYPE_AS(trp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(trp,wtp2) .or. SAME_TYPE_AS(trp,c1p2) .or. SAME_TYPE_AS(trp,ap2) .or. SAME_TYPE_AS(trp,c2p2) &
      ) stop 43



  ultdp => travmt
  if (.not.SAME_TYPE_AS(ultdp,travt) .or. .not.SAME_TYPE_AS(ultdp,travp2)) stop 44
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtmp2) .or. SAME_TYPE_AS(ultdp,c2mp2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,c1mp2) .or. SAME_TYPE_AS(ultdp,trmp2) .or. SAME_TYPE_AS(ultdp,amp2)) stop 45

  motp => travmt
  if (.not.SAME_TYPE_AS(motp,travt) .or. .not.SAME_TYPE_AS(motp,travp2)) stop 46
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtmp2) .or. SAME_TYPE_AS(motp,c2mp2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,c1mp2) .or. SAME_TYPE_AS(motp,trmp2) .or. SAME_TYPE_AS(motp,amp2)) stop 47

  nwtp => travmt
  if (.not.SAME_TYPE_AS(nwtp,travt) .or. .not.SAME_TYPE_AS(nwtp,travp2)) stop 48
  if (SAME_TYPE_AS(nwtp,ultdp2) .or. SAME_TYPE_AS(nwtp,motp2) .or. SAME_TYPE_AS(nwtp,wtmp2) .or. SAME_TYPE_AS(nwtp,c2mp2) &
      .or. SAME_TYPE_AS(nwtp,nwtp2) .or. SAME_TYPE_AS(nwtp,c1mp2) .or. SAME_TYPE_AS(nwtp,trmp2) .or. SAME_TYPE_AS(nwtp,amp2)) stop 49

  travp => travmt
  if (.not.SAME_TYPE_AS(travp,travt) .or. .not.SAME_TYPE_AS(travp,travp2)) stop 50
  if (SAME_TYPE_AS(travp,ultdp2) .or. SAME_TYPE_AS(travp,motp2) .or. SAME_TYPE_AS(travp,wtmp2) .or. SAME_TYPE_AS(travp,c2mp2) &
      .or. SAME_TYPE_AS(travp,nwtp2) .or. SAME_TYPE_AS(travp,c1mp2) .or. SAME_TYPE_AS(travp,trmp2) .or. SAME_TYPE_AS(travp,amp2)) stop 51


  ultdp => c1mt
  if (.not.SAME_TYPE_AS(ultdp,c1mt) .or. .not.SAME_TYPE_AS(ultdp,c1mp2)) stop 52
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtmp2) .or. SAME_TYPE_AS(ultdp,c2mp2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2) .or. SAME_TYPE_AS(ultdp,trmp2) .or. SAME_TYPE_AS(ultdp,amp2)) stop 53

  motp => c1mt
  if (.not.SAME_TYPE_AS(motp,c1mt) .or. .not.SAME_TYPE_AS(motp,c1mp2)) stop 54
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtmp2) .or. SAME_TYPE_AS(motp,c2mp2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2) .or. SAME_TYPE_AS(motp,trmp2) .or. SAME_TYPE_AS(motp,amp2)) stop 55

  wtmp => c1mt
  if (.not.SAME_TYPE_AS(wtmp,c1mt) .or. .not.SAME_TYPE_AS(wtmp,c1mp2)) stop 56
  if (SAME_TYPE_AS(wtmp,ultdp2) .or. SAME_TYPE_AS(wtmp,motp2) .or. SAME_TYPE_AS(wtmp,wtmp2) .or. SAME_TYPE_AS(wtmp,c2mp2) &
      .or. SAME_TYPE_AS(wtmp,nwtp2) .or. SAME_TYPE_AS(wtmp,travp2) .or. SAME_TYPE_AS(wtmp,trmp2) .or. SAME_TYPE_AS(wtmp,amp2)) stop 57


  ultdp => amt
  if (.not.SAME_TYPE_AS(ultdp,amt) .or. .not.SAME_TYPE_AS(ultdp,amp2)) stop 58
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtmp2) .or. SAME_TYPE_AS(ultdp,c2mp2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2) .or. SAME_TYPE_AS(ultdp,trmp2) .or. SAME_TYPE_AS(ultdp,c1mp2)) stop 59

  motp => amt
  if (.not.SAME_TYPE_AS(motp,amt) .or. .not.SAME_TYPE_AS(motp,amp2)) stop 60
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtmp2) .or. SAME_TYPE_AS(motp,c2mp2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2) .or. SAME_TYPE_AS(motp,trmp2) .or. SAME_TYPE_AS(motp,c1mp2)) stop 61

  wtmp => amt
  if (.not.SAME_TYPE_AS(wtmp,amt) .or. .not.SAME_TYPE_AS(wtmp,amp2)) stop 62
  if (SAME_TYPE_AS(wtmp,ultdp2) .or. SAME_TYPE_AS(wtmp,motp2) .or. SAME_TYPE_AS(wtmp,wtmp2) .or. SAME_TYPE_AS(wtmp,c2mp2) &
      .or. SAME_TYPE_AS(wtmp,nwtp2) .or. SAME_TYPE_AS(wtmp,travp2) .or. SAME_TYPE_AS(wtmp,trmp2) .or. SAME_TYPE_AS(wtmp,c1mp2)) stop 63

  amp => amt
  if (.not.SAME_TYPE_AS(amp,amt) .or. .not.SAME_TYPE_AS(amp,amp2)) stop 64
  if (SAME_TYPE_AS(amp,ultdp2) .or. SAME_TYPE_AS(amp,motp2) .or. SAME_TYPE_AS(amp,wtmp2) .or. SAME_TYPE_AS(amp,c2mp2) &
      .or. SAME_TYPE_AS(amp,nwtp2) .or. SAME_TYPE_AS(amp,travp2) .or. SAME_TYPE_AS(amp,trmp2) .or. SAME_TYPE_AS(amp,c1mp2)) stop 65


  ultdp => c2mt
  if (.not.SAME_TYPE_AS(ultdp,c2mt) .or. .not.SAME_TYPE_AS(ultdp,c2mp2)) stop 66
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) .or. SAME_TYPE_AS(ultdp,wtmp2) .or. SAME_TYPE_AS(ultdp,amp2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2) .or. SAME_TYPE_AS(ultdp,trmp2) .or. SAME_TYPE_AS(ultdp,c1mp2)) stop 67

  motp => c2mt
  if (.not.SAME_TYPE_AS(motp,c2mt) .or. .not.SAME_TYPE_AS(motp,c2mp2)) stop 68
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) .or. SAME_TYPE_AS(motp,wtmp2) .or. SAME_TYPE_AS(motp,amp2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2) .or. SAME_TYPE_AS(motp,trmp2) .or. SAME_TYPE_AS(motp,c1mp2)) stop 69

  wtmp => c2mt
  if (.not.SAME_TYPE_AS(wtmp,c2mt) .or. .not.SAME_TYPE_AS(wtmp,c2mp2)) stop 70
  if (SAME_TYPE_AS(wtmp,ultdp2) .or. SAME_TYPE_AS(wtmp,motp2) .or. SAME_TYPE_AS(wtmp,wtmp2) .or. SAME_TYPE_AS(wtmp,amp2) &
      .or. SAME_TYPE_AS(wtmp,nwtp2) .or. SAME_TYPE_AS(wtmp,travp2) .or. SAME_TYPE_AS(wtmp,trmp2) .or. SAME_TYPE_AS(wtmp,c1mp2)) stop 71

  amp => c2mt
  if (.not.SAME_TYPE_AS(amp,c2mt) .or. .not.SAME_TYPE_AS(amp,c2mp2)) stop 72
  if (SAME_TYPE_AS(amp,ultdp2) .or. SAME_TYPE_AS(amp,motp2) .or. SAME_TYPE_AS(amp,wtmp2) .or. SAME_TYPE_AS(amp,amp2) &
      .or. SAME_TYPE_AS(amp,nwtp2) .or. SAME_TYPE_AS(amp,travp2) .or. SAME_TYPE_AS(amp,trmp2) .or. SAME_TYPE_AS(amp,c1mp2)) stop 73

  c2mp => c2mt
  if (.not.SAME_TYPE_AS(c2mp,c2mt) .or. .not.SAME_TYPE_AS(c2mp,c2mp2)) stop 74
  if (SAME_TYPE_AS(c2mp,ultdp2) .or. SAME_TYPE_AS(c2mp,motp2) .or. SAME_TYPE_AS(c2mp,wtmp2) .or. SAME_TYPE_AS(c2mp,amp2) &
      .or. SAME_TYPE_AS(c2mp,nwtp2) .or. SAME_TYPE_AS(c2mp,travp2) .or. SAME_TYPE_AS(c2mp,trmp2) .or. SAME_TYPE_AS(c2mp,c1mp2)) stop 75


  ultdp => trmt
  if (.not.SAME_TYPE_AS(ultdp,trmt) .or. .not.SAME_TYPE_AS(ultdp,trmp2)) stop 76
  if (SAME_TYPE_AS(ultdp,ultdp2) .or. SAME_TYPE_AS(ultdp,motp2) &
      .or. SAME_TYPE_AS(ultdp,nwtp2) .or. SAME_TYPE_AS(ultdp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(ultdp,wtmp2) .or. SAME_TYPE_AS(ultdp,c1mp2) .or. SAME_TYPE_AS(ultdp,amp2) .or. SAME_TYPE_AS(ultdp,c2mp2) &
      ) stop 77

  motp => trmt
  if (.not.SAME_TYPE_AS(motp,trmt) .or. .not.SAME_TYPE_AS(motp,trmp2)) stop 78
  if (SAME_TYPE_AS(motp,ultdp2) .or. SAME_TYPE_AS(motp,motp2) &
      .or. SAME_TYPE_AS(motp,nwtp2) .or. SAME_TYPE_AS(motp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(motp,wtmp2) .or. SAME_TYPE_AS(motp,c1mp2) .or. SAME_TYPE_AS(motp,amp2) .or. SAME_TYPE_AS(motp,c2mp2) &
      ) stop 79

  wtmp => trmt
  if (.not.SAME_TYPE_AS(wtmp,trmt) .or. .not.SAME_TYPE_AS(wtmp,trmp2)) stop 80
  if (SAME_TYPE_AS(wtmp,ultdp2) .or. SAME_TYPE_AS(wtmp,motp2) &
      .or. SAME_TYPE_AS(wtmp,nwtp2) .or. SAME_TYPE_AS(wtmp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(wtmp,wtmp2) .or. SAME_TYPE_AS(wtmp,c1mp2) .or. SAME_TYPE_AS(wtmp,amp2) .or. SAME_TYPE_AS(wtmp,c2mp2) &
      ) stop 81

  amp => trmt
  if (.not.SAME_TYPE_AS(amp,trmt) .or. .not.SAME_TYPE_AS(amp,trmp2)) stop 82
  if (SAME_TYPE_AS(amp,ultdp2) .or. SAME_TYPE_AS(amp,motp2) &
      .or. SAME_TYPE_AS(amp,nwtp2) .or. SAME_TYPE_AS(amp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(amp,wtmp2) .or. SAME_TYPE_AS(amp,c1mp2) .or. SAME_TYPE_AS(amp,amp2) .or. SAME_TYPE_AS(amp,c2mp2) &
      ) stop 83

  trmp => trmt
  if (.not.SAME_TYPE_AS(trmp,trmt) .or. .not.SAME_TYPE_AS(trmp,trmp2)) stop 84
  if (SAME_TYPE_AS(trmp,ultdp2) .or. SAME_TYPE_AS(trmp,motp2) &
      .or. SAME_TYPE_AS(trmp,nwtp2) .or. SAME_TYPE_AS(trmp,travp2)& ! should be impossible, anyway
      .or. SAME_TYPE_AS(trmp,wtmp2) .or. SAME_TYPE_AS(trmp,c1mp2) .or. SAME_TYPE_AS(trmp,amp2) .or. SAME_TYPE_AS(trmp,c2mp2) &
      ) stop 85

end program dtpPtrAssignPolyType
