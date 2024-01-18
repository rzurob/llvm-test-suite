!*  ==================================================================
!*
!*  DATE                       : June 2, 2015
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Multiple allocate polymorphic types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
  implicit none

  class(*), pointer :: u
  class(*), pointer :: u2(:,:)

  integer(kind=4)   :: s

  type zero
    integer :: i
  end type

  type, extends(zero) :: one
    real(kind=4) :: r4
    real(kind=8) :: r8
  end type

  type, extends(one) :: two
    character(len=1)  :: c1
    character(len=16) :: c16
  end type

  type, extends(two) :: three
    complex(kind=4)  :: x4
    complex(kind=16) :: x16
  end type

  type, extends(three) :: four
    logical :: l
  end type

  type five
    integer               :: i = -77
    real                  :: r = -98327493.432
    character             :: c = "&"
    complex               :: x = (-32.0,16.0)
    logical               :: l = .FALSE.
    class(zero), pointer  :: o => NULL()
  end type

  type(zero),   pointer             :: z1, z2
  class(zero),  allocatable, target :: oz, az2o(:,:)

  type(one),    pointer             :: o1, o2
  class(one),   allocatable, target :: oo

  type(two),    pointer             :: t1, t2
  class(two),   allocatable, target :: ot, at2o(:,:)

  type(three),  pointer             :: h1, h2, ah2a(:,:), ah2b(:,:)
  class(three), allocatable, target :: oh    , ah2o(:,:)

  type(four),  pointer              :: f1, f2
  class(four), allocatable, target  :: of

  type(five),  pointer              :: c1, c2, ac1a(:), ac1b(:), ac3a(:,:,:), ac3b(:,:,:)
  class(five), allocatable, target  :: oc    , ac1o(:)         , ac3o(:,:,:)

  !Zero
  allocate(z1, z2, oz, u, source=zero(-5), stat=s)
  if (s    /= 0)  error stop(10)
  if (z1%i /= -5) error stop(11)
  if (z2%i /= -5) error stop(12)

  select type (oz)
  type is (zero)
    if (oz%i /= -5) error stop(910)
  class default
    error stop (601)
  end select
  select type (u)
  type is (zero)
    if (u%i  /= -5) error stop(911)
  class default
    error stop (602)
  end select
  deallocate(oz, u)

  !One
  allocate(o1, o2, oo, oz, u, source=one(i=3, r4=1.1_4, r8=-2.9_8), stat=s)
  if (s     /= 0)        error stop(20)
  if (o1%i  /= 3)        error stop(21)
  if (o1%r4 /= 1.1_4)    error stop(22)
  if (o1%r8 /= -2.9_8)   error stop(23)
  if (o2%i  /= 3)        error stop(24)
  if (o2%r4 /= 1.1_4)    error stop(25)
  if (o2%r8 /= -2.9_8)   error stop(26)

  select type (oo)
  type is (one)
    if (oo%i  /= 3)      error stop(920)
    if (oo%r4 /= 1.1_4)  error stop(921)
    if (oo%r8 /= -2.9_8) error stop(922)
  class default
    error stop (603)
  end select
  select type (oz)
  type is (one)
    if (oz%i  /= 3)      error stop(923)
    if (oz%r4 /= 1.1_4)  error stop(924)
    if (oz%r8 /= -2.9_8) error stop(925)
  class default
    error stop (604)
  end select
  select type (u)
  type is (one)
    if (u%i  /= 3)       error stop(926)
    if (u%r4 /= 1.1_4)   error stop(927)
    if (u%r8 /= -2.9_8)  error stop(928)
  class default
    error stop (605)
  end select

  deallocate(oo, oz, u)

  !Two
  allocate(t1, t2, ot, oo, oz, u,               &
         & source=two(i=0, r4=-1.1_4, r8=2.9_8, &
         & c1="", c16="Fortran Test"), stat=s)
  if (s      /= 0)                error stop(30)
  if (t1%i   /= 0)                error stop(31)
  if (t1%r4  /= -1.1_4)           error stop(32)
  if (t1%r8  /= 2.9_8)            error stop(33)
  if (t1%c1  /= "")               error stop(34)
  if (t1%c16 /= "Fortran Test")   error stop(35)
  if (t2%i   /= 0)                error stop(36)
  if (t2%r4  /= -1.1_4)           error stop(37)
  if (t2%r8  /= 2.9_8)            error stop(38)
  if (t2%c1  /= "")               error stop(39)
  if (t2%c16 /= "Fortran Test")   error stop(40)

  select type (ot)
  type is (two)
    if (ot%i   /= 0)              error stop(930)
    if (ot%r4  /= -1.1_4)         error stop(931)
    if (ot%r8  /= 2.9_8)          error stop(932)
    if (ot%c1  /= "")             error stop(933)
    if (ot%c16 /= "Fortran Test") error stop(934)
  class default
    error stop (606)
  end select
  select type (oo)
  type is (two)
    if (oo%i   /= 0)              error stop(935)
    if (oo%r4  /= -1.1_4)         error stop(936)
    if (oo%r8  /= 2.9_8)          error stop(937)
    if (oo%c1  /= "")             error stop(938)
    if (oo%c16 /= "Fortran Test") error stop(939)
  class default
    error stop (607)
  end select
  select type (oz)
  type is (two)
    if (oz%i   /= 0)              error stop(940)
    if (oz%r4  /= -1.1_4)         error stop(941)
    if (oz%r8  /= 2.9_8)          error stop(942)
    if (oz%c1  /= "")             error stop(943)
    if (oz%c16 /= "Fortran Test") error stop(944)
  class default
    error stop (608)
  end select
  select type (u)
  type is (two)
    if (u%i   /= 0)               error stop(945)
    if (u%r4  /= -1.1_4)          error stop(946)
    if (u%r8  /= 2.9_8)           error stop(947)
    if (u%c1  /= "")              error stop(948)
    if (u%c16 /= "Fortran Test")  error stop(949)
  class default
    error stop (609)
  end select
  deallocate(ot, oo, oz, u)

  !Three
  allocate(h1, h2, oh, ot, oo, oz, u,                                &
         & source=three(i=8, r4=-81.1_4, r8=28.89_8, c1="T", c16="", &
         & x4=(-9.1_4,4.0_4), x16=(0.0_16,-1.2_16)), stat=s)
  if (s      /= 0)                  error stop(50)
  if (h1%i   /= 8)                  error stop(51)
  if (h1%r4  /= -81.1_4)            error stop(52)
  if (h1%r8  /= 28.89_8)            error stop(53)
  if (h1%c1  /= "T")                error stop(54)
  if (h1%c16 /= "")                 error stop(55)
  if (h1%x4  /= (-9.1_4,4.0_4))     error stop(56)
  if (h1%x16 /= (0.0_16,-1.2_16))   error stop(57)
  if (h2%i   /= 8)                  error stop(58)
  if (h2%r4  /= -81.1_4)            error stop(59)
  if (h2%r8  /= 28.89_8)            error stop(60)
  if (h2%c1  /= "T")                error stop(61)
  if (h2%c16 /= "")                 error stop(62)
  if (h2%x4  /= (-9.1_4,4.0_4))     error stop(63)
  if (h2%x16 /= (0.0_16,-1.2_16))   error stop(64)
  select type (oh)
  type is (three)
    if (oh%i   /= 8)                error stop(950)
    if (oh%r4  /= -81.1_4)          error stop(951)
    if (oh%r8  /= 28.89_8)          error stop(952)
    if (oh%c1  /= "T")              error stop(953)
    if (oh%c16 /= "")               error stop(954)
    if (oh%x4  /= (-9.1_4,4.0_4))   error stop(955)
    if (oh%x16 /= (0.0_16,-1.2_16)) error stop(956)
  class default
    error stop (610)
  end select
  select type (ot)
  type is (three)
    if (ot%i   /= 8)                error stop(957)
    if (ot%r4  /= -81.1_4)          error stop(958)
    if (ot%r8  /= 28.89_8)          error stop(959)
    if (ot%c1  /= "T")              error stop(960)
    if (ot%c16 /= "")               error stop(961)
    if (ot%x4  /= (-9.1_4,4.0_4))   error stop(962)
    if (ot%x16 /= (0.0_16,-1.2_16)) error stop(963)
  class default
    error stop (611)
  end select
  select type (oo)
  type is (three)
    if (oo%i   /= 8)                error stop(964)
    if (oo%r4  /= -81.1_4)          error stop(965)
    if (oo%r8  /= 28.89_8)          error stop(966)
    if (oo%c1  /= "T")              error stop(967)
    if (oo%c16 /= "")               error stop(968)
    if (oo%x4  /= (-9.1_4,4.0_4))   error stop(969)
    if (oo%x16 /= (0.0_16,-1.2_16)) error stop(970)
  class default
    error stop (612)
  end select
  select type (oz)
  type is (three)
    if (oz%i   /= 8)                error stop(971)
    if (oz%r4  /= -81.1_4)          error stop(972)
    if (oz%r8  /= 28.89_8)          error stop(973)
    if (oz%c1  /= "T")              error stop(974)
    if (oz%c16 /= "")               error stop(975)
    if (oz%x4  /= (-9.1_4,4.0_4))   error stop(976)
    if (oz%x16 /= (0.0_16,-1.2_16)) error stop(977)
  class default
    error stop (613)
  end select
  select type (u)
  type is (three)
    if (u%i   /= 8)                 error stop(978)
    if (u%r4  /= -81.1_4)           error stop(979)
    if (u%r8  /= 28.89_8)           error stop(980)
    if (u%c1  /= "T")               error stop(981)
    if (u%c16 /= "")                error stop(982)
    if (u%x4  /= (-9.1_4,4.0_4))    error stop(983)
    if (u%x16 /= (0.0_16,-1.2_16))  error stop(984)
  class default
    error stop (614)
  end select
  deallocate(oh, ot, oo, oz, u)

  !Four
  allocate(f1, f2, of, oh, ot, oo, oz, u,             &
         & source=four(i=-8, r4=081.1_4, r8=+28.89_8, &
         & c1="$$", c16="Test Fortran$$",             &
         & x4=(1.2_4,0.0_4), x16=(-4.0_16,-9.1_16),   &
         & l=.FALSE.), stat=s)
  if (s      /= 0)                   error stop(80)
  if (f1%i   /= -8)                  error stop(81)
  if (f1%r4  /= 81.1_4)              error stop(82)
  if (f1%r8  /= 28.89_8)             error stop(83)
  if (f1%c1  /= "$")                 error stop(84)
  if (f1%c16 /= "Test Fortran$$")    error stop(85)
  if (f1%x4  /= (1.2_4,0.0_4))       error stop(86)
  if (f1%x16 /= (-4.0_16,-9.1_16))   error stop(87)
  if (f1%l   .NEQV. .FALSE.)         error stop(88)
  if (f2%i   /= -8)                  error stop(89)
  if (f2%r4  /= 81.1_4)              error stop(90)
  if (f2%r8  /= 28.89_8)             error stop(91)
  if (f2%c1  /= "$")                 error stop(92)
  if (f2%c16 /= "Test Fortran$$")    error stop(93)
  if (f2%x4  /= (1.2_4,0.0_4))       error stop(94)
  if (f2%x16 /= (-4.0_16,-9.1_16))   error stop(95)
  if (f2%l   .NEQV. .FALSE.)         error stop(96)

  select type (of)
  type is (four)
    if (of%i   /= -8)                error stop(97)
    if (of%r4  /= 81.1_4)            error stop(98)
    if (of%r8  /= 28.89_8)           error stop(99)
    if (of%c1  /= "$")               error stop(100)
    if (of%c16 /= "Test Fortran$$")  error stop(101)
    if (of%x4  /= (1.2_4,0.0_4))     error stop(102)
    if (of%x16 /= (-4.0_16,-9.1_16)) error stop(103)
    if (of%l   .NEQV. .FALSE.)       error stop(104)
  class default
    error stop (615)
  end select
  select type (oh)
  type is (four)
    if (oh%i   /= -8)                error stop(97)
    if (oh%r4  /= 81.1_4)            error stop(98)
    if (oh%r8  /= 28.89_8)           error stop(99)
    if (oh%c1  /= "$")               error stop(100)
    if (oh%c16 /= "Test Fortran$$")  error stop(101)
    if (oh%x4  /= (1.2_4,0.0_4))     error stop(102)
    if (oh%x16 /= (-4.0_16,-9.1_16)) error stop(103)
    if (oh%l   .NEQV. .FALSE.)       error stop(104)
  class default
    error stop (616)
  end select
  select type (ot)
  type is (four)
    if (ot%i   /= -8)                error stop(97)
    if (ot%r4  /= 81.1_4)            error stop(98)
    if (ot%r8  /= 28.89_8)           error stop(99)
    if (ot%c1  /= "$")               error stop(100)
    if (ot%c16 /= "Test Fortran$$")  error stop(101)
    if (ot%x4  /= (1.2_4,0.0_4))     error stop(102)
    if (ot%x16 /= (-4.0_16,-9.1_16)) error stop(103)
    if (ot%l   .NEQV. .FALSE.)       error stop(104)
  class default
    error stop (617)
  end select
  select type (oo)
  type is (four)
    if (oo%i   /= -8)                error stop(97)
    if (oo%r4  /= 81.1_4)            error stop(98)
    if (oo%r8  /= 28.89_8)           error stop(99)
    if (oo%c1  /= "$")               error stop(100)
    if (oo%c16 /= "Test Fortran$$")  error stop(101)
    if (oo%x4  /= (1.2_4,0.0_4))     error stop(102)
    if (oo%x16 /= (-4.0_16,-9.1_16)) error stop(103)
    if (oo%l   .NEQV. .FALSE.)       error stop(104)
  class default
    error stop (618)
  end select
  select type (oz)
  type is (four)
    if (oz%i   /= -8)                error stop(97)
    if (oz%r4  /= 81.1_4)            error stop(98)
    if (oz%r8  /= 28.89_8)           error stop(99)
    if (oz%c1  /= "$")               error stop(100)
    if (oz%c16 /= "Test Fortran$$")  error stop(101)
    if (oz%x4  /= (1.2_4,0.0_4))     error stop(102)
    if (oz%x16 /= (-4.0_16,-9.1_16)) error stop(103)
    if (oz%l   .NEQV. .FALSE.)       error stop(104)
  class default
    error stop (619)
  end select
  select type (u)
  type is (four)
    if (u%i   /= -8)                 error stop(97)
    if (u%r4  /= 81.1_4)             error stop(98)
    if (u%r8  /= 28.89_8)            error stop(99)
    if (u%c1  /= "$")                error stop(100)
    if (u%c16 /= "Test Fortran$$")   error stop(101)
    if (u%x4  /= (1.2_4,0.0_4))      error stop(102)
    if (u%x16 /= (-4.0_16,-9.1_16))  error stop(103)
    if (u%l   .NEQV. .FALSE.)        error stop(104)
  class default
    error stop (620)
  end select
!  deallocate(oh, ot, oo, oz, u)

  !Five
  allocate(c1, c2, oc, source=five(i=0, r=-2.2, c="C", x=(0,0), &
                                 & l=.TRUE., o=of), stat=s)

  if (s        /= 0)                 error stop(110)
  if (c1%i     /= 0)                 error stop(111)
  if (c1%r     /= -2.2)              error stop(112)
  if (c1%c     /= "C")               error stop(113)
  if (c1%x     /= (0,0))             error stop(114)
  if (c1%l     .NEQV. .TRUE.)        error stop(115)
  select type (o => c1%o)
  type is (four)
    if (o%i   /= -8)                 error stop(116)
    if (o%r4  /= 81.1_4)             error stop(117)
    if (o%r8  /= 28.89_8)            error stop(118)
    if (o%c1  /= "$")                error stop(119)
    if (o%c16 /= "Test Fortran$$")   error stop(120)
    if (o%x4  /= (1.2_4,0.0_4))      error stop(121)
    if (o%x16 /= (-4.0_16,-9.1_16))  error stop(122)
    if (o%l   .NEQV. .FALSE.)        error stop(123)
  class default
    error stop (621)
  end select
  if (c2%i     /= 0)                 error stop(124)
  if (c2%r     /= -2.2)              error stop(125)
  if (c2%c     /= "C")               error stop(126)
  if (c2%x     /= (0,0))             error stop(127)
  if (c2%l     .NEQV. .TRUE.)        error stop(128)
  select type (o => c2%o)
  type is (four)
    if (o%i   /= -8)                error stop(129)
    if (o%r4  /= 81.1_4)            error stop(130)
    if (o%r8  /= 28.89_8)           error stop(131)
    if (o%c1  /= "$")               error stop(132)
    if (o%c16 /= "Test Fortran$$")  error stop(133)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(134)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(135)
    if (o%l   .NEQV. .FALSE.)       error stop(136)
  class default
    error stop (622)
  end select
  if (oc%i     /= 0)                 error stop(137)
  if (oc%r     /= -2.2)              error stop(138)
  if (oc%c     /= "C")               error stop(139)
  if (oc%x     /= (0,0))             error stop(140)
  if (oc%l     .NEQV. .TRUE.)        error stop(141)
  select type (o => oc%o)
  type is (four)
    if (o%i   /= -8)                error stop(142)
    if (o%r4  /= 81.1_4)            error stop(143)
    if (o%r8  /= 28.89_8)           error stop(144)
    if (o%c1  /= "$")               error stop(145)
    if (o%c16 /= "Test Fortran$$")  error stop(146)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(147)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(148)
    if (o%l   .NEQV. .FALSE.)       error stop(149)
  class default
    error stop (623)
  end select

  deallocate(z1, z2, o1, o2, t1, t2, h1, h2, f1, f2, c1, c2)

  !Mold
  allocate(z1, z2, mold=oz, stat=s)
  if (s /= 0)  error stop(201)

  allocate(o1, o2, mold=oo, stat=s)
  if (s /= 0)  error stop(202)
  if (kind(o1%r4) /= 4) error stop(211)
  if (kind(o2%r8) /= 8) error stop(212)

  allocate(t1, t2, mold=ot, stat=s)
  if (s /= 0)  error stop(203)
  if (kind(t1%r4)  /= 4)  error stop(213)
  if (kind(t2%r8)  /= 8)  error stop(214)
  if (len(t1%c1)   /= 1)  error stop(215)
  if (len(t2%c16)  /= 16) error stop(216)

  allocate(h1, h2, mold=oh, stat=s)
  if (s /= 0)  error stop(204)
  if (kind(h2%r4)  /= 4)  error stop(217)
  if (kind(h1%r8)  /= 8)  error stop(218)
  if (len(h2%c1)   /= 1)  error stop(219)
  if (len(h1%c16)  /= 16) error stop(220)
  if (kind(h2%x4)  /= 4)  error stop(221)
  if (kind(h2%x16) /= 16) error stop(222)

  allocate(f1, f2, mold=of, stat=s)
  if (s /= 0)  error stop(205)
  if (kind(f2%r4)  /= 4)  error stop(223)
  if (kind(f1%r8)  /= 8)  error stop(224)
  if (len(f2%c1)   /= 1)  error stop(225)
  if (len(f1%c16)  /= 16) error stop(226)
  if (kind(f2%x4)  /= 4)  error stop(227)

  allocate(c1, c2, mold=oc, stat=s)
  if (s /= 0)  error stop(206)
  if (c2%i               /=   -77)           error stop(227)
  if (c1%r               /=   -98327493.432) error stop(228)
  if (c2%c               /=   "&")           error stop(229)
  if (c1%x               /=   (-32.0,16.0))  error stop(230)
  if (c2%l             .NEQV. .FALSE.)       error stop(231)
  if (associated(c1%o) .NEQV. .FALSE.)       error stop(232)

  !2D Arrays of "three"
  allocate(ah2o(3,4))
  ah2o = oh
  allocate(ah2a, ah2b, az2o, at2o, source=ah2o)
  if (ah2a(1,1)%i   /= -8)                error stop(351)
  if (ah2a(1,1)%r4  /= 81.1_4)            error stop(352)
  if (ah2a(1,1)%r8  /= 28.89_8)           error stop(353)
  if (ah2a(1,1)%c1  /= "$")               error stop(354)
  if (ah2a(1,1)%c16 /= "Test Fortran$$")  error stop(355)
  if (ah2a(1,1)%x4  /= (1.2_4,0.0_4))     error stop(356)
  if (ah2a(1,1)%x16 /= (-4.0_16,-9.1_16)) error stop(357)
  if (ah2b(2,3)%i   /= -8)                error stop(358)
  if (ah2b(2,3)%r4  /= 81.1_4)            error stop(359)
  if (ah2b(2,3)%r8  /= 28.89_8)           error stop(360)
  if (ah2b(2,3)%c1  /= "$")               error stop(361)
  if (ah2b(2,3)%c16 /= "Test Fortran$$")  error stop(362)
  if (ah2b(2,3)%x4  /= (1.2_4,0.0_4))     error stop(363)
  if (ah2b(2,3)%x16 /= (-4.0_16,-9.1_16)) error stop(364)
  if (ah2o(3,2)%i   /= -8)                error stop(365)
  if (ah2o(3,2)%r4  /= 81.1_4)            error stop(366)
  if (ah2o(3,2)%r8  /= 28.89_8)           error stop(367)
  if (ah2o(3,2)%c1  /= "$")               error stop(368)
  if (ah2o(3,2)%c16 /= "Test Fortran$$")  error stop(369)
  if (ah2o(3,2)%x4  /= (1.2_4,0.0_4))     error stop(370)
  if (ah2o(3,2)%x16 /= (-4.0_16,-9.1_16)) error stop(371)
  select type (o => az2o(3,4))
  type is (four)
    if (o%i   /= -8)                error stop(372)
    if (o%r4  /= 81.1_4)            error stop(373)
    if (o%r8  /= 28.89_8)           error stop(374)
    if (o%c1  /= "$")               error stop(375)
    if (o%c16 /= "Test Fortran$$")  error stop(376)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(377)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(378)
  class default
    error stop (630)
  end select
  select type (o => at2o(2,1))
  type is (four)
    if (o%i   /= -8)                error stop(379)
    if (o%r4  /= 81.1_4)            error stop(380)
    if (o%r8  /= 28.89_8)           error stop(381)
    if (o%c1  /= "$")               error stop(382)
    if (o%c16 /= "Test Fortran$$")  error stop(383)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(384)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(385)
  class default
    error stop (631)
  end select

  !1D Arrays of "Five"
  allocate(ac1o(5))
  ac1o = oc
  allocate(ac1a, ac1b, source=ac1o(2:4))

  if (ac1a(2)%i /= 0)                 error stop(511)
  if (ac1a(2)%r /= -2.2)              error stop(71)
  if (ac1a(2)%c /= "C")               error stop(513)
  if (ac1a(2)%x /= (0,0))             error stop(514)
  if (ac1a(2)%l .NEQV. .TRUE.)        error stop(515)
  select type (o => ac1a(2)%o)
  type is (four)
    if (o%i   /= -8)                  error stop(516)
    if (o%r4  /= 81.1_4)              error stop(517)
    if (o%r8  /= 28.89_8)             error stop(518)
    if (o%c1  /= "$")                 error stop(519)
    if (o%c16 /= "Test Fortran$$")    error stop(520)
    if (o%x4  /= (1.2_4,0.0_4))       error stop(521)
    if (o%x16 /= (-4.0_16,-9.1_16))   error stop(522)
    if (o%l   .NEQV. .FALSE.)         error stop(523)
  class default
    error stop (624)
  end select
  if (ac1b(3)%i /= 0)                 error stop(524)
  if (ac1b(3)%r /= -2.2)              error stop(525)
  if (ac1b(3)%c /= "C")               error stop(526)
  if (ac1b(3)%x /= (0,0))             error stop(527)
  if (ac1b(3)%l .NEQV. .TRUE.)        error stop(528)
  select type (o => ac1b(3)%o)
  type is (four)
    if (o%i   /= -8)                  error stop(529)
    if (o%r4  /= 81.1_4)              error stop(530)
    if (o%r8  /= 28.89_8)             error stop(531)
    if (o%c1  /= "$")                 error stop(532)
    if (o%c16 /= "Test Fortran$$")    error stop(533)
    if (o%x4  /= (1.2_4,0.0_4))       error stop(534)
    if (o%x16 /= (-4.0_16,-9.1_16))   error stop(535)
    if (o%l   .NEQV. .FALSE.)         error stop(536)
  class default
    error stop (625)
  end select
  if (ac1o(5)%i /= 0)                 error stop(537)
  if (ac1o(5)%r /= -2.2)              error stop(538)
  if (ac1o(5)%c /= "C")               error stop(539)
  if (ac1o(5)%x /= (0,0))             error stop(540)
  if (ac1o(5)%l .NEQV. .TRUE.)        error stop(541)
  select type (o => ac1o(5)%o)
  type is (four)
    if (o%i   /= -8)                  error stop(542)
    if (o%r4  /= 81.1_4)              error stop(543)
    if (o%r8  /= 28.89_8)             error stop(544)
    if (o%c1  /= "$")                 error stop(545)
    if (o%c16 /= "Test Fortran$$")    error stop(546)
    if (o%x4  /= (1.2_4,0.0_4))       error stop(547)
    if (o%x16 /= (-4.0_16,-9.1_16))   error stop(548)
    if (o%l   .NEQV. .FALSE.)         error stop(549)
  class default
    error stop (626)
  end select

!3D Sub-Arrays of "Five"
  allocate(ac3o(5,2:2,2))
  ac3o = oc
  allocate(ac3a, ac3b, source=ac3o(2:4,2:2,1:1))

  if (ac3a(1,1,1)%i /= 0)           error stop(551)
  if (ac3a(1,1,1)%r /= -2.2)        error stop(552)
  if (ac3a(1,1,1)%c /= "C")         error stop(553)
  if (ac3a(1,1,1)%x /= (0,0))       error stop(554)
  if (ac3a(1,1,1)%l .NEQV. .TRUE.)  error stop(555)
  select type (o => ac3a(1,1,1)%o)
  type is (four)
    if (o%i   /= -8)                error stop(556)
    if (o%r4  /= 81.1_4)            error stop(557)
    if (o%r8  /= 28.89_8)           error stop(558)
    if (o%c1  /= "$")               error stop(559)
    if (o%c16 /= "Test Fortran$$")  error stop(560)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(561)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(562)
    if (o%l   .NEQV. .FALSE.)       error stop(563)
  class default
    error stop (627)
  end select
  if (ac3b(3,1,1)%i  /= 0)          error stop(564)
  if (ac3b(3,1,1)%r  /= -2.2)       error stop(565)
  if (ac3b(3,1,1)%c  /= "C")        error stop(566)
  if (ac3b(3,1,1)%x  /= (0,0))      error stop(567)
  if (ac3b(3,1,1)%l  .NEQV. .TRUE.) error stop(568)
  select type (o => ac3b(3,1,1)%o)
  type is (four)
    if (o%i   /= -8)                error stop(569)
    if (o%r4  /= 81.1_4)            error stop(570)
    if (o%r8  /= 28.89_8)           error stop(571)
    if (o%c1  /= "$")               error stop(572)
    if (o%c16 /= "Test Fortran$$")  error stop(573)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(574)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(575)
    if (o%l   .NEQV. .FALSE.)       error stop(576)
  class default
    error stop (628)
  end select
  if (ac3o(5,2,1)%i /= 0)           error stop(577)
  if (ac3o(5,2,1)%r /= -2.2)        error stop(578)
  if (ac3o(5,2,1)%c /= "C")         error stop(579)
  if (ac3o(5,2,1)%x /= (0,0))       error stop(580)
  if (ac3o(5,2,1)%l .NEQV. .TRUE.)  error stop(581)
  select type (o => ac3o(5,2,1)%o)
  type is (four)
    if (o%i   /= -8)                error stop(582)
    if (o%r4  /= 81.1_4)            error stop(583)
    if (o%r8  /= 28.89_8)           error stop(584)
    if (o%c1  /= "$")               error stop(585)
    if (o%c16 /= "Test Fortran$$")  error stop(586)
    if (o%x4  /= (1.2_4,0.0_4))     error stop(587)
    if (o%x16 /= (-4.0_16,-9.1_16)) error stop(588)
    if (o%l   .NEQV. .FALSE.)       error stop(589)
  class default
    error stop (629)
  end select

  !Mold for arrays and sub-array
  deallocate(ah2a, ah2b, at2o, az2o)

  allocate(ah2a, ah2b, at2o, az2o, u2, mold=ah2o)

  if (size(ah2a)      /=   12)     error stop(241)
  if (any(shape(ah2a) /= (/3,4/))) error stop(242)
  if (rank(ah2b)      /=    2)     error stop(243)
  if (lbound(ah2b,1)  /=    1)     error stop(244)
  if (lbound(ah2a,2)  /=    1)     error stop(245)
  if (ubound(ah2b,1)  /=    3)     error stop(246)
  if (ubound(ah2a,2)  /=    4)     error stop(247)
  select type (o => at2o)
  type is (four)
    if (size(o)      /=   12)     error stop(248)
    if (any(shape(o) /= (/3,4/))) error stop(249)
    if (rank(o)      /=    2)     error stop(251)
    if (lbound(o,1)  /=    1)     error stop(252)
    if (lbound(o,2)  /=    1)     error stop(253)
    if (ubound(o,1)  /=    3)     error stop(254)
    if (ubound(o,2)  /=    4)     error stop(255)
  class default
    error stop (632)
  end select
  select type (o => az2o)
  type is (four)
    if (size(o)      /=   12)     error stop(25)
    if (any(shape(o) /= (/3,4/))) error stop(257)
    if (rank(o)      /=    2)     error stop(258)
    if (lbound(o,1)  /=    1)     error stop(259)
    if (lbound(o,2)  /=    1)     error stop(260)
    if (ubound(o,1)  /=    3)     error stop(261)
    if (ubound(o,2)  /=    4)     error stop(262)
  class default
    error stop (633)
  end select
  select type (o => u2)
  type is (four)
    if (size(o)      /=   12)     error stop(263)
    if (any(shape(o) /= (/3,4/))) error stop(264)
    if (rank(o)      /=    2)     error stop(265)
    if (lbound(o,1)  /=    1)     error stop(266)
    if (lbound(o,2)  /=    1)     error stop(267)
    if (ubound(o,1)  /=    3)     error stop(268)
    if (ubound(o,2)  /=    4)     error stop(269)
  class default
    error stop (634)
  end select

  deallocate(ac1a, ac1b)
  allocate(ac1a, ac1b, mold=ac1o(2:4))

  if (size(ac1a)      /=   3)    error stop(251)
  if (any(shape(ac1a) /= (/3/))) error stop(252)
  if (rank(ac1b)      /=   1)    error stop(253)
  if (lbound(ac1b,1)  /=   1)    error stop(254)
  if (ubound(ac1a,1)  /=   3)    error stop(255)

  deallocate(ac3a, ac3b)
  allocate(ac3a, ac3b, mold=ac3o(2:4,1:1,1:1))

  if (size(ac3a)      /=     3)      error stop(261)
  if (any(shape(ac3a) /= (/3,1,1/))) error stop(262)
  if (rank(ac3b)      /=     3)      error stop(263)
  if (lbound(ac3b,1)  /=     1)      error stop(264)
  if (lbound(ac3a,2)  /=     1)      error stop(265)
  if (lbound(ac3b,3)  /=     1)      error stop(266)
  if (ubound(ac3a,1)  /=     3)      error stop(267)
  if (ubound(ac3b,2)  /=     1)      error stop(268)
  if (ubound(ac3a,3)  /=     1)      error stop(269)

  if (any(ac3a%i                  /=   -77))           error stop(271)
  if (any(ac3b%r                  /=   -98327493.432)) error stop(272)
  if (any(ac3a%c                  /=   "&"))           error stop(273)
  if (any(ac3b%x                  /=   (-32.0,16.0)))  error stop(274)
  if (any(ac3a%l                .NEQV. .FALSE.))       error stop(275)
  if (associated(ac3b(1,1,1)%o) .NEQV. .FALSE.)        error stop(276)
  if (associated(ac3a(2,1,1)%o) .NEQV. .FALSE.)        error stop(277)
  if (associated(ac3b(3,1,1)%o) .NEQV. .FALSE.)        error stop(278)

  deallocate(z1, z2, o1, o2, t1, t2, h1, h2, f1, f2, c1, c2, &
           & ac1a, ac1b, ah2a, ah2b, ac3a, ac3b,             &
           & oz, oo, ot, oh, of, oc, ac1o, ah2o, ac3o)

end program main
