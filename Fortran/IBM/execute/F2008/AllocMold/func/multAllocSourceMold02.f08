!*  ==================================================================
!*
!*  DATE                       : June 2, 2015
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
!*  DESCRIPTION                : Multiple allocate arrays of
!*                               intrinsic types from source
!*
!* Defect 112510
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

  integer(kind=4) :: s

  real(kind=8), pointer             :: pr1(:),pr2(:)
  real(kind=8), allocatable, target :: tr1(:)

  real(kind=4), pointer             :: pr3(:), pr4(:), pr5(:), pr6(:)
  real(kind=4), allocatable, target :: tr2(:)

  character(len=2), pointer             :: pc1(:), pc2(:), pc3(:)
  character(len=2), target, allocatable :: tc(:)

  integer, pointer             :: pi1(:), pi2(:), pi3(:)
  integer, allocatable, target :: ti(:)

  complex(kind=8), pointer             :: px1(:), px2(:), px3(:)
  complex(kind=8), allocatable, target :: tx(:)

  logical, pointer             :: pl1(:), pl2(:), pl3(:)
  logical, allocatable, target :: tl(:)

  type custom
    integer   :: idno     = -77
    real      :: funds    = -98327493.432
    character :: special  = "&"
    complex   :: imNumber = (-32.0,16.0)
    logical   :: hungry   = .FALSE.
  end type

  type(custom), pointer             :: pm1(:), pm2(:), pm3(:)
  type(custom), allocatable, target :: tm(:)

 !Real Numbers of kind=8
  allocate(tr1(5))
  tr1=(/2.0_8,-9.1_8,0.4_8,-1.2_8,0.0_8/)

  allocate(pr1, pr2, source=tr1)

  if (pr1(1).ne.2.0_8)  error stop(1)
  if (pr1(2).ne.-9.1_8) error stop(2)
  if (pr1(3).ne.0.4_8)  error stop(3)
  if (pr1(4).ne.-1.2_8) error stop(4)
  if (pr1(5).ne.0.0_8)  error stop(5)

  if (pr2(1).ne.2.0_8)  error stop(6)
  if (pr2(2).ne.-9.1_8) error stop(7)
  if (pr2(3).ne.0.4_8)  error stop(8)
  if (pr2(4).ne.-1.2_8) error stop(9)
  if (pr2(5).ne.0.0_8)  error stop(10)

 !Real Numbers of kind=4
  allocate(tr2(3:7))
  tr2=(/2.0_4, -9.1_4, 0.4_4, 0.0_4, 14.0_4/)
  pr6=>tr2
  allocate(pr3, pr4, pr5, source=pr6)

  if (pr3(3).ne.+2.0_4) error stop(11)
  if (pr3(4).ne.-9.1_4) error stop(12)
  if (pr3(5).ne.+0.4_4) error stop(13)
  if (pr3(6).ne.+0.0_4) error stop(14)
  if (pr3(7).ne.14.0_4) error stop(15)

  if (pr4(3).ne.+2.0_4) error stop(16)
  if (pr4(4).ne.-9.1_4) error stop(17)
  if (pr4(5).ne.+0.4_4) error stop(18)
  if (pr4(6).ne.+0.0_4) error stop(19)
  if (pr4(7).ne.14.0_4) error stop(20)

  if (pr5(3).ne.+2.0_4) error stop(21)
  if (pr5(4).ne.-9.1_4) error stop(22)
  if (pr5(5).ne.+0.4_4) error stop(23)
  if (pr5(6).ne.+0.0_4) error stop(24)
  if (pr5(7).ne.14.0_4) error stop(25)

  if (pr6(3).ne.+2.0_4) error stop(26)
  if (pr6(4).ne.-9.1_4) error stop(27)
  if (pr6(5).ne.+0.4_4) error stop(28)
  if (pr6(6).ne.+0.0_4) error stop(29)
  if (pr6(7).ne.14.0_4) error stop(30)

 !Complex Numbers
  allocate(tx(4))
  tx=(/(2.0_8,16.0_8), (-9.1_8,4.0_8), (0.4_8,-1.2_8), (0.0_8,0.0_8)/)

  px3=>tx
  allocate(px1, px2, source=px3)

  if (px1(1).ne.(2.0_8,16.0_8)) error stop(31)
  if (px1(2).ne.(-9.1_8,4.0_8)) error stop(32)
  if (px1(3).ne.(0.4_8,-1.2_8)) error stop(33)
  if (px1(4).ne.(+0.0_8,0.0_8)) error stop(34)

  if (px2(1).ne.(2.0_8,16.0_8)) error stop(35)
  if (px2(2).ne.(-9.1_8,4.0_8)) error stop(36)
  if (px2(3).ne.(0.4_8,-1.2_8)) error stop(37)
  if (px2(4).ne.(+0.0_8,0.0_8)) error stop(38)

  if (px3(1).ne.(2.0_8,16.0_8)) error stop(39)
  if (px3(2).ne.(-9.1_8,4.0_8)) error stop(40)
  if (px3(3).ne.(0.4_8,-1.2_8)) error stop(41)
  if (px3(4).ne.(+0.0_8,0.0_8)) error stop(42)

 !Integers
  allocate(ti(-4:-1)) !Negative Bounds
  ti(-4)=2;ti(-3)=-9;ti(-2)=00;ti(-1)=99
  pi3=>ti
  allocate(pi1, pi2, source=pi3)

  if (pi1(-4).ne.02) error stop(43)
  if (pi1(-3).ne.-9) error stop(44)
  if (pi1(-2).ne. 0) error stop(45)
  if (pi1(-1).ne.99) error stop(46)

  if (pi2(-4).ne.02) error stop(47)
  if (pi2(-3).ne.-9) error stop(48)
  if (pi2(-2).ne.+0) error stop(49)
  if (pi2(-1).ne.99) error stop(50)

  if (pi3(-4).ne.02) error stop(51)
  if (pi3(-3).ne.-9) error stop(52)
  if (pi3(-2).ne.-0) error stop(53)
  if (pi3(-1).ne.99) error stop(54)

 !Logicals
  allocate(tl(6))
  tl=(/.TRUE., .FALSE., .TRUE., .TRUE., .FALSE., .TRUE./)
  pl3=>tl

  allocate(pl1, pl2, source=pl3)

  if (pl1(1).neqv..TRUE.)  error stop(51)
  if (pl1(2).neqv..FALSE.) error stop(52)
  if (pl1(3).neqv..TRUE.)  error stop(53)
  if (pl1(4).neqv..TRUE.)  error stop(54)
  if (pl1(5).neqv..FALSE.) error stop(55)
  if (pl1(6).neqv..TRUE.)  error stop(56)

  if (pl2(1).neqv..TRUE.)  error stop(57)
  if (pl2(2).neqv..FALSE.) error stop(58)
  if (pl2(3).neqv..TRUE.)  error stop(59)
  if (pl2(4).neqv..TRUE.)  error stop(60)
  if (pl2(5).neqv..FALSE.) error stop(61)
  if (pl2(6).neqv..TRUE.)  error stop(62)

  if (pl3(1).neqv..TRUE.)  error stop(63)
  if (pl3(2).neqv..FALSE.) error stop(64)
  if (pl3(3).neqv..TRUE.)  error stop(65)
  if (pl3(4).neqv..TRUE.)  error stop(66)
  if (pl3(5).neqv..FALSE.) error stop(67)
  if (pl3(6).neqv..TRUE.)  error stop(68)

 !Character -- Commented out due to defect 112510: https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/112510
!  tc = "Fortran Test"
  allocate(tc(4))
  tc = "Fortran Test"
  pc3=>tc

  allocate(pc1, pc2, source=pc3)

  if (pc1(1).ne."Fo")  error stop(63)
  if (pc1(2).ne."Fo") error stop(64)
  if (pc1(3).ne."Fo")  error stop(65)

  if (pc2(1).ne."Fo")  error stop(63)
  if (pc2(2).ne."Fo") error stop(64)
  if (pc2(3).ne."Fo")  error stop(65)

  if (pc3(1).ne."Fo")  error stop(63)
  if (pc3(2).ne."Fo") error stop(64)
  if (pc3(3).ne."Fo")  error stop(65)
  !Why is this working for
  !real*8, allocatable, target :: z(:)
  !but not for character*16, target, allocatable :: tc(:)
  !??

 !Integers
  allocate(tm(3))
  tm(2) = custom(idno=90, funds=30174984.43E4, special="$",  &
        & imNumber=(0,-96.12), hungry=.FALSE.)

  tm(3) = custom(idno=0, funds=-30174984.43E-4, special="!", &
        & imNumber=(-1,-96.12), hungry=.TRUE.)


  pm3=>tm
  allocate(pm1, pm2, source=pm3)

  ! Default Value
  if (pm1(1)%idno /= -77 .or. pm1(1)%funds /= -98327493.432           &
    & .or. pm1(1)%special /= "&" .or. pm1(1)%imNumber /= (-32.0,16.0) &
    & .or. pm1(1)%hungry.neqv..FALSE.) error stop (69)
  ! Other Values
  if (pm1(2)%idno /= 90 .or. pm1(2)%funds /= 30174984.43E4            &
    & .or. pm1(2)%special /= "$" .or. pm1(2)%imNumber /= (0,-96.12)   &
    & .or. pm1(2)%hungry.neqv..FALSE.) error stop (70)
  if (pm1(3)%idno /= 0 .or. pm1(3)%funds /= -30174984.43E-4           &
    & .or. pm1(3)%special /= "!" .or. pm1(3)%imNumber /= (-1,-96.12)  &
    & .or. pm1(3)%hungry.neqv..TRUE.) error stop (71)

  if (pm2(1)%idno /= -77 .or. pm2(1)%funds /= -98327493.432           &
    & .or. pm2(1)%special /= "&" .or. pm2(1)%imNumber /= (-32.0,16.0) &
    & .or. pm2(1)%hungry.neqv..FALSE.) error stop (72)
  if (pm2(2)%idno /= 90 .or. pm2(2)%funds /= 30174984.43E4            &
    & .or. pm2(2)%special /= "$" .or. pm2(2)%imNumber /= (0,-96.12)   &
    & .or. pm2(2)%hungry.neqv..FALSE.) error stop (73)
  if (pm2(3)%idno /= 0 .or. pm2(3)%funds /= -30174984.43E-4           &
    & .or. pm2(3)%special /= "!" .or. pm2(3)%imNumber /= (-1,-96.12)  &
    & .or. pm2(3)%hungry.neqv..TRUE.) error stop (74)

  if (pm3(1)%idno /= -77 .or. pm3(1)%funds /= -98327493.432           &
    & .or. pm3(1)%special /= "&" .or. pm3(1)%imNumber /= (-32.0,16.0) &
    & .or. pm3(1)%hungry.neqv..FALSE.) error stop (75)
  if (pm3(2)%idno /= 90 .or. pm3(2)%funds /= 30174984.43E4            &
    & .or. pm3(2)%special /= "$" .or. pm3(2)%imNumber /= (0,-96.12)   &
    & .or. pm3(2)%hungry.neqv..FALSE.) error stop (76)
  if (pm3(3)%idno /= 0 .or. pm3(3)%funds /= -30174984.43E-4           &
    & .or. pm3(3)%special /= "!" .or. pm3(3)%imNumber /= (-1,-96.12)  &
    & .or. pm3(3)%hungry.neqv..TRUE.) error stop (77)



  deallocate(pr1, pr2, pr3, pr4, pr5, pi1, pi2, px1, px2, &
           & pl1, pl2, pm1, pm2) !pc1, pc2, pc3)

! The following is allocation of subarray and currently casuses ICE
! RTC #114148
  allocate(pr1, pr2, source=tr1(2:4), stat=s)
  if (s              /=   0   ) error stop(200)
  if (size(pr1)     .ne.  3   ) error stop(201)
  if (any(shape(pr2).ne.(/3/))) error stop(202)
  if (ubound(pr1,1) .ne.  3   ) error stop(203)
  if (lbound(pr2,1) .ne.  1   ) error stop(204)

  allocate(pr3, pr4, pr5, pr6, mold=tr2, stat=s)
  if (s /= 0)  error stop(222)

  allocate(pi1, pi2, pi3, mold=ti, stat=s)
  if (s /= 0)  error stop(233)

  allocate(px1, px2, px3, mold=tx, stat=s)
  if (s /= 0)  error stop(244)

  allocate(pl1, pl2, pl3, mold=tl(3:5), stat=s)
  if (s /= 0)  error stop(255)
  if (lbound(pl1,1)      .ne.         1     ) error stop(253)
  if (ubound(pl2,1)      .ne.         3     ) error stop(254)

  allocate(pm1, pm2, pm3, mold=tm(1:2), stat=s)
  if (s                   /=          0     ) error stop(260)
  if (size(pm1)          .ne.         2     ) error stop(261)
  if (any(shape(pm2)     .ne.       (/2/)  )) error stop(262)
  if (lbound(pm3,1)      .ne.         1     ) error stop(263)
  if (ubound(pm1,1)      .ne.         2     ) error stop(264)
  if (any(pm2%idno       .ne.       -77    )) error stop(265)
  if (any(pm3%funds      .ne. -98327493.432)) error stop(266)
  if (any(pm1%special    .ne.       "&"    )) error stop(267)
  if (any(pm2%imnumber   .ne.  (-32.0,16.0))) error stop(268)
  if (any(pm3%hungry     .neqv.   .FALSE.  )) error stop(269)

  allocate(pc1, pc2, pc3, mold=tc, stat=s)
  if (s /= 0)  error stop(206)

  deallocate(pr1, pr2, pr3, pr4, pr5, pr6, pc1, pc2, pc3, &
           & pi1, pi2, pi3, px1, px2, px3, pl1, pl2, pl3, pm1, pm2, pm3, &
           & tr1, tr2, ti, tx, tl, tm, tc)

end program main
