!*  ==================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ==================================================================
!* 
!* TEST CASE TITLE            : "multAllocSourceMold01"
!* 
!* PROGRAMMER                 : Izhak Jakov
!* DATE                       : June 2, 2015
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!* 
!* PRIMARY FUNCTIONS TESTED   : ALLOCATE Statement with type-spec
!* SECONDARY FUNCTIONS TESTED :
!* 
!* 
!* DRIVER STANZA              : xlf2008
!* REQUIRED COMPILER OPTIONS  :
!* 
!* KEYWORD(S)                 :
!* TARGET(S)                  :
!* NUMBER OF TESTS CONDITIONS :
!* 
!* DESCRIPTION                : Multiple allocate intrinsic types
!*                              from source
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

  integer(kind=4) :: s
  
  real(kind=8), pointer             :: pr1, pr2
  real(kind=8), allocatable, target :: tr1

  real(kind=4), pointer             :: pr3, pr4, pr5, pr6
  real(kind=4), allocatable, target :: tr2

  character(len=1), pointer             :: pc1, pc2, pc3
  character(len=1), target, allocatable :: tc

  integer, pointer             :: pi1, pi2, pi3
  integer, allocatable, target :: ti

  complex(kind=8), pointer             :: px1, px2, px3
  complex(kind=8), allocatable, target :: tx
  
  logical, pointer             :: pl1, pl2, pl3
  logical, allocatable, target :: tl

  type custom
    integer   :: id       = 77
    real      :: funds    = 98327493.432
    character :: initial  = "J"
    complex   :: imNumber = (32.0,-16.0)
    logical   :: hungry   = .TRUE.
  end type

  type(custom), pointer             :: pm1, pm2, pm3
  type(custom), allocatable, target :: tm
  
 !Real Numbers of kind=8
  allocate(tr1)
  tr1=-9.1_8
  allocate(pr1, pr2, source=tr1)
  if (pr1.ne.-9.1_8) error stop(1)
  if (pr2.ne.-9.1_8) error stop(2)
  deallocate(pr1, pr2)
  
  tr1=00_8
  allocate(pr1, pr2, source=tr1)
  if (pr1.ne.0_8)   error stop(3)
  if (pr2.ne.0.0_8) error stop(4)     
  deallocate(pr1, pr2)
  
  tr1=0.002_8
  allocate(pr1, pr2, source=tr1)
  if (pr1.ne.0.002_8)  error stop(5)
  if (pr2.ne.+0.002_8) error stop(6)
  deallocate(pr1, pr2)

  allocate(pr1, pr2, mold=tr1, stat=s)
  if (s /= 0)  error stop(201)
  deallocate(pr1, pr2, tr1)
  
  
 !Real Numbers of kind=4 
  allocate(tr2)
  tr2=14.0_4
  pr6=>tr2
  allocate(pr3, pr4, pr5, source=pr6)
  
  if (pr3.ne.+14.0_4) error stop(7)
  if (pr4.ne.14.0_4)  error stop(8)
  if (pr5.ne.014.0_4) error stop(9)
  if (pr6.ne.14.0_4)  error stop(10)
  deallocate(pr3, pr4, pr5)
  
  allocate(pr6)
  pr6 = -55_4
  allocate(pr3, pr4, pr5, source=pr6)
  
  if (pr3.ne.-55.0_4)    error stop(11)
  if (pr4.ne.-55_4)      error stop(12)
  if (pr5.ne.-55.0000_4) error stop(13)
  if (pr6.ne.-55_4)      error stop(14)
  deallocate(pr3, pr4, pr5, pr6)
  
  allocate(pr3, pr4, pr5, mold=pr6, stat=s)
  if (s /= 0)  error stop(202)
  deallocate(pr3, pr4, pr5, tr2)
  
  
 !Complex Numbers
  tx=(2.0_8,16.0_8)
  px3=>tx
  allocate(px1, px2, source=px3)

  if (px1.ne.(2.0_8,16.0_8)) error stop(15)
  if (px2.ne.(2.0_8,16.0_8)) error stop(16)
  if (px3.ne.(2.0_8,16.0_8)) error stop(17)
  deallocate(px1, px2)

  tx=(-9.1_8,4.0_8)
  allocate(px3, source=tx)
  allocate(px1, px2, source=px3)
  if (px1.ne.(-9.1_8,4.0_8)) error stop(18)
  if (px2.ne.(-9.1_8,4.0_8)) error stop(19)
  if (px3.ne.(-9.1_8,4.0_8)) error stop(20)
  deallocate(px1, px2, px3)
  
  px3=(0.0_8,-1.2_8)
  allocate(px1, px2, source=px3)
  if (px1.ne.(00.0_8,-1.2_8)) error stop(21)
  if (px2.ne.(0.0_8,-1.2_8))  error stop(22)
  if (px3.ne.(0.00_8,-1.2_8)) error stop(23)
  deallocate(px1, px2)
  
  allocate(px1, px2, mold=px3, stat=s)
  if (s /= 0)  error stop(203)
  deallocate(px1, px2, tx)
  
 
 !Integers
  ti=2
  
  pi3=>ti
  allocate(pi1, pi2, source=pi3)
  if (pi1.ne.02) error stop(24)
  if (pi2.ne.+2) error stop(25)
  if (pi3.ne. 2) error stop(26)
  deallocate(pi1, pi2)

  ti=-9
  allocate(pi3, source=ti)
  allocate(pi1, pi2, source=pi3)
  if (pi1.ne.-09) error stop(27)
  if (pi2.ne.-9)  error stop(28)
  if (pi3.ne.-9)  error stop(29)
  deallocate(pi1, pi2)
  
  pi3=00
  allocate(pi1, pi2, source=pi3)
  if (pi1.ne.00) error stop(30)
  if (pi2.ne.0)  error stop(31)
  if (pi3.ne.0)  error stop(32)
  deallocate(pi1, pi2)
  
  allocate(pi1, pi2, mold=pi3, stat=s)
  if (s /= 0)  error stop(204)
  deallocate(pi1, pi2, pi3, ti)
  
 !Logicals
  tl=.TRUE.
  pl3=>tl
  allocate(pl1, pl2, source=pl3)
  if (pl1.neqv..TRUE.) error stop(33)
  if (pl2.neqv..TRUE.) error stop(34)
  if (pl3.neqv..TRUE.) error stop(35)
  deallocate(pl1, pl2)
  
  allocate(pl3, source=.FALSE.)
  allocate(pl1, pl2, source=pl3)
  if (pl1.neqv..FALSE.) error stop(36)
  if (pl2.neqv..FALSE.) error stop(37)
  if (pl3.neqv..FALSE.) error stop(38)
  deallocate(pl1, pl2, pl3)
  
  allocate(pl1, pl2, mold=pl3, stat=s)
  if (s /= 0)  error stop(205)
  deallocate(pl1, pl2, tl)
 
 !Character
  allocate(tc);
  tc="F"
  pc3=>tc
  allocate(pc1, pc2, source=pc3)
  if (pc1 /= "F")  error stop(39)
  if (pc2 /= "F")  error stop(40)
  if (pc3 /= "F")  error stop(41)
  deallocate(pc1, pc2)

  tc="Fortran Test"  
  allocate(pc3, source=tc) 
  allocate(pc1, pc2, source=pc3)
  if (pc1 /= "F")  error stop(42)
  if (pc2 /= "F")  error stop(43)
  if (pc3 /= "F")  error stop(44)
  deallocate(pc1, pc2)

  pc3=""  
  allocate(pc1, pc2, source=pc3)
  if (pc1 /= "")  error stop(45)
  if (pc2 /= "")  error stop(46)
  if (pc3 /= "")  error stop(47)
  deallocate(pc1, pc2)
  
  allocate(pc1, pc2, mold=pc3, stat=s)
  if (s /= 0)  error stop(206)
  deallocate(pc1, pc2, pc3, tc)

 !Custom Type
  tm=custom(id=-90, funds=102948017498.43, initial="K", &
         & imNumber=(0,-96.12), hungry=.FALSE.)      
  pm3=>tm
  allocate(pm1, pm2, source=pm3)
  if (pm1%id /= -90 .or. pm1%funds /= 102948017498.43         &
    & .or. pm1%initial /= "K" .or. pm1%imNumber /= (0,-96.12) &
    & .or. pm1%hungry.neqv..FALSE.) error stop (48)

  if (pm2%id /= -90 .or. pm2%funds /= 102948017498.43         &
    & .or. pm2%initial /= "K" .or. pm2%imNumber /= (0,-96.12) &
    & .or. pm2%hungry.neqv..FALSE.) error stop (49)
    
  if (pm3%id /= -90 .or. pm3%funds /= 102948017498.43         &
    & .or. pm3%initial /= "K" .or. pm3%imNumber /= (0,-96.12) &
    & .or. pm3%hungry.neqv..FALSE.) error stop (50)
  deallocate(pm1, pm2)

  allocate(pm1, pm2, mold=pm3, stat=s)
  if (s /= 0)  error stop(207)
  
  if (pm1%id /= 77 .or. pm1%funds /= 98327493.432         &
    & .or. pm1%initial /= "J" .or. pm1%imNumber /= (32.0,-16.0) &
    & .or. pm1%hungry.neqv..TRUE.) error stop (48)

  if (pm2%id /= 77 .or. pm2%funds /= 98327493.432         &
    & .or. pm2%initial /= "J" .or. pm2%imNumber /= (32.0,-16.0) &
    & .or. pm2%hungry.neqv..TRUE.) error stop (49)
    
  if (pm3%id /= 77 .or. pm3%funds /= 98327493.432         &
    & .or. pm3%initial /= "J" .or. pm3%imNumber /= (32.0,-16.0) &
    & .or. pm3%hungry.neqv..TRUE.) error stop (50)
  deallocate(pm1, pm2, tm)
  
end program main
