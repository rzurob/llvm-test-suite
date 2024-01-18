!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 15 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement with ultimate real components
!* 2. Use sequence statement in derived type
!* 3. Use equivalence statement
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type DT0(k1,l1)
     integer,kind :: k1 !k1=8
     integer,len  :: l1 !l1=2

     sequence
     real(k1) :: r1(l1)
     real(2*k1) :: r2
  end type
end module

module m2
use m1
  type DT1(k2,l2)
     integer,kind :: k2 !k2=4
     integer,len  :: l2 !l2=3

     sequence
     real(k2) :: r3(l2)
     type(DT0(2*k2,l2-1)) :: dtcomp
  end type

end module

program listDirectRealCompRead01
  use m2,RDT0=>DT0,RDT1=>DT1

  interface
     function readData(dt,unit)
       import
       implicit type(RDT1(4,*)) (d)
       implicit type(RDT1(4,:)) (r)
       allocatable :: readData(:)
       dimension :: dt(2:)
       integer :: unit
     end function
  end interface

  integer :: ios,i
  character(256) :: msg
  logical :: precision_r4,precision_r8,precision_r16

  implicit type(RDT1(4,3)) (T)
  implicit type(RDT1(4,:)) (D)

  allocatable :: d1(:)
  dimension :: t1(2),t2(3:4)

  equivalence(t1,t2)

  !initialize variable
  do i=1,2
     t1(i)%r3=0.1_4
     t1(i)%dtcomp%r1=0.1_8
     t1(i)%dtcomp%r2=0.1_16
  end do

  open(10,file='listDirectRealCompRead01.dat',form='formatted',&
       access='sequential',iostat=ios,iomsg=msg)

  if(ios <> 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
  end if

  allocate(d1(2),source=readData(t1,10))

  if(.not. precision_r4(t2(3)%r3(1),0.1))                 stop 11
  if(.not. precision_r4(t2(3)%r3(2),-3.4))                stop 12
  if(.not. precision_r4(t2(3)%r3(3),0.12E-01))            stop 13
  if(.not. precision_r8(t2(3)%dtcomp%r1(1),-2.1E2_8))     stop 14
  if(.not. precision_r8(t2(3)%dtcomp%r1(2),0.1_8))        stop 15
  if(.not. precision_r16(t2(3)%dtcomp%r2,3.2_16))         stop 16

  if(.not. precision_r4(t2(4)%r3(1),2.3_4))               stop 17
  if(.not. precision_r4(t2(4)%r3(2),-4.0E-15))            stop 18
  if(.not. precision_r4(t2(4)%r3(3),-3.0))                stop 19
  if(.not. precision_r8(t2(4)%dtcomp%r1(1),-2.0_8))       stop 20
  if(.not. precision_r8(t2(4)%dtcomp%r1(2),-2.E-01_8))    stop 21
  if(.not. precision_r16(t2(4)%dtcomp%r2,-3.7_16))        stop 22

  if(.not. precision_r4(d1(1)%r3(1),0.1))                 stop 23
  if(.not. precision_r4(d1(1)%r3(2),-3.4))                stop 24
  if(.not. precision_r4(d1(1)%r3(3),0.12E-01))            stop 25
  if(.not. precision_r8(d1(1)%dtcomp%r1(1),-2.1E2_8))     stop 26
  if(.not. precision_r8(d1(1)%dtcomp%r1(2),0.1_8))        stop 27
  if(.not. precision_r16(d1(1)%dtcomp%r2,3.2_16))         stop 28

  if(.not. precision_r4(d1(2)%r3(1),2.3_4))               stop 29
  if(.not. precision_r4(d1(2)%r3(2),-4.0E-15))            stop 30
  if(.not. precision_r4(d1(2)%r3(3),-3.0))                stop 31
  if(.not. precision_r8(d1(2)%dtcomp%r1(1),-2.0_8))       stop 32
  if(.not. precision_r8(d1(2)%dtcomp%r1(2),-2.E-01_8))    stop 33
  if(.not. precision_r16(d1(2)%dtcomp%r2,-3.7_16))        stop 34

  close(10)

end program

function readData(dt,unit)
   use m2 ,UDT0=>DT0,UDT1=>DT1
   implicit type(UDT1(4,*)) (d)
   implicit type(UDT1(4,:)) (r)
   dimension:: dt(2:)
   allocatable :: readData(:)
   integer :: unit

   ! following is the data we want to read
   !; -3,4 1,2E-02 ; 1*-2,1E2 1*; 5*3,2
   ! +2.3 , -4.0E-15 1*-3,-2. -2.-01 2*-3.7

   read(unit,*,decimal='comma') dt(lbound(dt,1))
   read(unit,*,decimal='point') dt(lbound(dt,1)+1)

   readData=dt

end function
