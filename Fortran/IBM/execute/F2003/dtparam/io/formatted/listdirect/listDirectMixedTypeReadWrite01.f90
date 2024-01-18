!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectMixedTypeReadWrite01.f
!*
!*  DATE                       : Jan. 16 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement with mixed ultimate intrinsic type components
!* 2. Write data with format edit descriptor and read back with default format.
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type Inner(k1,l1)
      integer,kind :: k1 !k1=4
      integer,len  :: l1 !l1=2

      sequence
      character(k1) :: c1(l1)="xxx"
      integer(k1)   :: i(l1+1)=-99
      logical(2*k1) :: g(l1+1)=.false.

   end type
end module

module m2
use m1
   type Outer(k2,l2)
      integer,kind :: k2 !k2=4
      integer,len  :: l2 !l2=3
      sequence
      real(k2)       :: r(l2)=-9.9
      character(l2)  :: c2(k2)="yyy"
      complex(k2+k2)  :: x(l2)=(-9.9_8,9.9_8)
      type(Inner(k2,l2-1)) :: inn1
   end type

   contains

     subroutine  readData(dt,unit)
       implicit type(Outer(4,*)) (D)
       dimension :: dt(:)
       intent(inout) :: dt
       integer,intent(in) :: unit

       read(unit,*,decimal='comma') dt(1)
       read(unit,*,decimal='point') dt(2)

     end subroutine

     subroutine  writeData(dt,unit)
       implicit type(Outer(4,*)) (D)
       integer,intent(in) :: unit

       write(unit,'(f5.2,e13.4,/e11.4e2)') dt%r
       write(unit,'( 4(a3,"," ) )' ) dt%c2
       write(unit,'("(", f7.3, ",", f7.3 ,")" , /," (",e12.4, ",", /,e12.4e2, ")",  " , (", en12.4, ",", es12.4 , ")" )' ) dt%x
       write(unit,'(a3, /, a3)')    dt%inn1%c1
       write(unit,'(3i4)')   dt%inn1%i
       write(unit,'(3l2,"/")') dt%inn1%g

     end subroutine

end module

program listDirectMixedTypeReadWrite01
use m2

   integer :: ios
   character(256) :: msg
   logical,external :: precision_r4,precision_x6

   implicit type(Outer(4,:)) (o-p)
   implicit type(Outer(4,3)) (r)

   allocatable :: ot(:)

   target :: ot

   pointer :: pt(:)

   allocate(Outer(4,3) :: ot(2:3))

   pt=>ot

   rt%r=[-4.1_4, 0.0001, -1.5e14]
   rt%c2=["xlf ","IBM ","test","xlc "]
   rt%x=[ (-1.2_8,1.2_8), (-3.4D-3,8.6D+05), (32.7_8,7.1_8)]
   rt%inn1%c1=["ABC","abc"]
   rt%inn1%i=[ -12,45_4, -34]
   rt%inn1%g=[.true.,.false.,.true.]

   open(10,file='listDirectMixedTypeReadWrite01.dat',form='formatted',&
        access='sequential',sign='plus',action='readwrite',status='old',&
        position='append',iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
   end if

   call writeData(rt,10)

   rewind 10

   call readData(ot,10)

   ! verify the results
   if(.not. precision_r4(pt(2)%r(1),-9.9_4))               stop 11
   if(.not. precision_r4(pt(2)%r(2),1.3_4))                stop 12
   if(.not. precision_r4(pt(2)%r(3),-5.1e-10))             stop 13
   if(any( pt(2)%c2 /= ["she","wil","cal","you"]))         stop 14

   if(.not. precision_x6(pt(2)%x(1),(-3.2_8,-8.3_8)))      stop 15
   if(.not. precision_x6(pt(2)%x(2),(6.3_8,-0.5_8)))       stop 16
   if(.not. precision_x6(pt(2)%x(3),(-9.9_8,9.9_8)))       stop 17

   if(any(pt(2)%inn1%c1 /=  ["XLF","IBM"]))                stop 18
   if(any(pt(2)%inn1%i /= [12,-13,14] ))                   stop 19
   if(any(pt(2)%inn1%g .neqv. [.false.,.true.,.false.]))   stop 20

   if(.not. precision_r4(pt(3)%r(1),-4.10_4))              stop 21
   if(.not. precision_r4(pt(3)%r(2),0.1e-3))               stop 22
   if(.not. precision_r4(pt(3)%r(3),-0.15e15))             stop 23
   if(any( pt(3)%c2 /= ["xlf","IBM","tes","xlc"]))         stop 24

   if(.not. precision_x6(pt(3)%x(1),(-1.2_8,1.2_8)))       stop 25
   if(.not. precision_x6(pt(3)%x(2),(-0.34d-2,0.86e6_8)))  stop 26
   if(.not. precision_x6(pt(3)%x(3),(32.7_8,7.1_8)))       stop 27

   if(any(pt(3)%inn1%c1 /=  ["ABC","abc"]))                stop 28
   if(any(pt(3)%inn1%i /= [-12,45,-34] ))                  stop 29
   if(any(pt(3)%inn1%g .neqv. [.true.,.false.,.true.]))    stop 30

   close(10)

end program
