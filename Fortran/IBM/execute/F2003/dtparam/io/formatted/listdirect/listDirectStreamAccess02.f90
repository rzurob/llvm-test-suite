!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 21 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test read statement with list directed IO with stream access
!* 2. Derived type has real,complex,character,integer ultimate components
!* 3. Derived type has multiple layers of derived type components
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type A
      real :: r1
      character(3) :: c1(4)="***"
   end type

   type B(k2,l2)
      integer,kind :: k2=4
      integer,len  :: l2=3

      real(k2)     :: r2(l2)=0.0
      type(A)      :: a1comp
   end type
end module

module m2
  use m1
  type C(k3)
    integer,kind :: k3=4

    complex(k3)  :: x1=(0.0,0.0)
    integer(k3)  :: i1=-99
  end type

  type D(k4,l4)
    integer,kind :: k4=8
    integer,len  :: l4=2

    complex(k4)  :: x2(l4)=(0.0,0.0)
    type(B(k4/2,l4*l4-1)) :: b1comp
    type(C(k4/2))  :: c1comp

  end type

  contains

    subroutine read(dummy,unit)
       type(D),intent(inout) :: dummy
       integer,intent(in)    :: unit
       integer :: mypos

       inquire(unit,pos=mypos)
       if(mypos /= 1)                      error stop 20

       read(unit,*,pos=mypos) dummy%x2
       inquire(unit,pos=mypos)
       if(mypos /= 33)                     error stop 21

       read(unit,*,decimal='comma') dummy%b1comp
       inquire(unit,pos=mypos)
       if(mypos /= 85)                     error stop 22

       read(unit,*,pos=mypos) dummy%c1comp

    end subroutine
end module

program listDirectStreamAccess02
use m2

   integer :: ios
   character(256) :: msg
   logical,external :: precision_x6,precision_x8,precision_r4

   ! tar has default parameter and component value
   type(D),target  :: tar
   type(D(8,:)),pointer :: ptr=>null()

   ptr=>tar

   open(10,file='listDirectStreamAccess02.dat',form='formatted',&
       access='stream',iostat=ios,iomsg=msg)

   if(ios <> 0) then

     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10

   end if

   ! following is the record we want to read
   !(-3.5,
   !+4.2) (12.4D02
   !,-2.6D-3)
   !; -999,9E-3  ; -4; 1*-5,E-1 "xlf"
   ! 'XLF'
   !; ; IBM ;
   !(1.2E-10,1.2) , -1234

   call read(ptr,10)

   !verify the results
   if(.not. precision_x6(ptr%x2(1),(-3.5_8,4.2_8) ))          error stop 11
   if(.not. precision_x6(ptr%x2(2),(12.4D02,-2.6D-3) ))       error stop 12
   if(.not. precision_r4(ptr%b1comp%r2(1),0.))                error stop 13
   if(.not. precision_r4(ptr%b1comp%r2(2),-999.9E-3))         error stop 14
   if(.not. precision_r4(ptr%b1comp%r2(3),-4._4))             error stop 15
   if(.not. precision_r4(ptr%b1comp%a1comp%r1,-0.5))          error stop 16
   if(any(ptr%b1comp%a1comp%c1 /= ["xlf","XLF","***","IBM"])) error stop 17
   if(.not. precision_x8(ptr%c1comp%x1,(1.2E-10,1.2) ))       error stop 18
   if(ptr%c1comp%i1 /= -1234)                                 error stop 19

   close(10)

end program
