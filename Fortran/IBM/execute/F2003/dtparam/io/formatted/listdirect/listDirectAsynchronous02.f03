!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test write and read statement with asynchronous IO
!* 2. Derived type has multiple layers of derived type components
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type inner1(k1,l1)
      integer,kind :: k1 !k1=4
      integer,len  :: l1 !l1=3

      sequence
      character(k1) :: c1(l1)="****"
      integer(k1)   :: i1(l1-1:l1)=-99
   end type
   type inner2(k2,l2)
      integer,kind :: k2 !k2=4
      integer,len  :: l2 !l2=4

      sequence
      logical(2*k2) :: g1(l2)=.false.
      type(inner1(k2,l2-1)) :: inn1
   end type
   type inner3(k3,l3)
      integer,kind :: k3 !k3=4
      integer,len  :: l3 !l3=3

      sequence
      real(k3)     :: r1(l3:l3+1)=-9.9
      type(inner2(k3,l3+1)) :: inn2
   end type
end module

module m2
  use m1

  type outer(k4,l4)
     integer,kind :: k4 ! k4=4
     integer,len  :: l4 ! l4=4

     sequence
     complex(2*k4) :: x1(l4-1)=(9.9_8,-9.9_8)
     type(inner3(k4,l4-1)) :: inn3
  end type

end module

program listDirectAsynchronous02
  use m2
  implicit none
  interface

     subroutine sub(outobj2)
       import
       type(outer(4,:)),pointer,intent(inout) :: outobj2(:)
     end subroutine

  end interface

  type(outer(4,:)),pointer :: outobj2(:)
  logical,external :: precision_r4,precision_x6

  call sub(outobj2)

   ! verify data

   if(.not. precision_x6(outobj2(1)%x1(1),(9.9_8,-9.9_8) ))    error stop 22
   if(.not. precision_x6(outobj2(1)%x1(2),(3.4_8,-7.8_8) ))    error stop 23
   if(.not. precision_x6(outobj2(1)%x1(3),(0.5_8,-0.5D-2) ))   error stop 24

   if(.not. precision_r4(outobj2(1)%inn3%r1(3),-3.4) )         error stop 25
   if(.not. precision_r4(outobj2(1)%inn3%r1(4),-2.3E-3) )      error stop 26

   if(any(outobj2(1)%inn3%inn2%g1 .neqv. &
                 [.false.,.true.,.true.,.false.] ))            error stop 27

   if(any(outobj2(1)%inn3%inn2%inn1%c1 /= ["test","team","star"]))  error stop 28
   if(any(outobj2(1)%inn3%inn2%inn1%i1 /= [-22,33]))                error stop 29

   if(.not. precision_x6(outobj2(2)%x1(1),(1.0_8,-1.0_8) ))     error stop 30
   if(.not. precision_x6(outobj2(2)%x1(2),(5.1_8,-4.2_8) ))     error stop 31
   if(.not. precision_x6(outobj2(2)%x1(3),(0.12D-2,-3.4D3) ))   error stop 32

   if(.not. precision_r4(outobj2(2)%inn3%r1(3),-3.567_4) )      error stop 33
   if(.not. precision_r4(outobj2(2)%inn3%r1(4),5.7E-3) )        error stop 34

   if(any(outobj2(2)%inn3%inn2%g1 .neqv. &
                 [.true.,.false.,.true.,.false.] ))             error stop 35

   if(any(outobj2(2)%inn3%inn2%inn1%c1 /= &
             ["\'GH\'","\"EF\"","ABCD"]))                       error stop 36
   if(any(outobj2(2)%inn3%inn2%inn1%i1 /= [-34,12]))            error stop 37

end program

subroutine sub(outobj2)

   use m2
   implicit none

   type(outer(4,:)),pointer,intent(inout) :: outobj2(:)

   type(outer(4,:)),allocatable  :: outobj1
   integer :: ios,idvar1,idvar2
   logical :: pending1,pending2
   character(256) :: msg

   allocate(outer(4,4) :: outobj1)
   allocate(outer(4,4) :: outobj2(2))

   outobj1%x1= [(5.1_8,-4.2_8),(0.12D-2,-3.4D3),(1._8,-1._8)]
   outobj1%inn3%r1= [-3.567E0,+5.7E-3]
   outobj1%inn3%inn2%g1=[.true.,.false.,.false.,.true.]
   outobj1%inn3%inn2%inn1%c1=["ABCD","\"EF\"","'GH'"]
   outobj1%inn3%inn2%inn1%i1=[-34,+12]

   open(10,file='listDirectAsynchronous02.dat',status='old',&
        form='formatted',access='sequential',position='append', &
        asynchronous='yes',iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
   end if

   ! append more data in input data file

   write(10,*,asynchronous='yes',id=idvar1,decimal='comma') outobj1%x1(1:2)
   write(10,*,asynchronous='yes',id=idvar2,sign='plus') outobj1%x1(3)

   wait(10,id=idvar1,iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"error in wait operation"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 11
   end if

   inquire(10,id=idvar1,pending=pending1)

   if(pending1 .neqv. .false.)    error stop 12

   wait(10,id=idvar2,iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"error in wait operation"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 13
   end if

   inquire(10,id=idvar2,pending=pending2)

   if(pending2 .neqv. .false.)   error stop 14

   write(10,*,asynchronous='yes',decimal='comma') outobj1%inn3%r1
   write(10,*,asynchronous='yes')  &
            outobj1%inn3%inn2%g1(1),outobj1%inn3%inn2%g1(3)
   write(10,*,asynchronous='yes')  &
            outobj1%inn3%inn2%g1(4),outobj1%inn3%inn2%g1(2)
   write(10,*,asynchronous='yes',delim='quote') &
            outobj1%inn3%inn2%inn1%c1(3:2:-1)
   write(10,*,asynchronous='yes',delim='apostrophe')  &
            outobj1%inn3%inn2%inn1%c1(1)
   write(10,*,asynchronous='yes') outobj1%inn3%inn2%inn1%i1

   wait(10)

   inquire(10,pending=pending1)

   if(pending1 .neqv. .false.)   error stop 15

   ! back to beginning of first record
   rewind 10

   ! start to read data into outobj2

   read(10,*,asynchronous='yes',id=idvar1) outobj2(1)

   wait(10,id=idvar1,iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"error in wait operation"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 16
   end if

   inquire(10,id=idvar1,pending=pending1)

   if(pending1 .neqv. .false.)    error stop 17

   read(10,*,asynchronous='yes',decimal='comma',id=idvar1) outobj2(2)%x1(2:3)

   wait(10,id=idvar1,iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"error in wait operation"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 18
   end if

   inquire(10,id=idvar1,pending=pending1)

   if(pending1 .neqv. .false.)    error stop 19

   read(10,*,asynchronous='yes') outobj2(2)%x1(1)

   read(10,*,asynchronous='yes',decimal="comma") outobj2(2)%inn3%r1

   read(10,*,asynchronous='yes') outobj2(2)%inn3%inn2

   wait(10,iostat=ios,iomsg=msg)

   if(ios <> 0) then
      print *,"error in wait operation"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 20
   end if

   inquire(10,pending=pending1)

   if(pending1 .neqv. .false.)    error stop 21

   close(10)

end subroutine
