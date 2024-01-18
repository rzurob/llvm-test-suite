!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 19 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test READ & WRITE with non-advancing IO, use stream access
!*  2. derived type is polymorphic
!*  3. read or write in subroutine
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(k1,l1)
      integer,kind :: k1 ! k1=4
      integer,len  :: l1 ! l1=2
      real(k1)     :: r1(l1)
      logical(k1)  :: g1(k1)
   end type

   type,extends(base) :: child(l2)
      integer,len     :: l2 ! l2=3
      character(k1+l2) :: c1(l1:l2)
      integer(k1)     :: i1(l2:k1)
   end type

   contains

     subroutine writeData1(unit,dt)
        class(*),intent(in) :: dt(2:)
        integer,intent(in) :: unit

! this is the data we write(7 records), we will optionally read it back
!******* +2.678-13.45
!*******   F   T   F   T
!*******TORONTOMARKHAM
!******* +15 -16 -2.678+13.45
!*******   T   F   T   F
!******* CANADAONTARIO
!******* -13 +14


        select type(dt)
           type is(child(4,*,*))
                write(unit,fmt='(f7.3,f6.2/4l4/2a7/2i4)',advance='no') dt(2)
                write(unit,fmt='(f7.3,f6.2/4l4/2a7/2i4)',advance='no') dt(3)
           class default
              stop 12
        end select
     end subroutine

     subroutine readData1(unit,dt)
        class(*),intent(inout) :: dt(4:)
        integer,intent(in)   :: unit
        integer :: ios,pos,eor,count

        select type(dt)
           type is(child(4,*,*))
              ! read r1, optinally choose data by using tab edit descriptor
              read(10,'(t3,f4.2,tr4,f3.2/)',advance='no',iostat=ios,&
                       size=count)                 dt(4)%r1
              if(count /= 7)                stop 14

              ! inquire current position
              inquire(10,pos=pos)
              if(pos /= 15 )                stop 15

              ! we reached end of first record
              ! backspace will bring in beginning of first record

              backspace 10

              ! skip first record, read second record
              read(10,'(2l4)',advance='no',pos=16,size=count) dt(4)%g1(1:2)
              if(count /= 8)                stop 16

              ! inquire current position
              inquire(10,pos=pos)
              if(pos /=24)                   stop 17

              ! back to beginning of second record
              backspace 10

              ! read data into g1(3:4)
              read(10,'(2l4)',advance='no',pos=16,size=count) dt(4)%g1(3:4)
              if(count /= 8)                stop 18

              inquire(10,pos=pos)
              if(pos /= 24)                 stop 19

              !let's read third record
              !skip some character,optionally choose data to read

              read(10,'(t6,a2,tr4,a3)',advance='no',iostat=ios, &
                  eor=100,pos=32,size=count)                    dt(4)%c1

              inquire(10,pos=pos)
              if(pos /= 46)                stop 20


              ! start to read dt(4)%i1 in fourth record
              read(10,'(t3,i2,tr1,i2,tr1)',advance='no', &
                        pos=47,size=count)                      dt(4)%i1
              if(count /= 4)               stop 21

              inquire(10,pos=pos)
              if(pos /= 55)                stop 22


              ! start to read dt(5),in fourth record
              ! optional choose data to use and skip record marker
              read(10,'(tr1,f4.1,tr2,f5.1,tr2)',advance='no',&
                       pos=55,size=count,eor=101)                      dt(5)%r1
              if(count /= 9)               stop 23

              inquire(10,pos=pos)
              if(pos /= 69)                stop 24

              ! read whole fifth record,and skip the end of record marker
              read(10,'(4l4,tr1)',advance='no',pos=69,size=count) dt(5)%g1

              if(count /= 16)              stop 25

              inquire(10,pos=pos)
              if(pos /= 86)                stop 26


              ! read the sixth record
              read(10,'(t2,a3,tr5,a4,tr2)',advance='no', &
                   pos=86,size=count) dt(5)%c1

              if(count /= 7)               stop 27

              inquire(10,pos=pos)
              if(pos /= 101)               stop 28

              ! read the seventh record
              read(10,'(tr2,i2,tr2,i2)',advance='no',pos=101, &
                   size=count) dt(5)%i1

              if(count /= 4)              stop 29


              return

100           print *,"end of record reached,iostat=",ios
              stop  30

101           print *,"end of record reached,iostat=",ios
              stop  31

           class default
              stop 11
        end select

     end subroutine

end module

program formatNonadvancingStream01
  use m
  implicit none

  integer :: ios
  character(300) :: msg

  class(*),pointer :: upoly(:)=>null()
  class(base(4,:)),target,allocatable :: poly(:)

  allocate(poly(-1:0),source= &
           [child(4,2,3)([-2.678,13.45],[.true.,.false.,.true.,.false.],&
             [" CANADA","ONTARIO"],[-13,14]),&
            child(4,2,3)([2.678,-13.45],[.false.,.true.,.false.,.true.],&
             ["TORONTO","MARKHAM"],[15,-16]) ] )

   upoly(3:) => poly

   open(unit=10,file="formatNonadvancingStream01.dat",form='formatted',&
        action='readwrite',access='stream',position='rewind',&
        status='replace',sign='plus',iostat=ios,iomsg=msg)

   if(ios /= 0) then

      print *,"error in opening the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10
   end if

   select type(x=>upoly(4:3:-1))
      type is(child(4,*,*))
         ! write data into file
         call writeData1(10,x)

         rewind 10

         call readData1(10,x)

      class default
         stop 13
   end select

   select type(poly)
      type is(child(4,*,*))
         write(*,'(f7.3,f6.2/4l4/2a7/2i4,:,/)',advance='yes')  poly
      class default
         stop 32
   end select

   close(10,iostat=ios,status='keep')

   if(ios /= 0) then
       print *,"error in closing the file"
       print *,"iostat=",ios
       stop 33
   end if

end program
