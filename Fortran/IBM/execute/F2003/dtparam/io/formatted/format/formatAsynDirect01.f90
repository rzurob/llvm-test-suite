!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 23 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test asynchronous READ & WRITE with direct access
!* 2. derived type is polymorphic
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
     integer,len :: l1 !l1=2
     character(l1) :: c1(l1:l1+1)
   end type

   type,extends(base) :: child(l2)
     integer,len :: l2 ! l2=3
     character(l1+l2) :: c2(l2:l2+1)
   end type

   type,extends(child) :: gen3(l3)
     integer,len :: l3 ! l3=4
     character(l1+l2+l3) :: c3(l3:l3+1)
   end type

   interface operator(+)
      module procedure dt_plus_dt
   end interface

   contains

    function dt_plus_dt(dt1,dt2)
       class(gen3(*,*,*)),intent(in) :: dt1,dt2
       type(gen3(2*dt1%l1,2*dt1%l2,2*dt1%l3)),allocatable :: dt_plus_dt

       allocate(gen3(2*dt1%l1,2*dt1%l2,2*dt1%l3) :: dt_plus_dt)
       dt_plus_dt%c1=dt1%c1//dt2%c1
       dt_plus_dt%c2=dt1%c2//dt2%c2
       dt_plus_dt%c3=dt1%c3//dt2%c3
    end function

end module

program formatAsynDirect01
  use m
  implicit none

  integer :: ios,record,i,j,idvar(9)
  character(256) :: msg

  class(base(:)),allocatable :: poly1,poly2
  type(gen3(:,:,:)),allocatable :: gen1,gen2(:)

  allocate(gen3(2,3,4) :: poly1,poly2)

  open(10,file='formatAsynDirect01.dat',asynchronous='yes', &
       form='formatted',action='readwrite',access='direct',&
       recl=32,status='replace',iostat=ios,iomsg=msg)

  if(ios /= 0) then
    print *,"fail to open the file"
    print *,"iostat=",ios
    print *,"iomsg=",msg
    stop 11
  end if


  select type(poly1)
     type is(gen3(*,*,*))
         poly1%c1=["12","34"]
         poly1%c2=["abcde","ABCDE"]
         poly1%c3=["greatwork","GREATWORK"]
     class default
       stop 12
  end select

  select type(poly2)
     type is(gen3(*,*,*))
        poly2%c1=["00","11"]
        poly2%c2=["hello","HELLO"]
        poly2%c3=["fantastic","FANTASTIC"]
     class default
       stop 13
  end select


  select type(x=>poly1)
     type is(gen3(*,*,*))
        select type(y=>poly2)
           type is(gen3(*,*,*))
             gen1=x+y
             record=0
             j=0

             ! write 3 copies of data into file,every data has 6 records
             do i=1,3
               write(10,'(a18/a18)',rec=5+record, asynchronous='yes',&
                   id=idvar(j+1),iostat=ios)   gen1%c3

               if(ios /= 0)    error stop 16

               write(10,'(a10/a10)',rec=3+record,asynchronous='yes',&
                   id=idvar(j+2),iostat=ios)   gen1%c2

               if(ios /= 0)    error stop 17

               write(10,'(a4/a4)',rec=1+record,asynchronous='yes',&
                   id=idvar(j+3),iostat=ios)   gen1%c1

               if(ios /= 0)    error stop 18

               record=record + 10
               j=j+3

             end do

           class default
             stop 15
        end select
     class default
        stop 14
  end select

  ! wait data to be transferred
  do i=1,9
      wait(10,id=idvar(i))
  end do

  allocate(gen3(gen1%l1,gen1%l2,gen1%l3) :: gen2(3))


  record=0
  j=0

  ! read data into gen2
  do i=1,3
    read(10,'(a18/a18)',rec=5+record, asynchronous='yes',&
         id=idvar(j+1),iostat=ios) gen2(i)%c3

    if(ios /= 0)    error stop 19

    read(10,'(a10/a10)',rec=3+record,asynchronous='yes',&
         id=idvar(j+2),iostat=ios) gen2(i)%c2

    if(ios /= 0)    error stop 20

    read(10,'(a4/a4)',rec=1+record,asynchronous='yes',&
           id=idvar(j+3),iostat=ios)  gen2(i)%c1

    if(ios /= 0)    error stop 21

    record=record + 10
    j=j+3

 end do

  ! wait data to be transferred
  do i=1,9
      wait(10,id=idvar(i))
  end do

  !output gen2 for verification
  do i=1,3
    write(*, '(2a4/2a10/2a18)')  gen2(i)
  end do

 close(10)

end program
