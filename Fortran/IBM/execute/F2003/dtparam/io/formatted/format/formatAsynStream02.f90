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
!* 1. test asynchronous READ & WRITE with stream access & nonadvancing IO
!* 2. derived type is polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1 ! l1=2
      character(l1+1) :: c1(l1)
   end type

   type,extends(base) :: child(k2,l2)
      integer,kind :: k2 ! k2=4
      integer,len  :: l2 ! l2=3
      integer(k2)  :: i1(k2)
      logical(k2)  :: g1(l1:l2)
   end type

end module

program formatAsynStream02
  use m
  implicit none

  interface

    subroutine readData(arg,unit)
       import
       class(base(*)),intent(inout),target :: arg(2:)
       integer,intent(in)  :: unit
    end subroutine

  end interface

  integer :: ios,idvar
  character(256) :: msg

  class(base(:)),allocatable,target :: tar1(:)

  class(base(:)),pointer  :: ptr1(:)=>null()

  allocate(child(2,4,3) :: tar1(2))

  ptr1=>tar1

  open(unit=1024,file='formatAsynStream02.dat',form='formatted',&
       action='readwrite',access='stream',status='old', &
       position='append',asynchronous='yes',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 11
  end if

  select type(tar1)
      type is(child(*,4,*))
         ! append one more record before read data
         write(1024,'(a)',advance='yes', &
               asynchronous='yes',id=idvar) ".true.,.false."

         wait(1024,id=idvar)

         ! point to the beignning of the first record
         rewind 1024

         ! start to read
         call readData(tar1,1024)

      class default
         stop 12
  end select

  select type(ptr1)
      type is(child(*,4,*))
          ! write results for verification
          write(*,'(2a3,4i5,2l1)',advance='yes',asynchronous='no') ptr1
      class default
         stop 26
  end select

  close(10)


end program

! following is the records we want to read
!ABCDEFGHIJKLMNOPQRSTUVWXYZ
!123456789012345789
!truefalsetreefall
!abcdefghijklmnopqrstuvwxyz
!-1 2 -3 4 - 5 6 - 7 8 - 9 0
!.true.,.false.

subroutine readData(arg,unit)
   use m
   class(base(*)),intent(inout),target :: arg(2:)
   integer,intent(in)  :: unit

   integer :: ios,pos
   character(256) :: msg

   select type(arg)
      type is(child(*,4,*))

        inquire(unit,pos=pos)
        if(pos /= 1)           stop 14

        read(unit,'(tr5,a3,tr5,a3,tr11)',advance='no',pos=pos, &
             asynchronous='yes',id=idvar,iostat=ios,iomsg=msg) arg(2)%c1

        if(ios /= 0) then
            print *,"fail to read data"
            print *,"iostat=",ios
            print *,"iomsg=",msg
            stop 15
        end if

        wait(unit,id=idvar)

        inquire(unit,pos=pos)

        if(pos /= 28)      stop 16

        ! read second record
        read(unit,'(4i3,tr7)',advance='no',pos=pos , &
            asynchronous='yes',id=idvar,iostat=ios,iomsg=msg) arg(2)%i1

        if(ios /= 0) then
            print *,"fail to read data"
            print *,"iostat=",ios
            print *,"iomsg=",msg
            stop 17
        end if

        wait(unit,id=idvar)

        inquire(unit,pos=pos)
        if(pos /= 47)      stop 18

        ! read third record
        read(unit,'(tr4,l1,tr4,l1,tr8)',advance='no',pos=pos , &
            asynchronous='yes',id=idvar,iostat=ios,iomsg=msg) arg(2)%g1

        wait(unit,id=idvar)

        if(ios /= 0) then
            print *,"fail to read data"
            print *,"iostat=",ios
            print *,"iomsg=",msg
            stop  19
        end if

        inquire(unit,pos=pos)
        if(pos /= 65)           stop 20

        ! read fourth record
        read(unit,'(tr5,a3,tr5,a3,tr11)',advance='no',pos=pos, &
             asynchronous='yes',id=idvar,iostat=ios,iomsg=msg) arg(3)%c1

        if(ios /= 0) then
            print *,"fail to read data"
            print *,"iostat=",ios
            print *,"iomsg=",msg
            stop 21
        end if

        wait(unit,id=idvar)

        inquire(unit,pos=pos)

        if(pos /= 92)      stop 22

        ! read fifth record
        read(unit,'(bz,i4,i5,2i6,tr7)',advance='no',pos=pos , &
            asynchronous='yes',id=idvar,iostat=ios,iomsg=msg) arg(3)%i1

        if(ios /= 0) then
            print *,"fail to read data"
            print *,"iostat=",ios
            print *,"iomsg=",msg
            stop 23
        end if

        wait(unit,id=idvar)

        inquire(unit,pos=pos)
        if(pos /= 120)      stop  24

        ! read sixth record
        read(unit,'(l6,tr1,l7)',advance='no',pos=pos , &
            asynchronous='yes',id=idvar,iostat=ios,iomsg=msg) arg(3)%g1

        wait(unit,id=idvar)

        if(ios /= 0) then
            print *,"fail to read data"
            print *,"iostat=",ios
            print *,"iomsg=",msg
            stop  25
        end if

      class default
          stop 13
   end select

end subroutine
