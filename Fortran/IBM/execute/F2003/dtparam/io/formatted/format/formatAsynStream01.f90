!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatAsynStream01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 21 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. test asynchronous READ & WRITE with stream access method
!*  2. read and write in type-bound procedure
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,l1)
     integer(2),kind :: k1
     integer(4),len :: l1 ! l1=3
     integer(k1) :: i1(l1)
     logical(k1) :: g1(l1)
     character(2*l1+1) :: c1(l1)
     contains
         procedure,pass :: readData
         procedure,pass :: writeData
  end type

     contains

       subroutine readData(this,unit)
          class(dtp(4,*)),intent(inout) :: this
          integer,intent(in) :: unit

          character(256) :: msg
          integer :: iostat,pos

          inquire(unit,pos=pos)
          select type(this)
             type is(dtp(4,*))
                 ! choose data to read
                 read(unit,'(tr2,i1,tr2,i1,tr2,i1,3l2,tr3,a4,tr4,a4,tr3,a3)', &
                  pos=pos,asynchronous='yes',iostat=ios,iomsg=msg) this 
                 
                 if(ios /= 0 ) then
                   print *,"fail to read data"
                   print *,"iostat=",ios
                   print *,"iomsg=",msg
                   stop 16
                 end if

             class default
                stop 15
          end select 
           
       end subroutine 

       subroutine writeData(this,unit)
          class(dtp(4,*)),intent(in)  :: this
          integer,intent(in) :: unit

          integer :: ios,idvar(10)
          character(256) :: msg
      
          select type(this)
            type is(dtp(4,*))
               do i=1,10 
                  ! write records
                  write(unit,'(3i3,3l2,3(a7,:,","))', &
                   asynchronous='yes',id=idvar(i),iostat=ios,iomsg=msg) this 

                  if(ios /= 0) then
                     print *,"fail to write the record"
                     print *,"iostat=",ios
                     print *,"iomsg=",msg
                     stop 13
                  end if
      
               end do

               do i=1,10
                  ! wait specified pending data to be completed
                  wait(unit,id=idvar(i),iostat=ios,iomsg=msg) 
               end do

               if(ios /=0 ) then
                     print *,"error in wait operation"
                     print *,"iostat=",ios
                     print *,"iomsg=",msg
                     stop 14
                  end if  
            class default
               stop 12
          end select
 
       end subroutine

end module

program formatAsynStream01
  use m
  implicit none

  type(dtp(4,3)) :: dtp1
  type(dtp(4,:)),allocatable :: dtp2(:)

  dtp1%i1=[11,12,13]
  dtp1%g1=[.true.,.false.,.true.]
  dtp1%c1=["xlftest","toronto","ontario"]

  allocate(dtp(4,3) :: dtp2(10))

  call sub

  contains

    subroutine sub

     integer :: ios,i

     character(256) :: msg

      open(10,file='formatAsynStream01.dat',status='replace',form='formatted', &
           asynchronous='yes',access='stream',action='readwrite',&
           sign='plus',position='rewind',iostat=ios,iomsg=msg)

      if(ios /= 0) then
        print *,"fail to open the file"
        print *,"iostat=",ios
        print *,"iomsg=",msg
        stop 11
      end if

      ! write 10 records
      call dtp1%writeData(10)
      
      rewind 10
 
      do i=1,10

         ! read records one by one
         call dtp2(i)%readData(10)

      end do

      ! wait all pending data to be transferred for unit 10
      wait(10,iostat=ios,iomsg=msg)

      if(ios /= 0) then
          print *,"error in wait operation"
          print *,"iostat=",ios
          print *,"iomsg=",msg
          stop 17
      end if

      do i=1,10
         ! output the data for verification
         write(*,'(3i3,3l2,tr1,a7,tr1,a7,tr1,a7)') dtp2(i)
      end do

      close(10,status='keep')

    end subroutine

end program
