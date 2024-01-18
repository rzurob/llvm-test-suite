!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 24 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test asynchronous READ & WRITE with direct access
!*  2. data to be read & writen has the vector subscripts
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type dtp(k1,l1)
     integer,kind :: k1 ! k1=4
     integer,len  :: l1 ! l1=3
     integer(k1)  :: i1(l1)
     real(k1)     :: r1(l1-1)
  end type

  contains

     subroutine allocateDT(dt1,dt2)
        type(dtp(4,*)),intent(in),asynchronous :: dt1(:)
        type(dtp(4,:)),allocatable,intent(out) :: dt2(:)

        allocate(dtp(4,dt1%l1) :: dt2(size(dt1,1)))
     end  subroutine
end module

program formatAsynDirect02
  use m
  implicit none

  integer :: ios,i,idvar1,idvar2
  logical :: pending1,pending2
  character(256) :: msg

  type(dtp(4,3)) :: dtp1(4)

  type(dtp(4,:)),allocatable :: dtp2(:)

  dtp1=[dtp(4,3)(i1=[(i,i=1,3)],r1=[1.23,-1.23]), &
        dtp(4,3)(i1=[(i,i=4,6)],r1=[45.67,-45.67]), &
        dtp(4,3)(i1=[(i,i=-3,-1)],r1=[0.234,-0.234]), &
        dtp(4,3)(i1=[(i,i=-6,-4)],r1=[9.1,-9.1]) ]

  open(10,status='replace',asynchronous='yes',form='formatted',&
       access='direct',recl=48,action='readwrite',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 11
  end if

  ! write data with array dtp1 has vector subscripts
  write(10,'(3i3,2f10.2)', asynchronous='yes', &
      id=idvar1,rec=3,iostat=ios,iomsg=msg) dtp1((/1,3/))

  if(ios /= 0) then
     print *,"error in writing data"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 12
  end if

  ! execute other statement when data is pending
  call allocateDT(dtp1,dtp2)

  ! trigger another write when data is pending
  write(10,'(3i3,2f10.2)', asynchronous='yes', &
        id=idvar2,rec=1,iostat=ios,iomsg=msg) dtp1((/2,4/))

  ! wait first write statement to be completed
  wait(10,id=idvar1)

  ! inquire state of first write
  inquire(10,pending=pending1,id=idvar1)
  if(pending1 .neqv. .false.)     stop 13

  if(ios /= 0)  then
      print *,"error in writing data"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 14
  end if

  ! wait second write to be completed
  wait(10,id=idvar2)

  ! inquire state of second write
  inquire(10,pending=pending2,id=idvar2)
  if(pending2 .neqv. .false.)     stop 15


  ! read data from first record
   read(10,'(3i3,2f10.2)',rec=1,asynchronous='yes',&
        iostat=ios,iomsg=msg)  dtp2((/4,1/))

  if(ios /= 0)  then
      print *,"error in reading data"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 16
  end if

   read(10,'(3i3,2f10.2)',rec=3,asynchronous='yes',&
     iostat=ios,iomsg=msg)  dtp2((/3,2/))

  if(ios /= 0)  then
      print *,"error in reading data"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 17
  end if

  ! wait all data to be transferred for unit 10
  wait(10)

  inquire(10,pending=pending1)
  if(pending1 .neqv. .false.)     stop 18

   ! output the data for verification
   write(*,'(3i3,2f6.2)')  dtp2


  close(10)

end program
