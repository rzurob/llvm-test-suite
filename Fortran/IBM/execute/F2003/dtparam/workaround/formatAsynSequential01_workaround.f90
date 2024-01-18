!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 21 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test asynchronous IO with sequential access method
!* 2. derived type has nested component which has sequence property
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner(k1,l1)
     integer,kind :: k1 ! k1=2
     integer,len  :: l1 ! l1=3
     sequence
     complex(2*k1) :: x1
     character(l1+1) :: c1(l1)
  end type

  type outer(k2,l2)
     integer,kind :: k2 ! k2=4
     integer,len  :: l2 ! l2=2
     integer(k2)  :: i1(k2)
     logical(k2)  :: g1(l2)
     type(inner(k2,k2-l2+1)) :: comp
  end type

  contains

    subroutine readData(unit,dt)
       integer,intent(in) :: unit
       type(outer(4,2)),asynchronous,intent(inout) :: dt(:)
       integer :: idvar,ios

       ! asynchronous read without ID= specifer
       read(unit,fmt='(4i5)',asynchronous="yes") dt(1)%i1
       read(unit,fmt='(2l2)',asynchronous="yes") dt(1)%g1
       read(unit,fmt='(f7.2,f7.3)',asynchronous="yes") dt(1)%comp%x1
       read(unit,fmt='(3a4)',asynchronous="yes") dt(1)%comp%c1

       ! execute other statement
       if(dt%l2 /= 2)        stop 17
       if(dt%comp%l1 /= 3)   stop 18

       ! wait all pending data to be transfered for unit 10
       wait(unit)

       ! read with ID= specifer
       read(unit,fmt='(4i5/2l2/f7.2,f7.3/3a4)', iostat=ios, &
            asynchronous="yes",id=idvar) dt(2)

       if(ios /= 0)          stop 19

       ! execuate other statement when pending
       call writeDT(dt(1))

       ! wait specified pending data to be completed
       wait(unit,id=idvar)

       call writeDT(dt(2))

       contains

             subroutine writeDT(dt)
                type(outer(4,2)),intent(in) :: dt

                write(*,'(4i5/2l2/f7.2,f7.3/3a4)') dt
             end subroutine
    end subroutine

end module

program formatAsynSequential01
  use m
  implicit none

  integer :: ios,idvar1,idvar2
  character(256) :: msg
  logical :: pending1,pending2
  character(10) :: asychar1,asychar2

  type(outer(4,:)),pointer :: outer1(:)=>null(),outer2(:)=>null()


  allocate(outer(4,2) :: outer1(-3:-2),outer2(2))

  outer1(-3)=outer(4,2)(i1=[11,-12,13,-14], &
                      g1=[.false.,.true.],&
                      comp=inner(4,3)(x1=(-3.56,1.2345),&
                                      c1=["ABCD","EFGH","IJKL"]) )

  outer1(-2)=outer(4,2)(i1=[21,-22,23,-24], &
                      g1=[.true.,.false.],&
                      comp=inner(4,3)(x1=(223.56,-0.00245),&
                                      c1=["abcd","efgh","ijkl"]) )


  open(10,form='formatted', status='scratch', &
       action='readwrite',position='rewind',access='sequential', &
       asynchronous='yes',iostat=ios,iomsg=msg)

  if( ios <> 0) then

     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg

     stop 10
  end if

  ! write data with ID= specifier
  write(10,id=idvar1,asynchronous="yes",fmt=100,iostat=ios)  outer1(-3)

  if(ios /= 0)  stop 11

  write(10,id=idvar2,asynchronous="yes",fmt=100,iostat=ios)  outer1(-2)

  if( ios /= 0) stop 12

  ! wait 2 write statement to complete
  wait(10,id=idvar1)
  wait(10,id=idvar2)


  inquire(10,pending=pending1,asynchronous=asychar1,id=idvar1)
  inquire(10,pending=pending2,asynchronous=asychar2,id=idvar2)

  if(asychar1 /= 'YES')                stop 13
  if(asychar2 /= 'YES')                stop 14

  if(pending1 .neqv. .false.)          stop 15
  if(pending2 .neqv. .false.)          stop 16


  rewind 10

  call readData(10,outer2)

100 format(sp,4i5.3,/2l2,/f7.2,f7.3/3a4)

  close(10)


end program
