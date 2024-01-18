!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatAsynSequential02.f   
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
!*  1. test Asynchronous READ with sequential access
!*  2. derived type is polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
      integer,len :: l1 ! l1=2
      character(l1) :: c1(l1:l1+1)
   end type

   type,extends(base) :: child(l2)
      integer,len :: l2 ! l2=3
      character(l1+l2) :: c2(l2:l2+1)
   end type

   contains

       function allocatePtr(dt)
          ! The base object of a variable shall have Asynchronous attribute in a scoping unit if
          ! 1)the variable appears in an executable statement or specification expression in that scoping unit and
          !  2) any statement of the scoping unit is execuated while variable is a pending I/O storage sequence affector 

          class(child(*,*)),asynchronous,intent(in) :: dt(2:) 
          class(base(2*dt%l1)),pointer :: allocatePtr(:)

          allocate(child(2*dt%l1,2*dt%l2) :: allocatePtr(size(dt)) )

       end function

end module

program formatAsynSequential02
  use m
  implicit none

  integer :: unit,ios,idvar
  character(256) :: msg

  class(base(2)),allocatable :: base1(:)
  class(base(:)),pointer :: base2(:)
 
  allocate(child(2,3) :: base1(-1:0))

  unit=1024

  open(unit,file='formatAsynSequential02.dat',status='old', &
        form='formatted',access='sequential',asynchronous='yes', &
        action='read',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     
     stop 10
  end if

  select type(x=>base1)
    type is(child(*,*))
        ! trigger asynchronous IO read
        read(unit,'(tr1,a2,tr2,a2,/tr2,a5,tl3,a5)',id=idvar, &
               asynchronous='yes',iostat=ios,iomsg=msg) x 

        if(ios /= 0)         stop 12

        ! execute other statement before wait
        allocate(base2(0:1),source=allocatePtr(x))
       
        wait(unit,id=idvar,iostat=ios)

        if(ios /= 0)         stop 13        

        write(*,'(a)')  "Value of base1:"
        write(*,'(a)')       x

        select type(y=>base2)
            type is(child(*,*))
                if(y%l1 /= 4)         stop 15
                if(y%l2 /= 6)         stop 16 

                ! back 2 records

                backspace unit 
                backspace unit
                
                ! read from third record
                read(unit,'(a4,a4/a10,a10)', &
                           asynchronous='yes',id=idvar) y(0)
               
                wait(unit,id=idvar)

                rewind unit

                ! read from first record 
                read(unit,'(a4/a4/tr15,a10,/tr15,a10)',asynchronous='yes') y(1)
                
                wait(unit)

                write(*,'(a)')  "Value of base2:"
                write(*,'(a)')       y 

            class default
                stop 14
        end select
    class default

      stop 11

  end select
  
  close(unit)  

end program
