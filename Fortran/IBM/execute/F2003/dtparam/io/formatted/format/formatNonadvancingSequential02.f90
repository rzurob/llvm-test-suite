!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatNonadvancingSequential02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 18 2008 
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
!* 1. read or write with non-advancing IO
!* 2. derived type has nested derived type component and derived type has sequence property
!* 3. 1 more data is appended at end of file 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(k1,l1)
      integer,kind :: k1  ! k1=2 
      integer,len  :: l1  ! l1=3
      sequence
      integer(k1)  :: i1(l1)
      logical(k1)  :: g1(l1:l1+1)
      real(2*k1) :: r1
   end type

   type outer(l2)
      integer,len  :: l2 ! l2=3
      sequence
      character(l2) :: c1
      type(inner(2,l2)) :: comp1 
   end type

end module

program formatNonadvancingSequential02
  use m
  implicit none

  integer :: ios,count

  character(300) :: msg
  character(10)  :: tmpchar

  real :: r1

  type(outer(:)),allocatable :: outer1(:)

  allocate(outer(3) :: outer1(2))

  open(unit=10,file='formatNonadvancingSequential02.dat',&
       form='formatted',action='readwrite',status='old',&
       access='sequential',position='append',iomsg=msg,iostat=ios)

  if(ios /= 0)  then
    print *,"error to open the file"
    print *,"iostat=",ios
    print *,"iomsg=",msg
    return
  end if

  ! add 1 more value in data file before start to read
  write(10,'(f5.2)',advance='no') -5.32

  rewind 10

  !start to read c1
  read(10,'(a3)',advance='no',eor=10,size=count,iostat=ios)  outer1(1)%c1
  
  if(count /= 3)         stop 11
  
  ! read i1, use TAB control edit descriptor 
  read(10,'(t2,bz,i3,tl1,i3,tl5,bn,i5)', &
        advance='no',eor=12,size=count,iostat=ios) outer1(1)%comp1%i1 

  if(count /= 11)        stop 13

  ! read g1
  read(10,'(2l2)',advance='no', &
           eor=14,size=count,iostat=ios) outer1(1)%comp1%g1

  if(count /= 4)         stop 15

  ! read r1
  read(10,'(/f4.1)',advance='no', &
        eor=16,size=count,iostat=ios) outer1(1)%comp1%r1 

  if(count /= 4)         stop 17 

  rewind 10

  ! read all data together with different format
  read(10,'(a3,3i2,l4,tl4,l4,/f5.2)',advance='no',&
      eor=18,size=count,iostat=ios)       outer1(2) 

  if(count /= 22)        stop 19

  write(*,'(2(a3/3i4/2l4/f5.2,:/))',advance='no') outer1

  endfile(10)

  close(unit=10,status='keep',iostat=ios)

  if(ios /= 0) then
     print *,"fail to close the file,iostat=",ios
     return
  end if 

  return

10 print *,"oops,end of file is reached!,iostat=",ios
   stop 10 
12 print *,"oops,end of file is reached!,iostat=",ios
   stop 12
14 print *,"oops,end of file is reached!,iostat=",ios
   stop 14
16 print *,"oops,end of file is reached!,iostat=",ios
   stop 16
18 print *,"oops,end of file is reached!,iostat=",ios
   stop 18 

end program
