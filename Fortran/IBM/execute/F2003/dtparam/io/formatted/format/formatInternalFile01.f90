!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatInternalFile01.f
!*
!*  DATE                       : Dec. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test read & write from or to internal file(string buffer)
!*  2. sequential data records which contains derived type information are read into character array buffer and read data out from buffer into derived type.
!*  3. internal file is default character array
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind  :: k1
     integer,len   :: l1
     integer(k1)   :: i(k1) !k1=2
     character(l1) :: c(l1) !l1=3
     logical(k1)   :: log(l1)
     real(k1+k1)   :: r(l1)
  end type

end module

program formatInternalFile01
  use m
  implicit none

  type(base(2,:)),allocatable :: base1,base2
  character(60) :: buffer(4)
  integer :: ios,i

  allocate(base(2,3) :: base1,base2)

  base1%i=[10,-20]
  base1%c=["xlf","ibm","xlc"]
  base1%log=[.true.,.false.,.true.]
  base1%r=[2.34,1.345E+02,-99.12]

  open(10,file="formatInternalFile01.dat",form='formatted',&
       access='sequential',sign='suppress',iostat=ios)

  if(ios /= 0) then
    print *,"fail to open the file, iostat=",ios
    stop 10
  else
     write(10,'(sp,2i4,/3a3/3l3/f5.2,e12.3,f7.2)') base1
  end if

  rewind(10)

  read(10,'(a30/a30/a30/a30)')  (buffer(i),i=1,4)

  do i=1,4
     write(*,'(a30)') buffer(i)
  end do

  read(buffer,'(2i4/3a3/3l3/f5.2,e12.3,f7.2)') base2

  write(*,'(sp,2i4,/3a3/3l3/f5.2,e12.3,f7.2)') base2

  close(10)

end program
