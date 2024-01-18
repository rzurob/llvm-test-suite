!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatInternalFile03.f
!*
!*  DATE                       : Dec. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test READ & WRITE in internal file
!*  2. derived type has nested derived type component, and both derived type has sequence property
!*  3. internal file is default character array pointer with deffered length parameter
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type inner(k1,l1)
     integer,kind  :: k1 !k1=2
     integer,len   :: l1 !l1=3
     sequence
     logical(k1)   :: log(l1)
     real(k1+k1)   :: r(l1)
  end type

  type outer(k2,l2)
    integer,kind  :: k2
    integer,len   :: l2
    sequence
    integer(k2)   :: i(k2) !k2=2
    character(l2) :: c(l2) !l2=3
    type(inner(2,l2)) :: comp(l2-1)
  end type

end module

program formatInternalFile03
  use m
  implicit none

  type(outer(2,:)),allocatable :: outer1,outer2
  character(:),pointer :: buffer(:)
  character(60),target :: ctar(6)
  integer :: ios,i


  allocate(outer(2,3) :: outer1,outer2)

  buffer(0:5)=>ctar

  outer1%i=[10,-20]
  outer1%c=["xlf","ibm","xlc"]
  outer1%comp(1)%log=[.true.,.false.,.true.]
  outer1%comp(1)%r=[2.34,1.345E+02,-99.12]
  outer1%comp(2)%log=[.false.,.true.,.false.]
  outer1%comp(2)%r=[1.23,1.234E+02,-88.23]

  write(buffer,'(sp,2i4,/3a3/,2(3l3/,f5.2,e12.3,f7.2,:,/) )') outer1

  do i=0,5
     write(*,'(a30)') buffer(i)
  end do

  read(buffer,'(2i4/3a3/,2(3l3/f5.2,e12.3,f7.2,:/))') outer2
  write(*,'(sp,2i4,/3a3/,2(3l3/f5.2,e12.3,f7.2,:/))') outer2

end program
