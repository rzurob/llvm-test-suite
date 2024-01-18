!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsScalarCharComp01.f
!*
!*  DATE                       : Oct. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : SPREAD(SOURCE,DIM,NCOPIES)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.114
!*  2. SOURCE IS SCALAR
!*  3. IF SOURCE IS SCALAR, EACH ELEMENT OF THE RESULT HAS A VALUE EQUAL TO SOURCE
!*  4. IF SOURCE IS SCALAR,THE SHAPE OF RESULT IS (MAX(NCOPIES,0)
!*  5. CONTAINER HAS CHARACTER SCALAR COMPONENT AND DT COMPONENT
!*  6. DEFECT 357409 355186
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type mychar(l1,l2)
    integer,len    :: l1
    integer(2),len :: l2
    character(l1+l2) :: ch1
  end type
end module

module m2
use m1
  type container(l3)
    integer,len    :: l3
    character(l3) :: ch2
    type(mychar(l3-1,l3+1)) :: mychar1
  end type
end module

program spreadSourceIsScalarCharComp01

  use m2
  implicit none

  type(container(3)),target  :: contain1
  type(container(3)),allocatable :: contain2
  type(container(:)),pointer :: contain3(:)=>null()
  type(container(:)),allocatable :: contain4(:)

  contain1=container(3)(ch2="123",mychar1=mychar(2,4)(ch1="xlf"//"test"))

  allocate(contain2,source=container(3)("456",mychar(2,4)("abcdef")) )

  allocate(contain3(-3:-1),source=spread(contain1,1,contain1%l3))

  if(.not. associated(contain3))                error stop 10_4
  if(lbound(contain3,1) /= -3)                  error stop 11_4
  if(ubound(contain3,1) /= -1)                  error stop 12_4
  if(any(shape(contain3) /= 3) )                error stop 13_4
  if(contain3%l3 /= 3)                          error stop 14_4
  if(contain3%ch2%len /= 3)                     error stop 15_4
  if(any(contain3%ch2 /= "123"))                error stop 16_4
  if(contain3%mychar1%l1 /= 2)                  error stop 17_4
  if(contain3%mychar1%l2 /= 4)                  error stop 18_4
  if(contain3%mychar1%ch1%len /= 6)             error stop 19_4
  if(any(contain3%mychar1%ch1 /= "xlftes"))     error stop 20_4

  contain4=spread(contain2,1,5)

  if(.not. allocated(contain4))                 error stop 21_4
  if(lbound(contain4,1) /= 1)                   error stop 22_4
  if(ubound(contain4,1) /= 5)                   error stop 23_4
  if(any(shape(contain4) /= 5) )                error stop 24_4
  if(contain4%l3 /= 3)                          error stop 25_4
  if(contain4%ch2%len /= 3)                     error stop 26_4
  if(any(contain4%ch2 /= "456"))                error stop 27_4
  if(contain4%mychar1%l1 /= 2)                  error stop 28_4
  if(contain4%mychar1%l2 /= 4)                  error stop 29_4
  if(contain4%mychar1%ch1%len /= 6)             error stop 30_4
  if(any(contain4%mychar1%ch1 /= "abcdef"))     error stop 31_4

end program
