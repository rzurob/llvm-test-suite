!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsScalarDTComp01.f
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
!*  5. COMPONENT IS DERIVED TYPE SCALAR
!*  6. DEFECT 357409 357486
!234567890123456789012345678901234567890123456789012345678901234567890

module m1
  type first(l1)
    integer,len :: l1
    character(l1) :: c1
  end type
  type second(l2)
    integer,len :: l2
    type(first(l2)) :: first1
  end type
  type third(l3)
    integer(2),len :: l3
    type(third(l3)),pointer :: third1=>null()
    type(second(l3)) :: second1
  end type
end module

module m2
use m1
   type container(l1,l2)
      integer,len :: l1,l2
      type(first(l1))  :: first2
      type(second(l2)) :: second2
      type(third(:)),allocatable   :: third2
   end type
end module

program spreadSourceIsScalarDTComp01

  use m2
  implicit none

  type(container(2,3)),target  :: contain1
  type(container(2,3)),allocatable :: contain2
  type(container(2,3)),pointer     :: contain3=>null()

  contain1=container(2,3)(first2=first(2)(c1="123"), &
               second2= second(3)(first1=first(3)(c1="456")), &
               third2=third(4)(null(), &
                      second(4)(first1=first(4)(c1="0000") ) ) )

  allocate(contain1%third2%third1,source=contain1%third2)

  contain2=contain1

  contain3=>contain1

  call verify(spread(contain1,1,3),1)
  call verify(spread(contain2,1,3),2)
  call verify(spread(contain3,1,3),3)

  contains

   subroutine verify(dt,flag)
      type(container(*,*)),intent(in) :: dt(:)
      integer,intent(in) :: flag

      print *,"test ",flag
      if(size(dt) /= 3)                              error stop 10_4
      if(dt%l1 /= 2)                                 error stop 11_4
      if(dt%l2 /= 3)                                 error stop 12_4
      if(dt%first2%l1 /= 2)                          error stop 13_4
      if(dt%first2%c1%len /= 2)                      error stop 14_4
      if(any(dt%first2%c1 /= "12"))                  error stop 15_4
      if(dt%second2%l2 /= 3)                         error stop 16_4
      if(dt%second2%first1%l1 /=3)                   error stop 17_4
      if(dt%second2%first1%c1%len /= 3)              error stop 18_4
      if(any(dt%second2%first1%c1 /= "456"))         error stop 19_4
      if(dt(1)%third2%l3 /= 4)                       error stop 20_4
      if(.not. associated(dt(2)%third2%third1))      error stop 21_4
      if(dt(2)%third2%second1%l2 /= 4)               error stop 22_4
      if(dt(1)%third2%second1%first1%c1 /= "0000" )  error stop 23_4

   end subroutine


end program
