!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 15 2008
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
!*  5. COMPONENT IS DERIVED TYPE ARRAY
!*  6. DEFECT 357496
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
    type(second(l3)) :: second1
  end type
end module

module m2
use m1
   type container(l1,l2)
      integer,len :: l1,l2
      type(first(l1))  :: first2(l1)
      type(second(l2)) :: second2(l1:l2)
      type(third(:)),allocatable   :: third2(:)
   end type
end module

program spreadSourceIsScalarDTComp02

  use m2
  implicit none

  type(container(2,3)),target  :: contain1
  type(container(2,3)),allocatable :: contain2
  type(container(2,3)),pointer     :: contain3=>null()

  contain1=container(2,3)( &
           [first(2)(c1="123"),first(2)(c1="456")], &
          [second(3)(first1=first(3)(c1="aaaa") ),&
                   second(3)(first1=first(3)(c1="bbbb") ) ] , null())

  allocate(contain1%third2(-2:-2),source=third(4)( &
                   second1=second(4)(first1=first(4)(c1="xlftest") ) ) )

  contain2=contain1

  contain3=>contain1

  call verify(spread(contain1,1,3),1)
  call verify(spread(contain2,1,3),2)
  call verify(spread(contain3,1,3),3)

  contains

     subroutine verify(dt,flag)
         type(container(*,*)),intent(in) :: dt(:)
         integer,intent(in) :: flag
         integer :: i

         print *,"test ",flag

         do i=lbound(dt,1),ubound(dt,1)
            if(dt(i)%l1 /= 2)                                 error stop 10_4
            if(dt(i)%l2 /= 3)                                 error stop 11_4
            if(lbound(dt(i)%first2,1) /= 1)                   error stop 12_4
            if(ubound(dt(i)%first2,1) /= 2)                   error stop 13_4
            if(dt(i)%first2%l1 /= 2)                          error stop 14_4
            if(any(dt(i)%first2%c1 /= ["12","45"]))           error stop 15_4
            if(dt(i)%second2%l2 /= 3)                         error stop 16_4
            if(lbound(dt(i)%second2,1) /= 2)                  error stop 17_4
            if(ubound(dt(i)%second2,1) /= 3)                  error stop 18_4
            if(any(dt(i)%second2%first1%c1 /= ["aaa","bbb"])) error stop 19_4
            if(lbound(dt(i)%third2,1) /= -2)                  error stop 20_4
            if(ubound(dt(i)%third2,1) /= -2)                  error stop 21_4
            if(dt(i)%third2%l3 /= 4)                          error stop 22_4
            if(dt(i)%third2%second1%l2 /= 4)                  error stop 23_4
            if(dt(i)%third2%second1%first1%l1 /= 4)           error stop 24_4
            if(dt(i)%third2(-2)%second1%first1%c1 /= "xlft")  error stop 25_4
         end do
     end subroutine
end program
