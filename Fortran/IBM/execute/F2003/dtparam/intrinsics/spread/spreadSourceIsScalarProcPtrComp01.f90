!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : spreadSourceIsScalarProcPtrComp01.f
!*
!*  DATE                       : Oct. 16 2008
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
!*  5. SOURCE HAS PROCEDURE POINTER COMPONENT AND DERIVED TYPE COMPONENT ALSO HAS PROCEDURE POINTER COMPONENT
!234567890123456789012345678901234567890123456789012345678901234567890
module m

  interface
     integer function fun1(int)
        integer,intent(in) :: int
     end function
  end interface

   type A(k1,l1)
     integer,kind :: k1
     integer,len  :: l1
     integer(k1)  :: i1(l1)
     procedure(fun1),nopass,pointer :: procptr1=>null()
   end type

   interface
      function fun2()
         import
         type(A(2,:)),allocatable :: fun2
      end function
   end interface

   type B(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     type(A(k2,l2)) :: a1
     procedure(fun2),nopass,pointer :: procptr2=>null()
   end type

end module

program spreadSourceIsScalarProcPtrComp01
  use m
  implicit none

  type(B(2,3)),allocatable :: b1
  type(B(2,3)),pointer     :: b2=>null()
  type(B(2,3)),target      :: b3

  allocate(B(2,3) :: b1)
  b1%a1=A(2,3)(i1=[1,2,3])
  b1%a1%procptr1=>fun1
  b1%procptr2=>fun2

  b3=b1
  b2=>b3

  print *,b1%a1%procptr1(1)

  call verify(spread(b1,1,99),1)
  call verify(spread(b2,1,99),2)
  call verify(spread(b3,1,99),3)

  contains

     subroutine verify(dt,flag)
        type(B(2,*)),intent(in) :: dt(:)
        integer :: i,flag
        type(A(2,:)),allocatable :: tmp


        print *,"test ", flag
        do i=lbound(dt,1),ubound(dt,1)
           if(dt(i)%k2 /= 2)                               error stop 10_4
           if(dt(i)%l2 /= 3)                               error stop 11_4
           if(dt(i)%a1%k1 /= 2)                            error stop 12_4
           if(dt(i)%a1%l1 /= 3)                            error stop 13_4
           if(any(dt(i)%a1%i1 /= [1,2,3] ))                error stop 14_4
           if(.not. associated(dt(i)%a1%procptr1,fun1))    error stop 15_4
           if(.not. associated(dt(i)%procptr2,fun2))       error stop 16_4
           if(dt(i)%a1%procptr1(i) /= 2*i)                 error stop 17_4

           tmp=dt(i)%procptr2()
           if(any(tmp%i1 /= [-1,-2,-3]))                   error stop 18_4
           if(.not. associated(tmp%procptr1,fun1))         error stop 19_4
           if(tmp%procptr1(i) /= 2*i)                      error stop 20_4

        end do

     end subroutine
end program

integer function fun1(int)
  integer,intent(in) :: int
  fun1=2*int
end function

function fun2()
  use m,only : A,B,fun1
  type(A(2,:)),allocatable  :: fun2

  allocate(fun2,source=A(2,3)(i1=[-1,-2,-3]))
  fun2%procptr1=>fun1

end function
