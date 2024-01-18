!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362181.f
!*
!*  DATE                       : Feb. 12 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 362181
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type A(l1)
     integer,len :: l1
     character(1),pointer :: c1
     character(1) :: c3(1)
  end type
  type B(l2)
     integer,len :: l2
     integer     :: i1(1:3)
     type(A(1)):: a2comp
  end type

  interface assignment(=)
    module procedure assignA,assignB
  end interface

  contains

    subroutine assignA(this,dt)
       class(A(1)),intent(inout) :: this
       type(A(1)),intent(in)     :: dt

       print *,"in assignA"

       this%c1=>dt%c1
       this%c3=dt%c3
    end subroutine

     subroutine assignB(this,dt)
        class(B(2)),intent(inout) :: this(:)
        type(B(2)),intent(in)     :: dt(:)

        print *,"in assignB"

        do i=1,2
           this(i)%i1=dt(i)%i1
           print *,dt(i)%i1,dt(i)%a2comp%c1,dt(i)%a2comp%c3
           this(i)%a2comp=dt(i)%a2comp
           print *,this(i)%i1,this(i)%a2comp%c1,this(i)%a2comp%c3
        end do
     end subroutine

end module

program d362181
   use m

   character(1),target :: c1(7)=["X","L","F","T","E","S","T"]

   type(A(1)),target :: a2comp1

   type(B(2)),target :: b1comp1(2)

   ! call assignA
   a2comp1=A(1)(c1(1),c1(7:7))

   ! call assignB
   b1comp1=[B(2)([1,2,3],a2comp1),B(2)([1,2,3],a2comp1)]

end program

