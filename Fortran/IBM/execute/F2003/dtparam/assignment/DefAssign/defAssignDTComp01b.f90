!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDTComp01a.f
!*
!*  DATE                       : Feb. 2 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Defined assignment with generic binding
!* 2. Defined subroutines are elemental subroutines
!* 3. Derived type has DT components
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type A(l1)
     integer,len :: l1 ! l1=2
     integer :: i1(l1)=-99
     contains
        procedure,pass :: assignA
        generic :: assignment(=) => assignA
  end type

  type B(l2)
     integer,len :: l2 ! l2=4
     character(l2) :: c1(l2)="****"
     contains
          procedure,pass :: assignB
          generic :: assignment(=)=> assignB
  end type

  type C(l3)
     integer,len :: l3  ! l3=3
     logical     :: g1(l3)=.false.
     type(A(l3-1)) :: a1comp(l3-1:l3)
     type(B(l3+1)) :: b1comp(l3:l3+1)
     contains
         procedure,pass :: assignC
         generic :: assignment(=) => assignC
  end type

  contains

     elemental subroutine assignA(this,arg)
         class(A(*)),intent(inout) :: this
         class(*),intent(in) :: arg
         select type(arg)
            type is(A(*))
               this%i1=arg%i1
            type is(integer)
               this%i1=arg
         end select
     end subroutine

     elemental subroutine assignB(this,arg)
         class(B(*)),intent(inout) :: this
         class(*),intent(in) :: arg
         select type(arg)
            type is(B(*))
               this%c1=arg%c1
            type is(character(*))
               this%c1=arg
         end select
     end subroutine

     elemental subroutine assignC(this,arg)
         class(C(*)),intent(inout) :: this
         class(C(*)),intent(in) :: arg

         this%g1=arg%g1

         this%a1comp=arg%a1comp  ! call assignA

         this%b1comp=arg%b1comp  ! call assignB

     end subroutine

end module

program defAssignDTComp01a
     use m
     implicit none

     call sub

end program

subroutine sub
  use m

  implicit type(A(:)) (A)
  implicit type(B(4)) (B)
  implicit type(C(:)) (C)

  allocatable :: a1(:),b1(:),c1(:)

  allocate(A(2) :: a1(2:3))
  allocate(b1(3:4))
  allocate(C(3) :: c1(2))

  a1=[A(2)([-11,-12]) ,A(2)([11,12])] ! call assignA

  if(any(a1(2)%i1 /= [-11,-12]))                         stop 11
  if(any(a1(3)%i1 /= [11,12]))                           stop 12

  a1(2)=A(2)([-1,-2])  ! call assignA
  a1(3)=A(2)([1,2]) ! call assignA

  if(any(a1(2)%i1 /= [-1,-2]))                           stop 13
  if(any(a1(3)%i1 /= [1,2]))                             stop 14

  a1=[10,-10]   ! call assignA

  if(any(a1(2)%i1 /= 10))                                stop 15
  if(any(a1(3)%i1 /= -10))                               stop 16

  ! call assignB
  b1=[B(4)(["abc","def","ghi","jkl"]),B(4)(["ABC","DEF","GHI","JKL"])]

  if(any(b1(3)%c1 /= ["abc","def","ghi","jkl"]))          stop 17
  if(any(b1(4)%c1 /= ["ABC","DEF","GHI","JKL"]))          stop 18

  ! switch b1(4) and b1(5)
  bb=b1(3)    ! call assignB
  b1(3)=b1(4) ! call assignB
  b1(4)=bb    ! call assignB

  if(any(b1(4)%c1 /= ["abc","def","ghi","jkl"]))          stop 19
  if(any(b1(3)%c1 /= ["ABC","DEF","GHI","JKL"]))          stop 20

  b1(3)="XYZ|"     ! call assignB
  b1(4)="xyz|"     ! call assignB

  if(any(b1(3)%c1 /= "XYZ|"))                             stop 21
  if(any(b1(4)%c1 /= "xyz|"))                             stop 22

  ! call assignC
  c1=[ C(3) (g1=[.true.,.false.,.true.],a1comp=a1,b1comp=b1) , &
       C(3) ([.false.,.true.,.false.], &
            [A(2)([5,-5]),A(2)([6,-6])], &
            [B(4)(["XLF","IBM","XLC","LAB"]), &
             B(4)(["xlf","ibm","xlc","lab"]) ]) ]

  if(any(c1(1)%g1 .neqv. [.true.,.false.,.true.] ))        stop 23
  if(any(c1(1)%a1comp(2)%i1 /= 10 ))                       stop 24
  if(any(c1(1)%a1comp(3)%i1 /= -10 ))                      stop 25
  if(any(c1(1)%b1comp(3)%c1 /= "XYZ|"))                    stop 26
  if(any(c1(1)%b1comp(4)%c1 /= "xyz|"))                    stop 27

  if(any(c1(2)%g1 .neqv. [.false.,.true.,.false.] ))       stop 28
  if(any(c1(2)%a1comp(2)%i1 /= [5,-5] ))                   stop 29
  if(any(c1(2)%a1comp(3)%i1 /= [6,-6] ))                   stop 30
  if(any(c1(2)%b1comp(3)%c1 /= ["XLF","IBM","XLC","LAB"])) stop 31
  if(any(c1(2)%b1comp(4)%c1 /= ["xlf","ibm","xlc","lab"])) stop 32

  c1=c1(2:1:-1)  ! call assignC

  if(any(c1(2)%g1 .neqv. [.true.,.false.,.true.] ))        stop 33
  if(any(c1(2)%a1comp(2)%i1 /= 10 ))                       stop 34
  if(any(c1(2)%a1comp(3)%i1 /= -10 ))                      stop 35
  if(any(c1(2)%b1comp(3)%c1 /= "XYZ|"))                    stop 36
  if(any(c1(2)%b1comp(4)%c1 /= "xyz|"))                    stop 37

  if(any(c1(1)%g1 .neqv. [.false.,.true.,.false.] ))       stop 38
  if(any(c1(1)%a1comp(2)%i1 /= [5,-5] ))                   stop 39
  if(any(c1(1)%a1comp(3)%i1 /= [6,-6] ))                   stop 40
  if(any(c1(1)%b1comp(3)%c1 /= ["XLF","IBM","XLC","LAB"])) stop 41
  if(any(c1(1)%b1comp(4)%c1 /= ["xlf","ibm","xlc","lab"])) stop 42

  a1=c1(1)%a1comp  ! call assignA
  b1=c1(1)%b1comp  ! call assignB

  if(any(a1(2)%i1 /= [5,-5] ))                             stop 43
  if(any(a1(3)%i1 /= [6,-6] ))                             stop 44
  if(any(b1(3)%c1 /= ["XLF","IBM","XLC","LAB"]))           stop 45
  if(any(b1(4)%c1 /= ["xlf","ibm","xlc","lab"]))           stop 46

end subroutine
