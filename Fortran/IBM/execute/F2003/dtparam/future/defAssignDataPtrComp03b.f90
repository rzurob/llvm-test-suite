!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 11 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with generic binding
!* 2. Derived type has derived type pointer components
!* 3. Rename derived type with USE statement
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type A(l1)
     integer,len :: l1 !l1=1
     character(l1),pointer :: c1
     character(:),pointer  :: c2(:)
     character(l1) :: c3(l1)
     contains
        procedure :: assignA
        generic   :: assignment(=)=>assignA
  end type

  contains

    elemental subroutine assignA(this,dt)
       class(A(*)),intent(inout) :: this
       type(A(*)),intent(in)     :: dt
       integer :: l,u

       l=lbound(dt%c2,1)

       u=ubound(dt%c2,1)

       allocate(this%c1,source=dt%c1)
       allocate(this%c2(l:u),source=dt%c2)
       this%c3=dt%c3

    end subroutine

end module

module m2
  use m1,TA=>A
  type B(l2)
     integer,len :: l2 !l2=2
     integer     :: i1(l2-1:l2+1)
     type(TA(l2-1)),pointer :: a1comp
     type(TA(l2-1)):: a2comp
     contains
       procedure :: assignB
       generic   :: assignment(=)=>assignB
  end type

  contains

    elemental subroutine assignB(this,dt)
       class(B(*)),intent(inout) :: this
       type(B(*)),intent(in)     :: dt

       this%i1=dt%i1
       allocate(this%a1comp)

       this%a1comp=dt%a1comp         ! call assignA
       this%a2comp=dt%a2comp         ! call assignA

    end subroutine

end module

module m3
  use m2,XA=>TA,XB=>B
  type C(l3)
    integer,len :: l3 ! l3=1
    type(XA(l3)),pointer   :: a3comp=>null()
    type(XB(l3+1)),pointer :: b1comp(:)=>null()
    contains
        procedure :: assignC
        generic   :: assignment(=)=>assignC
  end type

  contains
    elemental subroutine assignC(this,dt)
       class(C(*)),intent(inout) :: this
       type(C(*)),intent(in)     :: dt
       integer :: l,u

       l=lbound(dt%b1comp,1)
       u=ubound(dt%b1comp,1)

       allocate(this%a3comp)
       allocate(this%b1comp(l:u))

       this%a3comp=dt%a3comp ! call assignA
       this%b1comp=dt%b1comp ! call assignB

    end subroutine

end module

program defAssignDataPtrComp03b

   use m3,XC=>C
   implicit none

   character(1),target :: c1(7)=["X","L","F","T","E","S","T"]

   type(XC(2)) :: cobj1(1)

   type(XC(:)),allocatable,target :: cobj2

   type(XA(1)),target :: a3comp1

   type(XA(:)),allocatable,target :: a3comp2(:)

   type(XA(1)),allocatable,target :: a3comp3(:)

   type(XB(:)),allocatable,target :: b1comp1(:)

   type(XB(2)),target  :: b1comp2(2)

   ! call assignA
   a3comp1=XA(1)(null(),null(),"A")

   !--- verify a3comp1---!
   if(any(a3comp1%c3 /= "A"))                            error stop 12

   ! call assignA
   a3comp1=XA(1)(c1(1),c1(2:6),c1(7:7))

   !--- verify a3comp1---!
   if(a3comp1%c1 /= "X")                                 error stop 13
   if(any(a3comp1%c2 /= ["L","F","T","E","S"]))          error stop 14
   if(any(a3comp1%c3 /= "T"))                            error stop 15

   allocate(XA(1) :: a3comp2(2),a3comp3(1))

   ! call assignA
   a3comp2=[a3comp1,XA(1)(c1(7),c1(6:2:-1),c1(1:1))]

   !--- verify a3comp2---!
   if(a3comp2(1)%c1 /= "X")                               error stop 16
   if(any(a3comp2(1)%c2 /= ["L","F","T","E","S"]))        error stop 17
   if(any(a3comp2(1)%c3 /= "T"))                          error stop 18

   if(a3comp2(2)%c1 /= "T")                               error stop 19
   if(any(a3comp2(2)%c2 /= ["S","E","T","F","L"]))        error stop 20
   if(any(a3comp2(2)%c3 /= "X"))                          error stop 21

   ! call assignA
   a3comp3=a3comp2(2)

   !--- verify a3comp3---!
   if(a3comp3(1)%c1 /= "T")                                error stop 22
   if(any(a3comp3(1)%c2 /= ["S","E","T","F","L"]))         error stop 23
   if(any(a3comp3(1)%c3 /= "X"))                           error stop 24

   allocate(XB(2) :: b1comp1(2))

   ! call assignB
   b1comp1(1)=XB(2)([1,2,3],a3comp1,a3comp2(2))

   !--- verify b1comp1--!
   if(b1comp1(1)%a1comp%c1 /= "X")                         error stop 25
   if(any(b1comp1(1)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 26
   if(any(b1comp1(1)%a1comp%c3 /= "T"))                    error stop 27

   if(b1comp1(1)%a2comp%c1 /= "T")                         error stop 28
   if(any(b1comp1(1)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 29
   if(any(b1comp1(1)%a2comp%c3 /= "X"))                    error stop 30

   if(any(b1comp1(1)%i1 /= [1,2,3]))                       error stop 31

   ! call assignB
   b1comp1(2)=XB(2)([-1,-2,-3],a3comp2(1),a3comp2(2))

   !--- verify b1comp1--!
   if(b1comp1(2)%a1comp%c1 /= "X")                         error stop 32
   if(any(b1comp1(2)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 33
   if(any(b1comp1(2)%a1comp%c3 /= "T"))                    error stop 34

   if(b1comp1(2)%a2comp%c1 /= "T")                         error stop 35
   if(any(b1comp1(2)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 36
   if(any(b1comp1(2)%a2comp%c3 /= "X"))                    error stop 37

   if(any(b1comp1(2)%i1 /= [-1,-2,-3]))                    error stop 38

   ! call assignB
   ! reverse component
   b1comp1=b1comp1(2:1:-1)

   !--- verify b1comp1--!
  if(b1comp1(2)%a1comp%c1 /= "X")                            error stop 39
  if(any(b1comp1(2)%a1comp%c2 /= ["L","F","T","E","S"]))     error stop 40
  if(any(b1comp1(2)%a1comp%c3 /= "T"))                       error stop 41

  if(b1comp1(2)%a2comp%c1 /= "T")                            error stop 42
  if(any(b1comp1(2)%a2comp%c2 /= ["S","E","T","F","L"]))     error stop 43
  if(any(b1comp1(2)%a2comp%c3 /= "X"))                       error stop 44

  if(any(b1comp1(2)%i1 /= [1,2,3]))                          error stop 45

  if(b1comp1(1)%a1comp%c1 /= "X")                            error stop 46
  if(any(b1comp1(1)%a1comp%c2 /= ["L","F","T","E","S"]))     error stop 47
  if(any(b1comp1(1)%a1comp%c3 /= "T"))                       error stop 48

  if(b1comp1(1)%a2comp%c1 /= "T")                            error stop 49
  if(any(b1comp1(1)%a2comp%c2 /= ["S","E","T","F","L"]))     error stop 50
  if(any(b1comp1(1)%a2comp%c3 /= "X"))                       error stop 51

  if(any(b1comp1(1)%i1 /= [-1,-2,-3]))                       error stop 52

   ! call assignB
   b1comp2=b1comp1(1)

  !--- verify b1comp2---!
  if(b1comp2(1)%a1comp%c1 /= "X")                             error stop 53
  if(any(b1comp2(1)%a1comp%c2 /= ["L","F","T","E","S"]))      error stop 54
  if(any(b1comp2(1)%a1comp%c3 /= "T"))                        error stop 55

  if(b1comp2(1)%a2comp%c1 /= "T")                             error stop 56
  if(any(b1comp2(1)%a2comp%c2 /= ["S","E","T","F","L"]))      error stop 57
  if(any(b1comp2(1)%a2comp%c3 /= "X"))                        error stop 58

  if(any(b1comp2(1)%i1 /= [-1,-2,-3]))                        error stop 59

  if(b1comp2(2)%a1comp%c1 /= "X")                             error stop 60
  if(any(b1comp2(2)%a1comp%c2 /= ["L","F","T","E","S"]))      error stop 61
  if(any(b1comp2(2)%a1comp%c3 /= "T"))                        error stop 62

  if(b1comp2(2)%a2comp%c1 /= "T")                             error stop 63
  if(any(b1comp2(2)%a2comp%c2 /= ["S","E","T","F","L"]))      error stop 64
  if(any(b1comp2(2)%a2comp%c3 /= "X"))                        error stop 65

  if(any(b1comp2(2)%i1 /= [-1,-2,-3]))                        error stop 66

   allocate(XC(2) :: cobj2)

   ! call assignC
   cobj1=[XC(1)(a3comp1,b1comp1)]

   !--- verify cobj1---!

   associate(x=>cobj1)

   if(x(1)%a3comp%c1 /= "X")                                   error stop 67
   if(any(x(1)%a3comp%c2 /= ["L","F","T","E","S"]))            error stop 68
   if(any(x(1)%a3comp%c3 /= "T"))                              error stop 69

   if(x(1)%b1comp(1)%a1comp%c1 /= "X")                         error stop 70
   if(any(x(1)%b1comp(1)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 71
   if(any(x(1)%b1comp(1)%a1comp%c3 /= "T"))                    error stop 72

   if(x(1)%b1comp(1)%a2comp%c1 /= "T")                         error stop 73
   if(any(x(1)%b1comp(1)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 74
   if(any(x(1)%b1comp(1)%a2comp%c3 /= "X"))                    error stop 75

   if(any(x(1)%b1comp(1)%i1 /= [-1,-2,-3]))                    error stop 76

   if(x(1)%b1comp(2)%a1comp%c1 /= "X")                         error stop 77
   if(any(x(1)%b1comp(2)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 78
   if(any(x(1)%b1comp(2)%a1comp%c3 /= "T"))                    error stop 79

   if(x(1)%b1comp(2)%a2comp%c1 /= "T")                         error stop 80
   if(any(x(1)%b1comp(2)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 81
   if(any(x(1)%b1comp(2)%a2comp%c3 /= "X"))                    error stop 82

   if(any(x(1)%b1comp(2)%i1 /= [1,2,3]))                       error stop 83

   end associate

   ! call assignC
   cobj2=XC(1)(a3comp1,b1comp1)

   !--- verify cobj2---!
   associate(x=>cobj2)

   if(x%a3comp%c1 /= "X")                                   error stop 84
   if(any(x%a3comp%c2 /= ["L","F","T","E","S"]))            error stop 85
   if(any(x%a3comp%c3 /= "T"))                              error stop 86

   if(x%b1comp(1)%a1comp%c1 /= "X")                         error stop 87
   if(any(x%b1comp(1)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 88
   if(any(x%b1comp(1)%a1comp%c3 /= "T"))                    error stop 89

   if(x%b1comp(1)%a2comp%c1 /= "T")                         error stop 90
   if(any(x%b1comp(1)%a2comp%c2 /= ["S","E","T","F","L"]))   error stop 91
   if(any(x%b1comp(1)%a2comp%c3 /= "X"))                    error stop 92

   if(any(x%b1comp(1)%i1 /= [-1,-2,-3]))                    error stop 93

   if(x%b1comp(2)%a1comp%c1 /= "X")                         error stop 94
   if(any(x%b1comp(2)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 95
   if(any(x%b1comp(2)%a1comp%c3 /= "T"))                    error stop 96

   if(x%b1comp(2)%a2comp%c1 /= "T")                         error stop 97
   if(any(x%b1comp(2)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 98
   if(any(x%b1comp(2)%a2comp%c3 /= "X"))                    error stop 99

   if(any(x%b1comp(2)%i1 /= [1,2,3]))                       error stop 100

   end associate

   ! call assignC
   cobj1=XC(1)(a3comp1,b1comp1)

   !--- verify cobj1---!

   associate(x=>cobj1)

   if(x(1)%a3comp%c1 /= "X")                                   error stop 101
   if(any(x(1)%a3comp%c2 /= ["L","F","T","E","S"]))            error stop 102
   if(any(x(1)%a3comp%c3 /= "T"))                              error stop 103

   if(x(1)%b1comp(1)%a1comp%c1 /= "X")                         error stop 104
   if(any(x(1)%b1comp(1)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 105
   if(any(x(1)%b1comp(1)%a1comp%c3 /= "T"))                    error stop 106

   if(x(1)%b1comp(1)%a2comp%c1 /= "T")                         error stop 107
   if(any(x(1)%b1comp(1)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 108
   if(any(x(1)%b1comp(1)%a2comp%c3 /= "X"))                    error stop 109

   if(any(x(1)%b1comp(1)%i1 /= [-1,-2,-3]))                    error stop 110

   if(x(1)%b1comp(2)%a1comp%c1 /= "X")                         error stop 111
   if(any(x(1)%b1comp(2)%a1comp%c2 /= ["L","F","T","E","S"]))  error stop 112
   if(any(x(1)%b1comp(2)%a1comp%c3 /= "T"))                    error stop 113

   if(x(1)%b1comp(2)%a2comp%c1 /= "T")                         error stop 114
   if(any(x(1)%b1comp(2)%a2comp%c2 /= ["S","E","T","F","L"]))  error stop 115
   if(any(x(1)%b1comp(2)%a2comp%c3 /= "X"))                    error stop 116

   if(any(x(1)%b1comp(2)%i1 /= [1,2,3]))                       error stop 117

   end associate

end program
