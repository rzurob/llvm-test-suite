!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDataPtrComp03a.f
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
!* 1. Test defined assignment with interface block
!* 2. Derived type has derived type pointer components
!* 3. Rename derived type with USE statement
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type A(l1)
     integer,len :: l1 !l1=1
     character(l1),pointer :: c1
     character(:),pointer  :: c2(:)
     character(l1) :: c3(l1)
  end type
end module

module m2
  use m1,TA=>A
  type B(l2)
     integer,len :: l2 !l2=2
     integer     :: i1(l2-1:l2+1)
     type(TA(l2-1)),pointer :: a1comp
     type(TA(l2-1)):: a2comp
  end type
end module

module m3
  use m2,XA=>TA,XB=>B
  type C(l3)
    integer,len :: l3 ! l3=1
    type(XA(l3)),pointer   :: a3comp=>null()
    type(XB(l3+1)),pointer :: b1comp(:)=>null()
  end type

  interface assignment(=)
    module procedure assignA1,assignA2,assignA3,&
                     assignB1,assignB2,assignB3,&
                     assignC1,assignC2,assignC3
  end interface

  contains
     subroutine assignA1(this,dt)
        class(XA(*)),intent(inout) :: this(:)
        type(XA(*)),intent(in)     :: dt(:)

        print *,"in assignA1"

        do i=1,ubound(this,1)  ! intrinsic assignment
           this(i)%c1=>dt(i)%c1
           this(i)%c2=>dt(i)%c2
           this(i)%c3=dt(i)%c3
        end do
     end subroutine

    subroutine assignA2(this,dt)
       class(XA(*)),intent(inout) :: this
       type(XA(*)),intent(in)     :: dt

       print *,"in assignA2"

       this%c1=>dt%c1
       this%c2=>dt%c2
       this%c3=dt%c3

    end subroutine

    subroutine assignA3(this,dt)
       class(XA(*)),intent(inout) :: this(:)
       type(XA(*)),intent(in)     :: dt

       print *,"in assignA3"

       do i=1,ubound(this,1) ! intrinsic assignment
          this(i)%c1=>dt%c1
          this(i)%c2=>dt%c2
          this(i)%c3=dt%c3

       end do
    end subroutine

     subroutine assignB1(this,dt)
        class(XB(*)),intent(inout) :: this(:)
        type(XB(*)),intent(in)     :: dt(:)

        print *,"in assignB1"

        do i=1,ubound(this,1)
           this(i)%i1=dt(i)%i1
           allocate(this(i)%a1comp)

           this(i)%a1comp=dt(i)%a1comp   ! call assignA2
           this(i)%a2comp=dt(i)%a2comp   ! call assignA2

        end do

     end subroutine

    subroutine assignB2(this,dt)
       class(XB(*)),intent(inout) :: this
       type(XB(*)),intent(in)     :: dt

       print *,"in assignB2"
       this%i1=dt%i1
       allocate(this%a1comp)

       this%a1comp=dt%a1comp         ! call assignA2
       this%a2comp=dt%a2comp         ! call assignA2

    end subroutine

    subroutine assignB3(this,dt)
       class(XB(*)),intent(inout) :: this(:)
       type(XB(*)),intent(in)     :: dt

       print *,"in assignB3"

       do i=1,ubound(this,1)
         this(i)%i1=dt%i1
         allocate(this(i)%a1comp)

         this(i)%a1comp=dt%a1comp    ! call assignA2
         this(i)%a2comp=dt%a2comp    ! call assignA2
       end do
    end subroutine

     subroutine assignC1(this,dt)
        class(C(*)),intent(inout) :: this(:)
        type(C(*)),intent(in)     :: dt(:)
        integer :: l1,u1,l2,u2

        print *,"in assignC1"

        l1=lbound(this,1);u1=ubound(this,1)

        do i=l1,u1
            allocate(this(i)%a3comp)
            l2=lbound(dt(i)%b1comp,1);u2=ubound(dt(i)%b1comp,1)
            allocate(this(i)%b1comp(l2:u2))

            this(i)%a3comp=dt(i)%a3comp  ! call assignA2
            this(i)%b1comp=dt(i)%b1comp  ! call assignB1
        end do

     end subroutine

    subroutine assignC2(this,dt)
       class(C(*)),intent(inout) :: this
       type(C(*)),intent(in)     :: dt
       integer :: l,u
       print *,"in assignC2"

       l=lbound(dt%b1comp,1);u=ubound(dt%b1comp,1)

       allocate(this%a3comp)
       allocate(this%b1comp(l:u))

       this%a3comp=dt%a3comp ! call assignA2
       this%b1comp=dt%b1comp ! call assignB1

    end subroutine

    subroutine assignC3(this,dt)
       class(C(*)),intent(inout) :: this(:)
       type(C(*)),intent(in)     :: dt
       integer :: l1,u1,l2,u2

       print *,"in assignC3"

       l1=lbound(this,1);u1=ubound(this,1)

        do i=l1,u1
            allocate(this(i)%a3comp)
            l2=lbound(dt%b1comp,1);u2=ubound(dt%b1comp,1)
            allocate(this(i)%b1comp(l2:u2))

            this(i)%a3comp=dt%a3comp  ! call assignA2
            this(i)%b1comp=dt%b1comp  ! call assignB1
        end do

    end subroutine
end module

program defAssignDataPtrComp03a

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

   ! call assignA2
   a3comp1=XA(1)(null(),null(),"A")

   !--- verify a3comp1---!
   if(associated(a3comp1%c1))                            stop 10
   if(associated(a3comp1%c2))                            stop 11
   if(any(a3comp1%c3 /= "A"))                            stop 12

   ! call assignA2
   a3comp1=XA(1)(c1(1),c1(2:6),c1(7:7))

   !--- verify a3comp1---!
   if(a3comp1%c1 /= "X")                                 stop 13
   if(any(a3comp1%c2 /= ["L","F","T","E","S"]))          stop 14
   if(any(a3comp1%c3 /= "T"))                            stop 15

   allocate(XA(1) :: a3comp2(2),a3comp3(1))

   ! call assignA1
   a3comp2=[a3comp1,XA(1)(c1(7),c1(6:2:-1),c1(1:1))]

   !--- verify a3comp2---!
   if(a3comp2(1)%c1 /= "X")                               stop 16
   if(any(a3comp2(1)%c2 /= ["L","F","T","E","S"]))        stop 17
   if(any(a3comp2(1)%c3 /= "T"))                          stop 18

   if(a3comp2(2)%c1 /= "T")                               stop 19
   if(any(a3comp2(2)%c2 /= ["S","E","T","F","L"]))        stop 20
   if(any(a3comp2(2)%c3 /= "X"))                          stop 21

   ! call assignA3
   a3comp3=a3comp2(2)

   !--- verify a3comp3---!
   if(a3comp3(1)%c1 /= "T")                                stop 22
   if(any(a3comp3(1)%c2 /= ["S","E","T","F","L"]))         stop 23
   if(any(a3comp3(1)%c3 /= "X"))                           stop 24

   allocate(XB(2) :: b1comp1(2))

   ! call assignB2
   b1comp1(1)=XB(2)([1,2,3],a3comp1,a3comp2(2))

   !--- verify b1comp1--!
   if(b1comp1(1)%a1comp%c1 /= "X")                         stop 25
   if(any(b1comp1(1)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 26
   if(any(b1comp1(1)%a1comp%c3 /= "T"))                    stop 27

   if(b1comp1(1)%a2comp%c1 /= "T")                         stop 28
   if(any(b1comp1(1)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 29
   if(any(b1comp1(1)%a2comp%c3 /= "X"))                    stop 30

   if(any(b1comp1(1)%i1 /= [1,2,3]))                       stop 31

   ! call assignB2
   b1comp1(2)=XB(2)([-1,-2,-3],a3comp2(1),a3comp2(2))

   !--- verify b1comp1--!
   if(b1comp1(2)%a1comp%c1 /= "X")                         stop 32
   if(any(b1comp1(2)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 33
   if(any(b1comp1(2)%a1comp%c3 /= "T"))                    stop 34

   if(b1comp1(2)%a2comp%c1 /= "T")                         stop 35
   if(any(b1comp1(2)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 36
   if(any(b1comp1(2)%a2comp%c3 /= "X"))                    stop 37

   if(any(b1comp1(2)%i1 /= [-1,-2,-3]))                    stop 38

   ! call assignB1
   ! reverse component
   b1comp1=b1comp1(2:1:-1)

   !--- verify b1comp1--!
  if(b1comp1(2)%a1comp%c1 /= "X")                            stop 39
  if(any(b1comp1(2)%a1comp%c2 /= ["L","F","T","E","S"]))     stop 40
  if(any(b1comp1(2)%a1comp%c3 /= "T"))                       stop 41

  if(b1comp1(2)%a2comp%c1 /= "T")                            stop 42
  if(any(b1comp1(2)%a2comp%c2 /= ["S","E","T","F","L"]))     stop 43
  if(any(b1comp1(2)%a2comp%c3 /= "X"))                       stop 44

  if(any(b1comp1(2)%i1 /= [1,2,3]))                          stop 45

  if(b1comp1(1)%a1comp%c1 /= "X")                            stop 46
  if(any(b1comp1(1)%a1comp%c2 /= ["L","F","T","E","S"]))     stop 47
  if(any(b1comp1(1)%a1comp%c3 /= "T"))                       stop 48

  if(b1comp1(1)%a2comp%c1 /= "T")                            stop 49
  if(any(b1comp1(1)%a2comp%c2 /= ["S","E","T","F","L"]))     stop 50
  if(any(b1comp1(1)%a2comp%c3 /= "X"))                       stop 51

  if(any(b1comp1(1)%i1 /= [-1,-2,-3]))                       stop 52

   ! call assignB3
   b1comp2=b1comp1(1)

  !--- verify b1comp2---!
  if(b1comp2(1)%a1comp%c1 /= "X")                             stop 53
  if(any(b1comp2(1)%a1comp%c2 /= ["L","F","T","E","S"]))      stop 54
  if(any(b1comp2(1)%a1comp%c3 /= "T"))                        stop 55

  if(b1comp2(1)%a2comp%c1 /= "T")                             stop 56
  if(any(b1comp2(1)%a2comp%c2 /= ["S","E","T","F","L"]))      stop 57
  if(any(b1comp2(1)%a2comp%c3 /= "X"))                        stop 58

  if(any(b1comp2(1)%i1 /= [-1,-2,-3]))                        stop 59

  if(b1comp2(2)%a1comp%c1 /= "X")                             stop 60
  if(any(b1comp2(2)%a1comp%c2 /= ["L","F","T","E","S"]))      stop 61
  if(any(b1comp2(2)%a1comp%c3 /= "T"))                        stop 62

  if(b1comp2(2)%a2comp%c1 /= "T")                             stop 63
  if(any(b1comp2(2)%a2comp%c2 /= ["S","E","T","F","L"]))      stop 64
  if(any(b1comp2(2)%a2comp%c3 /= "X"))                        stop 65

  if(any(b1comp2(2)%i1 /= [-1,-2,-3]))                        stop 66

   allocate(XC(2) :: cobj2)

   ! call assignC1
   cobj1=[XC(1)(a3comp1,b1comp1)]

   !--- verify cobj1---!

   associate(x=>cobj1)

   if(x(1)%a3comp%c1 /= "X")                                   stop 67
   if(any(x(1)%a3comp%c2 /= ["L","F","T","E","S"]))            stop 68
   if(any(x(1)%a3comp%c3 /= "T"))                              stop 69

   if(x(1)%b1comp(1)%a1comp%c1 /= "X")                         stop 70
   if(any(x(1)%b1comp(1)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 71
   if(any(x(1)%b1comp(1)%a1comp%c3 /= "T"))                    stop 72

   if(x(1)%b1comp(1)%a2comp%c1 /= "T")                         stop 73
   if(any(x(1)%b1comp(1)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 74
   if(any(x(1)%b1comp(1)%a2comp%c3 /= "X"))                    stop 75

   if(any(x(1)%b1comp(1)%i1 /= [-1,-2,-3]))                    stop 76

   if(x(1)%b1comp(2)%a1comp%c1 /= "X")                         stop 77
   if(any(x(1)%b1comp(2)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 78
   if(any(x(1)%b1comp(2)%a1comp%c3 /= "T"))                    stop 79

   if(x(1)%b1comp(2)%a2comp%c1 /= "T")                         stop 80
   if(any(x(1)%b1comp(2)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 81
   if(any(x(1)%b1comp(2)%a2comp%c3 /= "X"))                    stop 82

   if(any(x(1)%b1comp(2)%i1 /= [1,2,3]))                       stop 83

   end associate

   ! call assignC2
   cobj2=XC(1)(a3comp1,b1comp1)

   !--- verify cobj2---!
   associate(x=>cobj2)

   if(x%a3comp%c1 /= "X")                                   stop 84
   if(any(x%a3comp%c2 /= ["L","F","T","E","S"]))            stop 85
   if(any(x%a3comp%c3 /= "T"))                              stop 86

   if(x%b1comp(1)%a1comp%c1 /= "X")                         stop 87
   if(any(x%b1comp(1)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 88
   if(any(x%b1comp(1)%a1comp%c3 /= "T"))                    stop 89

   if(x%b1comp(1)%a2comp%c1 /= "T")                         stop 90
   if(any(x%b1comp(1)%a2comp%c2 /= ["S","E","T","F","L"]))   stop 91
   if(any(x%b1comp(1)%a2comp%c3 /= "X"))                    stop 92

   if(any(x%b1comp(1)%i1 /= [-1,-2,-3]))                    stop 93

   if(x%b1comp(2)%a1comp%c1 /= "X")                         stop 94
   if(any(x%b1comp(2)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 95
   if(any(x%b1comp(2)%a1comp%c3 /= "T"))                    stop 96

   if(x%b1comp(2)%a2comp%c1 /= "T")                         stop 97
   if(any(x%b1comp(2)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 98
   if(any(x%b1comp(2)%a2comp%c3 /= "X"))                    stop 99

   if(any(x%b1comp(2)%i1 /= [1,2,3]))                       stop 100

   end associate

   ! call assignC3
   cobj1=XC(1)(a3comp1,b1comp1)

   !--- verify cobj1---!

   associate(x=>cobj1)

   if(x(1)%a3comp%c1 /= "X")                                   stop 101
   if(any(x(1)%a3comp%c2 /= ["L","F","T","E","S"]))            stop 102
   if(any(x(1)%a3comp%c3 /= "T"))                              stop 103

   if(x(1)%b1comp(1)%a1comp%c1 /= "X")                         stop 104
   if(any(x(1)%b1comp(1)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 105
   if(any(x(1)%b1comp(1)%a1comp%c3 /= "T"))                    stop 106

   if(x(1)%b1comp(1)%a2comp%c1 /= "T")                         stop 107
   if(any(x(1)%b1comp(1)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 108
   if(any(x(1)%b1comp(1)%a2comp%c3 /= "X"))                    stop 109

   if(any(x(1)%b1comp(1)%i1 /= [-1,-2,-3]))                    stop 110

   if(x(1)%b1comp(2)%a1comp%c1 /= "X")                         stop 111
   if(any(x(1)%b1comp(2)%a1comp%c2 /= ["L","F","T","E","S"]))  stop 112
   if(any(x(1)%b1comp(2)%a1comp%c3 /= "T"))                    stop 113

   if(x(1)%b1comp(2)%a2comp%c1 /= "T")                         stop 114
   if(any(x(1)%b1comp(2)%a2comp%c2 /= ["S","E","T","F","L"]))  stop 115
   if(any(x(1)%b1comp(2)%a2comp%c3 /= "X"))                    stop 116

   if(any(x(1)%b1comp(2)%i1 /= [1,2,3]))                       stop 117

   end associate

end program
