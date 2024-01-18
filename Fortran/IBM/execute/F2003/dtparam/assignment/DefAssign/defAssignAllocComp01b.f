!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with generic binding
!* 2. Derived type has mulitiple layers of allocatable components
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len :: l1
      character(l1),allocatable :: c1(:)
      character(:),allocatable :: c2(:)
      contains
         procedure,pass :: assignA
         generic :: assignment(=) => assignA
   end type

   type B(l2)
      integer,len :: l2
      integer,allocatable :: i1(:)
      character(l2),allocatable :: c3(:)
      contains
        procedure,pass :: assignB
        generic :: assignment(=) => assignB
   end type

   type C(l3)
      integer,len :: l3
      type(A(l3+1)),allocatable :: a1comp(:)
      type(B(:)),allocatable :: b1comp(:)
      contains
          procedure, pass :: assignC
          generic :: assignment(=) => assignC
   end type

   contains

       elemental subroutine assignC(this,dt)
           class(C(*)),intent(inout) :: this
           class(C(*)),intent(in) :: dt

           this%a1comp=dt%a1comp
           this%b1comp=dt%b1comp
       end subroutine

       elemental subroutine assignA(this,dt)
           class(A(*)),intent(inout) :: this
           class(A(3)),intent(in)  :: dt

           this%c1=dt%c1
           this%c2=dt%c2

       end subroutine

       elemental subroutine assignB(this,dt)
           class(B(*)),intent(inout) :: this
           class(B(*)),intent(in)  :: dt

           this%i1=dt%i1
           this%c3=dt%c3

       end subroutine

end module

program defAssignAllocComp01a
     use m
     implicit none

     type(C(:)),pointer :: cptr1=>null()

     type(C(2)),pointer :: cptr2(:)=>null()

     type(C(2)),target  :: ctar(2:3)

     type(A(3)) :: a1(2)

     type(A(3)) :: a2(2)

     type(B(1)) :: b1(2)

     type(B(2)) :: b2(2)

     ! call assignA

     a1 =[A(3)(c1=["cup","hat"],c2=["ab","cd","ef"]),&
          A(3)(c1=["AB","CD","EF"],c2=["CUP","HAT"])]

     ! call assignA
     a2 = [ A(3)(c1=["ibm"],c2=["red","get"]),&
            A(3)(c1=["RED","GET"],c2=["IBM"])]

     ! call assignB
     b1 = [B(1)(i1=[1,2,3,4],c3=["x","y","z"]), &
           B(1)(i1=[-1,-2,-3,-4],c3=["X","Y","Z"])]

     ! call assignB
     b2 = [B(2)(i1=[5,6],c3=["go","do"]),&
           B(2)(i1=[-5,-6,-7],c3=["GO","DO","TO"])  ]

     if(any(a1(1)%c1 /= ["cup","hat"]))         stop 10
     if(any(a1(1)%c2 /= ["ab","cd","ef"]))      stop 11
     if(any(a1(2)%c1 /= ["AB","CD","EF"]))      stop 12
     if(any(a1(2)%c2 /= ["CUP","HAT"]))         stop 13

     if(any(a2(1)%c1 /= ["ibm"]))               stop 14
     if(any(a2(1)%c2 /= ["red","get"]))         stop 15
     if(any(a2(2)%c1 /= ["RED","GET"]))         stop 16
     if(any(a2(2)%c2 /= ["IBM"]))               stop 17

     if(any(b1(1)%i1 /= [1,2,3,4]))             stop 18
     if(any(b1(1)%c3 /= ["x","y","z"]))         stop 19
     if(any(b1(2)%i1 /= [-1,-2,-3,-4]))         stop 20
     if(any(b1(2)%c3 /= ["X","Y","Z"]))         stop 21

     if(any(b2(1)%i1 /= [5,6]))                 stop 22
     if(any(b2(1)%c3 /= ["go","do"]))           stop 23
     if(any(b2(2)%i1 /= [-5,-6,-7]))            stop 24
     if(any(b2(2)%c3 /= ["GO","DO","TO"]))      stop 25

     call allocComp(ctar)

     ! call assignC
     ctar=[C(2)(a1comp=a1,b1comp=b1),&
            C(2)(a1comp=a2,b1comp=b2)]

     associate(x=>ctar(2)%a1comp)

       if(any(x(1)%c1 /= ["cup","hat"]))         stop 26
       if(any(x(1)%c2 /= ["ab","cd","ef"]))      stop 27
       if(any(x(2)%c1 /= ["AB","CD","EF"]))      stop 28
       if(any(x(2)%c2 /= ["CUP","HAT"]))         stop 29

     end associate

     associate(x=>ctar(3)%a1comp)

       if(any(x(1)%c1 /= ["ibm"]))               stop 30
       if(any(x(1)%c2 /= ["red","get"]))         stop 31
       if(any(x(2)%c1 /= ["RED","GET"]))         stop 32
       if(any(x(2)%c2 /= ["IBM"]))               stop 33

     end associate

     associate(x=>ctar(2)%b1comp)

       if(any(x(1)%i1 /= [1,2,3,4]))             stop 34
       if(any(x(1)%c3 /= ["x","y","z"]))         stop 35
       if(any(x(2)%i1 /= [-1,-2,-3,-4]))         stop 36
       if(any(x(2)%c3 /= ["X","Y","Z"]))         stop 37

     end associate

     associate(x=>ctar(3)%b1comp)

       if(any(x(1)%i1 /= [5,6]))                 stop 38
       if(any(x(1)%c3 /= ["go","do"]))           stop 39
       if(any(x(2)%i1 /= [-5,-6,-7]))            stop 40
       if(any(x(2)%c3 /= ["GO","DO","TO"]))      stop 41

     end associate

     allocate(cptr2(-1:0))

     call allocComp(cptr2)

    ! call assignC
     cptr2 = ctar(3:2:-1)

     associate(x=>cptr2(0)%a1comp)

       if(any(x(1)%c1 /= ["cup","hat"]))         stop 42
       if(any(x(1)%c2 /= ["ab","cd","ef"]))      stop 43
       if(any(x(2)%c1 /= ["AB","CD","EF"]))      stop 44
       if(any(x(2)%c2 /= ["CUP","HAT"]))         stop 45

     end associate

     associate(x=>cptr2(-1)%a1comp)

       if(any(x(1)%c1 /= ["ibm"]))               stop 46
       if(any(x(1)%c2 /= ["red","get"]))         stop 47
       if(any(x(2)%c1 /= ["RED","GET"]))         stop 48
       if(any(x(2)%c2 /= ["IBM"]))               stop 49

     end associate

     associate(x=>cptr2(0)%b1comp)

       if(any(x(1)%i1 /= [1,2,3,4]))             stop 50
       if(any(x(1)%c3 /= ["x","y","z"]))         stop 51
       if(any(x(2)%i1 /= [-1,-2,-3,-4]))         stop 52
       if(any(x(2)%c3 /= ["X","Y","Z"]))         stop 53

     end associate

     associate(x=>cptr2(-1)%b1comp)

       if(any(x(1)%i1 /= [5,6]))                 stop 54
       if(any(x(1)%c3 /= ["go","do"]))           stop 55
       if(any(x(2)%i1 /= [-5,-6,-7]))            stop 56
       if(any(x(2)%c3 /= ["GO","DO","TO"]))      stop 57

     end associate

     allocate(C(2) :: cptr1)

     allocate(cptr1%a1comp(2))
     allocate(B(1) :: cptr1%b1comp(2))

     ! call assignC
     cptr1=ctar(2)

     associate(x=>cptr1%a1comp)

       if(any(x(1)%c1 /= ["cup","hat"]))         stop 58
       if(any(x(1)%c2 /= ["ab","cd","ef"]))      stop 59
       if(any(x(2)%c1 /= ["AB","CD","EF"]))      stop 60
       if(any(x(2)%c2 /= ["CUP","HAT"]))         stop 61

     end associate

     associate(x=>cptr1%b1comp)

       if(any(x(1)%i1 /= [1,2,3,4]))             stop 62
       if(any(x(1)%c3 /= ["x","y","z"]))         stop 63
       if(any(x(2)%i1 /= [-1,-2,-3,-4]))         stop 64
       if(any(x(2)%c3 /= ["X","Y","Z"]))         stop 65

     end associate

     ! call assignA
     cptr1%a1comp=ctar(3)%a1comp

     ! call assignB
     cptr1%b1comp=ctar(3)%b1comp

     associate(x=>cptr1%a1comp)

       if(any(x(1)%c1 /= ["ibm"]))               stop 66
       if(any(x(1)%c2 /= ["red","get"]))         stop 67
       if(any(x(2)%c1 /= ["RED","GET"]))         stop 68
       if(any(x(2)%c2 /= ["IBM"]))               stop 69

     end associate

     associate(x=>cptr1%b1comp)

       if(any(x(1)%i1 /= [5,6]))                 stop 70
       if(any(x(1)%c3 /= ["go","do"]))           stop 71
       if(any(x(2)%i1 /= [-5,-6,-7]))            stop 72
       if(any(x(2)%c3 /= ["GO","DO","TO"]))      stop 73

     end associate

     contains

          subroutine allocComp(dt)
               type(C(2)),intent(inout) :: dt(:)
               integer :: l

               l=lbound(dt,1)

               allocate(A(3) :: dt(l)%a1comp(2))
               allocate(B(1) :: dt(l)%b1comp(2))
               allocate(A(3) :: dt(l+1)%a1comp(2))
               allocate(B(2) :: dt(l+1)%b1comp(2))

          end subroutine
end program
