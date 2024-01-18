!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignAllocComp02a.f
!*
!*  DATE                       : Feb. 7 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with generic binding
!* 2. Derived type has allocatable components
!* 3. Type bound procedures are elemental subroutine
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type A(k1,l1)
      integer,kind :: k1=4
      integer,len  :: l1=2
      character(k1),allocatable :: c1(:)
      logical(k1)  :: g1(l1) = .false.
      contains
         procedure :: assignA1
         procedure :: assignA2
         generic :: assignment(=) => assignA1,assignA2

  end type

  type B(k2,l2)
       integer,kind :: k2=2
       integer,len  :: l2=3
       integer(k2)  :: i1(l2)
       type(A(2*k2,l2)) :: a2comp
       contains
          procedure :: assignB
          generic :: assignment(=) => assignB
  end type

  type C(k3,l3)
     integer,kind :: k3=2
     integer,len  :: l3=2

     type(A(k3,l3-1)),allocatable   :: a1comp(:)
     type(B(k3,:)),allocatable      :: b1comp
     contains
        procedure :: assignC
        generic :: assignment(=) => assignC
  end type

  contains

     elemental subroutine assignC(this,dt)
        class(C(2,*)),intent(inout) :: this
        type(C(2,*)),intent(in)  :: dt

        allocate(this%a1comp(2))
        allocate(B(dt%k3,dt%l3) :: this%b1comp)

        this%a1comp=dt%a1comp       ! call assignA1
        this%b1comp=dt%b1comp       ! call assignB

     end subroutine

     elemental subroutine assignA1(this,dt)
        class(A(2,*)),intent(inout) :: this
        type(A(2,*)),intent(in)     :: dt

        this%c1=dt%c1 ! intrinsic assignment
        this%g1=dt%g1 ! intrinsic assignment

     end subroutine

     elemental subroutine assignB(this,dt)
         class(B(2,*)),intent(inout)  :: this
         type(B(2,*)),intent(in)      :: dt

         this%i1=dt%i1          ! call intrinsic assignment
         this%a2comp=dt%a2comp  ! call assignA2

     end subroutine

     elemental subroutine assignA2(this,dt)
        class(A(4,*)),intent(inout) :: this
        type(A(4,*)),intent(in)    :: dt

        this%c1=dt%c1 ! call intrinsic assignment
        this%g1=dt%g1 ! call intrinsic assignment

     end subroutine

     subroutine verify1(dt)
        type(C(2,*)),intent(in) :: dt

        if(dt%k3 /= 2)                                    stop 10
        if(dt%l3 /= 2)                                    stop 11
        if(dt%a1comp%k1 /= 2)                             stop 12
        if(dt%a1comp%l1 /= 1)                             stop 13
        if(any(dt%a1comp(1)%c1 /= ["AB","CD","EF"]))      stop 14
        if(any(dt%a1comp(2)%c1 /= ["NI","WE"]))           stop 15
        if(any(dt%a1comp(1)%g1 .neqv. .true.))            stop 16
        if(any(dt%a1comp(2)%g1 .neqv. .false.))           stop 17
        if(dt%b1comp%k2 /= 2 )                            stop 18
        if(dt%b1comp%l2 /= 2 )                            stop 19
        if(any(dt%b1comp%i1 /= [1,2]))                    stop 20
        if(dt%b1comp%a2comp%k1 /= 4)                      stop 21
        if(dt%b1comp%a2comp%l1 /= 2)                      stop 22
        if(any(dt%b1comp%a2comp%c1 /= ["WOOD","FOOD","MOOD"]))  stop 23
print*, dt%b1comp%a2comp%g1
        if(any(dt%b1comp%a2comp%g1 .neqv. [.true.,.false.]))    stop 24

     end subroutine

     subroutine verify2(dt)
        type(C(2,*)),intent(in) :: dt

        if(dt%k3 /= 2)                                    stop 25
        if(dt%l3 /= 2)                                    stop 26
        if(dt%a1comp%k1 /= 2)                             stop 27
        if(dt%a1comp%l1 /= 1)                             stop 28
        if(any(dt%a1comp(1)%c1 /= ["ab","cd"]))           stop 29
        if(any(dt%a1comp(2)%c1 /= ["ha","ca"]))           stop 30
        if(any(dt%a1comp(1)%g1 .neqv. .false.))           stop 31
        if(any(dt%a1comp(2)%g1 .neqv. .true.))            stop 32
        if(dt%b1comp%k2 /= 2 )                            stop 33
        if(dt%b1comp%l2 /= 2 )                            stop 34
        if(any(dt%b1comp%i1 /= [3,4]))                    stop 35
        if(dt%b1comp%a2comp%k1 /= 4)                      stop 36
        if(dt%b1comp%a2comp%l1 /= 2)                      stop 37
        if(any(dt%b1comp%a2comp%c1 /= ["wood","food","mood"]))  stop 38
        if(any(dt%b1comp%a2comp%g1 .neqv. [.false.,.true.]))    stop 39

     end subroutine

     subroutine verify3(dt)
        type(A(2,*)),intent(in) :: dt(3)

        if(any(dt(1)%c1 /= ["xy"]))                       stop 40
        if(any(dt(1)%g1 .neqv. .true.))                   stop 41
        if(any(dt(2)%c1 /= ["ab","cd"]))                  stop 42
        if(any(dt(2)%g1 .neqv. .false.))                  stop 43
        if(any(dt(3)%c1 /= ["ha","ca"]))                  stop 44
        if(any(dt(3)%g1 .neqv. .true.))                   stop 45

     end subroutine

     subroutine verify4(dt)
        type(B(2,*)),intent(in) :: dt(2)

        if(any(dt(1)%i1 /= [1,2]))                        stop 46
        if(any(dt(2)%i1 /= [3,4]))                        stop 47
        associate(x=>dt%a2comp)

           if(any(x(1)%c1 /= ["WOOD","FOOD","MOOD"]))     stop 48
           if(any(x(2)%c1 /= ["wood","food","mood"]))     stop 49
           if(any(x(1)%g1 .neqv. [.true.,.false.]))       stop 50
           if(any(x(2)%g1 .neqv. [.false.,.true.]))       stop 51

        end associate
     end subroutine

end module

program defAssignAllocComp02a
   use m

   implicit none

   type(A(2,:)),allocatable :: aobj1(:)

   type(B(2,2)),allocatable :: bobj1(:)

   type(C(2,:)),allocatable :: cobj1(:)

   type(C(2,2)) :: cobj2

   type(C(2,:)),allocatable :: cobj3(:)

   type(C(2,2)),allocatable :: cobj4

   allocate(C(2,2) :: cobj1(2))

   ! call assignC
   cobj1=[C(2,2)(a1comp=[A(2,1)(c1=["AB","CD","EF"],g1=[.true.]), &
                         A(2,1)(c1=["NICE","WELL"],g1=[.false.])] , &
                 b1comp=B(2,2)(i1=[1,2],a2comp=A(4,2) &
                        (c1=["WOOD","FOOD","MOOD"],g1=[.true.,.false.]))) , &
          C(2,2)(a1comp=[A(2,1)(c1=["ab","cd"],g1=[.false.]), &
                         A(2,1)(c1=["hat","cap"],g1=[.true.])], &
                 b1comp=B(2,2)(i1=[3,4],a2comp=A(4,2) &
                        (c1=["wood","food","mood"],g1=[.false.,.true.])) ) ]

print*, cobj1(1)%b1comp%a2comp%g1
   call verify1(cobj1(1))

   call verify2(cobj1(2))

   cobj2=cobj1(1) ! call assignC

   call verify1(cobj2)

   allocate(C(2,2) :: cobj3(size(cobj1)))

   ! call assignC
   cobj3=cobj1(2)

   call verify2(cobj3(1))

   call verify2(cobj3(2))

   allocate(cobj4)

   ! call assignC
   cobj4=cobj2

   call verify1(cobj4)

   allocate(A(2,1) :: aobj1(3))

   ! call assignA1
   aobj1=[A(2,1)(c1=["xy"],g1=.true.),cobj1(2)%a1comp]

   call verify3(aobj1)

   allocate(bobj1(0:1))

   ! call assignB
   bobj1(0:1)=[cobj1(1)%b1comp,cobj1(2)%b1comp]

   call verify4(bobj1)

end program
