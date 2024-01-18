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
!* 1. Test defined assignment with interface block
!* 2. Derived type has allocatable components
!* 3. Use elemental and non-elemental procedures
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type A(k1,l1)
      integer,kind :: k1=4
      integer,len  :: l1=2
      character(k1),allocatable :: c1(:)
      logical(k1)  :: g1(l1) = .false.
  end type

  type B(k2,l2)
       integer,kind :: k2=2
       integer,len  :: l2=3
       integer(k2)  :: i1(l2)
       type(A(2*k2,l2)) :: a2comp
  end type

  type C(k3,l3)
     integer,kind :: k3=2
     integer,len  :: l3=2

     type(A(k3,l3-1)),allocatable    :: a1comp(:)
     type(B(k3,:)),allocatable      :: b1comp
  end type

  interface assignment(=)
     module procedure assignC1,assignC2,assignC3,&
                      assignA1,assignA2,assignB1,assignB2
  end interface

  contains

     subroutine assignC1(this,dt)
        type(C(2,:)),allocatable,intent(inout) :: this(:)
        type(C(2,*)),intent(in)  :: dt(:)

        print *,"in assignC1"
        allocate(C(2,dt%l3) :: this(size(dt)))

        do i=lbound(this,1),ubound(this,1)
           allocate(this(i)%a1comp(2))
           allocate(B(dt%k3,dt%l3) :: this(i)%b1comp)

           this(i)%a1comp=dt(i)%a1comp ! call assignA1
           this(i)%b1comp=dt(i)%b1comp ! call assignB
        end do

     end subroutine

     subroutine assignC2(this,dt)
        type(C(2,:)),allocatable,intent(inout) :: this(:)
        type(C(2,*)),intent(in)  :: dt

        print *,"in assignC2"
        allocate(C(2,dt%l3) :: this(1))

        do i=lbound(this,1),ubound(this,1)
           allocate(this(i)%a1comp(2))
           allocate(B(dt%k3,dt%l3) :: this(i)%b1comp)

           this(i)%a1comp=dt%a1comp     ! call assignA1
           this(i)%b1comp=dt%b1comp     ! call assignB
        end do

     end subroutine

     subroutine assignC3(this,dt)
        type(C(2,2)),allocatable,intent(inout) :: this
        type(C(2,*)),intent(in)  :: dt

        print *,"in assignC3"

        allocate(C(2,dt%l3) :: this)
        allocate(this%a1comp(2))
        allocate(B(dt%k3,dt%l3) :: this%b1comp)

        this%a1comp=dt%a1comp       ! call assignA1
        this%b1comp=dt%b1comp       ! call assignB

     end subroutine

     elemental subroutine assignA1(this,dt)
        type(A(2,*)),intent(inout) :: this
        type(A(2,*)),intent(in)    :: dt

        allocate(this%c1(size(dt%c1)),source=dt%c1)

        this%g1=dt%g1

     end subroutine

     subroutine assignB1(this,dt)
         type(B(2,*)),intent(inout)  :: this
         type(B(2,*)),intent(in)     :: dt

         print *,"in assignB1"

         this%i1=dt%i1          ! call intrinsic assignment
         this%a2comp=dt%a2comp  ! call assignA2

     end subroutine

     subroutine assignB2(this,dt)
         type(B(2,*)),intent(inout)  :: this(:)
         type(B(2,*)),intent(in)     :: dt(:)

         print *,"in assignB2"

         do i=lbound(this,1),ubound(this,1)

           this(i)%i1=dt(i)%i1          ! call intrinsic assignment
           this(i)%a2comp=dt(i)%a2comp  ! call assignA2

         end do

     end subroutine

     subroutine assignA2(this,dt)
        type(A(4,*)),intent(inout) :: this
        type(A(4,*)),intent(in)    :: dt

        print *,"in assignA2"

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

   ! call assignC1
   cobj1=[C(2,2)(a1comp=[A(2,1)(c1=["AB","CD","EF"],g1=[.true.]), &
                         A(2,1)(c1=["NICE","WELL"],g1=[.false.])] , &
                 b1comp=B(2,2)(i1=[1,2],a2comp=A(4,2) &
                        (c1=["WOOD","FOOD","MOOD"],g1=[.true.,.false.]))) , &
          C(2,2)(a1comp=[A(2,1)(c1=["ab","cd"],g1=[.false.]), &
                         A(2,1)(c1=["hat","cap"],g1=[.true.])], &
                 b1comp=B(2,2)(i1=[3,4],a2comp=A(4,2) &
                        (c1=["wood","food","mood"],g1=[.false.,.true.])) ) ]

   call verify1(cobj1(1))

   call verify2(cobj1(2))

   cobj2=cobj1(1) ! call intrinsic assignment,cobj2 is non-allocatable

   call verify1(cobj2)

   ! call assignC2
   cobj3=[cobj1(2),cobj1(2)]

   call verify2(cobj3(1))

   call verify2(cobj3(2))

   ! call assignC3
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
