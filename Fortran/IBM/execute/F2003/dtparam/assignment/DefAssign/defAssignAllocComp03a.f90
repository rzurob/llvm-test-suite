!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 9 2009
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
!34567490123456749012345674901234567490123456749012345674901234567490
module m
   type :: A(l0)
      integer,len :: l0 ! l0=3

      sequence
      integer :: i1(l0:l0+1)
      character(l0),allocatable :: c1(:)
      logical,allocatable  :: g1(:)
   end type

   type :: base(l1)
      integer,len :: l1 ! l1=2
      type(A(l1+1)),allocatable :: a1comp(:)

   end type

   type,extends(base) :: child(l2)
      integer,len :: l2 ! l2=3

      type(A(l1+1)),allocatable :: a2comp(:)
      type(A(l2)),allocatable   :: a3comp(:)

   end type

   interface assignment(=)
      module procedure assignA,assignBase,assignChild
   end interface

   contains

      elemental subroutine assignA(tA1,tA2)
         type(A(*)),intent(inout) :: tA1
         type(A(*)),intent(in)    :: tA2

         TA1%i1=TA2%i1
         TA1%c1=TA2%c1
         TA1%g1=TA2%g1

      end subroutine

      subroutine assignBase(tB,tA)
         type(base(2)),intent(inout) :: tB(:)
         type(A(*)),intent(in)    :: tA(:)

         print *,"in assignBase"
         do i=lbound(tB,1),ubound(tB,1)

            ! call assignA
            tB(i)%a1comp= tA

         end do
      end subroutine

      elemental subroutine assignChild(tC1,tC2)
         class(child(*,*)),intent(inout) :: tC1
         class(child(*,*)),intent(in) :: tC2

         ! call assignA
         tC1%a1comp=tC2%a1comp
         tC1%a2comp=tC2%a2comp
         tC1%a3comp=tC2%a3comp

      end subroutine

end module

program defAssignAllocComp03a
   use m

   class(base(2)),allocatable :: obj1(:),obj3(:)

   type(child(2,:)),allocatable :: obj2(:)

   allocate(child(2,3) :: obj1(2:2))

   allocate(obj1(2)%a1comp(obj1%l1))

   ! call assignA
   obj1(2)%a1comp=[A(3)(i1=[1,2], &
                        c1=["abc","def","ghi"],&
                        g1=[.true.,.false.]),&
                   A(3)(i1=[3,4], &
                        c1=["ABC","DEF"], &
                        g1=[.false.,.true.,.false.])]

   allocate(child(2,3) :: obj2(1) )

   allocate(obj2(1)%a1comp(obj2%l1))
   allocate(obj2(1)%a2comp(obj2%l2:obj2%l2))
   allocate(obj2(1)%a3comp(obj2%l1:obj2%l1))

   select type(obj1)
      type is(child(*,*))
        allocate(obj1(2)%a2comp(obj1%l2:obj1%l2))
        allocate(obj1(2)%a3comp(obj1%l1:obj1%l1))

        ! call assignA
        obj1(2)%a2comp=[A(3)(i1=[11,12],&
                             c1=["IBM","XLF","ibm","xlf"],&
                             g1=[.true.]) ]
        ! call assignA
        obj1(2)%a3comp=[A(3)(i1=[13,14],&
                             c1=["CUT"],&
                             g1=[.true.,.false.,.false.,.true.] ) ]

        ! call assignChild
        obj2 = obj1

     class default
        stop 9
   end select

   allocate(obj3(-1:-1))

   allocate(obj3(-1)%a1comp(size(obj1(2)%a1comp)))

   ! call assignBase
   obj3=obj1(2)%a1comp

   if(any(obj1(2)%a1comp(1)%i1 /= [1,2] ))                             stop 10
   if(any(obj1(2)%a1comp(2)%i1 /= [3,4] ))                             stop 11
   if(any(obj1(2)%a1comp(1)%c1 /= ["abc","def","ghi"]))                stop 12
   if(any(obj1(2)%a1comp(2)%c1 /= ["ABC","DEF"]))                      stop 13
   if(any(obj1(2)%a1comp(1)%g1 .neqv. [.true.,.false.]))               stop 14
   if(any(obj1(2)%a1comp(2)%g1 .neqv. [.false.,.true.,.false.]))       stop 15

   select type(x=>obj1(2))
      type is(child(*,*))

        if(any(x%a2comp(3)%i1 /= [11,12] ))                            stop 16
        if(any(x%a2comp(3)%c1 /= ["IBM","XLF","ibm","xlf"]))           stop 17
        if(any(x%a2comp(3)%g1 .neqv. [.true.]))                        stop 18

        if(any(x%a3comp(2)%i1 /= [13,14] ))                            stop 19
        if(any(x%a3comp(2)%c1 /= ["CUT"]))                             stop 20
        if(any(x%a3comp(2)%g1 .neqv. [.true.,.false.,.false.,.true.])) stop 21

   end select

   associate(x=>obj2(1))

      if(any(x%a1comp(1)%i1 /= [1,2] ))                               stop 22
      if(any(x%a1comp(2)%i1 /= [3,4] ))                               stop 23
      if(any(x%a1comp(1)%c1 /= ["abc","def","ghi"]))                  stop 24
      if(any(x%a1comp(2)%c1 /= ["ABC","DEF"]))                        stop 25
      if(any(x%a1comp(1)%g1 .neqv. [.true.,.false.]))                 stop 26
      if(any(x%a1comp(2)%g1 .neqv. [.false.,.true.,.false.]))         stop 27

      if(any(x%a2comp(3)%i1 /= [11,12] ))                             stop 28
      if(any(x%a2comp(3)%c1 /= ["IBM","XLF","ibm","xlf"]))            stop 29
      if(any(x%a2comp(3)%g1 .neqv. [.true.]))                         stop 30

      if(any(x%a3comp(2)%i1 /= [13,14] ))                             stop 31
      if(any(x%a3comp(2)%c1 /= ["CUT"]))                              stop 32
      if(any(x%a3comp(2)%g1 .neqv. [.true.,.false.,.false.,.true.]))  stop 33

   end associate

   associate(x=>obj3(-1))

      if(any(x%a1comp(1)%i1 /= [1,2] ))                               stop 34
      if(any(x%a1comp(2)%i1 /= [3,4] ))                               stop 35
      if(any(x%a1comp(1)%c1 /= ["abc","def","ghi"]))                  stop 36
      if(any(x%a1comp(2)%c1 /= ["ABC","DEF"]))                        stop 37
      if(any(x%a1comp(1)%g1 .neqv. [.true.,.false.]))                 stop 38
      if(any(x%a1comp(2)%g1 .neqv. [.false.,.true.,.false.]))         stop 39

   end associate

end program
