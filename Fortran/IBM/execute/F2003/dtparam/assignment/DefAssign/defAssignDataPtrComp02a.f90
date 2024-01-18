!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDataPtrComp02a.f
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
!* 2. Derived type is polymorphic and has ulitimate pointer components
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(k1)
     integer,kind :: k1

     integer(k1),pointer :: i1(:,:)=>null()
     real(k1),pointer    :: r1(:,:)=>null()
   end type

   type base(l1)
     integer,len :: l1
     character(l1),pointer :: c1(:,:)=>null()
     type(A(4)) :: a1comp(2)
   end type

   type,extends(base) :: child(k2)
     integer,kind :: k2
     logical(k2),pointer :: g1(:,:)=>null()
     type(A(2*k2)) :: a2comp
   end type

   interface assignment(=)
     module procedure assignA1,assignA2,assignA3,assignA4, &
                      assignBase,assignChild1,assignChild2
   end interface

   contains

     subroutine assignA1(this,dt)
       class(A(4)),intent(inout) :: this(:)
       type(A(4)),intent(in)     :: dt

       print *,"in assignA1"

       do i=lbound(this,1),ubound(this,1)
          this(i)%i1=>dt%i1
          this(i)%r1=>dt%r1
       end do
     end subroutine

     subroutine assignA2(this,dt)
       class(A(4)),intent(inout) :: this(:)
       type(A(4)),intent(in)     :: dt(:)

       print *,"in assignA2"

       do i=lbound(this,1),ubound(this,1)
           this(i)%i1=>dt(i)%i1
           this(i)%r1=>dt(i)%r1
       end do

     end subroutine

     subroutine assignA3(this,dt)
       class(A(4)),intent(inout) :: this
       type(A(4)),intent(in)     :: dt

       print *,"in assignA3"

       this%i1=>dt%i1
       this%r1=>dt%r1

     end subroutine

     subroutine assignA4(this,dt)
       class(A(8)),intent(inout) :: this
       type(A(8)),intent(in)     :: dt

       print *,"in assignA4"
       this%i1=>dt%i1
       this%r1=>dt%r1

     end subroutine

     subroutine assignBase(this,dt)
       class(base(*)),intent(inout) :: this
       class(child(*,2)),intent(in)     :: dt

       print *,"in assignBase"

       select type(this)
           type is(child(*,2))
              this%c1=>dt%c1
              this%a1comp=dt%a1comp  ! call assignA2
              this%g1=>dt%g1
              this%a2comp=dt%a2comp  ! call assignA3
           type is(base(*))
              this%c1=>dt%c1
              this%a1comp=dt%a1comp  ! call assignA2
           class default
               stop 100
       end select

     end subroutine

     subroutine assignChild1(this,dt)
       class(child(*,2)),intent(inout) :: this(:,:)
       type(child(*,2)),intent(in)     :: dt(:,:)

       integer :: i,j

       print *,"in assignChild1"

       do i=lbound(this,1),ubound(this,1)
            do j=lbound(this,2),ubound(this,2)
               this(i,j)%c1=>dt(i,j)%c1
               this(i,j)%a1comp=dt(i,j)%a1comp ! call assignA2
               this(i,j)%g1=>dt(i,j)%g1
               this(i,j)%a2comp=dt(i,j)%a2comp ! call assignA3
            end do
       end do
     end subroutine

     subroutine assignChild2(this,dt)
       class(child(*,4)),intent(inout) :: this
       type(child(*,4)),intent(in)     :: dt

       print *,"in assignChild2"

       this%c1=>dt%c1
       this%a1comp=dt%a1comp  ! call assignA2

       this%g1=>dt%g1
       this%a2comp=dt%a2comp ! call assignA4

     end subroutine

end module

program defAssignDataPtrComp02a

     call sub

end program

subroutine sub

   use m , TA=>A,Tbase=>base,Tchild=>child

   logical,external :: precision_r4,precision_r8

   class(tbase(:)),pointer :: tb1(:,:)=>null(),tb2(:,:)=>null()

   type(tchild(3,2)),target :: tc1(1,1)

   type(tchild(3,4)),target :: tc2

   type(tA(4)),target :: a1(2)

   type(tA(4)),allocatable,target :: a2,a3(:)

   type(tA(8)),target :: a4

   integer,target :: i1(2,3)=reshape([1,2,3,4,5,6],(/2,3/))

   integer(8),target :: i2(2,3)

   real,target    :: r1(1,1)=reshape([1.2],(/1,1/))

   real(8),target :: r2(1,1)

   logical(2),target :: g1(2,2)=reshape([.true.,.false.,.true.,.false.], &
                                    (/2,2/) )

   logical(4),target :: g2(2,2)

   character(3),target :: c1(2,2)=reshape(["abc","ABC","def","DEF"],(/2,2/))

   i2=i1; r2=1.2_8; g2=g1;

   ! call assignA2
   a1=[tA(4)(i1,r1),tA(4)(i1(1:1,3:3),r1)]

   !---verify a1----!
   if(any(a1(1)%i1(1,:) /= [1,3,5]))                   stop 10
   if(any(a1(1)%i1(2,:) /= [2,4,6]))                   stop 11
   if(.not. precision_r4(a1(1)%r1,1.2_4))              stop 12
   if(any(a1(2)%i1 /= 5))                              stop 13
   if(.not. precision_r4(a1(2)%r1,1.2_4))              stop 14

   allocate(a2)

   ! call assignA3
   a2=a1(2)

   !--- verify a2---!
   if(any(a2%i1 /= 5))                                 stop 16
   if(.not. precision_r4(a2%r1,1.2_4))                 stop 17

   allocate(a3(size(a1)))

   ! call assignA1
   a3=tA(4)(i1(2:2,3:3),r1(:,:))

   !--- verify a3---!
   if(any(a3(1)%i1 /= 6))                              stop 19
   if(.not. precision_r4(a3(1)%r1,1.2_4))              stop 20
   if(any(a3(2)%i1 /= 6))                              stop 22
   if(.not. precision_r4(a3(2)%r1,1.2_4))              stop 23

   ! call assignA5
   a4=tA(8)(i2,r2)

   !--- verify a4---!
   if(any(a4%i1(1,:) /= [1,3,5]))                      stop 25
   if(any(a4%i1(2,:) /= [2,4,6]))                      stop 26
   if(.not. precision_r8(a4%r1,1.2_8))                 stop 27

   ! call assignChild1
   tc1= reshape([tchild(3,2)(c1,a1(2),g1,a1(1))],(/1,1/))

   !--- verify tc1---!
   if(any(tc1(1,1)%c1(1,:) /= ["abc","def"]))          stop 28
   if(any(tc1(1,1)%c1(2,:) /= ["ABC","DEF"]))          stop 29
   if(any(tc1(1,1)%a1comp(1)%i1(1,:) /= 5))            stop 30
   if(any(tc1(1,1)%a1comp(2)%i1(1,:) /= 5))            stop 31
   if(.not. precision_r4(tc1(1,1)%a1comp(1)%r1,1.2_4)) stop 32
   if(.not. precision_r4(tc1(1,1)%a1comp(2)%r1,1.2_4)) stop 33

   if(any(tc1(1,1)%g1(1,:) .neqv. [.true.,.true.]))    stop 34
   if(any(tc1(1,1)%g1(2,:) .neqv. [.false.,.false.]))  stop 35
   if(any(tc1(1,1)%a2comp%i1(1,:) /= [1,3,5]))         stop 36
   if(any(tc1(1,1)%a2comp%i1(2,:) /= [2,4,6]))         stop 37
   if(.not. precision_r4(tc1(1,1)%a2comp%r1,1.2_4))    stop 38

   allocate(tchild(3,2) :: tb1(1,1))

   ! call assignBase
   tb1(1,1)=tc1(1,1)

   !--- verify tb1----!
   select type(x=>tb1(1,1))
      type is(tchild(*,2))
        if(any(x%c1(1,:) /= ["abc","def"]))           stop 39
        if(any(x%c1(2,:) /= ["ABC","DEF"]))           stop 40
        if(any(x%a1comp(1)%i1(1,:) /= 5))             stop 41
        if(any(x%a1comp(2)%i1(1,:) /= 5))             stop 42
        if(.not. precision_r4(x%a1comp(1)%r1,1.2_4))  stop 43
        if(.not. precision_r4(x%a1comp(2)%r1,1.2_4))  stop 44
        if(any(x%g1(1,:) .neqv. [.true.,.true.]))     stop 45
        if(any(x%g1(2,:) .neqv. [.false.,.false.]))   stop 46
        if(any(x%a2comp%i1(1,:) /= [1,3,5]))          stop 47
        if(any(x%a2comp%i1(2,:) /= [2,4,6]))          stop 48
        if(.not. precision_r4(x%a2comp%r1,1.2_4))     stop 49

      class default
        stop 101
   end select

   allocate(tbase(3) :: tb2(1,1))

   ! call assignBase
   tb2(1,1)=tc1(1,1)

   !--- verify tb2---!
   if(any(tb2(1,1)%c1(1,:) /= ["abc","def"]))         stop 50
   if(any(tb2(1,1)%c1(2,:) /= ["ABC","DEF"]))         stop 51
   if(any(tb2(1,1)%a1comp(1)%i1(1,:) /= 5))           stop 52
   if(any(tb2(1,1)%a1comp(2)%i1(1,:) /= 5))           stop 53
   if(.not. precision_r4(tb2(1,1)%a1comp(1)%r1,1.2_4))   stop 54
   if(.not. precision_r4(tb2(1,1)%a1comp(2)%r1,1.2_4))   stop 55

   deallocate(tb1)

   allocate(tchild(3,4)  :: tb1(1,1))

   select type(tb1)
       type is(tchild(*,4))
           ! call assignChild2
           tb1(1,1)=tchild(3,4)(c1,a2,g2,a4)

           !--- verify tb1---!
           if(any(tb1(1,1)%c1(1,:) /= ["abc","def"]))           stop 56
           if(any(tb1(1,1)%c1(2,:) /= ["ABC","DEF"]))           stop 57
           if(any(tb1(1,1)%a1comp(1)%i1(1,:) /= 5))             stop 58
           if(any(tb1(1,1)%a1comp(2)%i1(1,:) /= 5))             stop 59
           if(.not. precision_r4(tb1(1,1)%a1comp(1)%r1,1.2_4))  stop 60
           if(.not. precision_r4(tb1(1,1)%a1comp(2)%r1,1.2_4))  stop 61
           if(any(tb1(1,1)%g1(1,:) .neqv. [.true.,.true.]))     stop 62
           if(any(tb1(1,1)%g1(2,:) .neqv. [.false.,.false.]))   stop 63
           if(any(tb1(1,1)%a2comp%i1(1,:) /= [1,3,5]))          stop 64
           if(any(tb1(1,1)%a2comp%i1(2,:) /= [2,4,6]))          stop 65
           if(.not. precision_r8(tb1(1,1)%a2comp%r1,1.2_8))     stop 66

       class default

           stop 102
   end select

end subroutine
