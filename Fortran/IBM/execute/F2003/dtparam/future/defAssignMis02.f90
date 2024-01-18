!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignMis02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 20 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. Test defined assignment with interface block
!* 2. Dummy argument for defined assignment is unlimited polymorphic type
!* 3. Invoke defined assignment in nested procedure
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len  :: l1 ! l1=2
      integer,allocatable  :: i1(:)
      character(l1),pointer :: c1(:)
   end type

   type B(l2)
      integer,len :: l2 ! l2=1
      type(A(l2+1)) :: acomp1(1)
      type(A(:)),pointer :: acomp2(:)
   end type

   interface assignment(=)
       module procedure assign1,assign2
   end interface

   contains

      subroutine subA1(this,dt)
         type(A(*)),intent(inout) :: this
         type(A(*)),intent(in)    :: dt
 
         print *,"in subA1" 
         call subA2(this,dt)

      end subroutine

      subroutine subA2(this,dt)
         class(A(*)),intent(inout) :: this
         class(A(*)),intent(in)    :: dt

         print *,"in subA2"
         call subA3(this,dt)

      end subroutine

      subroutine subA3(this,dt)
         class(*),intent(inout) :: this
         class(*),intent(in)    :: dt

         print *,"in subA3"
         this=dt  ! invoke assign1
      end subroutine

      subroutine assign1(this,dt)
         class(*),intent(inout) :: this
         class(*),intent(in)    :: dt

         print *,"in assign1"
         select type(this)
            type is(A(*))
              select type(dt)
                  type is (A(*))
                     this%i1=dt%i1 ! intrinsic assignment
                     this%c1=dt%c1 ! intrinsic assignment
                  class default
                      stop 11
              end select
            type is(B(*))
              select type(dt)
                  type is (B(*))
                     do i=lbound(dt%acomp1,1),ubound(dt%acomp1,1)
                         ! intrinsic assignment
                         this%acomp1(i)%i1=dt%acomp1(i)%i1 
                         this%acomp1(i)%c1=dt%acomp1(i)%c1
                     end do
                     this%acomp2=>dt%acomp2 ! invoke pointer assignment
                  class default
                      stop 12
              end select
            class default
              stop 10
         end select

      end subroutine

      subroutine subA4(this,dt)
         type(A(*)),intent(inout) :: this(:)
         type(A(*)),intent(in)    :: dt(:)

         print *,"in subA4"
         call subA5(this,dt)

      end subroutine

      subroutine subA5(this,dt)
         class(A(*)),intent(inout) :: this(:)
         class(A(*)),intent(in)    :: dt(:)

         print *,"in subA5"
         call subA6(this,dt)

      end subroutine

      subroutine subA6(this,dt)
         class(*),intent(inout) :: this(:)
         class(*),intent(in)    :: dt(:)

         print *,"in subA6"
         this=dt ! invoke assign2
      end subroutine

      subroutine assign2(this,dt)
         class(*),intent(inout) :: this(:)
         class(*),intent(in)    :: dt(:)
         integer :: i

         print *,"in assign2"
         select type(this)
            type is(A(*))
              select type(dt)
                  type is (A(*))
                     do i=lbound(this,1),ubound(this,1)
                         this(i)=dt(i) ! invoke assign1
                     end do
                  class default
                      stop 14
              end select

            type is(B(*))
              select type(dt)
                  type is (B(*))
                     do i=lbound(dt,1),ubound(dt,1)
                         do j=lbound(this(i)%acomp1,1),ubound(this(i)%acomp1,1)
                           this(i)%acomp1(j)=dt(i)%acomp1(j)  !invoke assign1
                         end do
                         this(i)%acomp2=>dt(i)%acomp2 !pointer assignment
                    end do
                  class default
                      stop 15
              end select

            class default
              stop 13
         end select

      end subroutine

      subroutine subB1(this,dt)
         type(B(*)),intent(inout) :: this
         type(B(*)),intent(in)    :: dt

         print *,"in subB1"
         call subB2(this,dt)

      end subroutine

      subroutine subB2(this,dt)
         class(B(*)),intent(inout) :: this
         class(B(*)),intent(in)    :: dt

         print *,"in subB2"
         call subB3(this,dt)

      end subroutine

      subroutine subB3(this,dt)
         class(*),intent(inout) :: this
         class(*),intent(in)    :: dt

         print *,"in subB3"
         this=dt
      end subroutine

      subroutine subB4(this,dt)
         type(B(*)),intent(inout) :: this(:)
         type(B(*)),intent(in)    :: dt(:)

         print *,"in subB4"
         call subB5(this,dt)

      end subroutine

      subroutine subB5(this,dt)
         class(B(*)),intent(inout) :: this(:)
         class(B(*)),intent(in)    :: dt(:)

         print *,"in subB5"
         call subB6(this,dt)

      end subroutine

      subroutine subB6(this,dt)
         class(*),intent(inout) :: this(:)
         class(*),intent(in)    :: dt(:)

         print *,"in subB6"
         this=dt
      end subroutine

end module

program defAssignMis02
     use m
     implicit none

     character(:),allocatable,target :: c1(:)
     
     type(A(2)) :: aobj1,aobj3(3)
     type(A(:)),target,allocatable :: aobj2,aobj4(:)

     type(B(1)) :: bobj1,bobj3(2)
     type(B(:)),allocatable :: bobj2,bobj4(:)

     c1=["ab","aB","Ab","AB"]

     print *,"**** TEST  1****"
     !invoke assign1
     aobj1=A(2)([1,2,3],c1)

     if(any(aobj1%i1 /= [1,2,3]))                            stop 16
     if(any(aobj1%c1 /= ["ab","aB","Ab","AB"]))              stop 17

     allocate(A(2) :: aobj2)
     allocate(A(2) :: aobj4(3))
     allocate(B(1) :: bobj2)
     allocate(B(1) :: bobj4(2))

     print *,"**** TEST  2****"
     call subA1(aobj2,aobj1)

     if(any(aobj2%i1 /= [1,2,3]))                            stop 18
     if(any(aobj2%c1 /= ["ab","aB","Ab","AB"]))              stop 19

     print *,"**** TEST  3****" 
     ! invoke assign2
     aobj3=[aobj1,A(2)([4,5,6],c1(1:2)),A(2)([-1,-2,-3],c1(3:4))]

     if(any(aobj3(1)%i1 /= [1,2,3]))                         stop 20
     if(any(aobj3(1)%c1 /= ["ab","aB","Ab","AB"]))           stop 21
     if(any(aobj3(2)%i1 /= [4,5,6]))                         stop 22
     if(any(aobj3(2)%c1 /= ["ab","aB"]))                     stop 23
     if(any(aobj3(3)%i1 /= [-1,-2,-3]))                      stop 24
     if(any(aobj3(3)%c1 /= ["Ab","AB"]))                     stop 25

     print *,"**** TEST  4****"
     call subA4(aobj4,aobj3)

     if(any(aobj4(1)%i1 /= [1,2,3]))                         stop 26
     if(any(aobj4(1)%c1 /= ["ab","aB","Ab","AB"]))           stop 27
     if(any(aobj4(2)%i1 /= [4,5,6]))                         stop 28
     if(any(aobj4(2)%c1 /= ["ab","aB"]))                     stop 29
     if(any(aobj4(3)%i1 /= [-1,-2,-3]))                      stop 30
     if(any(aobj4(3)%c1 /= ["Ab","AB"]))                     stop 31

     print *,"**** TEST  5****"
     ! invoke assign2
     bobj1=B(1)(aobj1,aobj4(2:3))

     if(any(bobj1%acomp1(1)%i1 /= [1,2,3]))                  stop 32
     if(any(bobj1%acomp1(1)%c1 /= ["ab","aB","Ab","AB"]))    stop 33

     if(any(bobj1%acomp2(1)%i1 /= [4,5,6]))                  stop 34
     if(any(bobj1%acomp2(1)%c1 /= ["ab","aB"]))              stop 35
     if(any(bobj1%acomp2(2)%i1 /= [-1,-2,-3]))               stop 36
     if(any(bobj1%acomp2(2)%c1 /= ["Ab","AB"]))              stop 37    

     print *,"**** TEST  6****"
     call subB1(bobj2,bobj1)

     if(any(bobj2%acomp1(1)%i1 /= [1,2,3]))                  stop 38
     if(any(bobj2%acomp1(1)%c1 /= ["ab","aB","Ab","AB"]))    stop 39

     if(any(bobj2%acomp2(1)%i1 /= [4,5,6]))                  stop 40
     if(any(bobj2%acomp2(1)%c1 /= ["ab","aB"]))              stop 41
     if(any(bobj2%acomp2(2)%i1 /= [-1,-2,-3]))               stop 42
     if(any(bobj2%acomp2(2)%c1 /= ["Ab","AB"]))              stop 43

     print *,"**** TEST  7****" 
     !invoke assign2
     bobj3=[bobj1, &
            B(1)(A(2)( [7,8,9],c1(1:1) ), &
                 aobj4(3:3) ) ]

     if(any(bobj3(1)%acomp1(1)%i1 /= [1,2,3]))                  stop 44
     if(any(bobj3(1)%acomp1(1)%c1 /= ["ab","aB","Ab","AB"]))    stop 45
     if(any(bobj3(1)%acomp2(1)%i1 /= [4,5,6]))                  stop 46
     if(any(bobj3(1)%acomp2(1)%c1 /= ["ab","aB"]))              stop 47
     if(any(bobj3(1)%acomp2(2)%i1 /= [-1,-2,-3]))               stop 48
     if(any(bobj3(1)%acomp2(2)%c1 /= ["Ab","AB"]))              stop 49

     if(any(bobj3(2)%acomp1(1)%i1 /= [7,8,9]))                  stop 50
     if(any(bobj3(2)%acomp1(1)%c1 /= ["ab"]))                   stop 51

     if(any(bobj3(2)%acomp2(1)%i1 /= [-1,-2,-3]))               stop 52
     if(any(bobj3(2)%acomp2(1)%c1 /= ["Ab","AB"]))              stop 53

     print *,"**** TEST  8****"
     call subB4(bobj4,bobj3)

     if(any(bobj4(1)%acomp1(1)%i1 /= [1,2,3]))                  stop 54
     if(any(bobj4(1)%acomp1(1)%c1 /= ["ab","aB","Ab","AB"]))    stop 55
     if(any(bobj4(1)%acomp2(1)%i1 /= [4,5,6]))                  stop 56
     if(any(bobj4(1)%acomp2(1)%c1 /= ["ab","aB"]))              stop 57
     if(any(bobj4(1)%acomp2(2)%i1 /= [-1,-2,-3]))               stop 58
     if(any(bobj4(1)%acomp2(2)%c1 /= ["Ab","AB"]))              stop 59

     if(any(bobj4(2)%acomp1(1)%i1 /= [7,8,9]))                  stop 60
     if(any(bobj4(2)%acomp1(1)%c1 /= ["ab"]))                   stop 61

     if(any(bobj4(2)%acomp2(1)%i1 /= [-1,-2,-3]))               stop 62
     if(any(bobj4(2)%acomp2(1)%c1 /= ["Ab","AB"]))              stop 63

end program
