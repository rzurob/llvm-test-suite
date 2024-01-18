!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 3 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with generic binding
!* 2. Use elemental & non-elemental subroutines
!* 3. Derived type has multiple DT components
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type inner1(l1)
     integer,len   :: l1 ! l1=3
     character(l1) :: c1(l1:l1+1,l1:l1+1)="***"
     contains
        procedure,pass :: assignInn1_1
        procedure,pass :: assignInn1_2
        generic :: assignment(=) => assignInn1_1,assignInn1_2
  end type

  type inner2(l2)
     integer,len   :: l2 ! l2=4
     integer       :: i1(l2)=-99
     contains
       procedure,pass :: assignInn2_1
       procedure,pass :: assignInn2_2
       generic :: assignment(=) => assignInn2_1,assignInn2_2
  end type

  type outer(l3,l4)
    integer,len :: l3,l4 ! l3=3,l4=4
    logical     :: g1(l3:l4)=.false.
    type(inner1(l3))  :: inn1comp(l3:l4)
    type(inner2(l4))  :: inn2comp(l3:l4)
    contains
      procedure,pass :: assignOut
      generic :: assignment(=) => assignOut
  end type

  private :: assignInn1_1,assignInn1_2,assignInn2_1,assignInn2_2,assignOut

  contains
      elemental subroutine assignInn1_1(this,arg)
         class(inner1(*)),intent(inout) :: this
         class(inner1(*)),intent(in)    :: arg

         this%c1=arg%c1

      end subroutine

      subroutine assignInn1_2(this,char)
         class(inner1(*)),intent(inout) :: this
         character(*),intent(in) :: char(:,:)

         print *,"in assignInn1_2"

         this%c1=char

      end subroutine

      elemental subroutine assignInn2_1(this,arg)
         class(inner2(*)),intent(inout) :: this
         class(inner2(*)),intent(in)  :: arg

         this%i1=arg%i1

      end subroutine

      subroutine assignInn2_2(this,int)
         class(inner2(*)),intent(inout) :: this
         integer,intent(in) :: int(:)

         print *,"in assignInn2_2"

         this%i1=int

      end subroutine

      subroutine assignOut(this,arg)
         class(outer(*,*)),intent(inout) :: this
         class(outer(*,*)),intent(in)    :: arg

         print *,"in assignOut"
         associate(x=>getFun(arg))
            this%g1=x%g1
            this%inn1comp=x%inn1comp ! call assignInn1_1
            this%inn2comp=x%inn2comp ! call assignInn2_1
         end associate

      end subroutine

      function getFun(arg)
         class(outer(*,*)),intent(in) :: arg
         class(outer(arg%l3,arg%l4)),allocatable :: getFun

         print *,"in getFun"

         allocate(getFun)

         getFun%g1=arg%g1
         getFun%inn1comp=arg%inn1comp ! call assignInn1_1
         getFun%inn2comp=arg%inn2comp ! call assignInn2_1

      end function

end module

program defAssignDTComp02b
  use m
  implicit none

   type(outer(3,4))  :: out1,out2

   type(outer(3,4)),allocatable :: out3

   allocate(out3)

   out1%g1=[.true.,.false.]

   ! call assignInn1_1
   out1%inn1comp= &
    [inner1(3)(c1=reshape(["abc","def","ghi","jkl"],(/2,2/) ) ), &
     inner1(3)(c1=reshape(["ABC","DEF","GHI","JKL"],(/2,2/) ) ) ]

   ! call assignInn2_1
   out1%inn2comp=[inner2(4)(i1=[10,11,12,13]),&
                  inner2(4)(i1=[-10,-11,-12,-13]) ]

   out2%g1=[.false.,.true.]

   ! call assignInn1_2

   out2%inn1comp(3)=reshape(["hat","get","fat","wet"],(/2,2/) )
   out2%inn1comp(4)=reshape(["HAT","GET","FAT","WET"],(/2,2/) )

   ! call assignInn2_2

   out2%inn2comp(3)=[1,2,3,4]
   out2%inn2comp(4)=[-1,-2,-3,-4]

   out3=getFun(out1) ! call getFun, then call ..., then call assignOut

   !output ptr for verification
   write(*,fmt='(2l2,8a4,8i3)') out3

   out3=getFun(out2) ! call getFun, then call ..., then call assignOut

   ! output ptr for verification
   write(*,'(2l2,8a4,8i3)') out3

end program
