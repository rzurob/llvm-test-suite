!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDTComp02a.f
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
!* 1. Test defined assignment with interface block
!* 2. Use elemental & non-elemental subroutines
!* 3. Derived type has multiple DT components
!234567490123456749012345674901234567490123456749012345674901234567490
module m
  type inner1(l1)
     integer,len   :: l1 ! l1=3
     character(l1) :: c1(l1:l1+1,l1:l1+1)="***"
  end type
  type inner2(l2)
     integer,len   :: l2 ! l2=4
     integer       :: i1(l2)=-99
  end type
  type outer(l3,l4)
    integer,len :: l3,l4 ! l3=3,l4=4
    logical     :: g1(l3:l4)=.false.
    type(inner1(l3))  :: inn1comp(l3:l4)
    type(inner2(l4))  :: inn2comp(l3:l4)
  end type

  interface assignment(=)
     module procedure assignInn1_1,assignInn1_2,&
         assignInn2_1,assignInn2_2,assignOut

  end interface assignment(=)
  contains
      elemental subroutine assignInn1_1(this,arg)
         class(inner1(*)),intent(inout) :: this
         class(inner1(*)),intent(in)    :: arg

         this%c1=arg%c1

      end subroutine

      subroutine assignInn1_2(this,char)
         type(inner1(*)),intent(inout) :: this
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
         class(outer(*,*)),intent(inout),pointer :: this
         class(outer(*,*)),intent(in),target    :: arg

         print *,"in assignOut"
         this=>getFun(arg)

      end subroutine

      function getFun(tar)
         class(outer(*,*)),intent(in),target :: tar
         class(outer(:,:)),pointer :: getFun

         print *,"in getFun"

         allocate(outer(tar%l3,tar%l4) :: getFun)

         getFun%g1=tar%g1
         getFun%inn1comp=tar%inn1comp ! call assignInn1_1
         getFun%inn2comp=tar%inn2comp ! call assignInn2_1

      end function

end module

program defAssignDTComp02a
  use m
  implicit none

   type(outer(3,4)),target  :: tar1,tar2

   type(outer(3,:)),pointer :: ptr=>null()

   tar1%g1=[.true.,.false.]

   ! call assignInn1_1
   tar1%inn1comp= &
    [inner1(3)(c1=reshape(["abc","def","ghi","jkl"],(/2,2/) ) ), &
     inner1(3)(c1=reshape(["ABC","DEF","GHI","JKL"],(/2,2/) ) ) ]

   ! call assignInn2_1
   tar1%inn2comp=[inner2(4)(i1=[10,11,12,13]),&
                  inner2(4)(i1=[-10,-11,-12,-13]) ]

   tar2%g1=[.false.,.true.]

   ! call assignInn1_2

   tar2%inn1comp(3)=reshape(["hat","get","fat","wet"],(/2,2/) )
   tar2%inn1comp(4)=reshape(["HAT","GET","FAT","WET"],(/2,2/) )

   ! call assignInn2_2

   tar2%inn2comp(3)=[1,2,3,4]
   tar2%inn2comp(4)=[-1,-2,-3,-4]

   ptr=>getFun(tar1) ! call intrinsic assignment

   !output ptr for verification
   write(*,fmt='(2l2,8a4,8i3)') ptr

   ptr=getFun(tar2) ! call assignOut

   ! output ptr for verification
   write(*,'(2l2,8a4,8i3)') ptr

end program
