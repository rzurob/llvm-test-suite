!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d362084.f
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
!* 1. defect 362084
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type dtp(l1)
      integer,len  :: l1

      character(l1),pointer :: cptr1(:)=>null()
   end type

   interface assignment(=)
      module procedure assignDT
   end interface
   contains

        subroutine assignDT(this,dt)
            type(dtp(*)),intent(inout) :: this(:)
            type(dtp(*)),intent(in)    :: dt(:)

            print *,"in assignDT"
            print *,lbound(this,1),ubound(this,1)

            do i=lbound(this,1),ubound(this,1)
                this(i)%cptr1=>dt(i)%cptr1

                print *,this(i)%cptr1 !<== cause segfault
            end do

        end subroutine
end module

program d362084
     use m
     implicit none

     type(dtp(3)),target :: tar1(2)

     type(dtp(:)),pointer :: ptr1(:)=>null()

     character(3),target :: ctar1(5)=["hat","cat","get","fat","net"]

     tar1=[dtp(3)(ctar1(1:2)), &
           dtp(3)(ctar1(3:5)) ]

     allocate(dtp(3) :: ptr1(3:4))
     ptr1=tar1(1:2)

end program
