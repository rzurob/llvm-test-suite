!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgDeferNonPoly2Dummy03.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Nov. 13 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1 actual argument tar is asscoiated with 2 different dummy argument in same procedure, it is modified through 2 dummy arguments after execution of procedure.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1,l2)
      integer,len      :: l1,l2
      character(l1+l2) :: c(l1:l2) 
   end type

   contains

   subroutine sub1
        type(dtp(:,:)),pointer        :: dtp1(:),dtp2(:)

        type(dtp(1,2)),target         :: tar(2:7)

        integer   :: i

        tar(2:4)=[dtp(1,2)(["000","111"]), &
                   dtp(1,2)(["aaa","bbb"]), &
                   dtp(1,2)(["123","456"])] 

        tar(5:7)=[dtp(1,2)(["222","333"]), &
                   dtp(1,2)(["ccc","ddd"]), &
                   dtp(1,2)(["789","012"])]

        dtp1=>tar(2:4)
        dtp2=>tar(5:7)

        call sub2(dtp1,dtp2)
    
        do i=lbound(tar,1), ubound(tar,1) 
           print *,tar(i)%c
        end do 
 
        contains

             subroutine sub2(arg1,arg2)
                type(dtp(*,*)),intent(inout) :: arg1(:)
                type(dtp(*,*)),intent(inout) :: arg2(:)
                character(3),dimension(1)    :: temp

                temp=arg1(ubound(arg1,1))%c(2:2)                 
                arg1(ubound(arg1,1))%c(2:2) = arg1(ubound(arg1,1))%c(1:1)
                arg1(ubound(arg1,1))%c(1:1)=temp

                temp=arg2(ubound(arg2,1))%c(2:2)
                arg2(ubound(arg2,1))%c(2:2) = arg2(ubound(arg2,1))%c(1:1)
                arg2(ubound(arg2,1))%c(1:1)=temp
 
                call sub3(arg1(lbound(arg1,1):ubound(arg1,1)-1) ,&
                          arg2(lbound(arg2,1):ubound(arg2,1)-1) )
             end subroutine       
   end subroutine

   subroutine sub3(arg1,arg2)
       type(dtp(*,*)),intent(inout)  :: arg1(:)
       type(dtp(*,*)),intent(inout)  :: arg2(:)
       integer :: i

       do i=lbound(arg1,1),lbound(arg1,1)
           arg1(i)%c=["444","555"]
       end do
       do i=lbound(arg2,1),lbound(arg2,1)
           arg2(i)%c=["666","777"]
       end do
        
   end subroutine
end module

program dummyArgDeferNonPoly2Dummy03
  use m
  implicit none

  type(dtp(:,:)),pointer :: dtp1(:)=>null()
   
  call sub1
  
end program

