!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. 2 Dummy arguments
!*  2. first dummy argument :
!*       first length type parameter is assumed
!*       second length type parameter is deferred
!*  3. second dummy argument:
!*       length type parameters are assumed
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(l1,l2)
      integer,len      :: l1,l2
      character(l1+l2) :: c
   end type

   contains

     subroutine sub1(arg1,arg2)
        type(dtp(*,:)),pointer,intent(inout) :: arg1
        type(dtp(*,*)),pointer,intent(in)    :: arg2

        arg1=>arg2

        call sub2(arg1)

        if(arg1%l1 /= 1)                    error stop 10_4
        if(arg1%l2 /= 7)                    error stop 11_4
        if(arg1%c  /= "xlftest")            error stop 12_4


        contains

           subroutine sub2(arg)
               type(dtp(*,:)),pointer,intent(inout) :: arg

               allocate(arg,source=dtp(1,7)("xlf"//"test"))
           end subroutine
     end subroutine

end module

program dummyArgDeferNonPoly2Dummy01
  use m
  implicit none

  type(dtp(1,:)),pointer :: dtp1=>null()
  type(dtp(1,2)), pointer :: dtp2=>null()
  type(dtp(1,2)),target  :: tar1=dtp(1,2)("xlf")

  dtp2=>tar1

  call sub1(dtp1,dtp2)

  if(dtp1%l1 /= 1)                         error stop 13_4
  if(dtp1%l2 /= 7)                         error stop 14_4
  if(dtp1%c  /= "xlftest")                 error stop 15_4

end program
