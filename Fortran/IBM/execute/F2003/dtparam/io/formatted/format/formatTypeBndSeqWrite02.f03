!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 9 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. test WRITE & PRINT statement in type-bound procedure
!* 2. derived type has nested derived type,and both derived type have multiple intrinsic components
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(l1)
     integer(8),len  :: l1
     integer         :: i1(l1) !l1=3
     character(l1+1) :: c1(l1)
     contains
        procedure,pass :: writeInner
        procedure,pass :: printInner
   end type

   type outer(l2)
     integer,len  :: l2
     logical      :: log1(l2) !l2=2
     real         :: r1(l2:l2)!l2=2
     complex(8)   :: x1
     type(inner(l2+1)) :: comp(l2)
     contains
        procedure,pass :: writeOuter
        procedure,pass :: printOuter
   end type

   contains
     subroutine writeInner(this)
       class(inner(*)),intent(in) :: this

        print *,"in writeInner"

        select type(this)
           type is(inner(*))
             write(6,101) this

           101 format(2(3i5/3a3))
           class default
             stop 11
        end select

     end subroutine

     subroutine printInner(this)
        class(inner(*)),intent(in) :: this

        print *,"in printInner"

        select type(this)
           type is(inner(*))
             print 103, this

           103 format(2(3i5/3a3))
           class default
             stop 13
        end select
     end subroutine

     subroutine writeOuter(this)
        class(outer(*)),intent(in) :: this

        print *,"in writeOuter"
        select type(this)
           type is(outer(*))
             write(6,100) this

             call this%comp(1)%printInner
             call this%comp(2)%printInner

             100 format(1x,2l4,sp,/f7.1/f7.1,f7.1,2(/3i5/3a3))

           class default
             stop 10
        end select

     end subroutine

     subroutine printOuter(this)
        class(outer(*)),intent(in) :: this

        print *,"in printOuter"

        select type(this)
           type is(outer(*))
             print 102,this

             call this%comp(1)%printInner
             call this%comp(2)%printInner

             102 format(1x,2l4,sp,/f7.1/f7.1,f7.1,2(/3i5/3a3))

           class default
             stop 12
        end select
     end subroutine

end module

program formatTypeBndSeqWrite02
  use m
  implicit none

  class(outer(:)),allocatable :: outer1

  allocate(outer(2) :: outer1)

  outer1%log1=[.true.,.false.]
  outer1%r1=-2.75
  outer1%x1=(2.22,-2.22)

  outer1%comp=[inner(3)(i1=[123,456,789],c1=["blue","greg","dark"]), &
               inner(3)(i1=[-123,-456,-789],c1=["abcd","efgh","ijkl"])]

  call outer1%writeOuter
  call outer1%printOuter

end program
