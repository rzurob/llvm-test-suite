!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 5 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. defect 361854
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type A(l1)
      integer,len  :: l1  ! l1=4
      character(l1) :: c1(4)
      logical       :: g1(4)
      contains
         procedure,pass :: assignA
         generic,public :: assignment(=) => assignA
   end type

   type :: child(l2)
      integer,len :: l2 ! l2=4
      type(A(4)) :: a1comp(4:5)
      type(A(4)) :: a2comp(4:5)
      contains
         procedure,pass(this) :: assign=>assignChild
         generic,public :: assignment(=)=> assign
   end type

   contains
      elemental subroutine assignA(this,ta)
         class(A(*)),intent(inout) :: this
         class(A(*)),intent(in)    :: ta
         this%c1=ta%c1
         this%g1=ta%g1
      end subroutine
      elemental subroutine assignChild(this,dt)
         class(child(*)),intent(inout) :: this
         class(child(*)),intent(in)    :: dt

         select type(dt)
            type is(child(*))
                 this%a1comp=dt%a1comp ! call assignA
                 this%a2comp=dt%a2comp ! call assignA
         end select
      end subroutine

end module

program d361854
    use m

    type(child(4)) :: child1

    child1=                      child(4)(&
                     a1comp=[A(4)(["abcd","efgh","ijkl","mnop"], &
                                  [.true.,.false.,.true.,.false.]),&
                             A(4)(["ABCD","EFGH","IJKL","MNOP"] ,&
                                  [.false.,.true.,.false.,.true.] )] , &
                     a2comp=[A(4)(["test","team","fail","pass"], &
                                  [.false.,.true.,.true.,.false.]), &
                             A(4)(["TEST","TEAM","FAIL","PASS"], &
                                  [.true.,.false.,.false.,.true.] ) ] )

    print *,child1

end program

