!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatTypeBndSeqWrite01.f
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
!* 1. test WRITE & PRINT statement in type bound procedure
!* 2. derived type has character & integer ultimate components
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(k1,l1)
     integer(8),kind :: k1
     integer(8),len  :: l1
     integer(k1)     :: i(l1-1:l1+1) ! l1=2
     contains
        procedure,pass :: writeInner
        procedure,pass :: printInner
   end type

   type outer(k2,l2)
     integer,kind :: k2
     integer,len  :: l2
     character(l2+1)  :: c(k2-1:k2+1) ! k2=4 l2=2
     type(inner(2*k2,l2+1)) :: comp
     contains
        procedure,pass :: writeOuter
        procedure,pass :: printOuter
   end type

   contains

      subroutine writeInner(this)
          class(inner(8,*)),intent(in) :: this

          print *,"in writeInner"
          select type(this)
             type is(inner(8,*))
               write(*, '(ss,i7/g7.3/i7.3)') this
             class default
               error stop 101_4
          end select
      end subroutine

      subroutine printInner(this)
          class(inner(8,*)),intent(in) :: this
          print *, "in printInner"

          select type(this)
             type is(inner(8,*))
               print '(b16/o24/z10)',this
             class default
               error stop 103_4
          end select

      end subroutine

      subroutine writeOuter(this)
          class(outer(4,*)),intent(in) :: this
          print *,"in writeOuter"
          select type(this)
             type is(outer(4,*))
                write(*, '(a5/a5/a5,sp,/i7/g7.3/i7.3)') this
                call this%comp%writeInner
             class default
                 error stop 100_4
          end select
      end subroutine

      subroutine printOuter(this)
          class(outer(4,*)),intent(in) :: this
          print *,"in printOuter"
          select type(this)
             type is(outer(4,*))
                print '(a5/a5/a5,sp,/b16/o24/z10)', this
                call this%comp%printInner
             class default
                 error stop 102_4
          end select
      end subroutine

end module

program formatTypeBndSeqWrite01
  use m
  implicit none

  type(outer(4,2)),target :: tar1
  type(outer(4,:)),pointer :: outer1=>null()

  tar1%c=["foo","red","pot"]

  tar1%comp=inner(8,3)(i=[123,-456,789])

  outer1=>tar1

  call outer1%writeOuter

  call outer1%printOuter

end program
