!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 10 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359977
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner
     integer     :: i(1:3)
   end type
   type outer(l2)
     integer,len  :: l2
     character(l2+1)  :: c(1:2)
     type(inner) :: comp
     contains
        procedure,pass :: writeOuter
   end type

   contains
      subroutine writeOuter(this)
          class(outer(*)),intent(in) :: this
          select type(this)
             type is(outer(*))
                print *,this%c,this%comp%i
             class default
                stop
          end select
      end subroutine
end module

program d359977

  use m
  implicit none

  type(outer(2)) :: outer1

  outer1%c=["foo","pot"]
  outer1%comp=inner(i=[123,-456,789])

  print *,outer1%c,outer1%comp%i

  call outer1%writeOuter

end program
