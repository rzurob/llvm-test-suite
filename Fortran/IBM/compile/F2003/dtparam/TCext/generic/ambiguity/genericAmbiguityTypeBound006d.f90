! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound006d.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : two argument with pass-arg to be first arg specified (for generic-name, operator, and assignment tb)
!*                                  - one arg polymorphic, the other non-polymorphic of the extended type
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module genericName

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: printi
         generic :: print => printi
         procedure, pass :: printj
         generic :: print => printj
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j
   end type

   contains

      subroutine printi(a, b)
         class(base(4)), intent(in) :: a
         class(base(4)), intent(in) :: b


      end subroutine

      subroutine printj(a, b)
         class(base(4)), intent(in) :: a
         type(child(4)), intent(in)  :: b


      end subroutine

end module

module binoperator

   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass :: addi
         generic :: operator(+) => addi
   end type

   type, extends(base1) :: child1    ! (4)
      integer(k2) :: j
      contains
         procedure, pass :: addj
         generic :: operator(+) => addj
   end type

   contains

      type(base1(4)) function addi(a, b)
         class(base1(4)), intent(in) :: a
         class(base1(4)), intent(in)  :: b

         addi = base1(4)(10)

      end function

      type(base1(4)) function addj(a, b)
         class(child1(4)), intent(in) :: a
         type(child1(4)), intent(in)  :: b

         addj = base1(4)(20)

      end function

end module

module assignment

   type base2(k3)    ! (4)
     integer, kind :: k3
     integer(k3)   :: i, j
      contains
         procedure, pass :: assgn1
         generic :: assignment(=) => assgn1
   end type

   type, extends(base2) :: child2    ! (4)
      contains
         procedure, pass :: assgn2
         generic :: assignment(=) => assgn2
   end type

   contains

      subroutine assgn1(a, b)
         class(base2(4)), intent(out) :: a
         class(base2(4)), intent(in)  :: b

      end subroutine

      subroutine assgn2(a, b)
         class(child2(4)), intent(out) :: a
         type(base2(4)), intent(in)  :: b

      end subroutine

end module

program genericAmbiguityTypeBound006d
end program
