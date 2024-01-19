! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C463/genericC463Assignment002d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : C463: first argument being INTENT(IN) or
!*                                     second argument not INTENT(INOUT) or INTENT(OUT)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: myagmt => btob
         generic :: assignment(=) => myagmt
   end type

   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: j
      contains
         procedure, pass :: myagmt => b1tob1
         procedure, pass :: myarrayagmt => b1arraytob1
         generic :: assignment(=) => myagmt, myarrayagmt
   end type

   contains

   subroutine btob ( a, b )
      class(base(4)) :: a, b
      intent(in) :: a !<- the first argument cannot have intent(in)
      intent(in) :: b

      print *, a%i, b%i

   end subroutine

   subroutine b1tob1 ( a, b )
      class(base1(4)) :: a, b
      intent(inout) :: a !<- the first argument can have intent(inout)
                         !<- second argument must have intent(in)
      print *, a%j, b%j

   end subroutine

   subroutine b1arraytob1 ( a, b )
      class(base1(4)) :: a, b(:)
      intent(out) :: a    !<- the first argument can have intent(out)
      intent(inout) :: b !<- the second argument cannot have intent(inout)

      print *, a%j, b%j

   end subroutine

end module

program genericC463Assignment002d
end

