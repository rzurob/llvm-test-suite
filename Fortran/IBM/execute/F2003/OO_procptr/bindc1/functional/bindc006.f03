!*  ===================================================================
!*
!*  DATE                       : 06/07/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Procedure Pointer with BIND(C) feature
!*                                        Procedure Pointer pointing at C functions with BIND(C) function interface,
!*                                        And invoke another have nested procedure pointer calls; and with logical dummy arguments(C_BOOL)
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

   use ISO_C_BINDING, only: C_INT, C_BOOL

   interface
      subroutine neg(i) BIND(C)
         import C_BOOL
         LOGICAL(C_BOOL), intent(inout) :: i
      end subroutine
   end interface

   interface
      logical(C_BOOL) function cptr ( i1, i2 ) BIND(C)
         import C_BOOL
         LOGICAL(C_BOOL), intent(in), value :: i1, i2
      end function
   end interface

   interface
      logical(C_BOOL) function and ( i1, i2 ) BIND(C, name='c_and')
         import C_BOOL
         LOGICAL(C_BOOL), intent(in), value :: i1, i2
      end function
   end interface

   interface
      logical(C_BOOL) function or ( i1, i2 ) BIND(C, name='c_or')
         import C_BOOL
         LOGICAL(C_BOOL), intent(in), value :: i1, i2
      end function
   end interface

end module

program bindc006
   use m

   logical(C_BOOL) :: l1, l2, l3
   procedure(neg), pointer :: p1
   procedure(and), pointer :: p2, p3

   l1 = .true.
   l2 = .false.

   p1 => neg
   p2 => and
   p3 => or

   l3 = p2 ( p3(l1, l2), .true._C_BOOL )
   if ( l3 .neqv. .true._C_BOOL ) error stop 1_4

   call p1(l1)
   call p1(l2)

   if ( ( l1 .neqv. .false._C_BOOL ) .or. ( l2 .neqv. .true._C_BOOL ) ) error stop 2_4
   if ( associated(p1, p2) .or. associated(p2, p3) .or. associated(p1, p3) ) error stop 3_4

   p2 => p3

   l1 = p3( ( p2 ( l1, .false._C_BOOL ) .or. .true._C_BOOL ) , .false._C_BOOL )

   if ( l1 .neqv. .true._C_BOOL ) error stop 4_4

end
