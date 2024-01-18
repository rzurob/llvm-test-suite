!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : With no class hierarchy
!*                                 - distinguishable dummy args are now placed before name distinguished dummys
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

   type b1
      integer :: i
      contains
         procedure, pass(dtv) :: fourargs1
         generic :: fourargs => fourargs1
   end type

   type, extends(b1) :: c1
      contains
         procedure, pass(dtv) :: fourargs2
         generic :: fourargs => fourargs2
   end type

   contains

      subroutine fourargs1(y,z,w,x,dtv)
         type(b1), intent(in) :: w, y
         type(c1), intent(in) :: x, z
         class(b1), intent(in) :: dtv

         print *, 'fourargs1'

      end subroutine

      subroutine fourargs2(z,y,x,w,dtv)
         type(b1), intent(in) :: x, y
         class(c1), intent(in) :: w, z
         class(c1), intent(in) :: dtv  !<- pass-object cannot distinguish the two procedures

         print *, 'fourargs2'

      end subroutine

end module

program genericAmbiguityTypeBound029
   use genericname

   class(c1), allocatable :: c1_2
   type(b1) :: b1_2 = b1(10)
   type(c1) :: c1_1 = c1(100)

   allocate ( c1 :: c1_2 )

   call c1_2%fourargs( c1_1, b1_2, x=b1_2, w=c1_1 )
   call c1_2%fourargs( b1_2, c1_1, x=c1_2, w=b1_2 )

end program
