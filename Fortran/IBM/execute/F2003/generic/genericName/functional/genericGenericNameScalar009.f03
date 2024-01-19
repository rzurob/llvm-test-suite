!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic, specific bindings and the subroutine itself with polymorphic types
!*                                             with different argument types
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

   type square
      integer :: side
      contains
         procedure, pass(a) :: getarea
         procedure, pass(a) :: getareaformultiple
         generic :: area => getarea, getareaformultiple
   end type

   type, extends(square) :: cube
      integer :: height
      contains
         procedure, pass(a) :: getvolume
         procedure, pass(a) :: getvolumeformultiple
         generic :: volume => getvolume, getvolumeformultiple
   end type


   contains

      integer function getarea ( a )
         class(square), intent(in) :: a

         getarea = a%side * a%side

         print *,'getarea'

      end function

      integer function getareaformultiple ( a, i )
         integer, intent(in) :: i
         class(square), intent(in) :: a

         getareaformultiple = a%getarea() * i
         print *,'getformultiple'

      end function

      integer function getvolume ( a )
         class(cube), intent(in) :: a

         getvolume = a%getarea() * a%height
         print *,'getvolume'

      end function

      integer function getvolumeformultiple ( a , i )
         class(cube), intent(in) :: a
         integer, intent(in) :: i

         getvolumeformultiple = a%getvolume() * i

         print *,'getvolumeformultiple'

      end function

end module

program genericGenericNameScalar009
   use m

   type(square) :: s1
   class(square), allocatable :: s2

   type(cube) :: c1
   class(cube), pointer :: c2

   integer i

   allocate ( s2, source = square(12) )
   allocate ( c2, source = cube(12,12) )

   s1 = square(side=10)
   c1 = cube(11,11)

   i = s1%getarea()
   print *,i

   i = s2%getarea()
   print *,i

   i = c1%getarea()
   print *,i

   i = c2%getarea()
   print *,i

   i = c1%getvolume()
   print *,i

   i = c2%getvolume()
   print *,i

   deallocate ( s2 )
   allocate ( s2, source = cube(100,100))
   select type ( s2 )
      type is ( cube )
         i = s2%getarea()
         print *,i
         i = s2%getvolume()
         print *,i
   end select

   associate ( g => square(50) )
      i = g%getarea()
      print *,i
   end associate

   associate ( h => cube(50,30) )
      i = h%getarea()
      print *,i

      i = h%getvolume()
      print *,i
   end associate

end program
