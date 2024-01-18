! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar009.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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

   type square(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: side
      contains
         procedure, pass(a) :: getarea
         procedure, pass(a) :: getareaformultiple
         generic :: area => getarea, getareaformultiple
   end type

   type, extends(square) :: cube(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: height
      contains
         procedure, pass(a) :: getvolume
         procedure, pass(a) :: getvolumeformultiple
         generic :: volume => getvolume, getvolumeformultiple
   end type


   contains

      integer function getarea ( a )
         class(square(*,4)), intent(in) :: a

         getarea = a%side * a%side

         print *,'getarea'

      end function

      integer function getareaformultiple ( a, i )
         integer, intent(in) :: i
         class(square(*,4)), intent(in) :: a

         getareaformultiple = a%getarea() * i
         print *,'getformultiple'

      end function

      integer function getvolume ( a )
         class(cube(*,4,*,4)), intent(in) :: a

         getvolume = a%getarea() * a%height
         print *,'getvolume'

      end function

      integer function getvolumeformultiple ( a , i )
         class(cube(*,4,*,4)), intent(in) :: a
         integer, intent(in) :: i

         getvolumeformultiple = a%getvolume() * i

         print *,'getvolumeformultiple'

      end function

end module

program genericGenericNameScalar009
   use m

   type(square(20,4)) :: s1
   class(square(:,4)), allocatable :: s2

   type(cube(20,4,20,4)) :: c1
   class(cube(:,4,:,4)), pointer :: c2

   integer i

   allocate ( s2, source = square(20,4)(12) )
   allocate ( c2, source = cube(20,4,20,4)(12,12) )

   s1 = square(20,4)(side=10)
   c1 = cube(20,4,20,4)(11,11)

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
   allocate ( s2, source = cube(20,4,20,4)(100,100))
   select type ( s2 )
      type is ( cube(*,4,*,4) )
         i = s2%getarea()
         print *,i
         i = s2%getvolume()
         print *,i
   end select

   associate ( g => square(20,4)(50) )
      i = g%getarea()
      print *,i
   end associate

   associate ( h => cube(20,4,20,4)(50,30) )
      i = h%getarea()
      print *,i

      i = h%getvolume()
      print *,i
   end associate

end program
