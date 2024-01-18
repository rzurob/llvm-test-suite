! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-01-18
!*
!*  DESCRIPTION                : miscellaneous (resolving type-bound: name conflict)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module generic_procedure_module

   type :: point
       real :: x,y

   contains
       procedure, nopass :: plus
   end type point

   interface plus
       module procedure point_plus_point
       module procedure point_plus_real
   end interface plus

contains

   function point_plus_point(v1, v2)     result(v3)
       class(point), intent(in) :: v1, v2
       type(point) :: v3

       v3%x = v1%x + v2%x
       v3%y = v1%y + v2%y

   end function point_plus_point

   function point_plus_real(v1, s)     result(v3)
       class(point), intent(in) :: v1
       real, intent(in) :: s
       type(point) :: v3

       v3%x = v1%x + s
       v3%y = v1%y + s

   end function point_plus_real


end module generic_procedure_module


